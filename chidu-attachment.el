;;; chidu-attachment.el --- JMAP attachment cards and transfers -*- lexical-binding: t; -*-

;;; Commentary:

;; JMAP already supplies a flat immutable `attachments' list and a download
;; URI template.  This module adapts that evidence to Appkit's shared media-card
;; and atomic transfer protocols.  Attachment metadata remains in the Store;
;; downloaded bytes are a private cache owned by the live Chidu application.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jka-compr)
(require 'mm-decode)
(require 'mm-view)
(require 'url-util)
(require 'appkit-chat-ins)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-media-effect)
(require 'appkit-transaction)
(require 'chidu-surface-operation)
(require 'appkit-media-card)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-ui)
(require 'chidu-jmap-http)
(require 'chidu-jmap-types)
(require 'chidu-runtime)
(require 'chidu-store)

(defcustom chidu-attachment-cache-subdirectory "attachments"
  "Private attachment cache directory below `chidu-data-root'."
  :type 'string
  :group 'chidu)

(defcustom chidu-attachment-inline-text-byte-limit (* 1024 1024)
  "Maximum attachment bytes eligible for toggled inline text display."
  :type 'positive-integer
  :group 'chidu)

(cl-defstruct (chidu-attachment-acquisition
               (:constructor chidu-attachment-acquisition-create))
  "One authenticated Appkit transfer owned by a Chidu application."
  transfer
  lifecycle-handle
  authorization
  completed-p)

(declare-function chidu-parsed-message-open
                  "chidu-parsed-message"
                  (app source-context attachment &optional select))

(defun chidu-attachment-context-p (context)
  "Return non-nil when CONTEXT can own JMAP attachment resources."
  (or (chidu-store-email-body-context-p context)
      (chidu-store-parsed-blob-context-p context)))

(defun chidu-attachment-context-endpoint (context)
  "Return JMAP Endpoint carried by attachment CONTEXT."
  (cond
   ((chidu-store-email-body-context-p context)
    (chidu-store-email-body-context-endpoint context))
   ((chidu-store-parsed-blob-context-p context)
    (chidu-store-parsed-blob-context-endpoint context))
   (t
    (signal 'wrong-type-argument
            (list 'chidu-attachment-context-p context)))))

(defun chidu-attachment-context-account (context)
  "Return JMAP Account carried by attachment CONTEXT."
  (cond
   ((chidu-store-email-body-context-p context)
    (chidu-store-email-body-context-account context))
   ((chidu-store-parsed-blob-context-p context)
    (chidu-store-parsed-blob-context-account context))
   (t
    (signal 'wrong-type-argument
            (list 'chidu-attachment-context-p context)))))

(defun chidu-attachment-context-body (context)
  "Return display body carried by attachment CONTEXT, or nil."
  (cond
   ((chidu-store-email-body-context-p context)
    (chidu-store-email-body-context-body context))
   ((chidu-store-parsed-blob-context-p context)
    (when-let* ((message
                 (chidu-store-parsed-blob-context-message context)))
      (chidu-store-parsed-message-body message)))
   (t
    (signal 'wrong-type-argument
            (list 'chidu-attachment-context-p context)))))

(defun chidu-attachment-context-owner-key (context)
  "Return stable Store-independent resource owner key for CONTEXT."
  (cond
   ((chidu-store-email-body-context-p context)
    (list 'email
          (chidu-store-email-body-context-local-email-id context)))
   ((chidu-store-parsed-blob-context-p context)
    (list 'parsed-blob
          (chidu-store-parsed-blob-context-blob-id context)
          (chidu-store-parsed-blob-context-profile-version context)))
   (t
    (signal 'wrong-type-argument
            (list 'chidu-attachment-context-p context)))))

(defun chidu-attachment-key (context attachment)
  "Return stable application resource key for CONTEXT and ATTACHMENT."
  (list
   'chidu-attachment
   (chidu-store-account-account-id
    (chidu-attachment-context-account context))
   (chidu-attachment-context-owner-key context)
   (chidu-store-email-attachment-part-id attachment)
   (chidu-store-email-attachment-blob-id attachment)))

(defun chidu-attachment-display-name (attachment &optional ordinal)
  "Return safe user-facing filename for ATTACHMENT and optional ORDINAL."
  (let* ((wire-name (chidu-store-email-attachment-name attachment))
         (name
          (and
           (stringp wire-name)
           (string-trim
            (replace-regexp-in-string "[[:cntrl:]\r\n\t]+" " " wire-name)))))
    (if (and name (not (string-empty-p name)))
        (truncate-string-to-width name 240 nil nil "…")
      (format "attachment-%s"
              (or ordinal
                  (chidu-store-email-attachment-part-id attachment)
                  "file")))))

(defun chidu-attachment-kind (attachment)
  "Return Appkit media-card kind for ATTACHMENT."
  (let ((type (chidu-store-email-attachment-media-type attachment)))
    (cond
     ((string-prefix-p "image/" type) 'image)
     ((string-prefix-p "video/" type) 'video)
     ((string-prefix-p "audio/" type) 'audio)
     (t 'file))))

(defun chidu-attachment--image-p (attachment)
  "Return non-nil when ATTACHMENT is a JMAP image part."
  (string-prefix-p
   "image/" (chidu-store-email-attachment-media-type attachment)))

(defun chidu-attachment-attached-message-p (attachment)
  "Return non-nil when ATTACHMENT is parseable as an attached message."
  (member (chidu-store-email-attachment-media-type attachment)
          '("message/rfc822" "message/global")))

(defun chidu-attachment--cid-source-value (source)
  "Return decoded Content-ID named by cid SOURCE, or nil.

The URI scheme is case-insensitive.  Preserve the decoded addr-spec for
exact comparison with JMAP's immutable `cid' evidence rather than inventing
additional normalization."
  (when (and (stringp source)
             (string-match-p "\\`[cC][iI][dD]:" source))
    (let ((decoded
           (condition-case nil
               (url-unhex-string (substring source 4))
             (error nil))))
      (when decoded
        (setq decoded (string-trim decoded "[[:space:]<]+" "[[:space:]>]+"))
        (unless (string-empty-p decoded) decoded)))))

(defun chidu-attachment-embedded-image (context source)
  "Return CONTEXT's unique image attachment referenced by HTML SOURCE.

SOURCE may be a cid URL or an exact Content-Location value.  Ambiguous
evidence deliberately returns nil: the flat JMAP `attachments' projection
does not retain enough multipart structure to choose among duplicate
Content-IDs safely."
  (when-let* ((body (chidu-attachment-context-body context))
              (attachments (chidu-store-email-body-attachments body))
              ((stringp source))
              ((not (string-empty-p source))))
    (let ((cid (chidu-attachment--cid-source-value source))
          matches)
      (cl-loop
       for attachment across attachments
       when
       (and
        (chidu-attachment--image-p attachment)
        (if cid
            (equal cid (chidu-store-email-attachment-cid attachment))
          (equal source (chidu-store-email-attachment-location attachment))))
       do (push attachment matches))
      (and (= (length matches) 1) (car matches)))))

(defun chidu-attachment--open-kind (attachment)
  "Return Appkit open kind for ATTACHMENT."
  (pcase (chidu-attachment-kind attachment)
    ('image 'image)
    ('video 'video)
    (_ 'file)))

(defun chidu-attachment--safe-name (attachment)
  "Return bounded filesystem-safe name for ATTACHMENT."
  (let* ((name
          (appkit-media-sanitize-filename
           (chidu-attachment-display-name attachment)))
         (extension (file-name-extension name t))
         (base (file-name-base name)))
    (if (<= (length name) 160)
        name
      (concat (truncate-string-to-width base 140 nil nil "") extension))))

(defun chidu-attachment--cache-directory (app)
  "Return APP's private attachment cache directory, creating it safely."
  (let*
      ((runtime (chidu-app-runtime app))
       (root
        (if (chidu-runtime-p runtime)
            (chidu-runtime-data-root runtime)
          chidu-data-root))
       (directory
        (expand-file-name chidu-attachment-cache-subdirectory root)))
    (make-directory directory t) (set-file-modes directory 448)
    directory))

(defun chidu-attachment-cache-path (app context attachment)
  "Return deterministic private cache path for CONTEXT ATTACHMENT in APP."
  (let* ((key (prin1-to-string (chidu-attachment-key context attachment)))
         (digest (secure-hash 'sha256 key))
         (name (chidu-attachment--safe-name attachment)))
    (expand-file-name
     (format "%s-%s" digest name)
     (chidu-attachment--cache-directory app))))

(defun chidu-attachment--state-table (app)
  "Return APP's application-owned resource table."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is no longer running"))
  (chidu-app-resources app))

(defun chidu-attachment--valid-cache-file-p (file attachment)
  "Return non-nil when FILE matches ATTACHMENT's immutable byte size."
  (when (file-regular-p file)
    (let ((attributes (file-attributes file 'integer)))
      (and attributes
           (= (file-attribute-size attributes)
              (chidu-store-email-attachment-size attachment))))))

(defun chidu-attachment-state (app context attachment)
  "Return normalized transfer state for CONTEXT ATTACHMENT in APP."
  (let* ((key (chidu-attachment-key context attachment))
         (table (chidu-attachment--state-table app))
         (entry (copy-tree (or (gethash key table) '())))
         (path
          (or (plist-get entry :path)
              (chidu-attachment-cache-path app context attachment)))
         (status (plist-get entry :status)))
    (setq entry (plist-put entry :path path))
    (cond
     ((chidu-attachment--valid-cache-file-p path attachment)
      (unless (eq status 'downloading)
        (setq entry (plist-put entry :status 'downloaded)
              entry (plist-put entry :error nil))))
     ((file-exists-p path)
      (ignore-errors (delete-file path))
      (unless (eq status 'downloading)
        (setq entry (plist-put entry :status 'not-downloaded)
              entry (plist-put entry :error nil))))
     ((eq status 'downloaded)
      (setq entry (plist-put entry :status 'not-downloaded)
            entry (plist-put entry :error nil)))
     ((null status)
      (setq entry (plist-put entry :status 'not-downloaded))))
    (puthash key entry table)
    entry))

(defun chidu-attachment--put-state (app key entry)
  "Commit attachment ENTRY below KEY and request reader presentation."
  (when (appkit-app-live-p app)
    (puthash key entry (chidu-app-resources app))
    (maphash
     (lambda (_identity registered)
       (let ((surface (cdr registered)))
         (when (and (appkit-surface-live-p surface)
                    (memq (appkit-surface-type-mode (appkit-surface-type surface))
                          '(chidu-conversation-mode chidu-message-mode
                                                    chidu-parsed-message-mode)))
           (chidu-surface-refresh surface))))
     (appkit-app-surfaces app)))
  entry)

(defun chidu-attachment--template-values (context attachment)
  "Return URI-template values for CONTEXT and ATTACHMENT."
  (let ((account
         (chidu-attachment-context-account context)))
    `(("accountId" . ,(chidu-store-account-remote-account-id account))
      ("blobId" . ,(chidu-store-email-attachment-blob-id attachment))
      ("type" . ,(chidu-store-email-attachment-media-type attachment))
      ("name" . ,(chidu-attachment-display-name attachment)))))

(defun chidu-attachment-download-url (context attachment)
  "Expand and validate JMAP download URL for CONTEXT and ATTACHMENT."
  (let* ((endpoint (chidu-attachment-context-endpoint context))
         (values (chidu-attachment--template-values context attachment)))
    ;; RFC 8620 explicitly permits the Session to advertise a dedicated
    ;; download origin.  The authenticated Session observation is the trust
    ;; boundary; Appkit still forbids protocol downgrade and never enables
    ;; curl's cross-origin credential forwarding for redirects.
    (chidu-jmap--expand-url-template
     (chidu-store-endpoint-download-url endpoint)
     "downloadUrl" values (mapcar #'car values))))

(defun chidu-attachment--resource (context attachment &optional file)
  "Return canonical Appkit resource for CONTEXT ATTACHMENT and optional FILE."
  (appkit-media-resource-create
   :file file
   :url (chidu-attachment-download-url context attachment)
   :name (chidu-attachment-display-name attachment)
   :mime-type (chidu-store-email-attachment-media-type attachment)))

(defun chidu-attachment--authorization (context)
  "Return mutable Authorization value for attachment CONTEXT."
  (let* ((endpoint (chidu-attachment-context-endpoint context))
         (secret (chidu-runtime--endpoint-secret endpoint))
         authorization)
    (unwind-protect
        (setq authorization
              (chidu-jmap-http--authorization
               (chidu-store-endpoint-login endpoint)
               (chidu-store-endpoint-authentication endpoint)
               secret))
      (clear-string secret))
    authorization))

(defun chidu-attachment--verification-target (target)
  "Return deterministic hidden verification path beside final TARGET."
  (let* ((target (expand-file-name target))
         (directory (or (file-name-directory target) default-directory))
         (digest (substring (secure-hash 'sha256 target) 0 32)))
    (expand-file-name (format ".chidu-verify-%s.part" digest) directory)))

(defun chidu-attachment--file-size (file)
  "Return regular FILE size, or nil."
  (when-let* ((attributes
               (and (file-regular-p file)
                    (file-attributes file 'integer))))
    (file-attribute-size attributes)))

(defun chidu-attachment--commit-acquired-file (file target attachment)
  "Verify acquired FILE for ATTACHMENT, then atomically publish TARGET.

Appkit may invoke several interested listeners for one shared transfer.  The
first listener renames FILE to TARGET after verification; later listeners
validate and reuse that exact TARGET."
  (let* ((expected (chidu-store-email-attachment-size attachment))
         (file-size (chidu-attachment--file-size file))
         (target-size (chidu-attachment--file-size target)))
    (cond
     ((and file-size (= file-size expected))
      (condition-case commit-error
          (progn
            (rename-file file target t)
            (set-file-modes target #o600)
            target)
        ((error quit)
         ;; Keep the verification file through the current callback turn so
         ;; every listener on the shared transfer observes the same failure.
         (run-at-time 0 nil
                      (lambda (path)
                        (when (file-exists-p path)
                          (ignore-errors (delete-file path))))
                      file)
         (signal (car commit-error) (cdr commit-error)))))
     ((and (null file-size) target-size (= target-size expected))
      ;; A previous listener already verified and published this transfer.
      (set-file-modes target #o600)
      target)
     (t
      (when (file-exists-p file)
        (run-at-time 0 nil
                     (lambda (path)
                       (when (file-exists-p path)
                         (ignore-errors (delete-file path))))
                     file))
      (error "Attachment size mismatch: expected %d bytes, got %s"
             expected (or file-size target-size "no file"))))))

(defun chidu-attachment--finish-acquisition
    (acquisition callback value &optional preserve-authorization-p)
  "Finish ACQUISITION once and invoke CALLBACK with VALUE.

PRESERVE-AUTHORIZATION-P leaves the mutable header bytes untouched.  Appkit
uses independently cancelable listeners on a shared transfer; canceling one
listener must not zero the credential still needed by another queued listener."
  (unless (chidu-attachment-acquisition-completed-p acquisition)
    (setf (chidu-attachment-acquisition-completed-p acquisition) t)
    (when-let* ((handle
                 (chidu-attachment-acquisition-lifecycle-handle acquisition)))
      (appkit-retire-handle handle)
      (setf (chidu-attachment-acquisition-lifecycle-handle acquisition) nil))
    (when-let* ((authorization
                 (chidu-attachment-acquisition-authorization acquisition)))
      (unless preserve-authorization-p (clear-string authorization))
      (setf (chidu-attachment-acquisition-authorization acquisition) nil))
    (setf (chidu-attachment-acquisition-transfer acquisition) nil)
    (funcall callback value)))

(cl-defun chidu-attachment--acquire
    (owner context attachment target success error)
  "Acquire CONTEXT ATTACHMENT into TARGET below Appkit OWNER.\n\nOWNER is a live Appkit application or view.  SUCCESS receives the verified\npath; ERROR receives a readable reason.  The returned acquisition may be\ncanceled independently while Appkit retains its shared underlying transfer."
  (unless (or (appkit-app-live-p owner) (appkit-surface-live-p owner))
    (user-error "Chidu attachment owner is no longer live"))
  (unless (functionp success)
    (signal 'wrong-type-argument (list 'functionp success)))
  (unless (functionp error)
    (signal 'wrong-type-argument (list 'functionp error)))
  (let*
      ((authorization (chidu-attachment--authorization context))
       (transfer-target
        (chidu-attachment--verification-target target))
       (acquisition
        (chidu-attachment-acquisition-create :authorization
                                             authorization))
       (headers
        (list (cons "Authorization" authorization)
              (cons "Accept"
                    (chidu-store-email-attachment-media-type
                     attachment))))
       transfer)
    (condition-case setup-error
        (progn
          (setq transfer
                (appkit-media-copy-or-download-resource-async
                 (chidu-attachment--resource context attachment)
                 transfer-target
                 (lambda (file)
                   (condition-case verify-error
                       (chidu-attachment--finish-acquisition
                        acquisition success
                        (chidu-attachment--commit-acquired-file file
                                                                target
                                                                attachment))
                     ((error quit)
                      (chidu-attachment--finish-acquisition
                       acquisition error
                       (error-message-string verify-error)))))
                 (lambda (reason)
                   (let
                       ((text
                         (if (stringp reason) reason
                           (condition-case nil
                               (error-message-string reason)
                             (error (format "%S" reason))))))
                     (unless (equal text "transfer canceled")
                       (when (file-exists-p transfer-target)
                         (ignore-errors (delete-file transfer-target))))
                     (chidu-attachment--finish-acquisition
                      acquisition error text
                      (equal text "transfer canceled"))))
                 :headers headers))
          (setf (chidu-attachment-acquisition-transfer acquisition)
                transfer)
          (when
              (and (appkit-media-transfer-p transfer)
                   (not
                    (chidu-attachment-acquisition-completed-p
                     acquisition)))
            (if
                (or (appkit-app-live-p owner)
                    (appkit-surface-live-p owner))
                (setf
                 (chidu-attachment-acquisition-lifecycle-handle
                  acquisition)
                 (appkit-register-handle owner 'function transfer
                                         #'appkit-media-cancel-transfer))
              (appkit-media-cancel-transfer transfer))))
      ((error quit)
       (chidu-attachment--finish-acquisition acquisition error
                                             (error-message-string
                                              setup-error))))
    acquisition))

(defun chidu-attachment-cancel-acquisition (acquisition)
  "Cancel ATTACHMENT ACQUISITION when it is still active."
  (when (and (chidu-attachment-acquisition-p acquisition)
             (not (chidu-attachment-acquisition-completed-p acquisition)))
    (if-let* ((handle
               (chidu-attachment-acquisition-lifecycle-handle acquisition)))
        (appkit-cancel-handle handle)
      (when-let* ((transfer
                   (chidu-attachment-acquisition-transfer acquisition)))
        (appkit-media-cancel-transfer transfer)))
    t))

(defun chidu-attachment-download (surface context attachment)
  "Request an App-owned download, without opening a viewer."
  (unless (appkit-surface-live-p surface)
    (user-error "Chidu attachment reader is no longer live"))
  (let ((app (appkit-surface-app surface)))
    (appkit-app-send app (list 'chidu-attachment 'download app context attachment))))

(defun chidu-attachment-cancel-download (surface context attachment)
  "Cancel the App download listener, retaining independent reader listeners."
  (let ((app (appkit-surface-app surface)))
    (unless (eq (plist-get (chidu-attachment-state app context attachment) :status)
                'downloading)
      (user-error "Chidu: attachment is not downloading"))
    (appkit-app-send app (list 'chidu-attachment 'cancel app context attachment))))

(defun chidu-attachment-open (surface context attachment)
  "Send open intent to the exact initiating Generated SURFACE."
  (unless (appkit-surface-live-p surface)
    (user-error "Chidu attachment reader is no longer live"))
  (appkit-surface-send surface (list 'chidu-attachment 'open context attachment)))

(defun chidu-attachment-save-as
    (view context attachment &optional target)
  "Save ATTACHMENT from CONTEXT in VIEW to TARGET, prompting when nil."
  (let*
      ((app (appkit-surface-app view))
       (state (chidu-attachment-state app context attachment))
       (cached
        (and (eq (plist-get state :status) 'downloaded)
             (plist-get state :path)))
       (default-name (chidu-attachment--safe-name attachment))
       (target
        (expand-file-name
         (or target
             (read-file-name "Save attachment as: " nil default-name
                             nil default-name)))))
    (when
        (and (file-exists-p target)
             (not (yes-or-no-p (format "Overwrite %s? " target))))
      (user-error "Save canceled"))
    (if cached
        (appkit-media-copy-or-download-resource-async
         (appkit-media-resource-create :file cached :name default-name)
         target
         (lambda (file) (set-file-modes file 384)
           (message "Chidu: saved attachment -> %s" file))
         (lambda (reason)
           (message "Chidu: failed to save attachment: %s" reason)))
      (chidu-attachment--acquire app context attachment target
                                 (lambda (file)
                                   (message
                                    "Chidu: saved attachment -> %s"
                                    file))
                                 (lambda (reason)
                                   (message
                                    "Chidu: failed to save attachment: %s"
                                    reason))))))

(defconst chidu-attachment--inline-text-extra-types
  '("application/emacs-lisp"
    "application/x-emacs-lisp"
    "application/x-patch"
    "application/x-sh"
    "application/x-shellscript"
    "application/javascript")
  "Non-`text/' media types whose `mm-inline-media-tests' entry only fontifies.")

(defconst chidu-attachment--inline-text-refused-types
  '("text/html" "text/calendar" "text/x-vcard")
  "`text/' media types Chidu never routes through `mm-display-inline'.

Their Gnus handlers do more than fontify text: `text/html' renders through
`mm-shr', whose own defaults permit remote image fetches and so would bypass
Chidu's offline HTML policy; `text/calendar' builds Gnus article state and
RSVP buttons that mutate the server; `text/x-vcard' needs a package Chidu
does not depend on.")

(defun chidu-attachment--inline-text-p (attachment)
  "Return non-nil when ATTACHMENT has a bounded inline text renderer.

The decision uses only the server-declared media type and size; Chidu never
sniffs bytes or infers a part's kind from its name."
  (let ((media-type
         (chidu-store-email-attachment-media-type attachment))
        (size (chidu-store-email-attachment-size attachment)))
    (and
     (<= size chidu-attachment-inline-text-byte-limit)
     (not (member media-type chidu-attachment--inline-text-refused-types))
     (or (string-prefix-p "text/" media-type)
         (member media-type chidu-attachment--inline-text-extra-types)))))

(defun chidu-attachment--card-context-at-point (&optional exact-p)
  "Return the attachment card context at point.

When EXACT-P is non-nil, ignore the buffer's single-attachment fallback and
only accept a card whose text-property span contains point.  Conversation TAB
dispatch needs this distinction so a single attachment never claims unrelated
message or reply text."
  (if exact-p
      (let ((appkit-media-card-fallback-context-function nil))
        (appkit-media-card-context-at-point))
    (appkit-media-card-context-at-point)))

(defun chidu-attachment-card-at-point-p (&optional exact-p)
  "Return non-nil when point resolves to a Chidu attachment card.

EXACT-P has the same meaning as in `chidu-attachment--card-context-at-point'."
  (when-let* ((card (chidu-attachment--card-context-at-point exact-p))
              (payload (plist-get card :payload)))
    (and (chidu-attachment-context-p (plist-get payload :context))
         (chidu-store-email-attachment-p (plist-get payload :attachment)))))

(defun chidu-attachment--inline-target-at-point (&optional exact-p)
  "Return (VIEW CONTEXT ATTACHMENT) for an inline-capable card at point.\n\nWhen EXACT-P is non-nil, do not use the single-card fallback."
  (when-let*
      ((view (appkit-current-surface)) ((appkit-surface-live-p view))
       (card (chidu-attachment--card-context-at-point exact-p))
       (payload (plist-get card :payload))
       (context (plist-get payload :context))
       (attachment (plist-get payload :attachment))
       ((chidu-attachment-context-p context))
       ((chidu-store-email-attachment-p attachment))
       ((chidu-attachment--inline-text-p attachment)))
    (list view context attachment)))

(defun chidu-attachment-inline-toggle-available-p (&optional exact-p)
  "Return non-nil when point names an inline-capable attachment.

When EXACT-P is non-nil, point must be inside the exact card span."
  (and (chidu-attachment--inline-target-at-point exact-p) t))

(defun chidu-attachment--inline-visible-p (state attachment)
  "Return whether STATE requests inline display for ATTACHMENT."
  (if (plist-member state :inline-visible)
      (and (plist-get state :inline-visible) t)
    (and
     (eq (plist-get state :status) 'downloaded)
     (not
      (equal
       (chidu-store-email-attachment-disposition attachment)
       "attachment")))))

(defun chidu-attachment--toggle-inline (view context attachment)
  "Toggle bounded inline display of ATTACHMENT in VIEW and CONTEXT."
  (unless (chidu-attachment--inline-text-p attachment)
    (user-error
     "Chidu: attachment has no bounded inline text renderer"))
  (let*
      ((app (appkit-surface-app view))
       (key (chidu-attachment-key context attachment))
       (state (chidu-attachment-state app context attachment))
       (visible
        (not (chidu-attachment--inline-visible-p state attachment))))
    (setq state (plist-put state :inline-visible visible))
    (chidu-attachment--put-state app key state)
    (when
        (and visible
             (not
              (memq (plist-get state :status)
                    '(downloading downloaded))))
      (chidu-attachment-download view context attachment))))

(defun chidu-attachment--toggle-inline-at-point (&optional exact-p)
  "Toggle the inline-capable attachment at point and return non-nil.

Return nil without signalling when no suitable card exists.  EXACT-P disables
the buffer-local single-attachment fallback."
  (when-let* ((target (chidu-attachment--inline-target-at-point exact-p)))
    (apply #'chidu-attachment--toggle-inline target)
    t))

(defun chidu-attachment-toggle-inline-at-point ()
  "Toggle bounded inline display for the attachment resolved at point.

The contextual Transient may use the single-attachment fallback."
  (interactive)
  (unless (chidu-attachment--toggle-inline-at-point nil)
    (user-error "Chidu: no inline-capable attachment at point")))

(defun chidu-attachment-toggle-inline-at-point-exact ()
  "Toggle bounded inline display for the exact attachment card at point."
  (interactive)
  (unless (chidu-attachment--toggle-inline-at-point t)
    (user-error "Chidu: no inline-capable attachment card at point")))

(defun chidu-attachment--insert-inline-text (attachment file)
  "Insert downloaded ATTACHMENT FILE using Gnus' safe text renderer.

`mm-display-inline' finishes by moving point to `point-min', and its handlers
widen freely, so the call is confined to a narrowing that starts empty at
point.  Point always ends at the end of the inserted text, including when a
handler fails part way; without that the caller would resume writing at the
top of the buffer."
  (let ((source (generate-new-buffer " *chidu attachment text*"))
        (start (point))
        handle)
    (unwind-protect
        (progn
          (with-current-buffer source
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file))
          (setq
           handle
           (mm-make-handle
            source
            (append
             (list (chidu-store-email-attachment-media-type attachment))
             (when-let* ((charset
                          (chidu-store-email-attachment-charset attachment)))
               (list (cons 'charset charset))))
            'binary nil
            ;; Gnus derives the mode from this filename, so it must stay a
            ;; bare presentation name and never reach the filesystem.
            (list
             "inline"
             (cons 'filename
                   (chidu-attachment--safe-name attachment)))))
          (save-restriction
            (narrow-to-region start start)
            (unwind-protect
                (condition-case error-data
                    (let (;; Gnus skips ASCII decoding when this fallback is nil,
                          ;; also skipping CRLF conversion.  Decode ASCII normally
                          ;; without stripping meaningful CR from mixed-EOL patches.
                          (mail-parse-charset (or mail-parse-charset 'us-ascii))
                          (mm-html-inhibit-images t)
                          (mm-html-blocked-images ".")
                          (enable-local-variables nil)
                          (enable-dir-local-variables nil)
                          ;; Gnus fontifies through `set-auto-mode', whose mode
                          ;; hooks would otherwise run over untrusted bytes.
                          (delay-mode-hooks t)
                          ;; `mm-display-inline-fontify' calls
                          ;; `mm-decompress-buffer' with the part's own
                          ;; filename and force, so a server-chosen name like
                          ;; "notes.gz" would spawn gzip on untrusted bytes
                          ;; regardless of the declared media type.
                          (jka-compr-compression-info-list nil))
                      (mm-display-inline handle))
                  ((error quit)
                   (goto-char (point-max))
                   (insert
                    (propertize
                     (format "Unable to display attachment inline: %s\n"
                             (error-message-string error-data))
                     'face 'error))))
              (goto-char (point-max)))))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(defun chidu-attachment--insert-inline-content
    (view context attachment prefix)
  "Insert visible ATTACHMENT inline content for VIEW and CONTEXT.\n\nPREFIX is the Appkit prefix state applied to every generated line.  Controls\nremain in the contextual Transient and TAB dispatch rather than occupying a\nbutton row inside the message."
  (let*
      ((state
        (chidu-attachment-state (appkit-surface-app view) context
                                attachment))
       (visible (chidu-attachment--inline-visible-p state attachment)))
    (when (and visible (eq (plist-get state :status) 'downloaded))
      (let ((start (point)))
        (chidu-attachment--insert-inline-text attachment
                                              (plist-get state :path))
        (unless (bolp) (insert "\n"))
        (appkit-ui-apply-line-prefix start (point) prefix)))))

(defun chidu-attachment-card-context (surface context attachment &optional state)
  "Return media-card actions for ATTACHMENT in exact reader SURFACE."
  (let* ((app (appkit-surface-app surface))
         (state (or state (chidu-attachment-state app context attachment)))
         (status (plist-get state :status))
         (key (chidu-attachment-key context attachment))
         (opening (equal key (chidu-attachment--reader-open-key
                              (appkit-surface-model surface)))))
    (appkit-media-card-context-create
     :payload (list :context context :attachment attachment)
     :kind (chidu-attachment-kind attachment)
     :title (chidu-attachment-display-name attachment)
     :open-action (lambda () (chidu-attachment-open surface context attachment))
     :download-action
     (unless (or opening (memq status '(downloading downloaded)))
       (lambda () (chidu-attachment-download surface context attachment)))
     :cancel-action
     (cond
      (opening
       (lambda () (appkit-surface-send surface (list 'chidu-attachment 'cancel-open key))))
      ((eq status 'downloading)
       (lambda () (chidu-attachment-cancel-download surface context attachment))))
     :save-as-action (lambda () (chidu-attachment-save-as surface context attachment)))))

(defun chidu-attachment--embedded-card-context (view context attachment state)
  "Return media context for embedded ATTACHMENT in VIEW and CONTEXT using STATE.

An uncached embedded image treats its primary action as an explicit download,
not as an external open.  Once cached, the ordinary attachment context opens
the local image in Emacs."
  (let ((card (chidu-attachment-card-context view context attachment state)))
    (unless (eq (plist-get state :status) 'downloaded)
      (setq card
            (plist-put
             card :open-action
             (unless (eq (plist-get state :status) 'downloading)
               (lambda ()
                 (chidu-attachment-download view context attachment))))))
    card))

(defun chidu-attachment--embedded-label (attachment state alt)
  "Return concise placeholder label for ATTACHMENT STATE and HTML ALT."
  (let ((name
         (if (and (stringp alt) (not (string-empty-p (string-trim alt))))
             (string-trim alt)
           (chidu-attachment-display-name attachment))))
    (pcase (plist-get state :status)
      ('downloading (format "[loading embedded image: %s]" name))
      ('error (format "[embedded image failed: %s]" name))
      (_ (format "[embedded image: %s]" name)))))

(defun chidu-attachment-insert-embedded-image
    (view context source &optional alt)
  "Insert CONTEXT's JMAP image referenced by HTML SOURCE into VIEW.\n\nOnly exact `cid' or Content-Location evidence from the same Email is accepted.\nCached images use Appkit's bounded inline preview; uncached images remain an\nexplicitly actionable placeholder and never start a transfer during render.\nReturn the matched attachment, or nil when SOURCE has no unique safe match."
  (when-let*
      ((attachment (chidu-attachment-embedded-image context source)))
    (let*
        ((app (appkit-surface-app view))
         (state (chidu-attachment-state app context attachment))
         (file (plist-get state :path))
         (card
          (chidu-attachment--embedded-card-context view context
                                                   attachment state))
         (open-action (plist-get card :open-action)) (start (point))
         (label
          (chidu-attachment--embedded-label attachment state alt))
         preview)
      (unless (bolp) (insert "\n")) (setq start (point))
      (when
          (and (eq (plist-get state :status) 'downloaded)
               (appkit-media-inline-image-rendering-available-p))
        (setq preview
              (condition-case nil
                  (appkit-media-preview-image-from-file file)
                (error nil))))
      (if (appkit-media-image-object-valid-p preview)
          (appkit-media-insert-image-slices preview open-action nil
                                            label
                                            "Open embedded image")
        (let
            ((face
              (pcase (plist-get state :status)
                ('downloading 'shadow) ('error 'error) (_ 'link))))
          (insert (propertize label 'face face))
          (appkit-media-add-action-properties start (point)
                                              open-action
                                              (if
                                                  (eq
                                                   (plist-get state
                                                              :status)
                                                   'downloaded)
                                                  "Open embedded image"
                                                "Download embedded image"))))
      (unless (bolp) (insert "\n"))
      (add-text-properties start (point)
                           (list appkit-media-card-context-property
                                 card 'chidu-attachment-key
                                 (chidu-attachment-key context
                                                       attachment)
                                 'chidu-embedded-attachment t
                                 'rear-nonsticky
                                 (list
                                  appkit-media-card-context-property
                                  'chidu-attachment-key
                                  'chidu-embedded-attachment)))
      attachment)))

(cl-defun chidu-attachment-insert-cards
    (view context &key prefix properties embedded-attachments)
  "Insert CONTEXT's attachment cards for VIEW.\n\nPREFIX is an Appkit prefix state or string.  PROPERTIES are propagated across\nall generated card text.  EMBEDDED-ATTACHMENTS is the set already represented\ninside the rendered HTML body and is omitted from the duplicate download list."
  (when-let*
      ((body (chidu-attachment-context-body context))
       (attachments (chidu-store-email-body-attachments body))
       ((> (length attachments) 0)))
    (let
        ((visible
          (cl-loop for attachment across attachments unless
                   (memq attachment embedded-attachments) collect
                   attachment)))
      (when visible
        (appkit-chat-ins-insert-prefixed-line
         (format "Attachments · %d" (length visible)) :prefix prefix
         :face 'bold :properties properties)
        (cl-loop for attachment in visible for ordinal from 1 for
                 state =
                 (chidu-attachment-state (appkit-surface-app view)
                                         context attachment)
                 for card-context =
                 (chidu-attachment-card-context view context
                                                attachment)
                 do
                 (appkit-chat-ins-insert-media-card :kind
                                                    (chidu-attachment-kind
                                                     attachment)
                                                    :title
                                                    (chidu-attachment-display-name
                                                     attachment
                                                     ordinal)
                                                    :details
                                                    (list
                                                     (file-size-human-readable
                                                      (chidu-store-email-attachment-size
                                                       attachment)
                                                      'iec " " "B")
                                                     (chidu-store-email-attachment-media-type
                                                      attachment))
                                                    :meta
                                                    (when-let*
                                                        ((disposition
                                                          (chidu-store-email-attachment-disposition
                                                           attachment)))
                                                      (list
                                                       disposition))
                                                    :status
                                                    (appkit-chat-ins-media-transfer-status-text
                                                     state)
                                                    :prefix prefix
                                                    :title-face 'bold
                                                    :meta-face 'shadow
                                                    :properties
                                                    (append properties
                                                            (list
                                                             'chidu-attachment-key
                                                             (chidu-attachment-key
                                                              context
                                                              attachment)))
                                                    :context
                                                    card-context
                                                    :body-inserter
                                                    (when
                                                        (chidu-attachment--inline-text-p
                                                         attachment)
                                                      (lambda
                                                        (prefix-state)
                                                        (chidu-attachment--insert-inline-content
                                                         view context
                                                         attachment
                                                         prefix-state)))
                                                    :open-help-echo
                                                    (if
                                                        (chidu-attachment-attached-message-p
                                                         attachment)
                                                        "Open attached message"
                                                      "Open attachment")))))))

(defun chidu-attachment-single-card-context (view context)
  "Return CONTEXT's only attachment card context for VIEW, or nil."
  (when-let* ((body (and context
                         (chidu-attachment-context-body context)))
              (attachments (chidu-store-email-body-attachments body))
              ((= (length attachments) 1)))
    (chidu-attachment-card-context view context (aref attachments 0))))

(provide 'chidu-attachment)

;;; chidu-attachment.el ends here

(defun chidu-attachment--acquisition-effect
    (owner context attachment path key success failure)
  "Describe one independently cancelable verified acquisition listener."
  (appkit-effect-create
   :key key
   :start
   (lambda (_context _input _observe resolve reject)
     (if (chidu-attachment--valid-cache-file-p path attachment)
         (progn (funcall resolve path) nil)
       (let ((acquisition
              (chidu-attachment--acquire
               owner context attachment path resolve reject)))
         (appkit-cancellation-create
          :kind 'logical
          :cancel (lambda () (chidu-attachment-cancel-acquisition acquisition))))))
   :success success :failure failure))

(defun chidu-attachment-app-update (_context model message)
  "Commit application-owned acquisition state and describe deferred work."
  (pcase message
    (`(chidu-attachment download ,app ,context ,attachment)
     (let* ((key (chidu-attachment-key context attachment))
            (entry (chidu-attachment-state app context attachment))
            (path (plist-get entry :path)))
       (if (memq (plist-get entry :status) '(downloading downloaded))
           (appkit-next :model model :render appkit-render-none)
         (setq entry (plist-put entry :status 'downloading)
               entry (plist-put entry :error nil))
         (chidu-attachment--put-state app key entry)
         (appkit-next
          :model model :render appkit-render-none
          :commands
          (list (appkit-command-start-effect
                 (chidu-attachment--acquisition-effect
                  app context attachment path (list 'chidu-download key)
                  (lambda (_input file) (list 'chidu-attachment 'downloaded app context attachment file))
                  (lambda (_input reason) (list 'chidu-attachment 'download-failed app context attachment reason)))))))))
    (`(chidu-attachment ,(and phase (or 'downloaded 'download-failed)) ,app ,context ,attachment ,value)
     (let* ((key (chidu-attachment-key context attachment))
            (entry (chidu-attachment-state app context attachment)))
       (setq entry (plist-put entry :status (if (eq phase 'downloaded) 'downloaded 'error))
             entry (plist-put entry :error (unless (eq phase 'downloaded) value)))
       (when (eq phase 'downloaded) (setq entry (plist-put entry :path value)))
       (chidu-attachment--put-state app key entry)
       (appkit-next :model model :render appkit-render-none)))
    (`(chidu-attachment cancel ,app ,context ,attachment)
     (let* ((key (chidu-attachment-key context attachment))
            (entry (chidu-attachment-state app context attachment)))
       (if (not (eq (plist-get entry :status) 'downloading))
           (appkit-next-reject 'attachment-not-downloading)
         (setq entry (plist-put entry :status 'not-downloaded)
               entry (plist-put entry :error nil))
         (chidu-attachment--put-state app key entry)
         (appkit-next :model model :render appkit-render-none
                      :commands (list (appkit-command-cancel-effect (list 'chidu-download key)))))))
    (_ (appkit-next-reject (list 'unknown-attachment-message message)))))

(defun chidu-attachment--set-reader-phase (model phase &optional problem key)
  "Commit PHASE, PROBLEM and pending open KEY to the owning reader MODEL."
  (cl-typecase model
    (chidu-message-state
     (chidu-message--set-media-phase model phase problem key))
    (chidu-conversation-state
     (chidu-conversation--set-media-phase model phase problem key))
    (chidu-parsed-message-state
     (chidu-parsed-message--set-media-phase model phase problem key)))
  model)

(defun chidu-attachment-insert-reader-problem (model)
  "Render an attachment failure committed to reader MODEL."
  (let ((problem
         (cl-typecase model
           (chidu-message-state (chidu-message-state-media-message model))
           (chidu-conversation-state (chidu-conversation-state-media-message model))
           (chidu-parsed-message-state (chidu-parsed-message-state-media-message model)))))
    (when problem
      (appkit-with-content-update (appkit-current-surface)
        (save-excursion
          (goto-char (point-max))
          (insert (propertize (format "\nAttachment: %s\n" problem) 'face 'error)))))))

(defun chidu-attachment--presentation-effect (surface context attachment file)
  "Describe the reader-owned presentation of ATTACHMENT FILE."
  (appkit-effect-create
   :key 'chidu-attachment-presentation
   :input
   (if (eq (chidu-attachment--open-kind attachment) 'video)
       (appkit-media-video-presentation-create
        (appkit-media-resource-create :file file) :label "Chidu attachment")
     file)
   :start
   (cond
    ((chidu-attachment-attached-message-p attachment)
     (lambda (_context _input _observe resolve reject)
       ;; Opening another Generated reader initializes its own lifecycle.
       ;; Run that UI operation outside the initiating reader's active pass.
       (let* ((canceled nil)
              (timer
               (run-at-time
                0 nil
                (lambda ()
                  (when (and (not canceled) (appkit-surface-live-p surface))
                    (condition-case condition
                        (progn
                          (require 'chidu-parsed-message)
                          (funcall resolve
                                   (chidu-parsed-message-open
                                    (appkit-surface-app surface) context attachment t)))
                      ((error quit) (funcall reject (error-message-string condition)))))))))
         (appkit-cancellation-create
          :kind 'logical :cancel (lambda () (setq canceled t) (cancel-timer timer))))))
    ((eq (chidu-attachment--open-kind attachment) 'video)
     #'appkit-media-video-presentation-start)
    (t #'appkit-media-file-presentation-start))
   :success (lambda (_input _value) '(chidu-attachment presented))
   :failure (lambda (_input reason) (list 'chidu-attachment 'failed reason))))

(defun chidu-attachment-surface-update (_context model message)
  "Commit exact-reader media intent before acquisition or presentation."
  (let* ((surface (appkit-current-surface))
         (app (appkit-surface-app surface)))
    (pcase message
      (`(chidu-attachment open ,context ,attachment)
       (let ((attached (chidu-attachment-attached-message-p attachment)))
         (chidu-attachment--set-reader-phase
          model (if attached 'presenting 'acquiring) nil
          (unless attached (chidu-attachment-key context attachment)))
         (appkit-next
          :model model :render t
          :commands
          (list
           (appkit-command-cancel-effect 'chidu-attachment-open)
           (appkit-command-cancel-effect 'chidu-attachment-presentation)
           (appkit-command-start-effect
            (if attached
                (chidu-attachment--presentation-effect surface context attachment nil)
              (chidu-attachment--acquisition-effect
               surface context attachment
               (plist-get (chidu-attachment-state app context attachment) :path)
               'chidu-attachment-open
               (lambda (_input file) (list 'chidu-attachment 'acquired context attachment file))
               (lambda (_input reason) (list 'chidu-attachment 'failed reason)))))))))
      (`(chidu-attachment acquired ,context ,attachment ,file)
       (let ((entry (chidu-attachment-state app context attachment)))
         (setq entry (plist-put entry :status 'downloaded)
               entry (plist-put entry :path file)
               entry (plist-put entry :error nil))
         (chidu-attachment--put-state app (chidu-attachment-key context attachment) entry))
       (chidu-attachment--set-reader-phase model 'presenting)
       (appkit-next :model model :render t
                    :commands
                    (list (appkit-command-start-effect
                           (chidu-attachment--presentation-effect surface context attachment file)))))
      (`(chidu-attachment cancel-open ,key)
       (if (equal key (chidu-attachment--reader-open-key model))
           (progn
             (chidu-attachment--set-reader-phase model 'idle)
             (appkit-next :model model :render t
                          :commands (list (appkit-command-cancel-effect 'chidu-attachment-open))))
         (appkit-next-reject 'superseded-attachment-open)))
      (`(chidu-attachment failed ,reason)
       (chidu-attachment--set-reader-phase model 'error reason)
       (appkit-next :model model :render t))
      (`(chidu-attachment presented)
       (chidu-attachment--set-reader-phase model 'idle)
       (appkit-next :model model :render appkit-render-none))
      (_ (appkit-next-reject (list 'unknown-attachment-message message))))))

(defun chidu-attachment--reader-open-key (model)
  "Return the pending reader-owned attachment acquisition key in MODEL."
  (cl-typecase model
    (chidu-message-state (chidu-message-state-media-key model))
    (chidu-conversation-state (chidu-conversation-state-media-key model))
    (chidu-parsed-message-state (chidu-parsed-message-state-media-key model))))
