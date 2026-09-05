;;; chidu-compose.el --- Structured outbound mail editor -*- lexical-binding: t; -*-

;;; Commentary:

;; Chidu Compose is a dedicated editor for a structured `ComposeDocument'.
;; Generated field labels are presentation; the buffer stores only field values
;; and the body.  A local ComposeWorkspace is a crash/offline recovery journal,
;; not the JMAP Draft Email.  Server Draft publication and Submission are
;; separate durable workflows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'mail-parse)
(require 'appkit-compose)
(require 'appkit-core)
(require 'appkit-surface)
(require 'chidu-view-operation)
(require 'chidu-contact)
(require 'chidu-contact-model)
(require 'chidu-compose-resource)
(require 'chidu-draft)
(require 'chidu-jmap-types)
(require 'chidu-runtime)
(require 'chidu-store)

(declare-function chidu "chidu" ())
(declare-function chidu--available-account-choices "chidu" (app))
(declare-function chidu-dispatch "chidu-transient" ())
(declare-function chidu-home-account-at-point "chidu-root" ())

(defvar chidu--app)

(defface chidu-compose-field-label
  '((t :inherit font-lock-keyword-face :weight semi-bold))
  "Face for generated Compose field labels."
  :group 'chidu)

(defface chidu-compose-status
  '((t :inherit shadow))
  "Face for Compose workspace status."
  :group 'chidu)

(defconst chidu-compose--field-specs
  '((to . "To")
    (cc . "Cc")
    (bcc . "Bcc")
    (reply-to . "Reply-To")
    (subject . "Subject"))
  "Ordered editable Compose header fields and their generated labels.")

(defvar-local chidu-compose--view nil
  "Appkit view owning the current Compose buffer.")

(defvar-local chidu-compose--context nil
  "Current durable `chidu-store-compose-context'.")

(defvar-local chidu-compose--identity nil
  "Identity currently selected by the editable workspace.")

(defvar-local chidu-compose--field-ranges nil
  "Alist mapping Compose field symbols to start/end marker pairs.")

(defvar-local chidu-compose--field-overlays nil
  "Generated field-label overlays owned by the current buffer.")

(defvar-local chidu-compose--body-start nil
  "Marker at the beginning of the editable Compose body.")

(defvar-local chidu-compose--resources (vector)
  "Ordered stable resources owned by the current Compose document.")

(defvar-local chidu-compose--resource-overlay nil
  "Generated attachment panel overlay owned by the current buffer.")

(defvar-local chidu-compose--checkpointed-generation 0
  "Newest local source generation committed to the workspace journal.")

(defvar-local chidu-compose--checkpoint-error nil
  "Most recent local workspace checkpoint failure, or nil.")

(defvar-local chidu-compose--closing-p nil
  "Non-nil while an explicit close or discard owns buffer teardown.")

(defvar-local chidu-compose--close-after-checkpoint-p nil
  "Non-nil when a successful checkpoint should close this buffer.")

(defvar-local chidu-compose--normalizing-p nil
  "Non-nil while Chidu normalizes a structured header field.")

(defvar-local chidu-compose--address-candidates (vector)
  "Bounded recipient candidates for the current completion request.")

(defvar-local chidu-compose--completion-operation nil
  "Current cancelable JMAP Contacts completion operation, or nil.")

(defvar-local chidu-compose--completion-token nil
  "Opaque owner of the current recipient completion request.")

(defun chidu-compose--workspace ()
  "Return the current buffer's durable Compose workspace."
  (unless (chidu-store-compose-context-p chidu-compose--context)
    (error "Chidu Compose buffer has no durable workspace context"))
  (chidu-store-compose-context-workspace chidu-compose--context))

(defun chidu-compose--workspace-id ()
  "Return the current Compose workspace id."
  (chidu-store-compose-workspace-workspace-id
   (chidu-compose--workspace)))

(defun chidu-compose--runtime ()
  "Return the live Chidu runtime owning this Compose buffer."
  (unless (and (appkit-surface-live-p chidu-compose--view)
               (appkit-app-live-p
                (appkit-surface-app chidu-compose--view)))
    (user-error "This composition is detached from the Chidu application"))
  (chidu-app-runtime (appkit-surface-app chidu-compose--view)))

(defun chidu-compose--failure-text (failure)
  "Return concise text for Chidu FAILURE."
  (if (chidu-result-failure-p failure)
      (chidu-runtime-error-message failure)
    (format "%s" failure)))

(defun chidu-compose--dirty-p ()
  "Return non-nil when the buffer is newer than its recovery checkpoint."
  (> (appkit-compose-generation)
     chidu-compose--checkpointed-generation))

(defun chidu-compose--context-current-p (workspace-id)
  "Return non-nil when this buffer still owns WORKSPACE-ID."
  (and (chidu-store-compose-context-p chidu-compose--context)
       (equal workspace-id (chidu-compose--workspace-id))))

(defun chidu-compose--field-range (field)
  "Return marker range for Compose FIELD, or nil."
  (alist-get field chidu-compose--field-ranges))

(defun chidu-compose--field-text (field)
  "Return Compose FIELD text without properties."
  (when-let* ((range (chidu-compose--field-range field)))
    (buffer-substring-no-properties (car range) (cdr range))))

(defconst chidu-compose--address-fields '(to cc bcc reply-to)
  "Compose fields that accept RFC 5322 address-list completion.")

(defun chidu-compose--address-token-bounds ()
  "Return current top-level address token bounds, or nil.

Commas inside quoted display names, comments, or angle addresses do not split a
candidate token."
  (let* ((field (chidu-compose--field-at-point))
         (range (and (memq field chidu-compose--address-fields)
                     (chidu-compose--field-range field))))
    (when range
      (let ((start (marker-position (car range)))
            (finish (marker-position (cdr range)))
            (cursor nil)
            (quote-p nil)
            (escaped-p nil)
            (angle-depth 0)
            (comment-depth 0)
            separators)
        (setq cursor start)
        (while (< cursor finish)
          (let ((character (char-after cursor)))
            (cond
             (escaped-p (setq escaped-p nil))
             ((and quote-p (= character ?\\)) (setq escaped-p t))
             ((and (zerop comment-depth) (= character ?\"))
              (setq quote-p (not quote-p)))
             ((and (not quote-p) (= character ?\())
              (setq comment-depth (1+ comment-depth)))
             ((and (not quote-p) (> comment-depth 0) (= character ?\)))
              (setq comment-depth (1- comment-depth)))
             ((and (not quote-p) (zerop comment-depth) (= character ?<))
              (setq angle-depth (1+ angle-depth)))
             ((and (not quote-p) (zerop comment-depth)
                   (> angle-depth 0) (= character ?>))
              (setq angle-depth (1- angle-depth)))
             ((and (not quote-p) (zerop comment-depth) (zerop angle-depth)
                   (= character ?,))
              (push cursor separators))))
          (setq cursor (1+ cursor)))
        (let* ((position (point))
               (left
                (cl-loop for separator in separators
                         when (< separator position)
                         maximize separator))
               (right
                (cl-loop for separator in separators
                         when (>= separator position)
                         minimize separator))
               (token-start (if left (1+ left) start))
               (token-end (or right finish)))
          (while (and (< token-start token-end)
                      (memq (char-after token-start) '(?\s ?\t)))
            (setq token-start (1+ token-start)))
          (while (and (> token-end token-start)
                      (memq (char-before token-end) '(?\s ?\t)))
            (setq token-end (1- token-end)))
          (cons token-start token-end))))))

(defun chidu-compose--address-candidate-string (candidate)
  "Return RFC 5322 display text for address CANDIDATE."
  (let ((name (chidu-store-email-address-name candidate))
        (email (chidu-store-email-address-email candidate)))
    (if (and (stringp name) (not (string-empty-p name)))
        (mail-header-make-address name email)
      email)))

(defun chidu-compose--address-completion-table (strings)
  "Return an email-category completion table over STRINGS."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (category . email))
      (complete-with-action action strings string predicate))))

(defun chidu-compose-completion-at-point ()
  "Complete the current recipient token from cached candidates."
  (when-let* ((bounds (chidu-compose--address-token-bounds)))
    (let (strings)
      (cl-loop
       for candidate across chidu-compose--address-candidates
       do (push (chidu-compose--address-candidate-string candidate)
                strings))
      (list
       (car bounds) (cdr bounds)
       (chidu-compose--address-completion-table (nreverse strings))
       :exclusive 'no
       :annotation-function (lambda (_string) "  Contact")))))

(defun chidu-compose--completion-current-p
    (workspace-id field query token)
  "Return non-nil when WORKSPACE-ID completion TOKEN still owns FIELD QUERY."
  (and (chidu-compose--context-current-p workspace-id)
       (eq token chidu-compose--completion-token)
       (eq field (chidu-compose--field-at-point))
       (when-let* ((bounds (chidu-compose--address-token-bounds)))
         (equal query
                (string-trim
                 (buffer-substring-no-properties
                  (car bounds) (cdr bounds)))))))

(defun chidu-compose--completion-succeeded
    (buffer workspace-id field query token result)
  "Install recipient RESULT in BUFFER for WORKSPACE-ID FIELD QUERY TOKEN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (chidu-compose--completion-current-p
             workspace-id field query token)
        (setq-local
         chidu-compose--completion-token nil
         chidu-compose--completion-operation nil
         chidu-compose--address-candidates result)
        (completion-at-point)))))

(defun chidu-compose--completion-failed
    (buffer workspace-id token failure)
  "Report recipient completion FAILURE for BUFFER WORKSPACE-ID TOKEN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (chidu-compose--context-current-p workspace-id)
                 (eq token chidu-compose--completion-token))
        (setq-local chidu-compose--completion-token nil
                    chidu-compose--completion-operation nil)
        (message "Chidu recipient completion failed: %s"
                 (chidu-compose--failure-text failure))))))

(defun chidu-compose-complete-address ()
  "Complete the current address token using JMAP ContactCards.

The request is explicit and asynchronous.  Subscribed, readable server
ContactCard results are installed before the ordinary Emacs completion UI
opens."
  (interactive)
  (unless (memq (chidu-compose--field-at-point)
                chidu-compose--address-fields)
    (user-error "Point is not in an address field"))
  (when-let* ((operation chidu-compose--completion-operation))
    (chidu-runtime-cancel-operation (chidu-compose--runtime) operation))
  (let* ((buffer (current-buffer))
         (runtime (chidu-compose--runtime))
         (workspace-id (chidu-compose--workspace-id))
         (field (chidu-compose--field-at-point))
         (bounds (or (chidu-compose--address-token-bounds)
                     (user-error "Cannot identify the current address token")))
         (query
          (string-trim
           (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (token (make-symbol "chidu-compose-completion"))
         operation)
    (setq-local chidu-compose--completion-token token
                chidu-compose--completion-operation nil
                chidu-compose--address-candidates (vector))
    (setq
     operation
     (chidu-complete-compose-addresses
      runtime chidu-compose--context query 64
      (lambda (result)
        (chidu-compose--completion-succeeded
         buffer workspace-id field query token result))
      (lambda (failure)
        (chidu-compose--completion-failed
         buffer workspace-id token failure))))
    (when (eq token chidu-compose--completion-token)
      (setq-local chidu-compose--completion-operation operation))))

(defun chidu-compose--document ()
  "Return an immutable structured document from the current buffer."
  (unless (and (markerp chidu-compose--body-start)
               (marker-position chidu-compose--body-start))
    (error "Chidu Compose body marker is unavailable"))
  (chidu-store-compose-document-create
   :to (string-trim (or (chidu-compose--field-text 'to) ""))
   :cc (string-trim (or (chidu-compose--field-text 'cc) ""))
   :bcc (string-trim (or (chidu-compose--field-text 'bcc) ""))
   :reply-to (string-trim (or (chidu-compose--field-text 'reply-to) ""))
   :subject (string-trim (or (chidu-compose--field-text 'subject) ""))
   :body
   (buffer-substring-no-properties chidu-compose--body-start (point-max))
   :resource-ids
   (vconcat
    (cl-loop
     for resource across chidu-compose--resources
     collect (chidu-store-compose-resource-resource-id resource)))))

(defun chidu-compose--editable-position-p (position)
  "Return non-nil when POSITION belongs to a field value or the body."
  (or
   (cl-loop
    for (_field . (start . end)) in chidu-compose--field-ranges
    thereis (<= start position end))
   (and (markerp chidu-compose--body-start)
        (marker-position chidu-compose--body-start)
        (>= position chidu-compose--body-start))))

(defun chidu-compose--before-change (beg end)
  "Reject a user edit from BEG to END that crosses structured boundaries."
  (unless chidu-compose--normalizing-p
    (cond
     ((eq beg end)
      (unless (chidu-compose--editable-position-p beg)
        (user-error "Text cannot be inserted between Compose fields")))
     ((text-property-any beg end 'chidu-compose-boundary t)
      (user-error "Compose field boundaries cannot be removed")))))

(defun chidu-compose--normalize-field (range)
  "Replace line breaks inside header RANGE and preserve point."
  (pcase-let* ((`(,start . ,end) range)
               (text (buffer-substring-no-properties start end)))
    (when (string-match-p "[\r\n]" text)
      (let* ((point-offset
              (and (<= start (point) end) (- (point) start)))
             (normalized
              (replace-regexp-in-string "[\r\n]+" " " text)))
        (delete-region start end)
        (goto-char start)
        (insert normalized)
        (when point-offset
          (goto-char (+ start (min point-offset (length normalized)))))))))

(defun chidu-compose--normalize-header-fields (&rest _ignored)
  "Replace pasted line breaks in structured header fields with spaces."
  (unless chidu-compose--normalizing-p
    (let ((chidu-compose--normalizing-p t)
          (inhibit-modification-hooks t))
      (appkit-compose-without-tracking
        (dolist (entry chidu-compose--field-ranges)
          (chidu-compose--normalize-field (cdr entry)))))))

(defun chidu-compose--resource-name (resource)
  "Return a useful display name for Compose RESOURCE."
  (or (chidu-store-compose-resource-name resource)
      (format "attachment-%s"
              (substring
               (chidu-store-compose-resource-resource-id resource) 0 8))))

(defun chidu-compose--resource-state (resource)
  "Return concise storage state for Compose RESOURCE."
  (cond
   ((and (chidu-store-compose-resource-digest resource)
         (chidu-store-compose-resource-remote-blob-id resource))
    "uploaded")
   ((chidu-store-compose-resource-remote-blob-id resource) "server")
   ((chidu-store-compose-resource-digest resource) "local")
   (t "unavailable")))

(defun chidu-compose--resource-panel-string ()
  "Return generated attachment panel text for the current document."
  (when (> (length chidu-compose--resources) 0)
    (concat
     "

"
     (propertize "Attachments
" 'face 'chidu-compose-field-label)
     (string-join
      (cl-loop
       for resource across chidu-compose--resources
       collect
       (concat
        "  "
        (propertize (chidu-compose--resource-name resource) 'face 'bold)
        (propertize
         (format "  %s · %s · %s"
                 (file-size-human-readable
                  (chidu-store-compose-resource-size resource))
                 (chidu-store-compose-resource-media-type resource)
                 (chidu-compose--resource-state resource))
         'face 'shadow)))
      "
")
     "
")))

(defun chidu-compose--refresh-resource-panel ()
  "Refresh the generated attachment panel without changing source text."
  (unless (overlayp chidu-compose--resource-overlay)
    (setq-local chidu-compose--resource-overlay
                (make-overlay (point-max) (point-max) nil nil t))
    (overlay-put chidu-compose--resource-overlay 'evaporate nil))
  (move-overlay chidu-compose--resource-overlay (point-max) (point-max))
  (overlay-put chidu-compose--resource-overlay 'after-string
               (chidu-compose--resource-panel-string)))

(defun chidu-compose--clear-resource-overlay ()
  "Delete the generated attachment panel overlay."
  (when (overlayp chidu-compose--resource-overlay)
    (delete-overlay chidu-compose--resource-overlay))
  (setq-local chidu-compose--resource-overlay nil))

(defun chidu-compose--clear-field-overlays ()
  "Delete generated field overlays in the current buffer."
  (mapc #'delete-overlay chidu-compose--field-overlays)
  (setq chidu-compose--field-overlays nil))

(defun chidu-compose--insert-boundary ()
  "Insert one protected structural newline."
  (let ((start (point)))
    (insert "\n")
    (add-text-properties
     start (point)
     '(chidu-compose-boundary t
                              rear-nonsticky (chidu-compose-boundary)))))

(defun chidu-compose--insert-field (field label value)
  "Insert Compose FIELD with generated LABEL and editable VALUE."
  (let* ((start (copy-marker (point) nil))
         (overlay (make-overlay start start nil nil nil)))
    (overlay-put
     overlay 'before-string
     (propertize (format "%-10s " (concat label ":"))
                 'face 'chidu-compose-field-label
                 'rear-nonsticky t))
    (overlay-put overlay 'evaporate nil)
    (push overlay chidu-compose--field-overlays)
    (insert value)
    (let ((end (copy-marker (point) nil)))
      (push (cons field (cons start end)) chidu-compose--field-ranges)
      (chidu-compose--insert-boundary)
      (set-marker-insertion-type end t))))

(defun chidu-compose--render-document (document)
  "Install structured DOCUMENT in the current Compose buffer."
  (unless (chidu-store-compose-document-p document)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-document-p document)))
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (chidu-compose--normalizing-p t))
    (appkit-compose-without-tracking
      (chidu-compose--clear-field-overlays)
      (erase-buffer)
      (setq chidu-compose--field-ranges nil)
      (dolist (spec chidu-compose--field-specs)
        (let* ((field (car spec))
               (label (cdr spec))
               (value
                (pcase field
                  ('to (chidu-store-compose-document-to document))
                  ('cc (chidu-store-compose-document-cc document))
                  ('bcc (chidu-store-compose-document-bcc document))
                  ('reply-to
                   (chidu-store-compose-document-reply-to document))
                  ('subject
                   (chidu-store-compose-document-subject document)))))
          (chidu-compose--insert-field field label value)))
      (setq chidu-compose--field-ranges
            (nreverse chidu-compose--field-ranges))
      (chidu-compose--insert-boundary)
      (setq chidu-compose--body-start (copy-marker (point) nil))
      (insert (chidu-store-compose-document-body document)))
    (chidu-compose--refresh-resource-panel)))

(defun chidu-compose--field-at-point ()
  "Return the structured field containing point, or `body'."
  (or
   (cl-loop
    for (field . (start . end)) in chidu-compose--field-ranges
    when (<= start (point) end)
    return field)
   (and (marker-position chidu-compose--body-start)
        (>= (point) chidu-compose--body-start)
        'body)))

(defun chidu-compose--goto-field (field)
  "Move point to Compose FIELD."
  (if (eq field 'body)
      (goto-char chidu-compose--body-start)
    (if-let* ((range (chidu-compose--field-range field)))
        (goto-char (cdr range))
      (error "Unknown Compose field: %S" field))))

(defun chidu-compose-next-field ()
  "Move to the next Compose field, wrapping after the body."
  (interactive)
  (let* ((fields (append (mapcar #'car chidu-compose--field-specs)
                         '(body)))
         (current (or (chidu-compose--field-at-point) 'body))
         (tail (memq current fields)))
    (chidu-compose--goto-field
     (or (cadr tail) (car fields)))))

(defun chidu-compose-previous-field ()
  "Move to the previous Compose field, wrapping before the first field."
  (interactive)
  (let* ((fields (append (mapcar #'car chidu-compose--field-specs)
                         '(body)))
         (current (or (chidu-compose--field-at-point) 'body))
         (index (or (cl-position current fields) 0)))
    (chidu-compose--goto-field
     (nth (mod (1- index) (length fields)) fields))))

(defun chidu-compose-newline ()
  "Move to the next field from a header, or insert a body newline."
  (interactive)
  (if (eq (chidu-compose--field-at-point) 'body)
      (newline)
    (chidu-compose-next-field)))

(defun chidu-compose--status-text ()
  "Return current Compose workspace and server-Draft status text."
  (let* ((context chidu-compose--context)
         (workspace
          (and (chidu-store-compose-context-p context)
               (chidu-store-compose-context-workspace context)))
         (attempt
          (and (chidu-store-compose-context-p context)
               (chidu-store-compose-context-publish-attempt context))))
    (cond
     ((appkit-compose-operation-active-p)
      (or (appkit-compose-status-text) "Working…"))
     (chidu-compose--checkpoint-error
      (format "Recovery checkpoint failed: %s"
              (chidu-compose--failure-text chidu-compose--checkpoint-error)))
     ((and attempt
           (eq 'unknown
               (chidu-store-draft-publish-attempt-phase attempt)))
      (if (chidu-compose--dirty-p)
          "Local changes · previous Draft save outcome unknown"
        "Draft save outcome unknown"))
     ((chidu-compose--dirty-p) "Local changes")
     ((> (length
          (chidu-store-compose-context-cleanup-attempts
           chidu-compose--context))
         0)
      "Draft saved · predecessor cleanup pending")
     ((and workspace
           (eql (chidu-store-compose-workspace-published-revision workspace)
                (chidu-store-compose-workspace-revision workspace)))
      "Draft saved to server")
     (t "Recovery copy updated"))))

(defun chidu-compose--header-line ()
  "Return the current Compose header line."
  (if (not (chidu-store-identity-p chidu-compose--identity))
      ""
    (concat
     " From: "
     (propertize
      (format "%s <%s>"
              (chidu-store-identity-name chidu-compose--identity)
              (chidu-store-identity-email chidu-compose--identity))
      'face 'chidu-compose-field-label
      'help-echo "Change Identity with C-c C-i")
     (propertize
      (format "  ·  %s" (chidu-compose--status-text))
      'face 'chidu-compose-status))))

(defun chidu-compose--init (_context input)
  "Adopt the durable Compose INPUT without replacing editor text later."
  (appkit-next :model input :render appkit-render-none))

(defun chidu-compose--update (context model message)
  "Commit a checkpoint or process owned work for this Compose editor."
  (pcase message
    (`(:context ,value)
     (appkit-next :model value :render appkit-render-none))
    (_ (chidu-surface-update context model message))))

(defun chidu-compose--renderer (_surface)
  "Create the renderer for the actual editable Compose buffer."
  (appkit-generated-renderer-create
   :mount (lambda (surface _app-read-view _model)
            (chidu-compose--install-context surface))
   :merge (lambda (_older newer) newer)
   :render (lambda (_surface _app-read-view _model _change)
             (force-mode-line-update) nil)
   :recover (lambda (_surface _app-read-view _model _condition)
              (force-mode-line-update) nil)
   :unmount
   (lambda (_surface)
     (when chidu-compose--completion-operation
       (chidu-runtime-cancel-operation
        (chidu-app-runtime (appkit-surface-app chidu-compose--view))
        chidu-compose--completion-operation)
       (setq-local chidu-compose--completion-operation nil))
     (appkit-compose-session-mode -1)
     (chidu-compose--clear-field-overlays)
     (chidu-compose--clear-resource-overlay))))

(defconst chidu-compose--surface-type
  (appkit-surface-type-create
   :name 'chidu-compose :mode #'chidu-compose-mode
   :init #'chidu-compose--init :update #'chidu-compose--update
   :renderer-factory #'chidu-compose--renderer)
  "Surface descriptor owning the visible Compose editor.")

(defun chidu-compose--install-context (view)
  "Install durable Compose state from newly attached VIEW."
  (let* ((context (appkit-surface-model view))
         (workspace
          (and (chidu-store-compose-context-p context)
               (chidu-store-compose-context-workspace context))))
    (unless (chidu-store-compose-workspace-p workspace)
      (error "Chidu Compose view has invalid workspace state"))
    (setq-local
     chidu-compose--view view
     chidu-compose--context context
     chidu-compose--identity
     (chidu-store-compose-context-identity context)
     chidu-compose--resources
     (chidu-store-compose-context-resources context)
     chidu-compose--checkpointed-generation
     (chidu-store-compose-workspace-revision workspace)
     chidu-compose--checkpoint-error nil
     header-line-format '(:eval (chidu-compose--header-line)))
    (chidu-compose--render-document
     (chidu-store-compose-workspace-document workspace))
    (appkit-compose-setup
     :snapshot-function #'chidu-compose--document
     :state-change-function (lambda (_session) (force-mode-line-update))
     :generation (chidu-store-compose-workspace-revision workspace))
    (set-buffer-modified-p nil)
    (chidu-compose--goto-field 'to)))

(defun chidu-compose--adopt-context (context)
  "Install durable Compose CONTEXT in the current live editor."
  (let ((workspace (chidu-store-compose-context-workspace context)))
    (setq-local
     chidu-compose--context context
     chidu-compose--identity
     (chidu-store-compose-context-identity context)
     chidu-compose--resources
     (chidu-store-compose-context-resources context)
     chidu-compose--checkpointed-generation
     (chidu-store-compose-workspace-revision workspace)
     chidu-compose--checkpoint-error nil)
    (when (appkit-surface-live-p chidu-compose--view)
      (chidu-post-surface-message chidu-compose--view (list :context context)))
    (chidu-compose--refresh-resource-panel)
    context))

(defun chidu-compose--checkpoint-succeeded
    (buffer workspace-id owner generation context)
  "Settle BUFFER WORKSPACE-ID OWNER at GENERATION with durable CONTEXT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (chidu-compose--context-current-p workspace-id)
                 (appkit-compose-operation-current-p owner))
        (chidu-compose--adopt-context context)
        (setq-local chidu-compose--checkpointed-generation generation)
        (appkit-compose-operation-finish owner)
        (unless (chidu-compose--dirty-p)
          (set-buffer-modified-p nil))
        (force-mode-line-update)
        (when chidu-compose--close-after-checkpoint-p
          (setq-local chidu-compose--close-after-checkpoint-p nil)
          (if (chidu-compose--dirty-p)
              (message "Chidu: newer local changes remain open")
            (let ((chidu-compose--closing-p t))
              (kill-buffer buffer))))))))

(defun chidu-compose--checkpoint-failed
    (buffer workspace-id owner failure)
  "Settle failed checkpoint OWNER for BUFFER WORKSPACE-ID with FAILURE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (chidu-compose--context-current-p workspace-id)
                 (appkit-compose-operation-current-p owner))
        (setq-local chidu-compose--checkpoint-error failure
                    chidu-compose--close-after-checkpoint-p nil)
        (appkit-compose-operation-finish owner)
        (force-mode-line-update)
        (message "Chidu checkpoint failed: %s"
                 (chidu-compose--failure-text failure))))))

(defun chidu-compose--draft-publish-succeeded
    (buffer workspace-id owner result)
  "Settle BUFFER WORKSPACE-ID OWNER with Draft publication RESULT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (chidu-compose--context-current-p workspace-id)
                 (appkit-compose-operation-current-p owner))
        (let* ((context (chidu-draft-publish-result-context result))
               (status (chidu-draft-publish-result-status result))
               (error-kind (chidu-draft-publish-result-error-kind result)))
          (chidu-compose--adopt-context context)
          (appkit-compose-operation-finish owner)
          (unless (chidu-compose--dirty-p)
            (set-buffer-modified-p nil))
          (force-mode-line-update)
          (message
           "%s"
           (pcase status
             ('saved "Chidu: Draft saved to server")
             ('unchanged "Chidu: server Draft is current")
             ('cleanup-pending
              (format "Chidu: Draft saved; predecessor cleanup pending%s"
                      (if error-kind (format " (%s)" error-kind) "")))
             ('cleanup-conflict
              (format "Chidu: Draft saved; predecessor changed remotely%s"
                      (if error-kind (format " (%s)" error-kind) "")))
             ('rejected
              (format "Chidu: server rejected Draft save%s"
                      (if error-kind (format " (%s)" error-kind) "")))
             ('unknown
              (format "Chidu: Draft save outcome is unknown%s"
                      (if error-kind (format " (%s)" error-kind) "")))
             (_ (format "Chidu: Draft publication %s" status)))))))))

(defun chidu-compose--draft-publish-failed
    (buffer workspace-id owner failure)
  "Settle failed server Draft OWNER for BUFFER WORKSPACE-ID with FAILURE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (chidu-compose--context-current-p workspace-id)
                 (appkit-compose-operation-current-p owner))
        (appkit-compose-operation-finish owner)
        (force-mode-line-update)
        (message "Chidu Draft save failed: %s"
                 (chidu-compose--failure-text failure))))))

(defun chidu-compose-save-draft ()
  "Save the current structured document as a JMAP server Draft."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "A Compose operation is already in progress"))
  (let* ((buffer (current-buffer))
         (runtime (chidu-compose--runtime))
         (workspace (chidu-compose--workspace))
         (workspace-id
          (chidu-store-compose-workspace-workspace-id workspace))
         (capture (appkit-compose-capture))
         (generation (plist-get capture :generation))
         (document (plist-get capture :value))
         (owner
          (appkit-compose-operation-begin
           'saving-draft
           :generation generation
           :label "Saving Draft to server…"))
         operation)
    (setq-local chidu-compose--checkpoint-error nil)
    (setq
     operation
     (chidu-publish-draft
      runtime chidu-compose--context chidu-compose--identity
      generation document
      (apply-partially
       #'chidu-compose--draft-publish-succeeded
       buffer workspace-id owner)
      (apply-partially
       #'chidu-compose--draft-publish-failed
       buffer workspace-id owner)))
    (when (appkit-compose-operation-current-p owner)
      (appkit-compose-operation-update
       owner
       :cancel-function
       (lambda ()
         (chidu-runtime-cancel-operation runtime operation))))
    owner))

(defun chidu-compose-checkpoint ()
  "Checkpoint the current structured document to local recovery storage."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "A Compose operation is already in progress"))
  (if (not (chidu-compose--dirty-p))
      (message "Chidu: recovery copy is current")
    (let* ((buffer (current-buffer))
           (runtime (chidu-compose--runtime))
           (workspace (chidu-compose--workspace))
           (workspace-id
            (chidu-store-compose-workspace-workspace-id workspace))
           (capture (appkit-compose-capture))
           (generation (plist-get capture :generation))
           (document (plist-get capture :value))
           (owner
            (appkit-compose-operation-begin
             'checkpointing
             :generation generation
             :label "Updating recovery copy…")))
      (setq-local chidu-compose--checkpoint-error nil)
      (chidu-runtime-checkpoint-compose-workspace
       runtime workspace-id
       (chidu-store-identity-identity-id chidu-compose--identity)
       (chidu-store-compose-workspace-revision workspace)
       generation document
       (apply-partially
        #'chidu-compose--checkpoint-succeeded
        buffer workspace-id owner generation)
       (apply-partially
        #'chidu-compose--checkpoint-failed
        buffer workspace-id owner))
      owner)))

(defun chidu-compose--kill-query ()
  "Protect active effects and uncheckpointed changes before buffer death."
  (cond
   (chidu-compose--closing-p t)
   ((appkit-compose-operation-active-p)
    (message "Chidu: wait for the current Compose operation to finish")
    nil)
   ((not (chidu-compose--dirty-p)) t)
   ((yes-or-no-p "Discard uncheckpointed Compose changes? ") t)
   (t nil)))

(defun chidu-compose-close ()
  "Close the current Compose buffer with an explicit local-change policy."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current Compose operation to finish"))
  (if (not (chidu-compose--dirty-p))
      (let ((chidu-compose--closing-p t))
        (kill-buffer (current-buffer)))
    (pcase
        (read-char-choice
         "Local changes: [c]heckpoint and close, [d]iscard changes, [q] cancel "
         '(?c ?d ?q))
      (?c
       (setq-local chidu-compose--close-after-checkpoint-p t)
       (chidu-compose-checkpoint))
      (?d
       (let ((chidu-compose--closing-p t))
         (kill-buffer (current-buffer))))
      (?q (message "Close canceled")))))

(defun chidu-compose-discard-workspace ()
  "Delete a local-only Compose workspace after confirmation."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current Compose operation to finish"))
  (let ((workspace (chidu-compose--workspace)))
    (when (or
           (chidu-store-compose-workspace-base-remote-email-id workspace)
           (chidu-store-compose-context-publish-attempt
            chidu-compose--context)
           (> (length
               (chidu-store-compose-context-cleanup-attempts
                chidu-compose--context))
              0))
      (user-error
       "This workspace owns server Draft evidence; remote discard is not implemented")))
  (unless (yes-or-no-p
           "Delete this local Compose workspace and its recovery copy? ")
    (user-error "Discard canceled"))
  (let* ((buffer (current-buffer))
         (runtime (chidu-compose--runtime))
         (workspace (chidu-compose--workspace))
         (workspace-id
          (chidu-store-compose-workspace-workspace-id workspace)))
    (chidu-runtime-discard-compose-workspace
     runtime workspace-id
     (chidu-store-compose-workspace-revision workspace)
     (lambda (_discarded-id)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((chidu-compose--closing-p t))
             (kill-buffer buffer))))
       (message "Chidu: local Compose workspace deleted"))
     (lambda (failure)
       (message "Chidu: cannot delete workspace: %s"
                (chidu-compose--failure-text failure))))))

(defun chidu-compose-send-unavailable ()
  "Refuse sending until Chidu's EmailSubmission reducer exists."
  (interactive)
  (user-error
   "Chidu can save server Drafts, but sending is not implemented yet"))

(defun chidu-compose--resource-edit-succeeded
    (buffer workspace-id owner context)
  "Adopt resource edit CONTEXT for BUFFER WORKSPACE-ID and OWNER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (chidu-compose--context-current-p workspace-id)
                 (appkit-compose-operation-current-p owner))
        (chidu-compose--adopt-context context)
        ;; The Store committed exactly one semantic membership edit.  Apply the
        ;; same edit to the live source generation after durable settlement.
        (appkit-compose-touch)
        (appkit-compose-operation-finish owner)
        (unless (chidu-compose--dirty-p)
          (set-buffer-modified-p nil))
        (force-mode-line-update)))))

(defun chidu-compose--resource-edit-failed
    (buffer workspace-id owner failure)
  "Settle failed resource edit OWNER in BUFFER WORKSPACE-ID with FAILURE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (chidu-compose--context-current-p workspace-id)
                 (appkit-compose-operation-current-p owner))
        (appkit-compose-operation-finish owner)
        (force-mode-line-update)
        (message "Chidu attachment change failed: %s"
                 (chidu-compose--failure-text failure))))))

(defun chidu-compose-attach-file (file)
  "Freeze FILE and attach its exact bytes to the current Compose document."
  (interactive (list (read-file-name "Attach file: " nil nil t)))
  (when (appkit-compose-operation-active-p)
    (user-error "A Compose operation is already in progress"))
  (let* ((buffer (current-buffer))
         (runtime (chidu-compose--runtime))
         (workspace (chidu-compose--workspace))
         (workspace-id
          (chidu-store-compose-workspace-workspace-id workspace))
         (endpoint (chidu-store-compose-context-endpoint chidu-compose--context))
         (resource
          (chidu-compose-resource-import
           (chidu-runtime-data-root runtime) file
           (chidu-store-endpoint-max-size-upload endpoint)))
         (capture (appkit-compose-capture))
         (generation (plist-get capture :generation))
         (document (plist-get capture :value))
         (resource-id
          (chidu-store-compose-resource-observation-resource-id resource))
         (updated
          (chidu-store-compose-document-with
           document
           :resource-ids
           (vconcat
            (chidu-store-compose-document-resource-ids document)
            (vector resource-id))))
         (owner
          (appkit-compose-operation-begin
           'adding-attachment
           :generation generation
           :label "Adding attachment…")))
    (chidu-runtime-add-compose-resource
     runtime workspace-id
     (chidu-store-identity-identity-id chidu-compose--identity)
     (chidu-store-compose-workspace-revision workspace)
     (1+ generation) updated resource
     (apply-partially
      #'chidu-compose--resource-edit-succeeded
      buffer workspace-id owner)
     (apply-partially
      #'chidu-compose--resource-edit-failed
      buffer workspace-id owner))
    owner))

(defun chidu-compose--read-resource ()
  "Return one current Compose resource selected by the user."
  (when (zerop (length chidu-compose--resources))
    (user-error "This composition has no attachments"))
  (let ((choices
         (cl-loop
          for resource across chidu-compose--resources
          collect
          (cons
           (format "%s — %s — %s"
                   (chidu-compose--resource-name resource)
                   (file-size-human-readable
                    (chidu-store-compose-resource-size resource))
                   (substring
                    (chidu-store-compose-resource-resource-id resource) 0 8))
           resource))))
    (cdr
     (assoc
      (completing-read "Remove attachment: " choices nil t)
      choices))))

(defun chidu-compose-remove-attachment (&optional resource)
  "Remove RESOURCE, or a selected attachment, from the current document."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "A Compose operation is already in progress"))
  (let* ((resource (or resource (chidu-compose--read-resource)))
         (resource-id (chidu-store-compose-resource-resource-id resource))
         (buffer (current-buffer))
         (runtime (chidu-compose--runtime))
         (workspace (chidu-compose--workspace))
         (workspace-id
          (chidu-store-compose-workspace-workspace-id workspace))
         (capture (appkit-compose-capture))
         (generation (plist-get capture :generation))
         (document (plist-get capture :value))
         (updated
          (chidu-store-compose-document-with
           document
           :resource-ids
           (vconcat
            (seq-remove
             (lambda (value) (equal value resource-id))
             (append
              (chidu-store-compose-document-resource-ids document) nil)))))
         (owner
          (appkit-compose-operation-begin
           'removing-attachment
           :generation generation
           :label "Removing attachment…")))
    (chidu-runtime-remove-compose-resource
     runtime workspace-id
     (chidu-store-identity-identity-id chidu-compose--identity)
     (chidu-store-compose-workspace-revision workspace)
     (1+ generation) updated resource-id
     (apply-partially
      #'chidu-compose--resource-edit-succeeded
      buffer workspace-id owner)
     (apply-partially
      #'chidu-compose--resource-edit-failed
      buffer workspace-id owner))
    owner))

(defun chidu-compose--sendable-account-p (account)
  "Return non-nil when ACCOUNT can own an outbound Compose workspace."
  (and (chidu-store-account-p account)
       (chidu-store-account-available-p account)
       (not (chidu-store-account-read-only-p account))
       (seq-contains-p
        (chidu-store-account-capabilities account)
        chidu-jmap-submission-capability #'equal)
       (seq-some #'chidu-store-identity-available-p
                 (chidu-store-account-identities account))))

(defun chidu-compose--read-account ()
  "Return an available submission Account for a new workspace."
  (let ((at-point
         (and (derived-mode-p 'chidu-home-mode)
              (chidu-home-account-at-point))))
    (if (chidu-compose--sendable-account-p at-point)
        at-point
      (let ((choices
             (seq-filter
              (lambda (entry)
                (chidu-compose--sendable-account-p (cdr entry)))
              (chidu--available-account-choices chidu--app))))
        (unless choices
          (user-error "No available JMAP submission Account"))
        (cdr
         (assoc
          (completing-read "Compose from account: " choices nil t)
          choices))))))

(defun chidu-compose--identity-label (identity)
  "Return a stable completion label for IDENTITY."
  (format "%s <%s> — %s"
          (chidu-store-identity-name identity)
          (chidu-store-identity-email identity)
          (substring (chidu-store-identity-identity-id identity) 0 8)))

(defun chidu-compose--read-identity (account &optional initial)
  "Read one available Identity from ACCOUNT, defaulting to INITIAL."
  (let* ((identities
          (seq-filter #'chidu-store-identity-available-p
                      (append (chidu-store-account-identities account) nil)))
         (choices
          (mapcar (lambda (identity)
                    (cons (chidu-compose--identity-label identity) identity))
                  identities)))
    (pcase identities
      (`() (user-error "Account has no available submission Identity"))
      (`(,identity) identity)
      (_
       (cdr
        (assoc
         (completing-read
          "From Identity: " choices nil t nil nil
          (and initial (chidu-compose--identity-label initial)))
         choices))))))

(defun chidu-compose-change-identity ()
  "Change the Identity bound to the current Compose workspace."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current Compose operation to finish"))
  (let* ((account
          (chidu-store-compose-context-account chidu-compose--context))
         (identity
          (chidu-compose--read-identity account chidu-compose--identity)))
    (unless (equal
             (chidu-store-identity-identity-id identity)
             (chidu-store-identity-identity-id chidu-compose--identity))
      (setq-local chidu-compose--identity identity)
      (appkit-compose-touch)
      (force-mode-line-update))))

(defun chidu-compose--buffer-name (context)
  "Return a stable Compose buffer name for CONTEXT."
  (let* ((workspace (chidu-store-compose-context-workspace context))
         (subject
          (chidu-store-compose-document-subject
           (chidu-store-compose-workspace-document workspace))))
    (format "*Chidu compose %s %s*"
            (if (string-empty-p subject)
                "new"
              (truncate-string-to-width subject 28 nil nil "…"))
            (substring
             (chidu-store-compose-workspace-workspace-id workspace) 0 8))))

(defun chidu-compose--open-context (app context)
  "Open durable Compose CONTEXT in APP without replacing a live editor."
  (let* ((workspace (chidu-store-compose-context-workspace context))
         (workspace-id
          (chidu-store-compose-workspace-workspace-id workspace))
         (identity (list 'compose workspace-id))
         (existing (appkit-app-surface app identity)))
    (if (appkit-surface-live-p existing)
        (progn
          (pop-to-buffer (appkit-surface-buffer existing))
          existing)
      (appkit-open-generated-surface
       chidu-compose--surface-type :app app :identity identity
       :buffer-name (chidu-compose--buffer-name context)
       :input context :select t))))

(defun chidu-compose-open-context (app context)
  "Open durable Compose CONTEXT in live Chidu APP."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-store-compose-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-context-p context)))
  (chidu-compose--open-context app context))

(defun chidu-compose--workspace-label (context)
  "Return one completion label for Compose CONTEXT."
  (let* ((workspace (chidu-store-compose-context-workspace context))
         (identity (chidu-store-compose-context-identity context))
         (subject
          (chidu-store-compose-document-subject
           (chidu-store-compose-workspace-document workspace))))
    (format "%s — %s — %s"
            (if (string-empty-p subject) "(no subject)" subject)
            (chidu-store-identity-email identity)
            (substring
             (chidu-store-compose-workspace-workspace-id workspace) 0 8))))

(defun chidu-compose--create-workspace (app account identity kind document)
  "Create and open one structured workspace in APP.

ACCOUNT and IDENTITY own KIND and initial DOCUMENT."
  (let* ((runtime (chidu-app-runtime app))
         (workspace-id (chidu-store-new-local-id)))
    (chidu-runtime-create-compose-workspace
     runtime workspace-id account identity kind document
     (lambda (context)
       (chidu-compose--open-context app context))
     (lambda (failure)
       (message "Chidu: cannot create composition: %s"
                (chidu-compose--failure-text failure))))))

(defun chidu-compose--endpoint-account (endpoint)
  "Choose a sendable mail Account belonging to ENDPOINT."
  (let* ((contacts-id
          (chidu-store-endpoint-primary-contacts-remote-account-id endpoint))
         (accounts
          (seq-filter
           #'chidu-compose--sendable-account-p
           (append (chidu-store-endpoint-accounts endpoint) nil)))
         (matching
          (and contacts-id
               (cl-find
                contacts-id accounts
                :key #'chidu-store-account-remote-account-id
                :test #'equal))))
    (or matching
        (pcase accounts
          (`() (user-error "Endpoint has no available submission Account"))
          (`(,account) account)
          (_
           (let ((choices
                  (mapcar
                   (lambda (account)
                     (cons (chidu-store-account-name account) account))
                   accounts)))
             (cdr
              (assoc
               (completing-read "Compose from account: " choices nil t)
               choices))))))))

(defun chidu-compose-to-contact (endpoint card)
  "Create a new composition to Contact CARD through ENDPOINT."
  (unless (appkit-app-live-p chidu--app) (chidu))
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (chidu-contact-card-p card)
    (signal 'wrong-type-argument (list 'chidu-contact-card-p card)))
  (let* ((email
          (or (chidu-contact-card-primary-email card)
              (user-error "Contact has no email address")))
         (account (chidu-compose--endpoint-account endpoint))
         (identity (chidu-compose--read-identity account))
         (name (chidu-contact-card-name card))
         (address (chidu-contact-value-value email))
         (document
          (chidu-store-compose-document-create
           :to
           (if (and name (not (string-empty-p name)))
               (mail-header-make-address name address)
             address))))
    (chidu-compose--create-workspace
     chidu--app account identity 'new document)))

;;;###autoload
(defun chidu-compose (&optional account)
  "Create and open a structured Compose workspace for ACCOUNT."
  (interactive)
  (unless (appkit-app-live-p chidu--app) (chidu))
  (let ((selected (or account (chidu-compose--read-account))))
    (unless (chidu-compose--sendable-account-p selected)
      (user-error "Account has no available submission Identity"))
    (chidu-compose--create-workspace
     chidu--app selected (chidu-compose--read-identity selected)
     'new (chidu-store-compose-document-create))))

;;;###autoload
(defun chidu-open-compose-workspace ()
  "Choose and resume one local Compose workspace."
  (interactive)
  (unless (appkit-app-live-p chidu--app)
    (chidu))
  (let* ((app chidu--app)
         (runtime (chidu-app-runtime app)))
    (chidu-runtime-list-compose-workspaces
     runtime
     (lambda (contexts)
       (let ((choices
              (mapcar
               (lambda (context)
                 (cons (chidu-compose--workspace-label context) context))
               (append contexts nil))))
         (unless choices
           (user-error "Chidu has no local Compose workspaces"))
         (chidu-compose--open-context
          app
          (cdr
           (assoc
            (completing-read "Resume composition: " choices nil t)
            choices)))))
     (lambda (failure)
       (message "Chidu: cannot list Compose workspaces: %s"
                (chidu-compose--failure-text failure))))))

(defun chidu-compose-ensure-app-stoppable (app)
  "Signal when APP owns active or uncheckpointed Compose buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'chidu-compose-mode)
                   (appkit-surface-live-p chidu-compose--view)
                   (eq app (appkit-surface-app chidu-compose--view)))
          (when (appkit-compose-operation-active-p)
            (user-error "A Chidu Compose operation is still in progress"))
          (when (chidu-compose--dirty-p)
            (user-error
             "A Chidu Compose buffer has uncheckpointed local changes"))))))
  t)

(defvar-keymap chidu-compose-mode-map
  :doc "Keymap for `chidu-compose-mode'."
  :parent text-mode-map
  "TAB" #'chidu-compose-next-field
  "<backtab>" #'chidu-compose-previous-field
  "M-TAB" #'chidu-compose-complete-address
  "RET" #'chidu-compose-newline
  "C-c C-c" #'chidu-compose-send-unavailable
  "C-x C-s" #'chidu-compose-save-draft
  "C-c C-s" #'chidu-compose-save-draft
  "C-c C-l" #'chidu-compose-checkpoint
  "C-c C-k" #'chidu-compose-close
  "C-c C-i" #'chidu-compose-change-identity
  "C-c C-a" #'chidu-compose-attach-file
  "C-c C-d" #'chidu-compose-remove-attachment
  "C-c ?" #'chidu-dispatch)

(define-derived-mode chidu-compose-mode text-mode "Chidu-Compose"
  "Edit one structured Chidu outbound mail document."
  (setq-local chidu-compose--view nil
              chidu-compose--context nil
              chidu-compose--identity nil
              chidu-compose--field-ranges nil
              chidu-compose--field-overlays nil
              chidu-compose--body-start nil
              chidu-compose--resources (vector)
              chidu-compose--resource-overlay nil
              chidu-compose--checkpointed-generation 0
              chidu-compose--checkpoint-error nil
              chidu-compose--closing-p nil
              chidu-compose--close-after-checkpoint-p nil
              chidu-compose--normalizing-p nil
              chidu-compose--address-candidates (vector)
              chidu-compose--completion-operation nil
              chidu-compose--completion-token nil
              buffer-offer-save nil)
  (visual-line-mode 1)
  (add-hook 'before-change-functions #'chidu-compose--before-change nil t)
  (add-hook 'after-change-functions
            #'chidu-compose--normalize-header-fields nil t)
  (add-hook 'completion-at-point-functions
            #'chidu-compose-completion-at-point nil t)
  (add-hook 'kill-buffer-query-functions #'chidu-compose--kill-query nil t)
  (add-hook
   'kill-buffer-hook
   (lambda ()
     (when (and chidu-compose--completion-operation
                (chidu-store-compose-context-p chidu-compose--context)
                (appkit-surface-live-p chidu-compose--view))
       (chidu-runtime-cancel-operation
        (chidu-compose--runtime) chidu-compose--completion-operation))
     (chidu-compose--clear-field-overlays)
     (chidu-compose--clear-resource-overlay))
   nil t))

(provide 'chidu-compose)

;;; chidu-compose.el ends here
