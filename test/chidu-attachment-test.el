;;; chidu-attachment-test.el --- Attachment contracts for Chidu -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'chidu-test-support)
(require 'appkit-core)
(require 'appkit-media-card)
(require 'chidu)
(require 'chidu-attachment)
(require 'chidu-conversation)
(require 'chidu-store)

(defun chidu-attachment-test--fixture
    (&optional download-url size attachment-options)
  "Return one attachment fixture using DOWNLOAD-URL, SIZE, ATTACHMENT-OPTIONS."
  (let* ((endpoint
          (chidu-store-endpoint-create
           :endpoint-id "endpoint"
           :session-url "https://mail.example.test/.well-known/jmap"
           :login "me@example.test"
           :authentication 'basic
           :api-url "https://mail.example.test/jmap/api"
           :download-url
           (or download-url
               (concat
                "https://mail.example.test/jmap/download/"
                "{accountId}/{blobId}/{name}?type={type}"))))
         (account
          (chidu-store-account-create
           :account-id "account"
           :remote-account-id "remote-account"
           :name "Mail"
           :available-p t))
         (attachment
          (chidu-store-email-attachment-create
           :part-id "part-1"
           :blob-id "blob-1"
           :size (or size 4)
           :name (or (plist-get attachment-options :name)
                     "report\n2026.pdf")
           :media-type (or (plist-get attachment-options :media-type)
                           "application/pdf")
           :charset (plist-get attachment-options :charset)
           :disposition (or (plist-get attachment-options :disposition)
                            "attachment")
           :language (vector "en")))
         (body
          (chidu-store-email-body-create
           :email-state "email-state"
           :text-content "Body"
           :html-content ""
           :truncated-p nil
           :encoding-problem-p nil
           :attachments (vector attachment)))
         (context
          (chidu-store-email-body-context-create
           :endpoint endpoint
           :account account
           :local-email-id "local-email"
           :remote-email-id "remote-email"
           :revision 1
           :body body)))
    (list context attachment)))

(defun chidu-attachment-test--view (app)
  "Return a disposable Generated reader below APP."
  (let ((surface
         (appkit-open-generated-surface
          (appkit-surface-type-create
           :name 'chidu-attachment-test :mode #'special-mode
           :init (lambda (_context _input)
                   (appkit-next :model (chidu-message-state-create)
                                :render appkit-render-none))
           :update #'chidu-surface-update
           :renderer-factory
           (lambda (_surface)
             (appkit-generated-renderer-create
              :mount (lambda (&rest _) nil)
              :merge (lambda (_old new) new)
              :render (lambda (&rest _) nil)
              :unmount (lambda (_surface) nil))))
          :app app :identity (make-symbol "attachment-reader")
          :buffer-name "*Chidu attachment test*")))
    (appkit-register-handle app 'function (appkit-surface-buffer surface) #'kill-buffer)
    surface))

(ert-deftest
    chidu-attachment-card-and-authenticated-transfer-form-one-contract
    ()
  (pcase-let*
      ((`(,context ,attachment) (chidu-attachment-test--fixture))
       (root (make-temp-file "chidu-attachment-test-" t))
       (chidu-data-root root)
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "attachment-app")))
       (view (chidu-attachment-test--view app))
       (captured-resource nil) (captured-target nil)
       (captured-headers nil) (authorization-reference nil)
       (authorization-before nil))
    (set-file-modes root 448)
    (unwind-protect
        (progn
          (should
           (equal
            (concat
             "https://mail.example.test/jmap/download/remote-account/"
             "blob-1/report%202026.pdf?type=application%2Fpdf")
            (chidu-attachment-download-url context attachment)))
          (with-current-buffer (appkit-surface-buffer view)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (chidu-attachment-insert-cards view context))
            (goto-char (point-min)) (search-forward "report 2026.pdf")
            (let*
                ((card (appkit-media-card-context-at-point))
                 (payload (plist-get card :payload)))
              (should card)
              (should (eq attachment (plist-get payload :attachment)))
              (should (functionp (plist-get card :open-action)))
              (should (functionp (plist-get card :download-action)))
              (should (functionp (plist-get card :save-as-action))))
            (should-not (string-match-p "blob-1" (buffer-string)))
            (dolist
                (label
                 '("[Download]" "[Retry]" "[Cancel]" "[Show inline]"
                   "[Hide inline]"))
              (should-not
               (string-match-p (regexp-quote label) (buffer-string)))))
          (cl-letf
              (((symbol-function 'chidu-runtime--endpoint-secret)
                (lambda (_endpoint) (copy-sequence "password")))
               ((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda
                  (resource target success _error &rest arguments)
                  (setq captured-resource resource captured-target
                        target captured-headers
                        (plist-get arguments :headers)
                        authorization-reference
                        (cdr (assoc "Authorization" captured-headers))
                        authorization-before
                        (copy-sequence authorization-reference))
                  (with-temp-buffer
                    (set-buffer-multibyte nil) (insert "data")
                    (let ((coding-system-for-write 'binary))
                      (write-region (point-min) (point-max) target nil
                                    'silent)))
                  (funcall success target) nil)))
            (prog1
                (chidu-attachment-download view context attachment)
              (chidu-test-drain app)))
          (should
           (equal "application/pdf"
                  (alist-get 'mime-type captured-resource)))
          (should
           (string-prefix-p ".chidu-verify-"
                            (file-name-nondirectory captured-target)))
          (should-not
           (equal
            (chidu-attachment-cache-path app context attachment)
            captured-target))
          (should (string-prefix-p "Basic " authorization-before))
          (should (seq-every-p #'zerop authorization-reference))
          (should
           (equal "application/pdf"
                  (cdr (assoc "Accept" captured-headers))))
          (let*
              ((state (chidu-attachment-state app context attachment))
               (published (plist-get state :path)))
            (should (eq 'downloaded (plist-get state :status)))
            (should (file-regular-p published))
            (should-not (file-exists-p captured-target))
            (should (= 384 (logand 511 (file-modes published)))))
          (with-current-buffer (appkit-surface-buffer view)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (chidu-attachment-insert-cards view context))
            (should (string-match-p "local:" (buffer-string)))
            (goto-char (point-min)) (search-forward "report 2026.pdf")
            (let ((card (appkit-media-card-context-at-point)))
              (should-not (plist-get card :download-action))
              (should (functionp (plist-get card :open-action)))))
          (when (appkit-app-live-p app) (appkit-app-close app))
          (when (file-directory-p root) (delete-directory root t))))))

(ert-deftest chidu-inline-text-attachment-renders-and-toggles ()
  (pcase-let*
      ((patch
        (concat "diff --git a/lisp/composite.el b/lisp/composite.el\n"
                "--- a/lisp/composite.el\n"
                "+++ b/lisp/composite.el\n" "@@ -1 +1 @@\n"
                "-old-rule\n" "+new-rule\n"))
       (`(,context ,attachment)
        (chidu-attachment-test--fixture nil (string-bytes patch)
                                        '(:name "change.patch"
                                          :media-type "text/x-diff"
                                          :charset "us-ascii"
                                          :disposition "inline")))
       (root (make-temp-file "chidu-inline-attachment-test-" t))
       (chidu-data-root root)
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "inline-attachment-app")))
       (view (chidu-attachment-test--view app)))
    (set-file-modes root 448)
    (unwind-protect
        (let
            ((path
              (chidu-attachment-cache-path app context attachment)))
          (with-temp-buffer
            (set-buffer-multibyte nil) (insert patch)
            (let ((coding-system-for-write 'binary))
              (write-region (point-min) (point-max) path nil 'silent)))
          (set-file-modes path 384)
          (cl-labels
              ((render nil
                 (with-current-buffer (appkit-surface-buffer view)
                   (let ((inhibit-read-only t))
                     (erase-buffer) (insert "preceding body text\n")
                     (chidu-attachment-insert-cards view context))))
               (toggle-exact nil
                 (with-current-buffer (appkit-surface-buffer view)
                   (goto-char (point-min))
                   (should (search-forward "change.patch" nil t))
                   (should
                    (chidu-attachment-inline-toggle-available-p t))
                   (prog1
                       (chidu-attachment-toggle-inline-at-point-exact)
                     (chidu-test-drain app)))))
            (render)
            (with-current-buffer (appkit-surface-buffer view)
              (should (= (point-max) (point))) (goto-char (point-min))
              (should (search-forward "+new-rule" nil t))
              (dolist
                  (label
                   '("[Show inline]" "[Hide inline]" "[Download]"
                     "[Retry]" "[Cancel]"))
                (should-not (search-forward label nil t)))
              (goto-char (point-min))
              (should-not (appkit-media-card-context-at-point))
              (should-not
               (get-text-property (point) 'chidu-attachment-key))
              (should (search-forward "change.patch" nil t))
              (should
               (eq attachment
                   (plist-get
                    (plist-get (appkit-media-card-context-at-point)
                               :payload)
                    :attachment)))
              (should (eq 'special-mode major-mode))
              (should-not buffer-file-name))
            (toggle-exact)
            (should-not
             (chidu-attachment--inline-visible-p
              (chidu-attachment-state app context attachment)
              attachment))
            (render)
            (with-current-buffer (appkit-surface-buffer view)
              (goto-char (point-min))
              (should-not (search-forward "+new-rule" nil t))
              (should-not (search-forward "[Show inline]" nil t)))
            (toggle-exact) (render)
            (with-current-buffer (appkit-surface-buffer view)
              (goto-char (point-min))
              (should (search-forward "+new-rule" nil t))
              (should-not (search-forward "[Hide inline]" nil t)))))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-inline-ascii-patch-decodes-eol-without-losing-content-cr ()
  "Uniform CRLF is decoded; mixed content CR must survive."
  (let* ((lf "--- a/file.txt\n+++ b/file.txt\n@@ -1 +1 @@\n-old\n+new\n")
         (crlf (string-replace "\n" "\r\n" lf))
         (mixed (string-replace "+new\n" "+new\r\n" lf))
         (file (make-temp-file "chidu-patch-eol-test-"))
         (mail-parse-charset nil))
    (unwind-protect
        (dolist (sample (list (cons lf lf) (cons crlf lf) (cons mixed mixed)))
          (let ((coding-system-for-write 'binary))
            (write-region (car sample) nil file nil 'silent))
          (with-temp-buffer
            (chidu-attachment--insert-inline-text
             (chidu-store-email-attachment-create
              :part-id "part" :blob-id "blob" :size (string-bytes (car sample))
              :name "change.patch" :media-type "text/x-patch"
              :charset "us-ascii" :disposition "inline")
             file)
            (should (equal (cdr sample)
                           (buffer-substring-no-properties (point-min) (point-max)))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file)
            (should (equal (car sample) (buffer-string)))))
      (delete-file file))))

(ert-deftest
    chidu-inline-text-attachments-keep-server-order-and-identity ()
  "Each card renders its own attachment, in the server's exact order."
  (let*
      ((first-text "FIRST-PART-BODY\n")
       (second-text "SECOND-PART-BODY\n")
       (first-attachment
        (chidu-store-email-attachment-create :part-id "part-1"
                                             :blob-id "blob-1" :size
                                             (string-bytes first-text)
                                             :name "first.patch"
                                             :media-type
                                             "text/x-diff" :charset
                                             "us-ascii" :disposition
                                             "inline" :language
                                             (vector)))
       (second-attachment
        (chidu-store-email-attachment-create :part-id "part-2"
                                             :blob-id "blob-2" :size
                                             (string-bytes
                                              second-text)
                                             :name "second.txt"
                                             :media-type "text/plain"
                                             :charset "us-ascii"
                                             :disposition "inline"
                                             :language (vector)))
       (base-context (car (chidu-attachment-test--fixture)))
       (context
        (chidu-store-email-body-context-with base-context :body
                                             (chidu-store-email-body-with
                                              (chidu-store-email-body-context-body
                                               base-context)
                                              :attachments
                                              (vector
                                               first-attachment
                                               second-attachment))))
       (root (make-temp-file "chidu-inline-order-test-" t))
       (chidu-data-root root)
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "inline-order-app")))
       (view (chidu-attachment-test--view app)))
    (set-file-modes root 448)
    (unwind-protect
        (progn
          (dolist
              (pair
               (list (cons first-attachment first-text)
                     (cons second-attachment second-text)))
            (let
                ((path
                  (chidu-attachment-cache-path app context (car pair))))
              (with-temp-buffer
                (set-buffer-multibyte nil) (insert (cdr pair))
                (let ((coding-system-for-write 'binary))
                  (write-region (point-min) (point-max) path nil
                                'silent)))
              (set-file-modes path 384)))
          (with-current-buffer (appkit-surface-buffer view)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (chidu-attachment-insert-cards view context))
            (let
                ((text
                  (buffer-substring-no-properties (point-min)
                                                  (point-max))))
              (should
               (< (string-match-p "Attachments · 2" text)
                  (string-match-p "first\\.patch" text)
                  (string-match-p "FIRST-PART-BODY" text)
                  (string-match-p "second\\.txt" text)
                  (string-match-p "SECOND-PART-BODY" text))))
            (dolist
                (pair
                 (list (cons "first.patch" first-attachment)
                       (cons "second.txt" second-attachment)))
              (goto-char (point-min))
              (should (search-forward (car pair) nil t))
              (should
               (eq (cdr pair)
                   (plist-get
                    (plist-get (appkit-media-card-context-at-point)
                               :payload)
                    :attachment))))))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-inline-text-requires-explicit-user-download ()
  "`inline' metadata never triggers a network request on its own."
  (pcase-let*
      ((text "explicit-download-body\n")
       (`(,context ,attachment)
        (chidu-attachment-test--fixture nil (string-bytes text)
                                        '(:name "note.txt"
                                          :media-type "text/plain"
                                          :charset "us-ascii"
                                          :disposition "inline")))
       (root (make-temp-file "chidu-inline-fetch-test-" t))
       (chidu-data-root root)
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "inline-fetch-app")))
       (view (chidu-attachment-test--view app)) (requests 0))
    (set-file-modes root 448)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chidu-runtime--endpoint-secret)
              (lambda (_endpoint) (copy-sequence "password")))
             ((symbol-function
               'appkit-media-copy-or-download-resource-async)
              (lambda
                (_resource target success _error &rest _arguments)
                (setq requests (1+ requests))
                (with-temp-buffer
                  (set-buffer-multibyte nil) (insert text)
                  (let ((coding-system-for-write 'binary))
                    (write-region (point-min) (point-max) target nil
                                  'silent)))
                (funcall success target) nil)))
          (with-current-buffer (appkit-surface-buffer view)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (chidu-attachment-insert-cards view context))
            (should (= 0 requests))
            (dolist
                (label
                 '("[Show inline]" "[Hide inline]" "[Download]"
                   "[Retry]" "[Cancel]"))
              (should-not
               (string-match-p (regexp-quote label)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
            (should-not
             (string-match-p "explicit-download-body"
                             (buffer-substring-no-properties
                              (point-min) (point-max))))
            (goto-char (point-min))
            (should (search-forward "note.txt" nil t))
            (prog1 (chidu-attachment-toggle-inline-at-point-exact)
              (chidu-test-drain app)))
          (should (= 1 requests))
          (should
           (eq 'downloaded
               (plist-get
                (chidu-attachment-state app context attachment)
                :status)))
          (with-current-buffer (appkit-surface-buffer view)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (chidu-attachment-insert-cards view context))
            (goto-char (point-min))
            (should (search-forward "explicit-download-body" nil t)))
          (should (= 1 requests)))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-inline-text-eligibility-follows-declared-type ()
  "Eligibility uses the declared type and size, never byte sniffing."
  (cl-flet ((eligible-p
              (media-type size)
              (chidu-attachment--inline-text-p
               (chidu-store-email-attachment-create
                :part-id "part" :blob-id "blob" :size size
                :name "part.bin" :media-type media-type
                :disposition "inline" :language (vector)))))
    (should (eligible-p "text/plain" 10))
    (should (eligible-p "text/x-diff" 10))
    (should (eligible-p "application/x-patch" 10))
    ;; Handlers that fetch remote images, build Gnus article state, or need
    ;; an undeclared dependency are refused even though they are `text/'.
    (should-not (eligible-p "text/html" 10))
    (should-not (eligible-p "text/calendar" 10))
    (should-not (eligible-p "text/x-vcard" 10))
    ;; Binary parts stay non-inline, and the byte limit is enforced.
    (should-not (eligible-p "application/pdf" 10))
    (should-not (eligible-p "image/png" 10))
    (should-not
     (eligible-p "text/plain"
                 (1+ chidu-attachment-inline-text-byte-limit)))
    (should
     (eligible-p "text/plain" chidu-attachment-inline-text-byte-limit))))

(ert-deftest chidu-inline-text-renders-untrusted-bytes-inertly ()
  "Rendering a cached part runs no subprocess and no content-supplied code.\n\nThe part's name is server-controlled, so a compression suffix must not reach\n`mm-decompress-buffer', and Gnus' `set-auto-mode' path must not run mode hooks\nover untrusted bytes."
  (pcase-let*
      ((text "untrusted-inert-body\n")
       (`(,context ,attachment)
        (chidu-attachment-test--fixture nil (string-bytes text)
                                        '(:name "notes.gz"
                                          :media-type "text/plain"
                                          :charset "us-ascii"
                                          :disposition "inline")))
       (root (make-temp-file "chidu-inline-inert-test-" t))
       (chidu-data-root root)
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "inline-inert-app")))
       (view (chidu-attachment-test--view app)) (processes 0)
       (hook-runs 0))
    (set-file-modes root 448)
    (unwind-protect
        (let
            ((path
              (chidu-attachment-cache-path app context attachment))
             (hook (lambda () (setq hook-runs (1+ hook-runs)))))
          (with-temp-buffer
            (set-buffer-multibyte nil) (insert text)
            (let
                ((coding-system-for-write 'binary)
                 (inhibit-file-name-handlers '(jka-compr-handler))
                 (inhibit-file-name-operation 'write-region))
              (write-region (point-min) (point-max) path nil 'silent)))
          (set-file-modes path 384)
          (add-hook 'fundamental-mode-hook hook)
          (add-hook 'text-mode-hook hook)
          (let
              ((count-process
                (lambda (&rest _arguments)
                  (setq processes (1+ processes)))))
            (advice-add 'call-process-region :before count-process)
            (advice-add 'call-process :before count-process)
            (unwind-protect
                (with-current-buffer (appkit-surface-buffer view)
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (chidu-attachment-insert-cards view context))
                  (goto-char (point-min))
                  (should
                   (search-forward "untrusted-inert-body" nil t)))
              (advice-remove 'call-process-region count-process)
              (advice-remove 'call-process count-process)
              (remove-hook 'fundamental-mode-hook hook)
              (remove-hook 'text-mode-hook hook)))
          (should (= 0 processes)) (should (= 0 hook-runs)))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest
    chidu-attachment-rejects-untrusted-or-unverified-downloads ()
  (pcase-let*
      ((`(,context ,attachment)
        (chidu-attachment-test--fixture nil 5))
       (`(,foreign-context ,_foreign-attachment)
        (chidu-attachment-test--fixture
         (concat "https://cdn.example.test/download/"
                 "{accountId}/{blobId}/{name}?type={type}")))
       (`(,incomplete-context ,_incomplete-attachment)
        (chidu-attachment-test--fixture
         (concat "https://mail.example.test/download/"
                 "{accountId}/{blobId}/{name}")))
       (root (make-temp-file "chidu-attachment-error-test-" t))
       (chidu-data-root root)
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "attachment-error-app")))
       (view (chidu-attachment-test--view app))
       (cancel-error-callback nil) (authorization-reference nil)
       (cancel-authorization-reference nil))
    (set-file-modes root 448)
    (unwind-protect
        (progn
          (should
           (string-prefix-p
            "https://cdn.example.test/download/remote-account/blob-1/"
            (chidu-attachment-download-url foreign-context attachment)))
          (should-error
           (chidu-attachment-download-url incomplete-context
                                          attachment)
           :type 'chidu-jmap-error)
          (cl-letf
              (((symbol-function 'chidu-runtime--endpoint-secret)
                (lambda (_endpoint) (copy-sequence "password")))
               ((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda
                  (_resource target success _error &rest arguments)
                  (setq authorization-reference
                        (cdr
                         (assoc "Authorization"
                                (plist-get arguments :headers))))
                  (with-temp-buffer
                    (set-buffer-multibyte nil) (insert "data")
                    (let ((coding-system-for-write 'binary))
                      (write-region (point-min) (point-max) target nil
                                    'silent)))
                  (funcall success target) nil)))
            (prog1
                (chidu-attachment-download view context attachment)
              (chidu-test-drain app)))
          (let*
              ((state (chidu-attachment-state app context attachment))
               (path (plist-get state :path)))
            (should (eq 'error (plist-get state :status)))
            (should
             (string-match-p "size mismatch" (plist-get state :error)))
            (should-not (file-exists-p path))
            (should (seq-every-p #'zerop authorization-reference)))
          (cl-letf
              (((symbol-function 'chidu-runtime--endpoint-secret)
                (lambda (_endpoint) (copy-sequence "password")))
               ((symbol-function 'appkit-media-transfer-p)
                (lambda (object) (eq object 'fake-transfer)))
               ((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda
                  (_resource _target _success error &rest arguments)
                  (setq cancel-error-callback error
                        cancel-authorization-reference
                        (cdr
                         (assoc "Authorization"
                                (plist-get arguments :headers))))
                  'fake-transfer))
               ((symbol-function 'appkit-media-cancel-transfer)
                (lambda (handle) (should (eq handle 'fake-transfer))
                  (funcall cancel-error-callback "transfer canceled")
                  t)))
            (prog1
                (chidu-attachment-download view context attachment)
              (chidu-test-drain app))
            (should
             (eq 'downloading
                 (plist-get
                  (chidu-attachment-state app context attachment)
                  :status)))
            (chidu-attachment-cancel-download view context attachment)
            (let
                ((state
                  (chidu-attachment-state app context attachment)))
              (should (eq 'not-downloaded (plist-get state :status)))
              (should-not (plist-get state :error)))
            (should
             (string-prefix-p "Basic " cancel-authorization-reference))))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest
    chidu-conversation-tab-dwim-prefers-exact-attachment-card ()
  (dolist
      (command
       '(chidu-conversation-tab-dwim
         chidu-conversation-evil-tab-dwim))
    (let (inline body replies)
      (cl-letf
          (((symbol-function
             'chidu-attachment-inline-toggle-available-p)
            (lambda (&optional exact-p) (should exact-p) t))
           ((symbol-function
             'chidu-attachment-toggle-inline-at-point-exact)
            (lambda () (setq inline t)))
           ((symbol-function 'chidu-attachment-card-at-point-p)
            (lambda (&optional _exact-p) t))
           ((symbol-function 'chidu-conversation-toggle-body)
            (lambda () (setq body t)))
           ((symbol-function 'chidu-conversation-toggle-replies)
            (lambda () (setq replies t))))
        (funcall command))
      (should inline) (should-not body) (should-not replies)))
  (let (body replies)
    (cl-letf
        (((symbol-function
           'chidu-attachment-inline-toggle-available-p)
          (lambda (&optional _exact-p) nil))
         ((symbol-function 'chidu-attachment-card-at-point-p)
          (lambda (&optional _exact-p) nil))
         ((symbol-function 'chidu-conversation-toggle-body)
          (lambda () (setq body t)))
         ((symbol-function 'chidu-conversation-toggle-replies)
          (lambda () (setq replies t))))
      (chidu-conversation-tab-dwim))
    (should replies) (should-not body))
  (let (body replies)
    (cl-letf
        (((symbol-function
           'chidu-attachment-inline-toggle-available-p)
          (lambda (&optional _exact-p) nil))
         ((symbol-function 'chidu-attachment-card-at-point-p)
          (lambda (&optional _exact-p) nil))
         ((symbol-function 'chidu-conversation-toggle-body)
          (lambda () (setq body t)))
         ((symbol-function 'chidu-conversation-toggle-replies)
          (lambda () (setq replies t))))
      (chidu-conversation-evil-tab-dwim))
    (should body) (should-not replies))
  (dolist
      (command
       '(chidu-conversation-tab-dwim
         chidu-conversation-evil-tab-dwim))
    (let (body replies)
      (cl-letf
          (((symbol-function
             'chidu-attachment-inline-toggle-available-p)
            (lambda (&optional _exact-p) nil))
           ((symbol-function 'chidu-attachment-card-at-point-p)
            (lambda (&optional exact-p) (should exact-p) t))
           ((symbol-function 'chidu-conversation-toggle-body)
            (lambda () (setq body t)))
           ((symbol-function 'chidu-conversation-toggle-replies)
            (lambda () (setq replies t))))
        (should-error (funcall command) :type 'user-error))
      (should-not body) (should-not replies))))

(ert-deftest chidu-html-embedded-images-use-local-jmap-evidence ()
  "HTML consumes same-Email JMAP resources without implicit network I/O."
  (let*
      ((cid-image
        (chidu-store-email-attachment-create :part-id "cid-part"
                                             :blob-id "cid-blob"
                                             :size 4 :name "logo.png"
                                             :media-type "image/png"
                                             :disposition "inline"
                                             :cid "logo@example.test"
                                             :language (vector)))
       (location-image
        (chidu-store-email-attachment-create :part-id "location-part"
                                             :blob-id "location-blob"
                                             :size 4 :name
                                             "banner.png" :media-type
                                             "image/png" :disposition
                                             "inline" :location
                                             "images/banner.png"
                                             :language (vector)))
       (base-context (car (chidu-attachment-test--fixture)))
       (body
        (chidu-store-email-body-create :email-state "email-state"
                                       :text-content "" :html-content
                                       (concat "<p>Before</p>"
                                               "<img src='cid:logo%40example.test' alt='Project logo'>"
                                               "<img src='images/banner.png' alt='Release banner'>"
                                               "<img src='https://tracker.invalid/pixel.png' alt='Remote image'>"
                                               "<p>After</p>")
                                       :truncated-p nil
                                       :encoding-problem-p nil
                                       :attachments
                                       (vector cid-image
                                               location-image)))
       (context
        (chidu-store-email-body-context-with base-context :body body))
       (root (make-temp-file "chidu-embedded-image-test-" t))
       (chidu-data-root root)
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "embedded-image-app")))
       (view (chidu-attachment-test--view app)) (requests 0))
    (set-file-modes root 448)
    (unwind-protect
        (cl-labels
            ((render (&optional format)
               (with-current-buffer (appkit-surface-buffer view)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (let
                       ((embedded
                         (chidu-message-insert-body body :view view
                                                    :context context :format format)))
                     (insert "\n")
                     (chidu-attachment-insert-cards view context
                                                    :embedded-attachments
                                                    embedded)
                     embedded)))))
          (cl-letf
              (((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda (&rest _) (setq requests (1+ requests))
                  (error "render must not acquire embedded resources"))))
            (should (equal (list cid-image location-image) (render)))
            (should (= requests 0))
            (with-current-buffer (appkit-surface-buffer view)
              (let
                  ((text
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))
                (should (string-match-p "Project logo" text))
                (should (string-match-p "Release banner" text))
                (should (string-match-p "Remote image" text))
                (should-not (string-match-p "Attachments ·" text)))
              (goto-char (point-min))
              (should (search-forward "Project logo" nil t))
              (should
               (eq cid-image
                   (plist-get
                    (plist-get (appkit-media-card-context-at-point)
                               :payload)
                    :attachment)))
              (goto-char (point-min))
              (should (search-forward "Remote image" nil t))
              (should-not (appkit-media-card-context-at-point))))
          ;; Changing representation reprojects the same attachment manifest:
          ;; inline resources disappear from cards only while HTML uses them.
          (setq body (chidu-store-email-body-with body :text-content "Plain alternative")
                context (chidu-store-email-body-context-with context :body body))
          (should-not (render 'plain))
          (with-current-buffer (appkit-surface-buffer view)
            (should (string-match-p "Attachments · 2" (buffer-string)))
            (should-not (text-property-not-all (point-min) (point-max)
                                              'chidu-embedded-attachment nil)))
          (should (equal (list cid-image location-image) (render 'html)))
          (with-current-buffer (appkit-surface-buffer view)
            (should-not (string-match-p "Attachments ·" (buffer-string))))
          (should (= requests 0))
          (let
              ((path
                (chidu-attachment-cache-path app context cid-image))
               preview-file)
            (write-region "data" nil path nil 'silent)
            (set-file-modes path 384)
            (cl-letf
                (((symbol-function
                   'appkit-media-inline-image-rendering-available-p)
                  (lambda () t))
                 ((symbol-function
                   'appkit-media-preview-image-from-file)
                  (lambda (file &rest _) (setq preview-file file)
                    'chidu-test-image))
                 ((symbol-function 'appkit-media-image-object-valid-p)
                  (lambda (image) (eq image 'chidu-test-image)))
                 ((symbol-function 'appkit-media-insert-image-slices)
                  (lambda (&rest _)
                    (insert "[rendered embedded image]"))))
              (render 'html) (should (equal path preview-file))
              (should (= requests 0))
              (with-current-buffer (appkit-surface-buffer view)
                (goto-char (point-min))
                (should
                 (search-forward "[rendered embedded image]" nil t))
                (should
                 (eq cid-image
                     (plist-get
                      (plist-get (appkit-media-card-context-at-point)
                                 :payload)
                      :attachment)))))))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when (file-directory-p root) (delete-directory root t)))))

(provide 'chidu-attachment-test)

;;; chidu-attachment-test.el ends here

(ert-deftest chidu-attachment-open-is-fenced-by-exact-reader-lifetime
    ()
  (pcase-let*
      ((`(,context ,attachment) (chidu-attachment-test--fixture))
       (root (make-temp-file "chidu-reader-open-" t))
       (chidu-data-root root)
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "reader-app")))
       (first (chidu-attachment-test--view app))
       (second (chidu-attachment-test--view app)) (listeners nil)
       (canceled nil) (opened nil))
    (unwind-protect
        (cl-letf
            (((symbol-function 'chidu-runtime--endpoint-secret)
              (lambda (_endpoint) (copy-sequence "password")))
             ((symbol-function 'appkit-media-transfer-p)
              (lambda (object) (memq object listeners)))
             ((symbol-function
               'appkit-media-copy-or-download-resource-async)
              (lambda
                (_resource target success failure &rest _arguments)
                (let ((listener (list target success failure)))
                  (push listener listeners) listener)))
             ((symbol-function 'appkit-media-cancel-transfer)
              (lambda (listener) (push listener canceled)
                (funcall (nth 2 listener) "transfer canceled")))
             ((symbol-function 'appkit-media-open-file)
              (lambda (file) (push file opened))))
          (chidu-attachment-open first context attachment)
          (chidu-attachment-open second context attachment)
          (should (= 2 (length listeners)))
          (appkit-surface-stop first)
          (should (equal canceled (list (cadr listeners))))
          (let ((target (caar listeners)))
            (write-region "data" nil target nil 'silent)
            (funcall (nth 1 (cadr listeners)) target)
            (should-not opened)
            (funcall (nth 1 (car listeners)) target)
            (should-not opened) (chidu-test-drain app)
            (should
             (equal opened
                    (list
                     (chidu-attachment-cache-path app context
                                                  attachment))))
            (should
             (eq 'downloaded
                 (plist-get
                  (chidu-attachment-state app context attachment)
                  :status)))))
      (appkit-app-close app) (delete-directory root t))))
