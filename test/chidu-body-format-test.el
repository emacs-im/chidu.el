;;; chidu-body-format-test.el --- Body representation selection -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'chidu)
(require 'chidu-message)
(require 'chidu-conversation)
(require 'chidu-parsed-message)
(require 'chidu-test-support)

(defun chidu-body-format-test--open (app kind)
  "Open a fully cached KIND reader below APP, without starting transport."
  (let* ((account (chidu-store-account-create :account-id "account" :name "Mail"))
         (mailbox (chidu-store-mailbox-create :name "Inbox"))
         (body (chidu-store-email-body-create :text-content "plain body\n"
                                              :html-content "<p>HTML body</p>"))
         (row (chidu-store-email-summary-row-create
               :local-email-id "first" :remote-email-id "remote-first"
               :remote-thread-id "thread" :from-name "Alice"
               :from-email "alice@example.test" :subject "Example"
               :received-at "2026-01-01T00:00:00Z" :preview "Preview" :unread-p t))
         (context (chidu-store-email-body-context-create
                   :account account :local-email-id "first" :remote-email-id "remote-first"
                   :revision 1 :body body))
         (state
          (pcase kind
            ('message (chidu-message-state-create :row row :account account :mailbox mailbox
                                                   :body-context context :phase 'idle))
            ('parsed (chidu-parsed-message-state-create
                      :account account :source-name "attached.eml" :phase 'idle
                      :context (chidu-store-parsed-blob-context-create
                                :account account :blob-id "attached" :revision 1
                                :message (chidu-store-parsed-message-create
                                          :subject "Attached example" :body body
                                          :from (vector (chidu-store-email-address-create
                                                         :name "Alice" :email "alice@example.test"))))))
            ('conversation
             (let ((state
                    (chidu-conversation-state-create
                     :account account :mailbox mailbox :remote-thread-id "thread"
                     :focus-local-email-id "first" :phase 'idle
                     :context (chidu-store-conversation-context-create
                               :account account :remote-thread-id "thread" :complete-p t
                               :rows (vector
                                      (chidu-store-conversation-row-create :summary-row row)
                                      (chidu-store-conversation-row-create
                                       :summary-row (chidu-store-email-summary-row-with
                                                     row :local-email-id "second"
                                                     :remote-email-id "remote-second")
                                       :parent-local-email-id "first" :depth 1))))))
               (dolist (id '("first" "second"))
                 (puthash id t (chidu-conversation-state-visible-bodies state))
                 (puthash id (chidu-store-email-body-context-with context :local-email-id id)
                          (chidu-conversation-state-body-contexts state)))
               state))))
         (view (appkit-open-generated-surface
                (pcase kind
                  ('message chidu-message--surface-type)
                  ('parsed chidu-parsed-message--surface-type)
                  ('conversation chidu-conversation--surface-type))
                :app app :identity (make-symbol "body-format-reader")
                :buffer-name "*Chidu body format test*" :input state)))
    (appkit-register-handle app 'function (appkit-surface-buffer view) #'kill-buffer)
    (chidu-test-drain view)
    view))

(defun chidu-body-format-test--bounds (id)
  "Return current buffer bounds or the Conversation row identified by ID."
  (if id
      (let ((start (save-excursion
                     (goto-char (point-min))
                     (when-let* ((match (text-property-search-forward
                                        'chidu-conversation-email-id id #'equal)))
                       (prop-match-beginning match)))))
        (should start)
        (cons start (next-single-property-change start 'chidu-conversation-email-id
                                                nil (point-max))))
    (cons (point-min) (point-max))))

(defun chidu-body-format-test--text (view &optional id)
  "Return VIEW's displayed text, optionally limited to row ID."
  (with-current-buffer (appkit-surface-buffer view)
    (let ((bounds (chidu-body-format-test--bounds id)))
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun chidu-body-format-test--choose (view label &optional id)
  "Activate the selector LABEL in VIEW, optionally within row ID."
  (with-current-buffer (appkit-surface-buffer view)
    (let ((bounds (chidu-body-format-test--bounds id)))
      (goto-char (car bounds))
      (should (search-forward label (cdr bounds) t))
      (goto-char (match-beginning 0))
      (chidu-activate-at-point)))
  (chidu-test-drain view))

(ert-deftest chidu-body-format-all-readers-switch-locally-and-independently ()
  (skip-unless (libxml-available-p))
  (let* ((root (make-temp-file "chidu-body-format-" t))
         (chidu-data-root root)
         (app (appkit-app-start chidu--app-type :identity (make-symbol "body-format-app"))))
    (unwind-protect
        (cl-letf (((symbol-function 'chidu-surface-operation-start)
                   (lambda (&rest _) (ert-fail "Body selection must not start an operation")))
                  ((symbol-function 'url-retrieve)
                   (lambda (&rest _) (ert-fail "Body selection must not fetch resources"))))
          (dolist (kind '(message conversation parsed))
            (let* ((view (chidu-body-format-test--open app kind))
                   (other (chidu-body-format-test--open app kind))
                   (id (and (eq kind 'conversation) "first")))
              (should (string-match-p "(\\*) text/plain" (chidu-body-format-test--text view id)))
              (chidu-body-format-test--choose view "text/html" id)
              (should (string-match-p "(\\*) text/html" (chidu-body-format-test--text view id)))
              (should (string-match-p "HTML body" (chidu-body-format-test--text view id)))
              (should-not (string-match-p "plain body" (chidu-body-format-test--text view id)))
              (should (string-match-p "plain body" (chidu-body-format-test--text other id)))
              ;; Ordinary local repaint preserves the override.
              (chidu-surface-refresh view)
              (chidu-test-drain view)
              (should (string-match-p "HTML body" (chidu-body-format-test--text view id)))
              (when id
                (should (string-match-p "plain body" (chidu-body-format-test--text view "second")))
                (should (equal "first" (chidu-conversation-state-focus-local-email-id
                                        (appkit-surface-model view)))))
              (chidu-body-format-test--choose view "text/plain" id)
              (should (string-match-p "plain body" (chidu-body-format-test--text view id)))
              (should-not (string-match-p "\\[auto\\]" (chidu-body-format-test--text view id)))
              (when id
                (chidu-body-format-test--choose view "text/html" "second")
                (should (string-match-p "plain body" (chidu-body-format-test--text view "first")))
                (should (string-match-p "HTML body" (chidu-body-format-test--text view "second"))))
              ;; A callback from a closed reader cannot change another reader.
              (let ((callback
                     (with-current-buffer (appkit-surface-buffer view)
                       (goto-char (point-min))
                       (search-forward "text/plain")
                       (appkit-ui-action-at (match-beginning 0)))))
                (appkit-surface-stop view)
                (should-error (funcall callback) :type 'user-error)))))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (delete-directory root t))))

(ert-deftest chidu-body-format-github-explicit-plain-bypasses-html ()
  (skip-unless (libxml-available-p))
  (let* ((url "https://github.com/example/project/pull/7#discussion_r42")
         (plain (concat "literal *plain* body\n\n-- \n"
                        "Reply to this email directly or view it on GitHub:\n" url
                        "\nYou are receiving this because you are subscribed.\n\n"
                        "Message ID: <example/project/pull/7@github.com>\n"))
         (body (chidu-store-email-body-create :text-content plain :html-content "<p>HTML body</p>")))
    (dolist (format '(nil plain html))
      (with-temp-buffer
        (delay-mode-hooks (chidu-message-mode))
        (let ((inhibit-read-only t))
          (cl-letf (((symbol-function 'chidu-message-insert-html)
                     (lambda (&rest _) (ert-fail "GitHub must not reach SHR"))))
            (chidu-message-insert-body body :sender "notifications@github.com"
                                       :format format :on-format-change #'ignore)))
        (goto-char (point-min))
        (should (equal url (chidu-browse--source-at-point)))
        (if (eq format 'plain)
            (progn
              (should (string-match-p "(\\*) text/plain" (buffer-string)))
              (should (search-forward plain nil t)))
          (should (string-match-p "(\\*) text/html" (buffer-string)))
          (should (string-match-p "HTML body" (buffer-string))))))
    (should (equal plain (chidu-store-email-body-text-content body)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'chidu-message-github-render)
                 (lambda (&rest _) (ert-fail "Explicit plain must bypass provider rendering"))))
        (chidu-message-insert-body body :sender "notifications@github.com" :format 'plain)
        (should (equal plain (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest chidu-body-format-selector-reports-actual-fallback ()
  (let ((body (chidu-store-email-body-create :text-content "plain" :html-content "<p>html</p>")))
    (cl-letf (((symbol-function 'chidu-message-github-parse) (lambda (_) nil))
              ((symbol-function 'chidu-message-insert-html)
               (lambda (&rest _) (ert-fail "GitHub failure must not reach SHR"))))
      (with-temp-buffer
        (chidu-message-insert-body body :sender "notifications@github.com" :on-format-change #'ignore)
        (should (string-match-p "(\\*) text/plain" (buffer-string)))
        (should-not (string-match-p "Unable to display" (buffer-string))))
      (with-temp-buffer
        (chidu-message-insert-body body :sender "notifications@github.com"
                                   :format 'html :on-format-change #'ignore)
        (should (string-match-p "(\\*) text/html" (buffer-string)))
        (should (string-match-p "Unable to display" (buffer-string)))))))

(ert-deftest chidu-body-format-only-offers-existing-representations ()
  (dolist (body (list (chidu-store-email-body-create :text-content "plain" :html-content "")
                      (chidu-store-email-body-create :text-content "" :html-content "<p>html</p>")
                      (chidu-store-email-body-create :text-content "" :html-content "")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'chidu-message-insert-html)
                 (lambda (&rest _) (insert "html") nil)))
        (chidu-message-insert-body body :format 'html :on-format-change #'ignore))
      (should-not (string-match-p "Body:" (buffer-string)))))
  (should-error (chidu-message-check-body-format
                 (chidu-store-email-body-create :text-content "plain" :html-content "") 'html)
                :type 'user-error))

(provide 'chidu-body-format-test)
;;; chidu-body-format-test.el ends here
