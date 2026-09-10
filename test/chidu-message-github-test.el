;;; chidu-message-github-test.el --- GitHub body presentation contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'chidu-message)

(ert-deftest chidu-message-github-matches-only-configured-addresses ()
  (let ((chidu-message-github-sender-addresses '("notifications@github.com"))
        calls)
    (cl-letf (((symbol-function 'treesit-ready-p) (lambda (&rest _) t))
              ((symbol-function 'appkit-fontify-string)
               (lambda (text mode)
                 (push (list text mode) calls)
                 (propertize text 'face 'bold))))
      (should (equal "body" (chidu-message-github-fontify
                             "body" " Notifications@GitHub.COM ")))
      (dolist (sender '(nil "GitHub" "other@github.com"
                       "notifications@github.com.example.test"
                       "github.com@example.test"))
        (should-not (chidu-message-github-fontify "body" sender)))
      (should (equal calls '(("body" markdown-ts-mode))))
      (let ((chidu-message-github-sender-addresses '("notify@enterprise.test")))
        (should (chidu-message-github-fontify "body" "notify@enterprise.test"))
        (should-not (chidu-message-github-fontify "body" "notifications@github.com"))))))

(ert-deftest chidu-message-highlighting-preserves-source-mode-and-warnings ()
  (let* ((source "> Alice\n\n```text\n> literal\n```\n")
         (body (chidu-store-email-body-create
                :text-content source :html-content ""
                :encoding-problem-p t :truncated-p t))
         (participants (list (chidu-text-participant-create
                              :identity "mail:alice@example.test"
                              :name "Alice" :email "alice@example.test"
                              :face 'font-lock-variable-name-face)))
         calls
         (chidu-message-body-fontify-functions
          (list (lambda (&rest _) nil)
                (lambda (text sender)
                  (push (list text sender) calls)
                  (propertize text 'face 'font-lock-keyword-face))
                (lambda (&rest _) (ert-fail "First successful highlighter wins")))))
    (with-temp-buffer
      (delay-mode-hooks (chidu-message-mode))
      (let ((inhibit-read-only t))
        (chidu-message-insert-body body :sender "sender@example"
                                   :participants participants))
      (should (equal calls (list (list source "sender@example"))))
      (should (eq major-mode 'chidu-message-mode))
      (should (eq 'warning (get-text-property (point-min) 'face)))
      (goto-char (point-min))
      (search-forward source)
      (let ((start (match-beginning 0)))
        (should (equal source
                       (substring-no-properties
                        (filter-buffer-substring start (point-max)))))
        (should (eq 'font-lock-keyword-face (get-text-property start 'face)))
        ;; Native syntax owns this body, including literal code containing >.
        (should-not (text-property-not-all start (point-max)
                                          'chidu-message-quote-depth nil))
        (should-not (text-property-not-all start (point-max)
                                          'chidu-person-identity nil))))
    (should (equal source (chidu-store-email-body-text-content body)))))

(ert-deftest chidu-message-github-unavailable-highlighting-keeps-ordinary-mail ()
  (let* ((source "> Alice wrote\n")
         (body (chidu-store-email-body-create :text-content source :html-content ""))
         (chidu-message-github-sender-addresses '("notifications@github.com"))
         (chidu-text-highlight-participants t)
         (participants (list (chidu-text-participant-create
                              :identity "mail:alice@example.test"
                              :name "Alice" :email "alice@example.test"
                              :face 'font-lock-variable-name-face))))
    (pcase-dolist (`(,sender ,ready ,enabled ,native)
                  '(("ordinary@example.test" t t nil)
                    ("notifications@github.com" nil t nil)
                    ("notifications@github.com" t t nil)
                    ("notifications@github.com" t nil nil)
                    ("notifications@github.com" t t "changed source")
                    ("notifications@github.com" t t mutate)))
      (let ((chidu-message-body-fontify-functions
             (and enabled '(chidu-message-github-fontify)))
            (calls 0))
        (with-temp-buffer
          (cl-letf (((symbol-function 'treesit-ready-p) (lambda (&rest _) ready))
                    ((symbol-function 'appkit-fontify-string)
                     (lambda (text _mode)
                       (cl-incf calls)
                       (if (eq native 'mutate)
                           (progn (aset text 0 ?!) text)
                         native))))
            (chidu-message-insert-body body :sender sender
                                       :participants participants))
          (should (= calls (if (and enabled ready
                                    (equal sender "notifications@github.com")) 1 0)))
          (should (equal source (buffer-substring-no-properties (point-min) (point-max))))
          (should (= 1 (get-text-property (point-min) 'chidu-message-quote-depth)))
          (should (get-text-property (point-min) 'appkit-ui-source-line-marker))
          (should (equal "mail:alice@example.test"
                         (get-text-property (+ 2 (point-min)) 'chidu-person-identity))))))))

(ert-deftest chidu-message-highlighting-leaves-html-and-inline-resources-alone ()
  (let ((body (chidu-store-email-body-create
               :text-content "" :html-content "<p>**literal**</p>"))
        (chidu-message-body-fontify-functions
         (list (lambda (&rest _) (ert-fail "HTML must not be fontified as source"))))
        (attachments (list 'inline-resource)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'chidu-message-insert-html)
                 (lambda (html view context)
                   (should (equal html "<p>**literal**</p>"))
                   (should (eq view 'view))
                   (should (eq context 'context))
                   (insert "**literal**")
                   attachments)))
        (should (eq attachments
                    (chidu-message-insert-body body :sender "notifications@github.com"
                                               :view 'view :context 'context)))
        (should (equal "**literal**" (buffer-string)))))))

(provide 'chidu-message-github-test)

;;; chidu-message-github-test.el ends here
