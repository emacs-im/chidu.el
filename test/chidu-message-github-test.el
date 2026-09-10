;;; chidu-message-github-test.el --- GitHub presentation contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'chidu-message)

(defun chidu-message-github-test--footer (url)
  "Return a synthetic plain notification footer containing URL."
  (concat "\n-- \nReply to this email directly or view it on GitHub:\n"
          url "\nYou are receiving this because you are subscribed to this thread.\n\n"
          "Message ID: <example/project/pull/7/review/9@github.com>\n"))

(ert-deftest chidu-message-github-selects-only-configured-senders ()
  (let* ((body (chidu-store-email-body-create :text-content "plain" :html-content "<p>HTML</p>"))
         (chidu-message-github-sender-addresses '("notifications@github.com"))
         (document (appkit-markup-document
                    (list (appkit-markup-paragraph (list (appkit-markup-text "HTML"))))))
         calls)
    (cl-letf (((symbol-function 'chidu-message-github-parse)
               (lambda (_) (push t calls) (cons document nil))))
      (dolist (sender '(nil "GitHub" "other@github.com"
                       "notifications@github.com.example.test" "github.com@example.test"))
        (with-temp-buffer
          (chidu-message-insert-body body :sender sender)
          (should (equal "plain" (buffer-string)))))
      (should-not calls)
      (with-temp-buffer
        (chidu-message-insert-body body :sender " Notifications@GitHub.COM ")
        (should (equal "HTML" (buffer-string))))
      (let ((chidu-message-github-sender-addresses '("notify@enterprise.test")))
        (should (chidu-message-github--sender-p "notify@enterprise.test"))
        (should-not (chidu-message-github--sender-p "notifications@github.com")))
      (should (= (length calls) 1)))))

(ert-deftest chidu-message-github-html-is-structured-inert-and-source-preserving ()
  (skip-unless (libxml-available-p))
  (let* ((url "https://github.com/example/project/pull/7#discussion_r42")
         (diff "+ (defun example ()\n+  \"Use `helper' and @literal.\")\n")
         (code "(message \"@code\")\n")
         (html
          (concat
           "<p></p><p><b>@alice</b> commented on this pull request.</p><hr>"
           "<p>In <a href=\"" url "\">example.el</a>:</p><pre>" diff "</pre>"
           "<blockquote><p>old <code>helper</code></p></blockquote>"
           "<p><a class=\"user-mention\" href=\"https://github.com/bob\">@bob</a> "
           "<code>@inline</code> &rarr; <b>new</b> "
           "<a href=\"https://example.test/@destination\">external</a> "
           "<a href=\"javascript:alert(1)\">unsafe</a></p>"
           "<div class=\"highlight highlight-source-emacs-lisp\"><pre>" code "</pre></div>"
           "<pre>+ ordinary unlabelled text</pre>"
           "<p>—<br>Reply to this email directly, <a href=\"" url
           "\">view it on GitHub</a>.<br>You are receiving this because you are subscribed.</p>"
           "<script>forbidden-script</script><style>forbidden-style</style>"
           "<img src=\"https://tracker.test/pixel\"><iframe>forbidden-frame</iframe>"))
         (body (chidu-store-email-body-create :text-content "fallback" :html-content html
                                               :encoding-problem-p t :truncated-p t))
         calls visited)
    (with-temp-buffer
      (delay-mode-hooks (chidu-message-mode))
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'chidu-message-insert-html)
                   (lambda (&rest _) (ert-fail "GitHub must not reach SHR")))
                  ((symbol-function 'treesit-parser-create)
                   (lambda (&rest _) (ert-fail "HTML must not be reparsed as Markdown")))
                  ((symbol-function 'url-retrieve)
                   (lambda (&rest _) (ert-fail "Rendering must not fetch resources")))
                  ((symbol-function 'browse-url)
                   (lambda (target &rest _) (push target visited)))
                  ((symbol-function 'appkit-fontify-string)
                   (lambda (text mode)
                     (push (cons text mode) calls)
                     (propertize text 'face 'font-lock-keyword-face))))
          (should-not (chidu-message-insert-body body :sender "notifications@github.com"))
          (should (eq major-mode 'chidu-message-mode))
          (should (eq (get-text-property (point-min) 'face) 'warning))
          (should (equal calls (list (cons code 'emacs-lisp-mode) (cons diff 'diff-mode))))
          (should-not visited)
          (should-not (string-match-p "forbidden-\\|fallback\\|tracker.test" (buffer-string)))
          (should (string-match-p "→" (buffer-string)))
          (goto-char (point-min))
          (search-forward diff)
          (should (equal diff (buffer-substring-no-properties (match-beginning 0) (point))))
          (should-not (text-property-not-all (match-beginning 0) (point)
                                            appkit-ui-action-property nil))
          (dolist (entry '(("@alice" . "https://github.com/alice")
                           ("@bob" . "https://github.com/bob")
                           ("external" . "https://example.test/@destination")))
            (goto-char (point-min))
            (search-forward (car entry))
            (goto-char (match-beginning 0))
            (chidu-activate-at-point)
            (should (equal (car visited) (cdr entry)))
            ;; g o is independent of the activated inline object.
            (chidu-browse-at-point)
            (should (equal (car visited) url)))
          (dolist (literal '("@inline" "unsafe" "+ ordinary"))
            (goto-char (point-min))
            (search-forward literal)
            (should-not (get-text-property (match-beginning 0) appkit-ui-action-property)))
          (goto-char (point-min))
          (search-forward "old")
          (should (get-text-property (match-beginning 0) 'line-prefix)))))
    (should (equal html (chidu-store-email-body-html-content body)))
    (should (equal "fallback" (chidu-store-email-body-text-content body)))))

(ert-deftest chidu-message-github-semantic-lists-and-literal-code ()
  (skip-unless (libxml-available-p))
  (let* ((parsed (chidu-message-github-parse
                  (concat "<h2>Title</h2><ol start=\"3\"><li>one<ul><li>nested</li></ul></li>"
                          "<li>two</li></ol><pre><code class=\"language-unknown\">"
                          "  @user &lt;tag&gt;\n</code></pre>"
                          "<p><code>a  b\nc</code></p>")))
         (blocks (appkit-markup-document-blocks (car parsed))))
    (should (appkit-markup-heading-p (nth 0 blocks)))
    (should (= 3 (appkit-markup-list-start (nth 1 blocks))))
    (should (appkit-markup-list-p
             (nth 1 (appkit-markup-list-item-blocks
                     (car (appkit-markup-list-items (nth 1 blocks)))))))
    (should (equal "  @user <tag>\n" (appkit-markup-preformatted-text (nth 2 blocks))))
    (should (equal "unknown" (appkit-markup-preformatted-language (nth 2 blocks))))
    (should (equal "a  b\nc" (appkit-markup-plain-text
                              (appkit-markup-document (last blocks)))))))

(ert-deftest chidu-message-github-fallback-keeps-plain-footer-and-ordinary-mail ()
  (let* ((url "https://github.com/example/project/pull/7#discussion_r42")
         (source (concat "> Alice wrote\n" (chidu-message-github-test--footer url)))
         (body (chidu-store-email-body-create :text-content source :html-content "<p>HTML</p>"))
         (participants (list (chidu-text-participant-create
                              :identity "mail:alice@example.test"
                              :name "Alice" :email "alice@example.test"
                              :face 'font-lock-variable-name-face)))
         (chidu-text-highlight-participants t))
    (dolist (sender '("ordinary@example.test" "notifications@github.com"))
      (with-temp-buffer
        (delay-mode-hooks (chidu-message-mode))
        (let ((inhibit-read-only t))
          (cl-letf (((symbol-function 'chidu-message-github-parse) (lambda (_) nil)))
            (chidu-message-insert-body body :sender sender :participants participants)))
        (should (equal source (buffer-substring-no-properties (point-min) (point-max))))
        (should (= 1 (get-text-property (point-min) 'chidu-message-quote-depth)))
        (should (equal "mail:alice@example.test"
                       (get-text-property (+ 2 (point-min)) 'chidu-person-identity)))
        (goto-char (point-min))
        (should (equal (chidu-browse--source-at-point)
                       (and (equal sender "notifications@github.com") url)))))
    ;; Missing libxml in an HTML-only notification must not route to SHR either.
    (with-temp-buffer
      (cl-letf (((symbol-function 'libxml-available-p) (lambda () nil))
                ((symbol-function 'chidu-message-insert-html)
                 (lambda (&rest _) (ert-fail "GitHub fallback reached SHR"))))
        (chidu-message-insert-body
         (chidu-store-email-body-create :text-content "" :html-content "<p>body</p>")
         :sender "notifications@github.com")
        (should (get-text-property (point-min) 'face))))))

(ert-deftest chidu-message-renderer-dispatch-preserves-inline-attachment-contract ()
  (let* ((body (chidu-store-email-body-create :text-content "" :html-content "<p>ordinary</p>"))
         (attachments (list 'inline-resource)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'chidu-message-insert-html)
                 (lambda (html view context)
                   (should (equal html "<p>ordinary</p>"))
                   (should (eq view 'view))
                   (should (eq context 'context))
                   (insert "ordinary")
                   attachments)))
        (should (eq attachments
                    (chidu-message-insert-body body :sender "ordinary@example.test"
                                               :view 'view :context 'context)))))
    (let ((chidu-message-body-render-functions
           (list (lambda (&rest _) nil)
                 (lambda (received sender view context)
                   (should (eq body received))
                   (should (equal sender "sender"))
                   (should (eq view 'view))
                   (should (eq context 'context))
                   (insert "native")
                   (cons t attachments))
                 (lambda (&rest _) (ert-fail "First renderer must win")))))
      (with-temp-buffer
        (should (eq attachments
                    (chidu-message-insert-body body :sender "sender" :view 'view :context 'context)))
        (should (equal "native" (buffer-string)))))))

(provide 'chidu-message-github-test)
;;; chidu-message-github-test.el ends here
