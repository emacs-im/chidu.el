;;; chidu-browse-test.el --- Exact mail web-target tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'chidu-browse)
(require 'chidu-conversation)

(ert-deftest chidu-browse-source-never-crosses-conversation-rows ()
  (with-temp-buffer
    (delay-mode-hooks (chidu-conversation-mode))
    (let ((inhibit-read-only t) visited second)
      (insert (propertize "First heading\nFirst body\n"
                          'chidu-conversation-email-id "first"))
      (put-text-property 15 20 'chidu-browse-source-url "https://github.com/example/first")
      (setq second (point))
      (insert (propertize "Second heading\nSecond body\n"
                          'chidu-conversation-email-id "second"))
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (push url visited))))
        (goto-char (point-min))
        (chidu-browse-at-point)
        (should (equal visited '("https://github.com/example/first")))
        (goto-char second)
        (should-error (chidu-browse-at-point) :type 'user-error)
        (put-text-property second (point-max) 'chidu-browse-source-url
                           "https://github.com/example/second#comment")
        (chidu-browse-add-link second (+ second 6) "https://github.com/alice")
        (chidu-browse-at-point)
        (should (equal (car visited) "https://github.com/example/second#comment"))
        (chidu-activate-at-point)
        (should (equal (car visited) "https://github.com/alice"))
        (forward-line 1)
        (chidu-browse-at-point)
        (should (equal (car visited) "https://github.com/example/second#comment"))))))

(ert-deftest chidu-browse-does-not-dispatch-non-web-uris ()
  (cl-letf (((symbol-function 'browse-url)
             (lambda (&rest _) (ert-fail "Invalid target reached browser"))))
    (dolist (url '("javascript:alert(1)" "file:///tmp/example" "https://user@example.test/"
                   "https://github.com/\nmalformed"))
      (should-error (chidu-browse-open url) :type 'user-error))))

(ert-deftest chidu-activate-links-controls-and-row-focus-not-source ()
  (with-temp-buffer
    (delay-mode-hooks (chidu-conversation-mode))
    (let ((inhibit-read-only t) visited activated focused)
      (insert "https://example.test/written\nHTML label\nControl\nRow\n")
      (put-text-property (point-min) (point-max)
                         'chidu-browse-source-url "https://github.com/example/source")
      (goto-char (point-min))
      (forward-line 1)
      (put-text-property (point) (line-end-position) 'shr-url "https://example.test/html")
      (forward-line 1)
      (appkit-ui-add-action (point) (line-end-position) (lambda () (setq activated t)))
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (push url visited)))
                ((symbol-function 'chidu-conversation-focus)
                 (lambda () (setq focused t))))
        (goto-char (point-min))
        (chidu-activate-at-point)
        (should (equal (car visited) "https://example.test/written"))
        (forward-line 1)
        (chidu-activate-at-point)
        (should (equal (car visited) "https://example.test/html"))
        (forward-line 1)
        (chidu-activate-at-point)
        (should activated)
        (forward-line 1)
        (chidu-activate-at-point)
        (should focused)
        (should (= 2 (length visited)))
        (setq major-mode 'chidu-message-mode)
        (should-error (chidu-activate-at-point) :type 'user-error)
        (should (= 2 (length visited)))))))

(ert-deftest chidu-activate-prefers-semantic-link-over-url-looking-label ()
  (with-temp-buffer
    (appkit-markup-ui-insert-document
     (appkit-markup-document
      (list (appkit-markup-paragraph
             (list (appkit-markup-link "https://example.test/target"
                                      (list (appkit-markup-text "https://example.test/label")))))))
     :interactive-p t
     :link-action (lambda (url) (apply-partially #'chidu-browse-open url)))
    (goto-char (point-min))
    (let (visited)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq visited url))))
        (chidu-activate-at-point)
        (should (equal visited "https://example.test/target"))))))

(ert-deftest chidu-browse-source-marker-survives-outer-row-properties ()
  (with-temp-buffer
    (delay-mode-hooks (chidu-conversation-mode))
    (let ((inhibit-read-only t) second
          (url "https://github.com/example/first#comment"))
      (chidu-conversation--insert-body-region
       "" '(chidu-conversation-email-id "first" rear-nonsticky (chidu-conversation-email-id))
       (lambda ()
         (let ((start (point)))
           (insert "First body\n")
           (chidu-message-github--set-source start (point) url))))
      (setq second (point))
      (chidu-conversation--insert-body-region
       "" '(chidu-conversation-email-id "second" rear-nonsticky (chidu-conversation-email-id))
       (lambda () (insert "Second body\n")))
      (goto-char (point-min))
      (should (equal url (chidu-browse--source-at-point)))
      (goto-char second)
      (should-not (chidu-browse--source-at-point)))))

(ert-deftest chidu-reader-native-return-bindings ()
  (require 'chidu-parsed-message)
  (dolist (map (list chidu-message-mode-map chidu-parsed-message-mode-map
                     chidu-conversation-mode-map))
    (dolist (key '("RET" "<return>"))
      (should (eq (keymap-lookup map key) #'chidu-activate-at-point)))))

(provide 'chidu-browse-test)

;;; chidu-browse-test.el ends here
