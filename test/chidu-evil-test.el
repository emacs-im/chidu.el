;;; chidu-evil-test.el --- Tests for Chidu's native Evil bindings -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'evil)
(require 'chidu)
(require 'chidu-conversation)
(require 'chidu-search)

(ert-deftest chidu-evil-registers-application-modes-in-normal-state ()
  (dolist (mode '(chidu-home-mode
                  chidu-summary-mode
                  chidu-search-mode
                  chidu-conversation-mode
                  chidu-message-mode))
    (should (eq (evil-initial-state mode) 'normal))))

(ert-deftest chidu-evil-reader-return-and-original-post-bindings ()
  (require 'chidu-parsed-message)
  (chidu-evil-setup)
  (dolist (mode '(chidu-message-mode chidu-conversation-mode
                  chidu-parsed-message-mode))
    (with-temp-buffer
      (funcall mode)
      (evil-local-mode 1)
      (dolist (state '(normal motion))
        (evil-change-state state)
        (should (eq (key-binding (kbd "g o")) #'chidu-browse-at-point))
        (should (eq (key-binding (kbd "RET")) #'chidu-activate-at-point))
        (should (eq (key-binding (kbd "<return>")) #'chidu-activate-at-point))))))

(provide 'chidu-evil-test)

;;; chidu-evil-test.el ends here
