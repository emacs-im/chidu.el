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

(provide 'chidu-evil-test)

;;; chidu-evil-test.el ends here
