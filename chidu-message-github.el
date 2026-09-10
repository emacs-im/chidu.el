;;; chidu-message-github.el --- GitHub notification body presentation -*- lexical-binding: t; -*-

;;; Commentary:

;; GitHub notification plain-text parts retain Markdown source.  This adapter
;; owns the sender policy and native Markdown highlighting, not mail transport
;; or the reader's major mode.  A sender match is only a presentation hint.

;;; Code:

(require 'subr-x)
(require 'appkit-fontify)

(defcustom chidu-message-github-sender-addresses '("notifications@github.com")
  "Parsed From addresses whose plain-text bodies use GitHub presentation.
Matching is case-insensitive and exact, never against display names.
Additional notification addresses, such as an Enterprise instance's address,
may be added explicitly.  This is not a sender authentication policy."
  :type '(repeat string)
  :group 'chidu)

(defun chidu-message-github-fontify (text sender)
  "Return Markdown faces on unchanged notification TEXT from SENDER.
Return nil for other senders or unavailable highlighting.  Both Markdown
Tree-sitter grammars must already be installed; never install them here.
This function is suitable for `chidu-message-body-fontify-functions'."
  (when (and (stringp sender)
             (member-ignore-case (string-trim sender)
                                 chidu-message-github-sender-addresses)
             (treesit-ready-p '(markdown markdown-inline) t))
    (appkit-fontify-string text 'markdown-ts-mode)))

(provide 'chidu-message-github)

;;; chidu-message-github.el ends here
