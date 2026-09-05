;;; chidu-result.el --- Typed operation results for Chidu -*- lexical-binding: t; -*-

;;; Commentary:

;; Expected Store and transport failures are data.  Unexpected programming
;; errors remain Lisp conditions.

;;; Code:

(require 'chidu-record)

(chidu-define-record chidu-result-ok
    "Successful asynchronous operation result."
  value)

(chidu-define-record chidu-result-failure
    "Expected Store, transport, or domain failure."
  kind
  data
  retryable-p)

(provide 'chidu-result)

;;; chidu-result.el ends here
