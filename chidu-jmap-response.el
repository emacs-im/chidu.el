;;; chidu-jmap-response.el --- Strict JMAP method response envelope -*- lexical-binding: t; -*-

;;; Commentary:

;; Parse the common JMAP Response/Invocation envelope once.  Object-specific
;; adapters remain responsible for properties, set coverage, and resource caps.

;;; Code:

(require 'cl-lib)
(require 'chidu-record)
(require 'chidu-jmap-types)

(chidu-define-record chidu-jmap-method-response
    "One validated single-call JMAP method response."
  session-state
  arguments)

(defun chidu-jmap--parse-method-response-item
    (item session-state expected-method expected-call-id expected-account-id)
  "Parse ITEM with SESSION-STATE.

Require EXPECTED-METHOD, EXPECTED-CALL-ID, and EXPECTED-ACCOUNT-ID."
  (unless (and (vectorp item) (= 3 (length item)))
    (signal 'chidu-jmap-error
            (list (format "%s method response is malformed"
                          expected-method))))
  (let* ((name (chidu-jmap--string (aref item 0) "method response name"))
         (arguments
          (chidu-jmap--hash
           (aref item 1) (format "%s arguments" expected-method)))
         (call-id
          (chidu-jmap--string (aref item 2) "method response call id")))
    (unless (equal call-id expected-call-id)
      (signal 'chidu-jmap-error
              (list (format "%s call id mismatch" expected-method))))
    (when (equal name "error")
      (signal
       'chidu-jmap-error
       (list
        (format
         "%s method error: %s"
         expected-method
         (chidu-jmap--string
          (chidu-jmap--required
           arguments "type" (format "%s method error" expected-method))
          "method error type")))))
    (unless (equal name expected-method)
      (signal 'chidu-jmap-error
              (list (format "%s returned unexpected method"
                            expected-method))))
    (let ((actual-account-id
           (chidu-jmap--id
            (chidu-jmap--required arguments "accountId" expected-method)
            (format "%s accountId" expected-method))))
      (unless (equal actual-account-id expected-account-id)
        (signal 'chidu-jmap-error
                (list (format "%s accountId mismatch"
                              expected-method)))))
    (chidu-jmap-method-response-create
     :session-state session-state
     :arguments arguments)))

(defun chidu-jmap-parse-method-responses (bytes expected)
  "Parse JMAP BYTES according to ordered EXPECTED method specifications.

EXPECTED is a list of (METHOD CALL-ID ACCOUNT-ID).  Return a vector of
`chidu-jmap-method-response' values in the same order."
  (let* ((wire (chidu-jmap--parse-json-object bytes "JMAP API response"))
         (session-state
          (chidu-jmap--string
           (chidu-jmap--required wire "sessionState" "JMAP API response")
           "API sessionState"))
         (responses
          (chidu-jmap--vector
           (chidu-jmap--required
            wire "methodResponses" "JMAP API response")
           "methodResponses")))
    (unless (= (length responses) (length expected))
      (signal 'chidu-jmap-error
              '("JMAP API returned unexpected response count")))
    (vconcat
     (cl-loop
      for item across responses
      for specification in expected
      collect
      (pcase-let ((`(,method ,call-id ,account-id) specification))
        (chidu-jmap--parse-method-response-item
         item session-state method call-id account-id))))))

(defun chidu-jmap-parse-single-method-response
    (bytes expected-method expected-call-id expected-account-id)
  "Parse BYTES for EXPECTED-METHOD, EXPECTED-CALL-ID, and EXPECTED-ACCOUNT-ID."
  (aref
   (chidu-jmap-parse-method-responses
    bytes
    (list (list expected-method expected-call-id expected-account-id)))
   0))

(provide 'chidu-jmap-response)

;;; chidu-jmap-response.el ends here
