;;; chidu-jmap-seen.el --- Exact JMAP $seen mutation -*- lexical-binding: t; -*-

;;; Commentary:

;; Apply one idempotent per-key Email/set patch.  The shared update-only Set
;; adapter owns transport and exact target coverage; this module maps its one
;; result into the narrow $seen domain response used by `chidu-seen.el'.

;;; Code:

(require 'chidu-jmap-set)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-store)

(chidu-define-record chidu-jmap-seen-response
    "Remote settlement of one explicit $seen mutation."
  outcome
  error-kind
  error-description)

(defun chidu-jmap-seen--updates (remote-email-id desired-seen-p)
  "Return one-target update map for REMOTE-EMAIL-ID and DESIRED-SEEN-P."
  (let ((updates (make-hash-table :test #'equal))
        (patch (make-hash-table :test #'equal)))
    (puthash "keywords/$seen" (if desired-seen-p t :json-null) patch)
    (puthash remote-email-id patch updates)
    updates))

(defun chidu-jmap-seen--request
    (remote-account-id remote-email-id desired-seen-p)
  "Return REMOTE-ACCOUNT-ID Email/set request for REMOTE-EMAIL-ID.

DESIRED-SEEN-P is the exact $seen value."
  (chidu-jmap-set-update-request
   remote-account-id "seen-set"
   (chidu-jmap-seen--updates remote-email-id desired-seen-p)))

(defun chidu-jmap-seen--response (response remote-email-id)
  "Return narrow $seen RESPONSE for REMOTE-EMAIL-ID."
  (let ((result
         (or (chidu-jmap-set-result-for-id response remote-email-id)
             (signal 'chidu-jmap-error
                     '("Email/set omitted the requested $seen target")))))
    (chidu-jmap-seen-response-create
     :outcome (chidu-jmap-set-target-result-outcome result)
     :error-kind (chidu-jmap-set-target-result-error-kind result)
     :error-description
     (chidu-jmap-set-target-result-error-description result))))

(defun chidu-jmap-seen--validate-response
    (bytes remote-account-id remote-email-id)
  "Validate Email/set BYTES for REMOTE-ACCOUNT-ID and REMOTE-EMAIL-ID."
  (chidu-jmap-seen--response
   (chidu-jmap-set-validate-update-response
    bytes "Email/set" "seen-set" remote-account-id
    (vector remote-email-id))
   remote-email-id))

(defun chidu-jmap-set-seen
    (context secret remote-email-id desired-seen-p deliver)
  "Set REMOTE-EMAIL-ID's $seen key in CONTEXT using owned SECRET.

DESIRED-SEEN-P is the exact target value; call DELIVER with the typed result."
  (unless (chidu-store-seen-context-p context)
    (signal 'wrong-type-argument (list 'chidu-store-seen-context-p context)))
  (unless (and (stringp remote-email-id)
               (not (string-empty-p remote-email-id)))
    (signal 'wrong-type-argument (list 'nonempty-string-p remote-email-id)))
  (unless (memq desired-seen-p '(nil t))
    (signal 'wrong-type-argument (list 'booleanp desired-seen-p)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (chidu-jmap-set-update
   (chidu-store-seen-context-endpoint context)
   (chidu-store-seen-context-account context)
   secret "seen-set"
   (chidu-jmap-seen--updates remote-email-id desired-seen-p)
   (lambda (result)
     (if (chidu-result-ok-p result)
         (funcall
          deliver
          (chidu-result-ok-create
           :value
           (chidu-jmap-seen--response
            (chidu-result-ok-value result) remote-email-id)))
       (funcall deliver result)))))

(provide 'chidu-jmap-seen)

;;; chidu-jmap-seen.el ends here
