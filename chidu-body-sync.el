;;; chidu-body-sync.el --- Selected Email body materialization -*- lexical-binding: t; -*-

;;; Commentary:

;; A per-message local-first workflow.  It does not occupy the Account-wide
;; synchronization lane: operation identity rejects late callbacks, while the
;; Store body revision supplies the canonical CAS fence.

;;; Code:

(require 'chidu-jmap-body)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)

(defun chidu-body-sync--deliver
    (runtime operation result success-function error-function)
  "Finish RUNTIME OPERATION with RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--deliver-result
   runtime operation result success-function error-function))

(defun chidu-body-sync--after-commit
    (runtime operation result success-function error-function)
  "Finish RUNTIME OPERATION after body Store RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-body-sync--deliver
   runtime operation result success-function error-function))

(defun chidu-body-sync--after-fetch
    (runtime operation context result success-function error-function)
  "Commit fetched body RESULT for CONTEXT in RUNTIME OPERATION."
  (when (chidu-runtime--operation-current-p runtime operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-body-sync--deliver
       runtime operation result success-function error-function))
     ((chidu-result-ok-p result)
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-replace-email-body-create
        :account-id
        (chidu-store-account-account-id
         (chidu-store-email-body-context-account context))
        :local-email-id
        (chidu-store-email-body-context-local-email-id context)
        :remote-email-id
        (chidu-store-email-body-context-remote-email-id context)
        :expected-revision
        (chidu-store-email-body-context-revision context)
        :observation (chidu-result-ok-value result))
       (lambda (store-result)
         (chidu-body-sync--after-commit
          runtime operation store-result
          success-function error-function))))
     (t
      (chidu-body-sync--deliver
       runtime operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-body-sync--start-fetch
    (runtime operation context success-function error-function)
  "Start selected body fetch for CONTEXT in RUNTIME OPERATION."
  (let ((endpoint (chidu-store-email-body-context-endpoint context))
        secret)
    (if (null (chidu-store-endpoint-api-url endpoint))
        (chidu-body-sync--deliver
         runtime operation
         (chidu-result-failure-create
          :kind 'endpoint-not-connected
          :data
          (list :account-id
                (chidu-store-account-account-id
                 (chidu-store-email-body-context-account context)))
          :retryable-p nil)
         success-function error-function)
      (condition-case error-data
          (setq secret (chidu-runtime--endpoint-secret endpoint))
        (error
         (chidu-body-sync--deliver
          runtime operation
          (chidu-runtime--condition-failure
           'credential-error error-data nil)
          success-function error-function)))
      (when (and secret
                 (chidu-runtime--operation-current-p runtime operation))
        (condition-case error-data
            (let ((cancel
                   (chidu-jmap-fetch-email-body
                    context secret chidu-email-body-value-byte-limit
                    (lambda (result)
                      (chidu-body-sync--after-fetch
                       runtime operation context result
                       success-function error-function)))))
              ;; The JMAP adapter owns and clears SECRET after this point.
              (setq secret nil)
              (chidu-runtime--set-operation-cancel
               runtime operation cancel))
          (error
           (when secret (clear-string secret))
           (chidu-body-sync--deliver
            runtime operation
            (chidu-runtime--condition-failure
             'jmap-request-failed error-data nil)
            success-function error-function)))))))

(defun chidu-body-sync--after-load
    (runtime operation result success-function error-function)
  "Continue RUNTIME OPERATION after local body load RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (when (chidu-runtime--operation-current-p runtime operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-body-sync--deliver
       runtime operation result success-function error-function))
     ((chidu-result-ok-p result)
      (let ((context (chidu-result-ok-value result)))
        (unless (chidu-store-email-body-context-p context)
          (signal 'chidu-invariant-error
                  (list "Body Store returned invalid context" context)))
        (chidu-body-sync--start-fetch
         runtime operation context success-function error-function)))
     (t
      (chidu-body-sync--deliver
       runtime operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-refresh-email-body
    (runtime account row success-function error-function)
  "Refresh ACCOUNT's selected Summary ROW body in RUNTIME.

Call SUCCESS-FUNCTION with the committed local body context or ERROR-FUNCTION
with a typed failure."
  (chidu-runtime--assert-open runtime)
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-email-summary-row-p row)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-summary-row-p row)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let ((operation (chidu-runtime--begin-operation runtime)))
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-get-email-body-create
      :account-id (chidu-store-account-account-id account)
      :local-email-id (chidu-store-email-summary-row-local-email-id row)
      :remote-email-id (chidu-store-email-summary-row-remote-email-id row))
     (lambda (result)
       (chidu-body-sync--after-load
        runtime operation result success-function error-function)))
    operation))

(provide 'chidu-body-sync)

;;; chidu-body-sync.el ends here
