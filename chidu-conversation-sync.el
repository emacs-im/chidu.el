;;; chidu-conversation-sync.el --- Refresh on-demand Conversations -*- lexical-binding: t; -*-

;;; Commentary:

;; A per-Thread local-first workflow: load the committed projection, fetch
;; bounded Thread/Email metadata, then CAS-replace the reply tree.  It uses a
;; normal runtime operation rather than occupying the Account bootstrap lane.

;;; Code:

(require 'chidu-jmap-conversation)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)

(defun chidu-conversation-sync--deliver
    (runtime operation result success-function error-function)
  "Finish RUNTIME OPERATION with RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--deliver-result
   runtime operation result success-function error-function))

(defun chidu-conversation-sync--after-fetch
    (runtime operation context result success-function error-function)
  "Commit fetched Conversation RESULT against CONTEXT in RUNTIME OPERATION."
  (when (chidu-runtime--operation-current-p runtime operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-conversation-sync--deliver
       runtime operation result success-function error-function))
     ((chidu-result-ok-p result)
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-replace-conversation-create
        :account-id
        (chidu-store-account-account-id
         (chidu-store-conversation-context-account context))
        :remote-thread-id
        (chidu-store-conversation-context-remote-thread-id context)
        :expected-revision
        (chidu-store-conversation-context-revision context)
        :observation (chidu-result-ok-value result))
       (lambda (store-result)
         (chidu-conversation-sync--deliver
          runtime operation store-result
          success-function error-function))))
     (t
      (chidu-conversation-sync--deliver
       runtime operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-conversation-sync--start-fetch
    (runtime operation context limit success-function error-function)
  "Fetch CONTEXT with LIMIT in RUNTIME OPERATION."
  (let ((endpoint (chidu-store-conversation-context-endpoint context))
        secret)
    (if (null (chidu-store-endpoint-api-url endpoint))
        (chidu-conversation-sync--deliver
         runtime operation
         (chidu-result-failure-create
          :kind 'endpoint-not-connected
          :data
          (list :account-id
                (chidu-store-account-account-id
                 (chidu-store-conversation-context-account context)))
          :retryable-p nil)
         success-function error-function)
      (condition-case error-data
          (setq secret (chidu-runtime--endpoint-secret endpoint))
        (error
         (chidu-conversation-sync--deliver
          runtime operation
          (chidu-runtime--condition-failure
           'credential-error error-data nil)
          success-function error-function)))
      (when (and secret
                 (chidu-runtime--operation-current-p runtime operation))
        (condition-case error-data
            (let ((cancel
                   (chidu-jmap-fetch-conversation
                    context secret limit
                    (lambda (result)
                      (chidu-conversation-sync--after-fetch
                       runtime operation context result
                       success-function error-function)))))
              ;; The JMAP adapter owns and clears SECRET after this point.
              (setq secret nil)
              (chidu-runtime--set-operation-cancel
               runtime operation cancel))
          (error
           (when secret (clear-string secret))
           (chidu-conversation-sync--deliver
            runtime operation
            (chidu-runtime--condition-failure
             'jmap-request-failed error-data nil)
            success-function error-function)))))))

(defun chidu-conversation-sync--after-load
    (runtime operation result limit success-function error-function)
  "Continue RUNTIME OPERATION after local RESULT using LIMIT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (when (chidu-runtime--operation-current-p runtime operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-conversation-sync--deliver
       runtime operation result success-function error-function))
     ((chidu-result-ok-p result)
      (let ((context (chidu-result-ok-value result)))
        (unless (chidu-store-conversation-context-p context)
          (signal 'chidu-invariant-error
                  (list "Conversation Store returned invalid context" context)))
        (chidu-conversation-sync--start-fetch
         runtime operation context limit
         success-function error-function)))
     (t
      (chidu-conversation-sync--deliver
       runtime operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-refresh-conversation
    (runtime account remote-thread-id success-function error-function
             &optional limit)
  "Refresh ACCOUNT's REMOTE-THREAD-ID Conversation in RUNTIME.

Call SUCCESS-FUNCTION with the committed local projection or ERROR-FUNCTION
with a typed failure.  LIMIT defaults to `chidu-conversation-email-limit'."
  (chidu-runtime--assert-open runtime)
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (and (stringp remote-thread-id)
               (not (string-empty-p remote-thread-id)))
    (signal 'wrong-type-argument
            (list 'nonempty-string-p remote-thread-id)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let ((page-limit (or limit chidu-conversation-email-limit))
        (operation (chidu-runtime--begin-operation runtime)))
    (unless (and (integerp page-limit) (> page-limit 0))
      (signal 'wrong-type-argument (list 'positive-integer-p page-limit)))
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-get-conversation-create
      :account-id (chidu-store-account-account-id account)
      :remote-thread-id remote-thread-id)
     (lambda (result)
       (chidu-conversation-sync--after-load
        runtime operation result page-limit
        success-function error-function)))
    operation))

(provide 'chidu-conversation-sync)

;;; chidu-conversation-sync.el ends here
