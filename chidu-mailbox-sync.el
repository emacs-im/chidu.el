;;; chidu-mailbox-sync.el --- In-process Mailbox synchronization -*- lexical-binding: t; -*-

;;; Commentary:

;; One small Account workflow: load durable context, run asynchronous
;; Mailbox/get, then commit through a closed Store CAS operation.

;;; Code:

(require 'chidu-jmap-mailbox)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)

(defun chidu-runtime--sync-after-commit
    (runtime state generation operation result success-function error-function)
  "Finish RUNTIME STATE GENERATION OPERATION after commit RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--finish-account-sync
   runtime state generation operation result success-function error-function))

(defun chidu-runtime--sync-after-fetch
    (runtime state generation operation context result
             success-function error-function)
  "Continue RUNTIME STATE GENERATION OPERATION after CONTEXT fetch RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (when (chidu-runtime--account-current-p
         runtime state generation operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-runtime--finish-account-sync
       runtime state generation operation result
       success-function error-function))
     ((chidu-result-ok-p result)
      (setf (chidu-account-runtime-phase state) 'committing)
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-observe-mailbox-snapshot-create
        :account-id (chidu-account-runtime-account-id state)
        :expected-revision
        (chidu-store-mailbox-sync-context-revision context)
        :observation (chidu-result-ok-value result))
       (lambda (commit-result)
         (chidu-runtime--sync-after-commit
          runtime state generation operation commit-result
          success-function error-function))))
     (t
      (chidu-runtime--finish-account-sync
       runtime state generation operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-runtime--sync-start-fetch
    (runtime state generation operation context success-function error-function)
  "For RUNTIME STATE GENERATION OPERATION, fetch sync CONTEXT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (let ((endpoint (chidu-store-mailbox-sync-context-endpoint context))
        secret)
    (when (chidu-runtime--account-current-p
           runtime state generation operation)
      (condition-case error-data
          (setq secret (chidu-runtime--endpoint-secret endpoint))
        (error
         (chidu-runtime--finish-account-sync
          runtime state generation operation
          (chidu-runtime--condition-failure
           'credential-error error-data nil)
          success-function error-function))))
    (when (and secret
               (chidu-runtime--account-current-p
                runtime state generation operation))
      (setf (chidu-account-runtime-phase state) 'fetching)
      (let ((cancel
             (chidu-jmap-fetch-mailboxes
              context secret
              (lambda (result)
                (chidu-runtime--sync-after-fetch
                 runtime state generation operation context result
                 success-function error-function)))))
        ;; The JMAP adapter owns and clears SECRET after validation.
        (setq secret nil)
        (when (chidu-runtime--account-current-p
               runtime state generation operation)
          (setf (chidu-account-runtime-cancel-function state) cancel)
          (chidu-runtime--set-operation-cancel
           runtime operation cancel))))))

(defun chidu-runtime--sync-after-load
    (runtime state generation operation result success-function error-function)
  "Continue RUNTIME STATE GENERATION OPERATION after load RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (when (chidu-runtime--account-current-p
         runtime state generation operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-runtime--finish-account-sync
       runtime state generation operation result
       success-function error-function))
     ((chidu-result-ok-p result)
      (chidu-runtime--sync-start-fetch
       runtime state generation operation (chidu-result-ok-value result)
       success-function error-function))
     (t
      (chidu-runtime--finish-account-sync
       runtime state generation operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-runtime-sync-mailboxes
    (runtime account success-function error-function)
  "In RUNTIME, synchronize ACCOUNT and call SUCCESS-FUNCTION or ERROR-FUNCTION.

Only the curl request is asynchronous.  The Store transition remains a closed
operation and is currently executed in-process."
  (chidu-runtime--assert-open runtime)
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let* ((account-id (chidu-store-account-account-id account))
         (state (chidu-runtime--account-state runtime account-id)))
    (if (not (eq 'idle (chidu-account-runtime-phase state)))
        (progn
          (funcall
           error-function
           (chidu-result-failure-create
            :kind 'account-busy
            :data (list :account-id account-id
                        :phase (chidu-account-runtime-phase state))
            :retryable-p t))
          nil)
      (let* ((operation (chidu-runtime--begin-operation runtime))
             (generation (cl-incf (chidu-account-runtime-generation state))))
        (setf (chidu-account-runtime-phase state) 'loading
              (chidu-account-runtime-operation-id state)
              (chidu-runtime-operation-id operation)
              (chidu-runtime-operation-cancel-cleanup-function operation)
              (lambda ()
                (when (eql (chidu-account-runtime-operation-id state)
                           (chidu-runtime-operation-id operation))
                  (setf (chidu-account-runtime-phase state) 'idle
                        (chidu-account-runtime-operation-id state) nil
                        (chidu-account-runtime-cancel-function state) nil))))
        (chidu-runtime--store-call
         runtime
         (chidu-store-op-get-mailbox-sync-context-create
          :account-id account-id)
         (lambda (result)
           (chidu-runtime--sync-after-load
            runtime state generation operation result
            success-function error-function)))
        operation))))

(provide 'chidu-mailbox-sync)

;;; chidu-mailbox-sync.el ends here
