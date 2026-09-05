;;; chidu-parse-sync.el --- Local-first parsed Blob materialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Materialize one account-scoped Blob through JMAP `Email/parse'.  Parsed Blob
;; state is independent from top-level Email identity and Account-wide Email
;; synchronization.  Operation ownership rejects late callbacks; the Store
;; revision is the canonical CAS fence.

;;; Code:

(require 'chidu-jmap-parse)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)

(defun chidu-parse-sync--deliver
    (runtime operation result success-function error-function)
  "Finish RUNTIME OPERATION with RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--deliver-result
   runtime operation result success-function error-function))

(defun chidu-parse-sync--after-fetch
    (runtime operation context result success-function error-function)
  "Commit parsed RESULT for CONTEXT in RUNTIME OPERATION."
  (when (chidu-runtime--operation-current-p runtime operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-parse-sync--deliver
       runtime operation result success-function error-function))
     ((chidu-result-ok-p result)
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-replace-parsed-blob-create
        :account-id
        (chidu-store-account-account-id
         (chidu-store-parsed-blob-context-account context))
        :blob-id (chidu-store-parsed-blob-context-blob-id context)
        :profile-version
        (chidu-store-parsed-blob-context-profile-version context)
        :expected-revision
        (chidu-store-parsed-blob-context-revision context)
        :observation (chidu-result-ok-value result))
       (lambda (store-result)
         (chidu-parse-sync--deliver
          runtime operation store-result
          success-function error-function))))
     (t
      (chidu-parse-sync--deliver
       runtime operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-parse-sync--start-fetch
    (runtime operation context body-value-byte-limit
             success-function error-function)
  "Start parsing CONTEXT in RUNTIME OPERATION with BODY-VALUE-BYTE-LIMIT."
  (let ((endpoint (chidu-store-parsed-blob-context-endpoint context))
        secret)
    (if (null (chidu-store-endpoint-api-url endpoint))
        (chidu-parse-sync--deliver
         runtime operation
         (chidu-result-failure-create
          :kind 'endpoint-not-connected
          :data
          (list :account-id
                (chidu-store-account-account-id
                 (chidu-store-parsed-blob-context-account context)))
          :retryable-p nil)
         success-function error-function)
      (condition-case error-data
          (setq secret (chidu-runtime--endpoint-secret endpoint))
        (error
         (chidu-parse-sync--deliver
          runtime operation
          (chidu-runtime--condition-failure
           'credential-error error-data nil)
          success-function error-function)))
      (when (and secret
                 (chidu-runtime--operation-current-p runtime operation))
        (condition-case error-data
            (let ((cancel
                   (chidu-jmap-fetch-parsed-blob
                    context secret body-value-byte-limit
                    (lambda (result)
                      (chidu-parse-sync--after-fetch
                       runtime operation context result
                       success-function error-function)))))
              ;; The JMAP adapter owns and clears SECRET after this point.
              (setq secret nil)
              (chidu-runtime--set-operation-cancel
               runtime operation cancel))
          (error
           (when secret (clear-string secret))
           (chidu-parse-sync--deliver
            runtime operation
            (chidu-runtime--condition-failure
             'jmap-request-failed error-data nil)
            success-function error-function)))))))

(defun chidu-parse-sync--after-load
    (runtime operation body-value-byte-limit result
             success-function error-function)
  "Continue RUNTIME OPERATION after local parsed Blob load RESULT.

BODY-VALUE-BYTE-LIMIT selects the parse profile.  Dispatch to SUCCESS-FUNCTION
or ERROR-FUNCTION."
  (when (chidu-runtime--operation-current-p runtime operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-parse-sync--deliver
       runtime operation result success-function error-function))
     ((chidu-result-ok-p result)
      (let ((context (chidu-result-ok-value result)))
        (unless (chidu-store-parsed-blob-context-p context)
          (signal 'chidu-invariant-error
                  (list "Parsed Blob Store returned invalid context" context)))
        (chidu-parse-sync--start-fetch
         runtime operation context body-value-byte-limit
         success-function error-function)))
     (t
      (chidu-parse-sync--deliver
       runtime operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-refresh-parsed-blob
    (runtime account blob-id success-function error-function
             &optional body-value-byte-limit)
  "Refresh ACCOUNT BLOB-ID as a read-only parsed message in RUNTIME.

Call SUCCESS-FUNCTION with the committed parsed Blob context or ERROR-FUNCTION
with a typed failure.  BODY-VALUE-BYTE-LIMIT defaults to
`chidu-email-body-value-byte-limit'."
  (chidu-runtime--assert-open runtime)
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (and (stringp blob-id) (not (string-empty-p blob-id)))
    (signal 'wrong-type-argument (list 'nonempty-string-p blob-id)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let* ((limit (or body-value-byte-limit
                    chidu-email-body-value-byte-limit))
         (profile-version (chidu-jmap-parse-profile-version limit))
         (operation (chidu-runtime--begin-operation runtime)))
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-get-parsed-blob-create
      :account-id (chidu-store-account-account-id account)
      :blob-id blob-id
      :profile-version profile-version)
     (lambda (result)
       (chidu-parse-sync--after-load
        runtime operation limit result
        success-function error-function)))
    operation))

(provide 'chidu-parse-sync)

;;; chidu-parse-sync.el ends here
