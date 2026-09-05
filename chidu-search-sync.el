;;; chidu-search-sync.el --- Refresh and extend Email search -*- lexical-binding: t; -*-

;;; Commentary:

;; Search refresh replaces one committed query window.  Load-more anchors after
;; the durable last query id and appends one page only while queryState remains
;; unchanged.  The combined JMAP request keeps Email/query, Email/get, and
;; SearchSnippet/get aligned through result references.

;;; Code:

(require 'chidu-jmap-email)
(require 'chidu-jmap-search)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-search-query)
(require 'chidu-store)

(defcustom chidu-search-page-size 50
  "Number of Email hits requested for each search page."
  :type 'positive-integer
  :group 'chidu)

(defun chidu-search-sync--finish
    (runtime state generation operation result success-function error-function)
  "Finish RUNTIME search STATE GENERATION OPERATION with RESULT.

Call SUCCESS-FUNCTION for success or ERROR-FUNCTION for failure."
  (chidu-runtime--finish-account-sync
   runtime state generation operation result success-function error-function))

(defun chidu-search-sync--commit-operation
    (state mode context spec observation)
  "Return Store operation for STATE, MODE, CONTEXT, SPEC, and OBSERVATION."
  (let ((account-id (chidu-account-runtime-account-id state))
        (query-key (chidu-search-spec-query-key spec))
        (revision (chidu-store-search-context-revision context)))
    (pcase mode
      ('replace
       (chidu-store-op-replace-search-create
        :account-id account-id
        :query-key query-key
        :expected-revision revision
        :observation observation))
      ('append
       (chidu-store-op-append-search-create
        :account-id account-id
        :query-key query-key
        :expected-revision revision
        :expected-query-state
        (chidu-store-search-context-query-state context)
        :expected-cursor-remote-email-id
        (chidu-store-search-context-cursor-remote-email-id context)
        :observation observation))
      (_ (signal 'chidu-invariant-error
                 (list "Unknown Search synchronization mode" mode))))))

(defun chidu-search-sync--after-fetch
    (runtime state generation operation mode context spec result
             success-function error-function)
  "Commit fetched RESULT for a search MODE in RUNTIME.

STATE, GENERATION, and OPERATION fence stale work.  CONTEXT and SPEC identify
the CAS base; call SUCCESS-FUNCTION or ERROR-FUNCTION after commit."
  (when (chidu-runtime--account-current-p
         runtime state generation operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-search-sync--finish
       runtime state generation operation result
       success-function error-function))
     ((chidu-result-ok-p result)
      (setf (chidu-account-runtime-phase state)
            (if (eq mode 'append)
                'search-appending
              'search-committing))
      (chidu-runtime--store-call
       runtime
       (chidu-search-sync--commit-operation
        state mode context spec (chidu-result-ok-value result))
       (lambda (commit-result)
         (chidu-search-sync--finish
          runtime state generation operation commit-result
          success-function error-function))))
     (t
      (chidu-search-sync--finish
       runtime state generation operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-search-sync--start-fetch
    (runtime state generation operation mode context spec requested-page-size
             success-function error-function)
  "Fetch one search MODE page for CONTEXT and SPEC in RUNTIME.

STATE, GENERATION, and OPERATION fence stale callbacks.  REQUESTED-PAGE-SIZE is
the user page size; call SUCCESS-FUNCTION or ERROR-FUNCTION after Store commit."
  (let* ((endpoint (chidu-store-search-context-endpoint context))
         (bounds
          (chidu-jmap-email-page-bounds endpoint requested-page-size))
         (page-size (car bounds))
         (request-limit (cdr bounds))
         secret)
    (if (null (chidu-store-endpoint-api-url endpoint))
        (chidu-search-sync--finish
         runtime state generation operation
         (chidu-result-failure-create
          :kind 'endpoint-not-connected
          :data (list :account-id (chidu-account-runtime-account-id state))
          :retryable-p nil)
         success-function error-function)
      (when (chidu-runtime--account-current-p
             runtime state generation operation)
        (condition-case error-data
            (setq secret (chidu-runtime--endpoint-secret endpoint))
          (error
           (chidu-search-sync--finish
            runtime state generation operation
            (chidu-runtime--condition-failure
             'credential-error error-data nil)
            success-function error-function))))
      (when (and secret
                 (chidu-runtime--account-current-p
                  runtime state generation operation))
        (setf (chidu-account-runtime-phase state)
              (if (eq mode 'append)
                  'search-page-fetching
                'search-fetching))
        (condition-case error-data
            (let ((cancel
                   (funcall
                    (if (eq mode 'append)
                        #'chidu-jmap-fetch-more-search
                      #'chidu-jmap-fetch-search-page)
                    context spec secret page-size request-limit
                    (lambda (result)
                      (chidu-search-sync--after-fetch
                       runtime state generation operation mode context spec
                       result success-function error-function)))))
              ;; The JMAP adapter owns and clears SECRET after this point.
              (setq secret nil)
              (when (chidu-runtime--account-current-p
                     runtime state generation operation)
                (setf (chidu-account-runtime-cancel-function state) cancel)
                (chidu-runtime--set-operation-cancel
                 runtime operation cancel)))
          (error
           (when secret (clear-string secret))
           (chidu-search-sync--finish
            runtime state generation operation
            (chidu-runtime--condition-failure
             'jmap-request-failed error-data nil)
            success-function error-function)))))))

(defun chidu-search-sync--after-load
    (runtime state generation operation mode result spec page-size
             success-function error-function)
  "Continue search MODE in RUNTIME after local RESULT.

STATE, GENERATION, and OPERATION fence stale callbacks.  SPEC and PAGE-SIZE
identify the request; call SUCCESS-FUNCTION or ERROR-FUNCTION after settlement."
  (when (chidu-runtime--account-current-p
         runtime state generation operation)
    (cond
     ((chidu-result-failure-p result)
      (chidu-search-sync--finish
       runtime state generation operation result
       success-function error-function))
     ((chidu-result-ok-p result)
      (let ((context (chidu-result-ok-value result)))
        (unless (chidu-store-search-context-p context)
          (signal 'chidu-invariant-error
                  (list "Search Store returned invalid context" context)))
        (if (and (eq mode 'append)
                 (not (chidu-store-search-context-maybe-more-p context)))
            (chidu-search-sync--finish
             runtime state generation operation
             (chidu-result-failure-create
              :kind 'pagination-exhausted
              :data
              (list :account-id (chidu-account-runtime-account-id state)
                    :query-key (chidu-search-spec-query-key spec))
              :retryable-p nil)
             success-function error-function)
          (chidu-search-sync--start-fetch
           runtime state generation operation mode context spec page-size
           success-function error-function))))
     (t
      (chidu-search-sync--finish
       runtime state generation operation
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil)
       success-function error-function)))))

(defun chidu-search-sync--start
    (runtime account spec mode success-function error-function limit)
  "Start one search MODE workflow for ACCOUNT and SPEC in RUNTIME.

Call SUCCESS-FUNCTION with committed context or ERROR-FUNCTION with failure;
LIMIT overrides `chidu-search-page-size'."
  (chidu-runtime--assert-open runtime)
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-search-spec-p spec)
    (signal 'wrong-type-argument (list 'chidu-search-spec-p spec)))
  (unless (memq mode '(replace append))
    (signal 'wrong-type-argument (list '(member replace append) mode)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let* ((page-size (or limit chidu-search-page-size))
         (account-id (chidu-store-account-account-id account))
         (state (chidu-runtime--account-state runtime account-id)))
    (unless (and (integerp page-size) (> page-size 0))
      (signal 'wrong-type-argument (list 'positive-integer-p page-size)))
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
        (setf (chidu-account-runtime-phase state)
              (if (eq mode 'append)
                  'search-page-loading
                'search-loading)
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
         (chidu-store-op-get-search-create
          :account-id account-id
          :query-key (chidu-search-spec-query-key spec))
         (lambda (result)
           (chidu-search-sync--after-load
            runtime state generation operation mode result spec page-size
            success-function error-function)))
        operation))))

(defun chidu-refresh-search
    (runtime account spec success-function error-function &optional limit)
  "Replace ACCOUNT's search SPEC in RUNTIME with the newest page.

Call SUCCESS-FUNCTION or ERROR-FUNCTION; LIMIT overrides the default page size."
  (chidu-search-sync--start
   runtime account spec 'replace success-function error-function limit))

(defun chidu-load-more-search
    (runtime account spec success-function error-function &optional limit)
  "Append ACCOUNT's next search SPEC page in RUNTIME.

Call SUCCESS-FUNCTION or ERROR-FUNCTION; LIMIT overrides the default page size."
  (chidu-search-sync--start
   runtime account spec 'append success-function error-function limit))

(provide 'chidu-search-sync)

;;; chidu-search-sync.el ends here
