;;; chidu-runtime.el --- In-process runtime for Chidu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Chidu is an ordinary Emacs Lisp application.  Domain state, JMAP workflows,
;; Store calls, and UI live in the interactive Emacs.  curl is already an
;; asynchronous subprocess boundary; a separate Store worker may be added later
;; behind the same closed Store operations if measurements justify it.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'appkit-app)
(require 'subr-x)
(require 'url-parse)
(require 'chidu-jmap-discovery)
(require 'chidu-result)
(require 'chidu-search-query)
(require 'chidu-store)

(declare-function chidu-store-sqlite-create
                  "chidu-store-sqlite" (data-root))

(defgroup chidu nil
  "JMAP-first mail client for Emacs."
  :group 'mail)

(defcustom chidu-data-root
  (let ((xdg-data-home (getenv "XDG_DATA_HOME")))
    (expand-file-name
     "chidu"
     (if (and xdg-data-home (not (string-empty-p xdg-data-home)))
         xdg-data-home
       (expand-file-name ".local/share" "~"))))
  "Directory containing Chidu's durable local Store."
  :type 'directory
  :group 'chidu)

(defcustom chidu-store-slow-operation-seconds 0.05
  "Warn when one Store operation takes longer than this many seconds.

Set to nil to disable warnings.  Measurements include queue time if a future
backend completes asynchronously."
  :type '(choice (const :tag "Disable warnings" nil) number)
  :group 'chidu)

(cl-defstruct (chidu-app-model (:constructor chidu-app-model-create))
  "Domain state and owned runtime resources for one Chidu App."
  runtime state
  (requests (make-hash-table :test #'equal))
  (resources (make-hash-table :test #'equal)))

(defun chidu-app-runtime (app)
  "Return the in-process runtime owned by APP's domain model."
  (chidu-app-model-runtime (appkit-app-model app)))

(defun chidu-app-state (app)
  "Return APP's committed Account and Mailbox directory state."
  (chidu-app-model-state (appkit-app-model app)))

(defun chidu-app-requests (app)
  "Return APP's incarnation-local domain workflow registry."
  (chidu-app-model-requests (appkit-app-model app)))

(defun chidu-app-resources (app)
  "Return APP's attachment resource cache."
  (chidu-app-model-resources (appkit-app-model app)))

(cl-defstruct (chidu-runtime-operation
               (:constructor chidu-runtime-operation-create))
  "One cancelable in-process asynchronous operation."
  id
  cancel-function
  cancel-cleanup-function)

(cl-defstruct (chidu-account-runtime
               (:constructor chidu-account-runtime-create))
  "Ephemeral synchronization state for one local Account."
  account-id
  (phase 'idle)
  (generation 0)
  operation-id
  cancel-function)

(cl-defstruct (chidu-runtime (:constructor chidu-runtime--create))
  "Resources and ephemeral state owned by one interactive Chidu application."
  data-root
  store
  (operations (make-hash-table :test #'eql))
  (next-operation-id 0)
  (accounts (make-hash-table :test #'equal))
  (store-call-count 0)
  (store-total-seconds 0.0)
  (store-max-seconds 0.0)
  last-store-operation
  (last-store-seconds 0.0)
  closed-p)

(cl-defun chidu-runtime-open (&key (data-root chidu-data-root) store)
  "Open an in-process Chidu runtime for DATA-ROOT, optionally using STORE."
  (let* ((root (expand-file-name data-root))
         opened-store)
    (condition-case error-data
        (progn
          (setq opened-store
                (or store
                    (progn
                      (require 'chidu-store-sqlite)
                      (chidu-store-sqlite-create root))))
          (unless (chidu-store-capability-p opened-store)
            (signal 'wrong-type-argument
                    (list 'chidu-store-capability-p opened-store)))
          (chidu-runtime--create :data-root root :store opened-store))
      (error
       (when (and opened-store (not store))
         (ignore-errors (chidu-store-close opened-store)))
       (signal (car error-data) (cdr error-data))))))

(defun chidu-runtime--assert-open (runtime)
  "Return RUNTIME, or signal when it is not live."
  (unless (chidu-runtime-p runtime)
    (signal 'wrong-type-argument (list 'chidu-runtime-p runtime)))
  (when (chidu-runtime-closed-p runtime)
    (signal 'chidu-invariant-error '("Chidu runtime is closed")))
  runtime)

(defun chidu-runtime--condition-failure (kind error-data &optional retryable-p)
  "Return failure KIND from ERROR-DATA, marked retryable by RETRYABLE-P."
  (chidu-result-failure-create
   :kind kind
   :data (list :message (error-message-string error-data))
   :retryable-p retryable-p))

(defun chidu-runtime--begin-operation (runtime)
  "Allocate and register one operation in RUNTIME."
  (chidu-runtime--assert-open runtime)
  (let* ((id (cl-incf (chidu-runtime-next-operation-id runtime)))
         (operation (chidu-runtime-operation-create :id id)))
    (puthash id operation (chidu-runtime-operations runtime))
    operation))

(defun chidu-runtime--operation-current-p (runtime operation)
  "Return non-nil when OPERATION is still current in RUNTIME."
  (and (chidu-runtime-p runtime)
       (not (chidu-runtime-closed-p runtime))
       (eq operation
           (gethash (chidu-runtime-operation-id operation)
                    (chidu-runtime-operations runtime)))))

(defun chidu-runtime--account-state (runtime account-id)
  "Return RUNTIME's ephemeral state for ACCOUNT-ID, creating it if needed."
  (or (gethash account-id (chidu-runtime-accounts runtime))
      (let ((state (chidu-account-runtime-create :account-id account-id)))
        (puthash account-id state (chidu-runtime-accounts runtime))
        state)))

(defun chidu-runtime--account-current-p
    (runtime state generation operation)
  "Return non-nil when RUNTIME Account STATE owns GENERATION and OPERATION."
  (and (chidu-runtime--operation-current-p runtime operation)
       (= generation (chidu-account-runtime-generation state))
       (eql (chidu-runtime-operation-id operation)
            (chidu-account-runtime-operation-id state))))

(defun chidu-runtime--finish-account-sync
    (runtime state generation operation result success-function error-function)
  "In RUNTIME, finish STATE GENERATION OPERATION with RESULT.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (when (chidu-runtime--account-current-p
         runtime state generation operation)
    (setf (chidu-account-runtime-phase state) 'idle
          (chidu-account-runtime-operation-id state) nil
          (chidu-account-runtime-cancel-function state) nil)
    (chidu-runtime--deliver-result
     runtime operation result success-function error-function)))

(defun chidu-runtime--set-operation-cancel (runtime operation cancel)
  "Record CANCEL for current OPERATION in RUNTIME."
  (when (and cancel
             (chidu-runtime--operation-current-p runtime operation))
    (unless (functionp cancel)
      (signal 'chidu-invariant-error
              (list "operation returned a non-function cancel value" cancel)))
    (setf (chidu-runtime-operation-cancel-function operation) cancel)))

(defun chidu-runtime--finish-operation (runtime operation)
  "Remove current OPERATION from RUNTIME and return non-nil if it was current."
  (when (chidu-runtime--operation-current-p runtime operation)
    (remhash (chidu-runtime-operation-id operation)
             (chidu-runtime-operations runtime))
    t))

(defun chidu-runtime-cancel-operation (runtime operation)
  "Cancel OPERATION if it is still current in RUNTIME."
  (when (and (chidu-runtime-p runtime)
             (chidu-runtime-operation-p operation)
             (eq operation
                 (gethash (chidu-runtime-operation-id operation)
                          (chidu-runtime-operations runtime))))
    (remhash (chidu-runtime-operation-id operation)
             (chidu-runtime-operations runtime))
    (when-let* ((cancel (chidu-runtime-operation-cancel-function operation)))
      (condition-case-unless-debug _error
          (funcall cancel)
        (error nil)))
    (when-let* ((cleanup
                 (chidu-runtime-operation-cancel-cleanup-function operation)))
      (condition-case-unless-debug _error
          (funcall cleanup)
        (error nil)))
    t))

(defun chidu-runtime--record-store-duration (runtime operation started)
  "Record elapsed Store time in RUNTIME for OPERATION started at STARTED."
  (let ((elapsed (- (float-time) started)))
    (cl-incf (chidu-runtime-store-call-count runtime))
    (cl-incf (chidu-runtime-store-total-seconds runtime) elapsed)
    (setf (chidu-runtime-store-max-seconds runtime)
          (max elapsed (chidu-runtime-store-max-seconds runtime))
          (chidu-runtime-last-store-operation runtime) (type-of operation)
          (chidu-runtime-last-store-seconds runtime) elapsed)
    (when (and chidu-store-slow-operation-seconds
               (> elapsed chidu-store-slow-operation-seconds))
      (display-warning
       'chidu
       (format "Store operation %S took %.3fs in interactive Emacs"
               (type-of operation) elapsed)
       :warning))
    elapsed))

(defun chidu-runtime-store-metrics (runtime)
  "Return bounded Store timing metrics for RUNTIME."
  (chidu-runtime--assert-open runtime)
  (list :count (chidu-runtime-store-call-count runtime)
        :total-seconds (chidu-runtime-store-total-seconds runtime)
        :max-seconds (chidu-runtime-store-max-seconds runtime)
        :last-operation (chidu-runtime-last-store-operation runtime)
        :last-seconds (chidu-runtime-last-store-seconds runtime)))

(defun chidu-runtime--store-call (runtime operation deliver)
  "Execute closed Store OPERATION in RUNTIME and call DELIVER with its result."
  (chidu-runtime--assert-open runtime)
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let ((started (float-time))
        (completed-p nil))
    (condition-case error-data
        (chidu-store-call
         (chidu-runtime-store runtime) operation
         (lambda (result)
           (unless completed-p
             (setq completed-p t)
             (chidu-runtime--record-store-duration runtime operation started)
             (funcall deliver result))))
      (error
       (if completed-p
           ;; A synchronous consumer callback fault is a programming error.
           ;; Do not misreport or silently swallow it as a Store failure.
           (signal (car error-data) (cdr error-data))
         (setq completed-p t)
         (chidu-runtime--record-store-duration runtime operation started)
         (funcall
          deliver
          (chidu-runtime--condition-failure
           'store-error error-data nil)))))))

(defun chidu-runtime--deliver-result
    (runtime operation result success-function error-function)
  "Complete OPERATION with RESULT in RUNTIME.

Dispatch to SUCCESS-FUNCTION or ERROR-FUNCTION."
  (when (chidu-runtime--finish-operation runtime operation)
    (cond
     ((chidu-result-ok-p result)
      (funcall success-function (chidu-result-ok-value result)))
     ((chidu-result-failure-p result)
      (funcall error-function result))
     (t
      (funcall
       error-function
       (chidu-result-failure-create
        :kind 'invalid-result
        :data (list :value result)
        :retryable-p nil))))))

(defun chidu-runtime--simple-store-operation
    (runtime store-operation success-function error-function)
  "Run STORE-OPERATION in RUNTIME and call SUCCESS-FUNCTION or ERROR-FUNCTION."
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let ((operation (chidu-runtime--begin-operation runtime)))
    (chidu-runtime--store-call
     runtime store-operation
     (lambda (result)
       (chidu-runtime--deliver-result
        runtime operation result success-function error-function)))
    operation))

(defun chidu-runtime-info (runtime success-function error-function)
  "Read RUNTIME's Store identity via SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--simple-store-operation
   runtime (chidu-store-op-runtime-create)
   success-function error-function))

(defun chidu-runtime-list-endpoints (runtime success-function error-function)
  "Read RUNTIME Endpoints via SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--simple-store-operation
   runtime (chidu-store-op-list-endpoints-create)
   success-function error-function))

(defun chidu-runtime-list-compose-workspaces
    (runtime success-function error-function)
  "Read RUNTIME Compose workspaces via SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--simple-store-operation
   runtime (chidu-store-op-list-compose-workspaces-create)
   success-function error-function))

(defun chidu-runtime-get-compose-workspace
    (runtime workspace-id success-function error-function)
  "Read WORKSPACE-ID from RUNTIME via SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-get-compose-workspace-create
    :workspace-id workspace-id)
   success-function error-function))

(defun chidu-runtime-drafts
    (runtime account mailbox limit success-function error-function)
  "Read ACCOUNT's bounded canonical Drafts MAILBOX from RUNTIME."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-mailbox-p mailbox)
    (signal 'wrong-type-argument (list 'chidu-store-mailbox-p mailbox)))
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'positive-integer-p limit)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-get-drafts-create
    :account-id (chidu-store-account-account-id account)
    :mailbox-id (chidu-store-mailbox-mailbox-id mailbox)
    :limit limit)
   success-function error-function))

(defun chidu-runtime-create-compose-workspace
    (runtime workspace-id account identity kind document
             success-function error-function)
  "Create WORKSPACE-ID in RUNTIME and settle via the supplied functions.

ACCOUNT, IDENTITY, KIND, and DOCUMENT initialize the local workspace."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-identity-p identity)
    (signal 'wrong-type-argument (list 'chidu-store-identity-p identity)))
  (unless (chidu-store-compose-document-p document)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-document-p document)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-create-compose-workspace-create
    :workspace-id workspace-id
    :account-id (chidu-store-account-account-id account)
    :identity-id (chidu-store-identity-identity-id identity)
    :kind kind
    :document document)
   success-function error-function))

(defun chidu-runtime-add-compose-resource
    (runtime workspace-id identity-id expected-revision revision document
             resource success-function error-function)
  "Checkpoint DOCUMENT and append RESOURCE below WORKSPACE-ID in RUNTIME."
  (unless (chidu-store-compose-resource-observation-p resource)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-resource-observation-p resource)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-add-compose-resource-create
    :workspace-id workspace-id
    :identity-id identity-id
    :expected-revision expected-revision
    :revision revision
    :document document
    :resource resource)
   success-function error-function))

(defun chidu-runtime-remove-compose-resource
    (runtime workspace-id identity-id expected-revision revision document
             resource-id success-function error-function)
  "Checkpoint DOCUMENT and remove RESOURCE-ID below WORKSPACE-ID in RUNTIME."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-remove-compose-resource-create
    :workspace-id workspace-id
    :identity-id identity-id
    :expected-revision expected-revision
    :revision revision
    :document document
    :resource-id resource-id)
   success-function error-function))

(defun chidu-runtime-set-compose-resource-blob
    (runtime workspace-id resource-id remote-blob-id
             success-function error-function)
  "Set RESOURCE-ID confirmed REMOTE-BLOB-ID below WORKSPACE-ID in RUNTIME."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-set-compose-resource-blob-create
    :workspace-id workspace-id
    :resource-id resource-id
    :remote-blob-id remote-blob-id)
   success-function error-function))

(defun chidu-runtime-checkpoint-compose-workspace
    (runtime workspace-id identity-id expected-revision revision document
             success-function error-function)
  "CAS-checkpoint WORKSPACE-ID REVISION over EXPECTED-REVISION in RUNTIME.

IDENTITY-ID and DOCUMENT are committed before settling the supplied functions."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-checkpoint-compose-workspace-create
    :workspace-id workspace-id
    :identity-id identity-id
    :expected-revision expected-revision
    :revision revision
    :document document)
   success-function error-function))

(defun chidu-runtime-accept-draft-publish
    (runtime workspace-id identity-id expected-revision revision document
             attempt-id message-id
             success-function error-function)
  "In RUNTIME, accept WORKSPACE-ID server Draft publication.

IDENTITY-ID, EXPECTED-REVISION, REVISION, DOCUMENT, ATTEMPT-ID, and MESSAGE-ID
freeze the exact local intent.  Call SUCCESS-FUNCTION or ERROR-FUNCTION."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-accept-draft-publish-create
    :workspace-id workspace-id
    :identity-id identity-id
    :expected-revision expected-revision
    :revision revision
    :document document
    :attempt-id attempt-id
    :message-id message-id)
   success-function error-function))

(defun chidu-runtime-mark-draft-publish-unknown
    (runtime attempt-id success-function error-function)
  "Fence ATTEMPT-ID before its remote create may run in RUNTIME."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-mark-draft-publish-unknown-create
    :attempt-id attempt-id)
   success-function error-function))

(defun chidu-runtime-retry-draft-publish-create
    (runtime attempt-id success-function error-function)
  "Return reconciled ATTEMPT-ID to a safe pending create in RUNTIME."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-retry-draft-publish-create-create
    :attempt-id attempt-id)
   success-function error-function))

(defun chidu-runtime-settle-draft-publish-create
    (runtime attempt-id outcome remote-email-id remote-blob-id error-kind
             success-function error-function)
  "Settle ATTEMPT-ID create OUTCOME in RUNTIME with remote identity evidence."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-settle-draft-publish-create-create
    :attempt-id attempt-id
    :outcome outcome
    :remote-email-id remote-email-id
    :remote-blob-id remote-blob-id
    :error-kind error-kind)
   success-function error-function))

(defun chidu-runtime-settle-draft-publish-cleanup
    (runtime attempt-id outcome error-kind success-function error-function)
  "Settle ATTEMPT-ID predecessor cleanup OUTCOME in RUNTIME."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-settle-draft-publish-cleanup-create
    :attempt-id attempt-id
    :outcome outcome
    :error-kind error-kind)
   success-function error-function))

(defun chidu-runtime-discard-compose-workspace
    (runtime workspace-id expected-revision success-function error-function)
  "CAS-discard WORKSPACE-ID at EXPECTED-REVISION from RUNTIME."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-discard-compose-workspace-create
    :workspace-id workspace-id
    :expected-revision expected-revision)
   success-function error-function))

(defun chidu-runtime-configure-endpoint
    (runtime session-url login authentication success-function error-function)
  "In RUNTIME, configure SESSION-URL, LOGIN, and AUTHENTICATION.

Call SUCCESS-FUNCTION with the Endpoint or ERROR-FUNCTION with a failure."
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-configure-endpoint-create
    :session-url session-url
    :login login
    :authentication authentication)
   success-function error-function))

(defun chidu-runtime-list-mailboxes
    (runtime account success-function error-function)
  "Read ACCOUNT's local Mailbox projection from RUNTIME."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-list-mailboxes-create
    :account-id (chidu-store-account-account-id account))
   success-function error-function))

(defun chidu-runtime-email-sync-context
    (runtime account success-function error-function)
  "Read ACCOUNT's durable Email synchronization context from RUNTIME."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-get-email-sync-context-create
    :account-id (chidu-store-account-account-id account))
   success-function error-function))

(defun chidu-runtime-mailbox-summary
    (runtime account mailbox limit success-function error-function)
  "Read ACCOUNT's canonical MAILBOX Summary from RUNTIME.

LIMIT is the maximum number of rows returned."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-mailbox-p mailbox)
    (signal 'wrong-type-argument (list 'chidu-store-mailbox-p mailbox)))
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'positive-integer-p limit)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-get-mailbox-summary-create
    :account-id (chidu-store-account-account-id account)
    :mailbox-id (chidu-store-mailbox-mailbox-id mailbox)
    :limit limit)
   success-function error-function))

(defun chidu-runtime-search
    (runtime account spec success-function error-function)
  "Read ACCOUNT's committed local search SPEC from RUNTIME."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-search-spec-p spec)
    (signal 'wrong-type-argument (list 'chidu-search-spec-p spec)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-get-search-create
    :account-id (chidu-store-account-account-id account)
    :query-key (chidu-search-spec-query-key spec))
   success-function error-function))

(defun chidu-runtime-email-body
    (runtime account row success-function error-function)
  "Read ACCOUNT's committed local body for Summary ROW from RUNTIME."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-email-summary-row-p row)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-summary-row-p row)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-get-email-body-create
    :account-id (chidu-store-account-account-id account)
    :local-email-id (chidu-store-email-summary-row-local-email-id row)
    :remote-email-id (chidu-store-email-summary-row-remote-email-id row))
   success-function error-function))

(defun chidu-runtime-parsed-blob
    (runtime account blob-id profile-version success-function error-function)
  "Read ACCOUNT's parsed BLOB-ID PROFILE-VERSION from RUNTIME."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (and (stringp blob-id) (not (string-empty-p blob-id)))
    (signal 'wrong-type-argument (list 'nonempty-string-p blob-id)))
  (unless (and (stringp profile-version)
               (not (string-empty-p profile-version)))
    (signal 'wrong-type-argument
            (list 'nonempty-string-p profile-version)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-get-parsed-blob-create
    :account-id (chidu-store-account-account-id account)
    :blob-id blob-id
    :profile-version profile-version)
   success-function error-function))

(defun chidu-runtime-conversation
    (runtime account remote-thread-id success-function error-function)
  "Read ACCOUNT's REMOTE-THREAD-ID Conversation from RUNTIME."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (and (stringp remote-thread-id)
               (not (string-empty-p remote-thread-id)))
    (signal 'wrong-type-argument
            (list 'nonempty-string-p remote-thread-id)))
  (chidu-runtime--simple-store-operation
   runtime
   (chidu-store-op-get-conversation-create
    :account-id (chidu-store-account-account-id account)
    :remote-thread-id remote-thread-id)
   success-function error-function))

(defun chidu-runtime--endpoint-origin (endpoint)
  "Return auth-source host and port for Store ENDPOINT."
  (let* ((url (url-generic-parse-url
               (chidu-store-endpoint-session-url endpoint)))
         (scheme (url-type url))
         (host (url-host url))
         (port (or (url-port url) 443)))
    (unless (and (string= scheme "https")
                 (stringp host) (not (string-empty-p host))
                 (integerp port) (> port 0) (< port 65536))
      (error "Endpoint has an invalid HTTPS Session URL: %s"
             (chidu-store-endpoint-session-url endpoint)))
    (list host port)))

(defun chidu-runtime--endpoint-secret (endpoint)
  "Return a mutable copy of ENDPOINT's auth-source secret, or signal."
  (pcase-let ((`(,host ,port) (chidu-runtime--endpoint-origin endpoint)))
    (let* ((entry
            (car
             (auth-source-search
              :max 1
              :host host
              :port port
              :user (chidu-store-endpoint-login endpoint)
              :require '(:user :secret))))
           (secret-value (and entry (plist-get entry :secret)))
           (secret (if (functionp secret-value)
                       (funcall secret-value)
                     secret-value)))
      (unless (and (stringp secret) (not (string-empty-p secret)))
        (error "No auth-source secret for %s:%d login %s"
               host port (chidu-store-endpoint-login endpoint)))
      (copy-sequence secret))))

(defun chidu-runtime-connect-endpoint
    (runtime endpoint success-function error-function)
  "In RUNTIME, discover ENDPOINT and call SUCCESS-FUNCTION or ERROR-FUNCTION.

The secret is read directly from `auth-source' and transferred to the curl
adapter directly.  It is never serialized through an intermediate protocol."
  (chidu-runtime--assert-open runtime)
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let ((operation (chidu-runtime--begin-operation runtime))
        secret)
    (when (chidu-runtime--operation-current-p runtime operation)
      (condition-case error-data
          (setq secret (chidu-runtime--endpoint-secret endpoint))
        (error
         (chidu-runtime--deliver-result
          runtime operation
          (chidu-runtime--condition-failure
           'credential-error error-data nil)
          success-function error-function))))
    (when (and secret
               (chidu-runtime--operation-current-p runtime operation))
      (let ((cancel
             (chidu-jmap-discover
              endpoint secret
              (lambda (result)
                (when (chidu-runtime--operation-current-p runtime operation)
                  (cond
                   ((chidu-result-failure-p result)
                    (chidu-runtime--deliver-result
                     runtime operation result
                     success-function error-function))
                   ((chidu-result-ok-p result)
                    (chidu-runtime--store-call
                     runtime
                     (chidu-store-op-observe-session-create
                      :endpoint-id
                      (chidu-store-endpoint-endpoint-id endpoint)
                      :observation (chidu-result-ok-value result))
                     (lambda (store-result)
                       (chidu-runtime--deliver-result
                        runtime operation store-result
                        success-function error-function))))
                   (t
                    (chidu-runtime--deliver-result
                     runtime operation result
                     success-function error-function))))))))
        ;; The JMAP adapter owns and clears SECRET after validation.
        (setq secret nil)
        (chidu-runtime--set-operation-cancel runtime operation cancel)))
    operation))

(defun chidu-runtime-error-message (failure)
  "Return a concise human-readable message for FAILURE or local error data."
  (cond
   ((stringp failure) failure)
   ((chidu-result-failure-p failure)
    (or (plist-get (chidu-result-failure-data failure) :message)
        (pcase (chidu-result-failure-kind failure)
          ('unknown-endpoint "Unknown JMAP Endpoint")
          ('unknown-account "Unknown JMAP Account")
          ('account-unavailable "JMAP Account is unavailable")
          ('account-read-only "JMAP Account is read-only")
          ('unknown-mailbox "Unknown JMAP Mailbox")
          ('mailbox-unavailable "JMAP Mailbox is unavailable")
          ('unknown-email "Unknown JMAP Email")
          ('email-not-found "JMAP Email is no longer available")
          ('email-index-unavailable "Canonical Email index is unavailable; build it from the Account menu")
          ('email-identity-mismatch "JMAP Email identity changed unexpectedly")
          ('mailbox-not-drafts "The selected Mailbox is not the Drafts Mailbox")
          ('mailbox-read-forbidden "The selected Mailbox is not readable")
          ('draft-no-longer-canonical "This Draft changed; refresh the Drafts view")
          ('draft-not-found "The server Draft no longer exists")
          ('draft-left-mailbox "The Email is no longer in the Drafts Mailbox")
          ('email-is-not-draft "The Email no longer has the $draft keyword")
          ('draft-identity-unavailable "No available JMAP Identity matches this Draft")
          ('draft-identity-ambiguous "More than one JMAP Identity matches this Draft")
          ('draft-originator-unsupported "The Draft From address cannot be reproduced by an available Identity")
          ('draft-body-truncated "The complete Draft body could not be fetched")
          ('draft-body-encoding-problem "The Draft body has a decoding problem")
          ('draft-body-structure-unsupported "The Draft MIME structure cannot be edited losslessly yet")
          ('draft-body-metadata-unsupported "The Draft body carries metadata Chidu cannot preserve yet")
          ('draft-metadata-unsupported "The external Draft carries metadata Chidu cannot preserve yet")
          ('draft-html-unsupported "Editing HTML Drafts is not implemented yet")
          ('unknown-compose-resource "Unknown Compose attachment")
          ('compose-resource-membership-conflict "Compose attachment membership changed unexpectedly")
          ('compose-resource-not-uploaded "A Compose attachment has not been uploaded")
          ('compose-resource-bytes-unavailable "Exact local bytes for a Compose attachment are unavailable")
          ('compose-resource-too-large "A Compose attachment exceeds the server upload limit")
          ('compose-attachments-too-large "Compose attachments exceed the server message limit")
          ('invalid-jmap-response "The JMAP server returned an invalid response")
          ('unknown-thread "Unknown JMAP Thread")
          ('thread-not-found "JMAP Thread is no longer available")
          ('thread-mismatch "JMAP Thread identity changed unexpectedly")
          ('invalid-search-query "Email search query is invalid")
          ('search-query-mismatch "Email search identity changed unexpectedly")
          ('projection-stale "The cached result changed locally; refresh before loading more")
          ('mailbox-move-same-mailbox "Source and destination Mailboxes are identical")
          ('mailbox-move-source-forbidden "The source Mailbox does not permit removing Email")
          ('mailbox-move-destination-forbidden "The destination Mailbox does not permit adding Email")
          ('mailbox-mutation-busy "Another Mailbox mutation is already active for this Account")
          ('trash-mailbox-unavailable "The Account has no available Trash Mailbox")
          ('trash-destination-forbidden "The Trash Mailbox does not allow adding Email")
          ('query-state-changed "The result order changed; refresh from the newest page")
          ('pagination-exhausted "No additional Email page is available")
          ('pagination-cursor-conflict "The Email page cursor changed; refresh the result")
          ('pagination-cursor-missing "The server did not return a usable Email page cursor")
          ('query-page-overlap "The next Email page overlapped the cached page; refresh the result")
          ('cannot-calculate-changes "Server cannot calculate the requested Email changes")
          ('session-state-conflict "JMAP Session changed during reconciliation")
          ('state-mismatch "JMAP state changed during reconciliation")
          ('unexpected-content-type "Server returned an unexpected content type")
          ('invalid-event-source "Server returned an invalid EventSource stream")
          ('event-source-startup-failed "Unable to start the EventSource request")
          ('endpoint-not-connected "JMAP Endpoint is not connected")
          ('account-busy "Account synchronization is already active")
          ('authentication-rejected "JMAP authentication was rejected")
          ('transport-unavailable "JMAP transport is unavailable")
          ('network-error "JMAP network request failed")
          ('http-error "JMAP HTTP request failed")
          ('canceled "Operation was canceled")
          (_ (format "Chidu operation failed: %s"
                     (chidu-result-failure-kind failure))))))
   ((and (consp failure) (symbolp (car failure)))
    (error-message-string failure))
   (t (format "%S" failure))))

(defun chidu-runtime-close (runtime)
  "Cancel active work and close RUNTIME once."
  (when (and (chidu-runtime-p runtime)
             (not (chidu-runtime-closed-p runtime)))
    (setf (chidu-runtime-closed-p runtime) t)
    (let (operations)
      (maphash
       (lambda (_id operation)
         (push operation operations))
       (chidu-runtime-operations runtime))
      (dolist (operation operations)
        (chidu-runtime-cancel-operation runtime operation)))
    (maphash
     (lambda (_account-id state)
       (setf (chidu-account-runtime-phase state) 'stopped
             (chidu-account-runtime-operation-id state) nil
             (chidu-account-runtime-cancel-function state) nil))
     (chidu-runtime-accounts runtime))
    (clrhash (chidu-runtime-accounts runtime))
    (when-let* ((store (chidu-runtime-store runtime)))
      (ignore-errors (chidu-store-close store))
      (setf (chidu-runtime-store runtime) nil))
    t))

(provide 'chidu-runtime)

;;; chidu-runtime.el ends here
