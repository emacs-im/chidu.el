;;; chidu-draft.el --- Durable JMAP Draft publication -*- lexical-binding: t; -*-

;;; Commentary:

;; Publish one immutable ComposeWorkspace revision as a JMAP Draft Email.  The
;; create is durably accepted before the request, uncertain responses are
;; reconciled by Message-ID, and a confirmed new remote head is adopted before
;; its predecessor is destroyed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-compose-resource)
(require 'chidu-jmap-draft)
(require 'chidu-jmap-upload)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)

(chidu-define-record chidu-draft-publish-result
    "Settled user-visible result of one server Draft publication workflow."
  context
  status
  error-kind)

(cl-defstruct (chidu-draft-workflow
               (:constructor chidu-draft-workflow-create))
  "Ephemeral control for one durable Draft publication workflow."
  runtime
  runtime-operation
  context
  identity
  generation
  document
  success-function
  error-function
  (effect-id 0)
  cleanup-error-kind)

(defun chidu-draft--current-p (workflow)
  "Return non-nil when WORKFLOW still owns its runtime operation."
  (chidu-runtime--operation-current-p
   (chidu-draft-workflow-runtime workflow)
   (chidu-draft-workflow-runtime-operation workflow)))

(defun chidu-draft--finish (workflow result)
  "Finish current WORKFLOW successfully with RESULT."
  (let ((runtime (chidu-draft-workflow-runtime workflow))
        (operation (chidu-draft-workflow-runtime-operation workflow)))
    (when (chidu-runtime--finish-operation runtime operation)
      (funcall (chidu-draft-workflow-success-function workflow) result))))

(defun chidu-draft--fail (workflow failure)
  "Finish current WORKFLOW with typed FAILURE."
  (let ((runtime (chidu-draft-workflow-runtime workflow))
        (operation (chidu-draft-workflow-runtime-operation workflow)))
    (when (chidu-runtime--finish-operation runtime operation)
      (funcall (chidu-draft-workflow-error-function workflow) failure))))

(defun chidu-draft--failure-kind (failure)
  "Return stable text identifying typed FAILURE."
  (if (chidu-result-failure-p failure)
      (symbol-name (chidu-result-failure-kind failure))
    "invalid-result"))

(defun chidu-draft--result (context status &optional error-kind)
  "Return publication result for CONTEXT STATUS and ERROR-KIND."
  (chidu-draft-publish-result-create
   :context context :status status :error-kind error-kind))

(defun chidu-draft--attempt (workflow)
  "Return WORKFLOW's current durable publication attempt, or nil."
  (chidu-store-compose-context-publish-attempt
   (chidu-draft-workflow-context workflow)))

(defun chidu-draft--cleanup-attempt (workflow)
  "Return WORKFLOW's oldest durable predecessor cleanup, or nil."
  (let ((attempts
         (chidu-store-compose-context-cleanup-attempts
          (chidu-draft-workflow-context workflow))))
    (and (> (length attempts) 0) (aref attempts 0))))

(defun chidu-draft--set-context (workflow context)
  "Install durable CONTEXT in WORKFLOW and return it."
  (setf (chidu-draft-workflow-context workflow) context)
  context)

(defun chidu-draft--store (workflow operation callback)
  "Run Store OPERATION for current WORKFLOW and call CALLBACK with its result."
  (when (chidu-draft--current-p workflow)
    (chidu-runtime--store-call
     (chidu-draft-workflow-runtime workflow) operation callback)))

(defun chidu-draft--message-id (attempt-id identity)
  "Return stable Message-ID text for ATTEMPT-ID and IDENTITY."
  (let* ((email (chidu-store-identity-email identity))
         (at (string-match "@\\([^@]+\\)\\'" email)))
    (unless at
      (signal 'chidu-invariant-error
              (list "Identity email has no domain" :email email)))
    (format "chidu.%s@%s" attempt-id (match-string 1 email))))

(defun chidu-draft--settle-create
    (workflow outcome remote-email-id remote-blob-id error-kind callback)
  "Settle WORKFLOW create OUTCOME with remote identity evidence.

REMOTE-EMAIL-ID and REMOTE-BLOB-ID identify a successful create.
ERROR-KIND identifies failure.  Call CALLBACK with the Store result."
  (let ((attempt (or (chidu-draft--attempt workflow)
                     (signal 'chidu-invariant-error
                             '("Draft publication attempt disappeared")))))
    (chidu-draft--store
     workflow
     (chidu-store-op-settle-draft-publish-create-create
      :attempt-id (chidu-store-draft-publish-attempt-attempt-id attempt)
      :outcome outcome
      :remote-email-id remote-email-id
      :remote-blob-id remote-blob-id
      :error-kind error-kind)
     callback)))

(defun chidu-draft--after-cleanup-settlement
    (workflow outcome error-kind result)
  "Continue WORKFLOW after cleanup OUTCOME Store RESULT and ERROR-KIND."
  (cond
   ((chidu-result-failure-p result) (chidu-draft--fail workflow result))
   ((chidu-result-ok-p result)
    (let ((context
           (chidu-draft--set-context
            workflow (chidu-result-ok-value result))))
      (when (eq outcome 'rejected)
        (setf (chidu-draft-workflow-cleanup-error-kind workflow) error-kind))
      (cond
       ((and (memq outcome '(succeeded rejected))
             (chidu-draft--cleanup-attempt workflow))
        (chidu-draft--dispatch-cleanup workflow))
       ((eq outcome 'unknown)
        (chidu-draft--finish
         workflow
         (chidu-draft--result context 'cleanup-pending error-kind)))
       ((chidu-draft-workflow-cleanup-error-kind workflow)
        (chidu-draft--finish
         workflow
         (chidu-draft--result
          context 'cleanup-conflict
          (chidu-draft-workflow-cleanup-error-kind workflow))))
       (t
        (chidu-draft--finish
         workflow (chidu-draft--result context 'saved))))))
   (t
    (chidu-draft--fail
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-draft--settle-cleanup (workflow outcome error-kind)
  "Settle WORKFLOW predecessor cleanup OUTCOME and ERROR-KIND."
  (let ((attempt
         (or (chidu-draft--cleanup-attempt workflow)
             (signal 'chidu-invariant-error
                     '("Draft cleanup attempt disappeared")))))
    (chidu-draft--store
     workflow
     (chidu-store-op-settle-draft-publish-cleanup-create
      :attempt-id (chidu-store-draft-publish-attempt-attempt-id attempt)
      :outcome outcome
      :error-kind error-kind)
     (lambda (result)
       (chidu-draft--after-cleanup-settlement
        workflow outcome error-kind result)))))

(defun chidu-draft--start-cleanup-effect
    (workflow starter continuation)
  "Start one cleanup WORKFLOW effect using STARTER and CONTINUATION."
  (let ((effect-id (cl-incf (chidu-draft-workflow-effect-id workflow)))
        (pending-p t)
        secret
        cancel)
    (condition-case _error-data
        (setq secret
              (chidu-runtime--endpoint-secret
               (chidu-store-compose-context-endpoint
                (chidu-draft-workflow-context workflow))))
      (error
       (chidu-draft--settle-cleanup
        workflow 'unknown "credential-error")))
    (when (and secret (chidu-draft--current-p workflow))
      (condition-case error-data
          (progn
            (setq
             cancel
             (funcall
              starter secret
              (lambda (result)
                (setq pending-p nil)
                (when (and (chidu-draft--current-p workflow)
                           (= effect-id
                              (chidu-draft-workflow-effect-id workflow)))
                  (setf
                   (chidu-runtime-operation-cancel-function
                    (chidu-draft-workflow-runtime-operation workflow))
                   nil)
                  (funcall continuation workflow result)))))
            (clear-string secret)
            (setq secret nil)
            (when (and pending-p
                       (chidu-draft--current-p workflow)
                       (= effect-id
                          (chidu-draft-workflow-effect-id workflow)))
              (if (functionp cancel)
                  (chidu-runtime--set-operation-cancel
                   (chidu-draft-workflow-runtime workflow)
                   (chidu-draft-workflow-runtime-operation workflow)
                   cancel)
                (chidu-draft--settle-cleanup
                 workflow 'unknown "jmap-request-did-not-settle"))))
        (error
         (when secret (clear-string secret))
         (chidu-draft--settle-cleanup
          workflow 'unknown
          (format "jmap-request-failed:%s"
                  (error-message-string error-data))))))))

(defun chidu-draft--cleanup-evidence-matches-p
    (workflow evidence)
  "Return non-nil when EVIDENCE still identifies WORKFLOW's predecessor Draft."
  (let* ((context (chidu-draft-workflow-context workflow))
         (attempt (chidu-draft--cleanup-attempt workflow))
         (drafts-mailbox (chidu-store-compose-context-drafts-mailbox context)))
    (and
     attempt
     drafts-mailbox
     (chidu-jmap-draft-cleanup-evidence-found-p evidence)
     (equal
      (chidu-store-draft-publish-attempt-predecessor-remote-blob-id attempt)
      (chidu-jmap-draft-cleanup-evidence-remote-blob-id evidence))
     (seq-contains-p
      (chidu-jmap-draft-cleanup-evidence-remote-mailbox-ids evidence)
      (chidu-store-mailbox-remote-mailbox-id drafts-mailbox)
      #'equal)
     (seq-contains-p
      (chidu-jmap-draft-cleanup-evidence-keywords evidence)
      "$draft" #'equal))))

(defun chidu-draft--dispatch-cleanup-destroy (workflow state)
  "Conditionally destroy WORKFLOW predecessor at Email STATE."
  (let ((context (chidu-draft-workflow-context workflow))
        (attempt (chidu-draft--cleanup-attempt workflow)))
    (chidu-draft--start-cleanup-effect
     workflow
     (lambda (secret deliver)
       (chidu-jmap-draft-cleanup-destroy
        context attempt state secret deliver))
     #'chidu-draft--after-cleanup-jmap)))

(defun chidu-draft--after-cleanup-read (workflow result)
  "Continue WORKFLOW after authoritative predecessor read RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-draft--settle-cleanup
     workflow 'unknown (chidu-draft--failure-kind result)))
   ((not (chidu-result-ok-p result))
    (chidu-draft--fail
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))
   (t
    (let ((evidence (chidu-result-ok-value result)))
      (cond
       ((not (chidu-jmap-draft-cleanup-evidence-p evidence))
        (chidu-draft--fail
         workflow
         (chidu-result-failure-create
          :kind 'invalid-result :data (list :value evidence)
          :retryable-p nil)))
       ((not (chidu-jmap-draft-cleanup-evidence-found-p evidence))
        (chidu-draft--settle-cleanup workflow 'succeeded "notFound"))
       ((not (chidu-draft--cleanup-evidence-matches-p workflow evidence))
        (chidu-draft--settle-cleanup
         workflow 'rejected "remoteConflict"))
       (t
        (chidu-draft--dispatch-cleanup-destroy
         workflow (chidu-jmap-draft-cleanup-evidence-state evidence))))))))

(defun chidu-draft--after-cleanup-jmap (workflow result)
  "Settle WORKFLOW after conditional predecessor cleanup RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-draft--settle-cleanup
     workflow 'unknown (chidu-draft--failure-kind result)))
   ((chidu-result-ok-p result)
    (let* ((value (chidu-result-ok-value result))
           (outcome (chidu-jmap-draft-cleanup-result-outcome value))
           (error-kind (chidu-jmap-draft-cleanup-result-error-kind value)))
      (chidu-draft--settle-cleanup
       workflow
       (if (equal error-kind "stateMismatch") 'unknown outcome)
       error-kind)))
   (t
    (chidu-draft--fail
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-draft--dispatch-cleanup (workflow)
  "Read WORKFLOW's oldest predecessor before conditional cleanup."
  (let ((context (chidu-draft-workflow-context workflow))
        (attempt (chidu-draft--cleanup-attempt workflow)))
    (unless (and attempt
                 (eq 'cleanup-pending
                     (chidu-store-draft-publish-attempt-phase attempt)))
      (signal 'chidu-invariant-error
              '("Draft cleanup dispatched in the wrong phase")))
    (chidu-draft--start-cleanup-effect
     workflow
     (lambda (secret deliver)
       (chidu-jmap-draft-cleanup-read
        context attempt secret deliver))
     #'chidu-draft--after-cleanup-read)))

(defun chidu-draft--stale-local-resource (workflow)
  "Return one local-backed WORKFLOW resource with confirmed Blob evidence."
  (cl-find-if
   (lambda (resource)
     (and (chidu-store-compose-resource-digest resource)
          (chidu-store-compose-resource-remote-blob-id resource)))
   (chidu-store-compose-context-resources
    (chidu-draft-workflow-context workflow))))

(defun chidu-draft--clear-stale-resource-blobs (workflow error-kind)
  "Clear recoverable stale Blob evidence in WORKFLOW, then report ERROR-KIND."
  (if-let* ((resource (chidu-draft--stale-local-resource workflow)))
      (let* ((context (chidu-draft-workflow-context workflow))
             (workspace (chidu-store-compose-context-workspace context)))
        (chidu-draft--store
         workflow
         (chidu-store-op-set-compose-resource-blob-create
          :workspace-id
          (chidu-store-compose-workspace-workspace-id workspace)
          :resource-id
          (chidu-store-compose-resource-resource-id resource)
          :remote-blob-id nil)
         (lambda (result)
           (cond
            ((chidu-result-failure-p result)
             (chidu-draft--fail workflow result))
            ((chidu-result-ok-p result)
             (chidu-draft--set-context
              workflow (chidu-result-ok-value result))
             (chidu-draft--clear-stale-resource-blobs
              workflow error-kind))
            (t
             (chidu-draft--fail
              workflow
              (chidu-result-failure-create
               :kind 'invalid-result :data (list :value result)
               :retryable-p nil)))))))
    (chidu-draft--finish
     workflow
     (chidu-draft--result
      (chidu-draft-workflow-context workflow) 'rejected error-kind))))

(defun chidu-draft--after-create-settlement (workflow outcome error-kind result)
  "Continue WORKFLOW after create OUTCOME and ERROR-KIND Store RESULT."
  (cond
   ((chidu-result-failure-p result) (chidu-draft--fail workflow result))
   ((chidu-result-ok-p result)
    (let ((context
           (chidu-draft--set-context
            workflow (chidu-result-ok-value result))))
      (pcase outcome
        ('succeeded
         (if (chidu-draft--cleanup-attempt workflow)
             (chidu-draft--dispatch-cleanup workflow)
           (chidu-draft--finish
            workflow (chidu-draft--result context 'saved))))
        ('rejected
         (if (equal error-kind "blobNotFound")
             (chidu-draft--clear-stale-resource-blobs
              workflow error-kind)
           (chidu-draft--finish
            workflow (chidu-draft--result context 'rejected error-kind))))
        ('unknown
         (chidu-draft--finish
          workflow (chidu-draft--result context 'unknown error-kind))))))
   (t
    (chidu-draft--fail
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-draft--after-create-jmap (workflow result)
  "Settle WORKFLOW after remote Draft create RESULT."
  (when (chidu-draft--current-p workflow)
    (setf
     (chidu-runtime-operation-cancel-function
      (chidu-draft-workflow-runtime-operation workflow))
     nil)
    (cond
     ((chidu-result-failure-p result)
      (let ((kind (chidu-draft--failure-kind result)))
        (chidu-draft--settle-create
         workflow 'unknown nil nil kind
         (lambda (settled)
           (chidu-draft--after-create-settlement
            workflow 'unknown kind settled)))))
     ((chidu-result-ok-p result)
      (let* ((value (chidu-result-ok-value result))
             (outcome (chidu-jmap-draft-create-result-outcome value))
             (remote-id
              (chidu-jmap-draft-create-result-remote-email-id value))
             (remote-blob-id
              (chidu-jmap-draft-create-result-remote-blob-id value))
             (error-kind
              (chidu-jmap-draft-create-result-error-kind value)))
        (chidu-draft--settle-create
         workflow outcome remote-id remote-blob-id error-kind
         (lambda (settled)
           (chidu-draft--after-create-settlement
            workflow outcome error-kind settled)))))
     (t
      (chidu-draft--fail
       workflow
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil))))))

(defun chidu-draft--after-mark-unknown (workflow secret result)
  "Dispatch WORKFLOW create with SECRET after Store fence RESULT."
  (cond
   ((chidu-result-failure-p result)
    (clear-string secret)
    (chidu-draft--fail workflow result))
   ((chidu-result-ok-p result)
    (let* ((context
            (chidu-draft--set-context
             workflow (chidu-result-ok-value result)))
           (attempt (chidu-draft--attempt workflow)))
      (if (not (chidu-draft--current-p workflow))
          (clear-string secret)
        (condition-case error-data
            (let ((cancel
                   (chidu-jmap-draft-create
                    context
                    (chidu-store-compose-workspace-document
                     (chidu-store-compose-context-workspace context))
                    (chidu-store-draft-publish-attempt-attempt-id attempt)
                    (chidu-store-draft-publish-attempt-message-id attempt)
                    secret
                    (lambda (value)
                      (chidu-draft--after-create-jmap workflow value)))))
              (clear-string secret)
              (chidu-runtime--set-operation-cancel
               (chidu-draft-workflow-runtime workflow)
               (chidu-draft-workflow-runtime-operation workflow)
               cancel))
          (error
           (clear-string secret)
           (chidu-draft--settle-create
            workflow 'unknown nil nil "jmap-request-failed"
            (lambda (settled)
              (chidu-draft--after-create-settlement
               workflow 'unknown
               (error-message-string error-data)
               settled))))))))
   (t
    (clear-string secret)
    (chidu-draft--fail
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-draft--dispatch-create (workflow)
  "Dispatch WORKFLOW's known-pending remote Draft create."
  (let* ((context (chidu-draft-workflow-context workflow))
         (attempt (chidu-draft--attempt workflow))
         secret)
    (unless (and attempt
                 (eq 'pending
                     (chidu-store-draft-publish-attempt-phase attempt)))
      (signal 'chidu-invariant-error
              '("Draft create dispatched in the wrong phase")))
    (condition-case error-data
        (setq secret
              (chidu-runtime--endpoint-secret
               (chidu-store-compose-context-endpoint context)))
      (error
       (chidu-draft--fail
        workflow
        (chidu-result-failure-create
         :kind 'credential-error
         :data (list :message (error-message-string error-data))
         :retryable-p t))))
    (when (and secret (chidu-draft--current-p workflow))
      (chidu-draft--store
       workflow
       (chidu-store-op-mark-draft-publish-unknown-create
        :attempt-id
        (chidu-store-draft-publish-attempt-attempt-id attempt))
       (lambda (result)
         (chidu-draft--after-mark-unknown workflow secret result))))))

(defun chidu-draft--after-retry-pending (workflow result)
  "Continue WORKFLOW after RESULT returns an absent create to pending."
  (cond
   ((chidu-result-failure-p result) (chidu-draft--fail workflow result))
   ((chidu-result-ok-p result)
    (chidu-draft--set-context workflow (chidu-result-ok-value result))
    (chidu-draft--dispatch-create workflow))
   (t
    (chidu-draft--fail
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-draft--after-reconcile-jmap (workflow result)
  "Continue WORKFLOW after uncertain-create reconciliation RESULT."
  (when (chidu-draft--current-p workflow)
    (setf
     (chidu-runtime-operation-cancel-function
      (chidu-draft-workflow-runtime-operation workflow))
     nil)
    (cond
     ((chidu-result-failure-p result)
      (let ((kind (chidu-draft--failure-kind result)))
        (chidu-draft--settle-create
         workflow 'unknown nil nil kind
         (lambda (settled)
           (chidu-draft--after-create-settlement
            workflow 'unknown kind settled)))))
     ((chidu-result-ok-p result)
      (let ((matches (chidu-result-ok-value result))
            (attempt (chidu-draft--attempt workflow)))
        (pcase (length matches)
          (0
           (chidu-draft--store
            workflow
            (chidu-store-op-retry-draft-publish-create-create
             :attempt-id
             (chidu-store-draft-publish-attempt-attempt-id attempt))
            (lambda (settled)
              (chidu-draft--after-retry-pending workflow settled))))
          (1
           (let ((match (aref matches 0)))
             (chidu-draft--settle-create
              workflow 'succeeded
              (chidu-jmap-draft-reconcile-match-remote-email-id match)
              (chidu-jmap-draft-reconcile-match-remote-blob-id match)
              nil
              (lambda (settled)
                (chidu-draft--after-create-settlement
                 workflow 'succeeded nil settled)))))
          (_
           (chidu-draft--settle-create
            workflow 'unknown nil nil "multipleDraftMatches"
            (lambda (settled)
              (chidu-draft--after-create-settlement
               workflow 'unknown "multipleDraftMatches" settled)))))))
     (t
      (chidu-draft--fail
       workflow
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil))))))

(defun chidu-draft--reconcile-create (workflow)
  "Reconcile WORKFLOW's uncertain Draft create before any retry."
  (let* ((context (chidu-draft-workflow-context workflow))
         (attempt (chidu-draft--attempt workflow))
         secret)
    (unless (and attempt
                 (eq 'unknown
                     (chidu-store-draft-publish-attempt-phase attempt)))
      (signal 'chidu-invariant-error
              '("Draft reconciliation dispatched in the wrong phase")))
    (condition-case error-data
        (setq secret
              (chidu-runtime--endpoint-secret
               (chidu-store-compose-context-endpoint context)))
      (error
       (chidu-draft--fail
        workflow
        (chidu-result-failure-create
         :kind 'credential-error
         :data (list :message (error-message-string error-data))
         :retryable-p t))))
    (when (and secret (chidu-draft--current-p workflow))
      (condition-case error-data
          (let ((cancel
                 (chidu-jmap-draft-reconcile
                  context attempt secret
                  (lambda (result)
                    (chidu-draft--after-reconcile-jmap workflow result)))))
            (clear-string secret)
            (chidu-runtime--set-operation-cancel
             (chidu-draft-workflow-runtime workflow)
             (chidu-draft-workflow-runtime-operation workflow)
             cancel))
        (error
         (clear-string secret)
         (chidu-draft--fail
          workflow
          (chidu-result-failure-create
           :kind 'jmap-request-failed
           :data (list :message (error-message-string error-data))
           :retryable-p t)))))))

(defun chidu-draft--dispatch (workflow)
  "Resume WORKFLOW from its durable publication phase."
  (let ((attempt (chidu-draft--attempt workflow)))
    (unless attempt
      (signal 'chidu-invariant-error
              '("Draft publication has no durable attempt")))
    (pcase (chidu-store-draft-publish-attempt-phase attempt)
      ('pending (chidu-draft--dispatch-create workflow))
      ('unknown (chidu-draft--reconcile-create workflow))
      (_
       (chidu-draft--fail
        workflow
        (chidu-result-failure-create
         :kind 'draft-publish-phase-conflict
         :data
         (list :phase
               (chidu-store-draft-publish-attempt-phase attempt))
         :retryable-p nil))))))

(defun chidu-draft--missing-resource (workflow)
  "Return WORKFLOW's first resource without a confirmed Blob id."
  (cl-find-if
   (lambda (resource)
     (null (chidu-store-compose-resource-remote-blob-id resource)))
   (chidu-store-compose-context-resources
    (chidu-draft-workflow-context workflow))))

(defun chidu-draft--accept-new (workflow)
  "Accept WORKFLOW's captured revision as a durable Draft create intent."
  (let* ((context (chidu-draft-workflow-context workflow))
         (workspace (chidu-store-compose-context-workspace context))
         (identity (chidu-draft-workflow-identity workflow))
         (attempt-id (chidu-store-new-local-id))
         (message-id (chidu-draft--message-id attempt-id identity)))
    (chidu-draft--store
     workflow
     (chidu-store-op-accept-draft-publish-create
      :workspace-id
      (chidu-store-compose-workspace-workspace-id workspace)
      :identity-id (chidu-store-identity-identity-id identity)
      :expected-revision
      (chidu-store-compose-workspace-revision workspace)
      :revision (chidu-draft-workflow-generation workflow)
      :document (chidu-draft-workflow-document workflow)
      :attempt-id attempt-id
      :message-id message-id)
     (lambda (result)
       (chidu-draft--after-accept workflow result)))))

(defun chidu-draft--after-resource-blob (workflow result)
  "Continue WORKFLOW after persisting one uploaded Blob RESULT."
  (cond
   ((chidu-result-failure-p result) (chidu-draft--fail workflow result))
   ((chidu-result-ok-p result)
    (chidu-draft--set-context workflow (chidu-result-ok-value result))
    (chidu-draft--prepare-resources workflow))
   (t
    (chidu-draft--fail
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-draft--after-resource-upload (workflow resource result)
  "Continue WORKFLOW after uploading RESOURCE with typed RESULT."
  (when (chidu-draft--current-p workflow)
    (setf
     (chidu-runtime-operation-cancel-function
      (chidu-draft-workflow-runtime-operation workflow))
     nil)
    (cond
     ((chidu-result-failure-p result) (chidu-draft--fail workflow result))
     ((chidu-result-ok-p result)
      (let* ((value (chidu-result-ok-value result))
             (context (chidu-draft-workflow-context workflow))
             (workspace (chidu-store-compose-context-workspace context)))
        (chidu-draft--store
         workflow
         (chidu-store-op-set-compose-resource-blob-create
          :workspace-id
          (chidu-store-compose-workspace-workspace-id workspace)
          :resource-id
          (chidu-store-compose-resource-resource-id resource)
          :remote-blob-id (chidu-jmap-upload-result-blob-id value))
         (lambda (settled)
           (chidu-draft--after-resource-blob workflow settled)))))
     (t
      (chidu-draft--fail
       workflow
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result)
        :retryable-p nil))))))

(defun chidu-draft--upload-resource (workflow resource)
  "Upload exact local RESOURCE bytes for WORKFLOW."
  (let* ((runtime (chidu-draft-workflow-runtime workflow))
         (context (chidu-draft-workflow-context workflow))
         (endpoint (chidu-store-compose-context-endpoint context))
         (account (chidu-store-compose-context-account context))
         file secret)
    (condition-case error-data
        (setq file
              (chidu-compose-resource-local-file
               (chidu-runtime-data-root runtime) resource))
      (error
       (chidu-draft--fail
        workflow
        (chidu-runtime--condition-failure
         'compose-resource-bytes-unavailable error-data nil))))
    (when (and file (chidu-draft--current-p workflow))
      (condition-case error-data
          (setq secret (chidu-runtime--endpoint-secret endpoint))
        (error
         (chidu-draft--fail
          workflow
          (chidu-runtime--condition-failure
           'credential-error error-data t))))
      (when (and secret (chidu-draft--current-p workflow))
        (condition-case error-data
            (let ((cancel
                   (chidu-jmap-upload-compose-resource
                    endpoint account resource file secret
                    (lambda (result)
                      (chidu-draft--after-resource-upload
                       workflow resource result)))))
              (clear-string secret)
              (setq secret nil)
              (chidu-runtime--set-operation-cancel
               runtime
               (chidu-draft-workflow-runtime-operation workflow)
               cancel))
          (error
           (when secret (clear-string secret))
           (chidu-draft--fail
            workflow
            (chidu-runtime--condition-failure
             'jmap-upload-failed error-data t))))))))

(defun chidu-draft--prepare-resources (workflow)
  "Upload missing WORKFLOW resources, then accept the Draft create."
  (if-let* ((resource (chidu-draft--missing-resource workflow)))
      (if (chidu-store-compose-resource-digest resource)
          (chidu-draft--upload-resource workflow resource)
        (chidu-draft--fail
         workflow
         (chidu-result-failure-create
          :kind 'compose-resource-bytes-unavailable
          :data
          (list :resource-id
                (chidu-store-compose-resource-resource-id resource))
          :retryable-p nil)))
    (chidu-draft--accept-new workflow)))

(defun chidu-draft--after-accept (workflow result)
  "Continue WORKFLOW after durable accept RESULT."
  (cond
   ((chidu-result-failure-p result) (chidu-draft--fail workflow result))
   ((chidu-result-ok-p result)
    (chidu-draft--set-context workflow (chidu-result-ok-value result))
    (chidu-draft--dispatch workflow))
   (t
    (chidu-draft--fail
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-publish-draft
    (runtime context identity generation document
             success-function error-function)
  "In RUNTIME, publish CONTEXT DOCUMENT GENERATION with IDENTITY as a Draft.

The returned runtime operation is cancelable.  SUCCESS-FUNCTION receives a
`chidu-draft-publish-result'; ERROR-FUNCTION receives an infrastructure or
Store failure.  An existing durable attempt is resumed before a newer document
revision may be published."
  (unless (chidu-runtime-p runtime)
    (signal 'wrong-type-argument (list 'chidu-runtime-p runtime)))
  (unless (chidu-store-compose-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-context-p context)))
  (unless (chidu-store-identity-p identity)
    (signal 'wrong-type-argument (list 'chidu-store-identity-p identity)))
  (unless (and (integerp generation) (>= generation 0))
    (signal 'wrong-type-argument (list 'nonnegative-integer-p generation)))
  (unless (chidu-store-compose-document-p document)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-document-p document)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let* ((operation (chidu-runtime--begin-operation runtime))
         (workflow
          (chidu-draft-workflow-create
           :runtime runtime
           :runtime-operation operation
           :context context
           :identity identity
           :generation generation
           :document document
           :success-function success-function
           :error-function error-function))
         (workspace (chidu-store-compose-context-workspace context))
         (attempt (chidu-store-compose-context-publish-attempt context)))
    (cond
     (attempt (chidu-draft--dispatch workflow))
     ((and
       (eql generation
            (chidu-store-compose-workspace-published-revision workspace))
       (equal document (chidu-store-compose-workspace-document workspace)))
      (if (chidu-draft--cleanup-attempt workflow)
          (chidu-draft--dispatch-cleanup workflow)
        (chidu-draft--finish
         workflow (chidu-draft--result context 'unchanged))))
     (t (chidu-draft--prepare-resources workflow)))
    operation))

(provide 'chidu-draft)

;;; chidu-draft.el ends here
