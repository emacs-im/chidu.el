;;; chidu-seen.el --- Explicit read and unread commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Opening, focusing, and displaying Email never mutate $seen.  This module owns
;; only explicit user actions: accept a durable optimistic intent, send one
;; idempotent Email/set patch, then commit, revert, or retain an unknown outcome.

;;; Code:

(require 'cl-lib)
(require 'appkit-app)
(require 'chidu-jmap-email)
(require 'chidu-jmap-seen)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-surface-operation)
(require 'chidu-store)

(chidu-define-record chidu-seen-target
    "One explicit read-state command target."
  app
  account
  local-email-id
  remote-email-id
  unread-p)

(cl-defstruct (chidu-seen-workflow
               (:constructor chidu-seen-workflow-create))
  "Ephemeral control state for one explicit read-state command."
  target
  desired-seen-p
  intent
  runtime-operation
  reconciled-p
  quiet-p)

(defvar-local chidu-seen-target-function nil
  "Function returning the `chidu-seen-target' at point.")

(defvar-local chidu-seen-targets-function nil
  "Function returning selected `chidu-seen-target' objects.

List views use this to resolve process marks, region, or point.  Conversation
and standalone readers leave it nil and operate on the Email at point.")

(defun chidu-seen-change-for-account-p (change account)
  "Return non-nil when CHANGE belongs to ACCOUNT."
  (and (chidu-store-seen-change-p change)
       (chidu-store-account-p account)
       (equal
        (chidu-store-account-account-id
         (chidu-store-seen-change-account change))
        (chidu-store-account-account-id account))))

(defun chidu-seen-update-summary-row (row change)
  "Return ROW with effective read state from CHANGE when identities match."
  (if (and (chidu-store-email-summary-row-p row)
           (chidu-store-seen-change-p change)
           (equal
            (chidu-store-email-summary-row-local-email-id row)
            (chidu-store-seen-change-local-email-id change)))
      (chidu-store-email-summary-row-with
       row :unread-p (chidu-store-seen-change-unread-p change))
    row))

(defun chidu-seen--target-valid-p (target)
  "Return non-nil when TARGET is a complete live read-state target."
  (and (chidu-seen-target-p target)
       (appkit-app-live-p (chidu-seen-target-app target))
       (chidu-store-account-p (chidu-seen-target-account target))
       (chidu-store-local-id-p (chidu-seen-target-local-email-id target))
       (stringp (chidu-seen-target-remote-email-id target))
       (not (string-empty-p (chidu-seen-target-remote-email-id target)))
       (memq (chidu-seen-target-unread-p target) '(nil t))))

(defun chidu-seen-target-at-point ()
  "Return the explicit read-state target at point, or signal a user error."
  (unless (functionp chidu-seen-target-function)
    (user-error "This buffer has no Email read-state target"))
  (let ((target (funcall chidu-seen-target-function)))
    (unless (chidu-seen--target-valid-p target)
      (user-error "No Email at point"))
    target))

(defun chidu-seen-targets ()
  "Return deduplicated explicit read-state targets for the current command."
  (let ((raw (if (functionp chidu-seen-targets-function)
                 (funcall chidu-seen-targets-function)
               (list (chidu-seen-target-at-point))))
        (seen (make-hash-table :test #'equal))
        targets)
    (unless (listp raw)
      (user-error "Email selection returned an invalid target set"))
    (dolist (target raw)
      (unless (chidu-seen--target-valid-p target)
        (user-error "Email selection contains an invalid target"))
      (let ((key
             (list
              (chidu-store-account-account-id
               (chidu-seen-target-account target))
              (chidu-seen-target-local-email-id target))))
        (unless (gethash key seen)
          (puthash key t seen)
          (push target targets))))
    (or (nreverse targets)
        (user-error "No Email selected"))))

(defun chidu-seen--app (workflow)
  "Return Appkit application owned by WORKFLOW."
  (chidu-seen-target-app (chidu-seen-workflow-target workflow)))

(defun chidu-seen--runtime (workflow)
  "Return live Chidu runtime owned by WORKFLOW."
  (let* ((app (chidu-seen--app workflow))
         (runtime (and (appkit-app-live-p app) (chidu-app-runtime app))))
    (and (chidu-runtime-p runtime) runtime)))

(defun chidu-seen--request-key (account local-email-id)
  "Return per-Email Appkit request key for ACCOUNT and LOCAL-EMAIL-ID."
  (list 'seen-intent
        (chidu-store-account-account-id account)
        local-email-id))

(defun chidu-seen--emit (workflow change)
  "Emit one committed or optimistic read-state CHANGE for WORKFLOW."
  (let ((app (chidu-seen--app workflow)))
    (when (appkit-app-live-p app)
      (chidu-post-app-message app (list 'chidu-seen-changed app change)))))

(defun chidu-seen--message (change)
  "Display concise user feedback for read-state CHANGE."
  (let ((state (if (chidu-store-seen-change-unread-p change)
                   "unread"
                 "read")))
    (pcase (chidu-store-seen-change-phase change)
      ('unchanged (message "Chidu: already %s" state))
      ('pending (message "Chidu: marking %s…" state))
      ('committed (message "Chidu: marked %s" state))
      ('reverted
       (message "Chidu: read state rejected%s"
                (if-let* ((kind
                           (chidu-store-seen-change-error-kind change)))
                    (format " (%s)" kind)
                  "")))
      ('unknown
       (message "Chidu: read state is not confirmed; it will be retried")))))

(defun chidu-seen--finish (workflow result)
  "Finish WORKFLOW with typed RESULT."
  (when-let* ((runtime (chidu-seen--runtime workflow)))
    (chidu-runtime--deliver-result
     runtime
     (chidu-seen-workflow-runtime-operation workflow)
     result
     (lambda (change)
       (when (and (chidu-store-seen-change-p change)
                  (not (chidu-seen-workflow-quiet-p workflow))
                  (not (eq 'pending
                           (chidu-store-seen-change-phase change))))
         (chidu-seen--message change)))
     (lambda (failure)
       (unless (chidu-seen-workflow-quiet-p workflow)
         (message "Chidu: %s" (chidu-runtime-error-message failure)))))))

(defun chidu-seen--failure-outcome (failure)
  "Return safe settlement outcome for transport FAILURE."
  (let ((kind (chidu-result-failure-kind failure))
        (status (plist-get (chidu-result-failure-data failure) :status)))
    (cond
     ((eq kind 'request-too-large) 'rejected)
     ;; Keep local-first intent when dispatch was impossible, credentials need
     ;; repair, or the response cannot prove whether the idempotent patch ran.
     ((memq kind '(credential-error authentication-rejected
                                    transport-unavailable jmap-request-failed
                                    network-error response-too-large invalid-jmap-response
                                    unexpected-http-status))
      'unknown)
     ((eq kind 'http-error)
      (if (and (integerp status)
               (<= 400 status) (< status 500)
               (not (memq status '(408 425 429))))
          'rejected
        'unknown))
     (t 'unknown))))

(defun chidu-seen--settle (workflow outcome error-kind)
  "Settle WORKFLOW with OUTCOME and optional ERROR-KIND."
  (when-let* ((runtime (chidu-seen--runtime workflow)))
    (let* ((target (chidu-seen-workflow-target workflow))
           (account (chidu-seen-target-account target))
           (local-email-id (chidu-seen-target-local-email-id target))
           (intent (chidu-seen-workflow-intent workflow))
           (operation-id (chidu-store-seen-intent-operation-id intent))
           (app (chidu-seen--app workflow))
           (request-key (chidu-seen--request-key account local-email-id)))
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-settle-seen-intent-create
        :account-id (chidu-store-account-account-id account)
        :local-email-id local-email-id
        :operation-id operation-id
        :outcome outcome
        :error-kind error-kind)
       (lambda (result)
         (remhash request-key (chidu-app-requests app))
         (cond
          ((and (chidu-result-failure-p result)
                (eq 'stale-operation
                    (chidu-result-failure-kind result)))
           ;; Opposite commands for one Email are serialized.  The old request
           ;; finishes first; only then retry the newest durable intent.
           (chidu-seen--finish
            workflow (chidu-result-ok-create :value nil))
           (chidu-retry-seen-intents app account))
          ((chidu-result-ok-p result)
           (let ((change (chidu-result-ok-value result)))
             (chidu-seen--emit workflow change)
             (chidu-seen--finish workflow result)
             (when
                 (and
                  (eq 'unknown (chidu-store-seen-change-phase change))
                  (equal "serverPartialFail"
                         (chidu-store-seen-change-error-kind change)))
               (chidu-retry-seen-intents app account))))
          (t (chidu-seen--finish workflow result))))))))

(defun chidu-seen--after-jmap (workflow result)
  "Settle JMAP RESULT for WORKFLOW."
  (when-let* ((runtime (chidu-seen--runtime workflow))
              ((chidu-runtime--operation-current-p
                runtime (chidu-seen-workflow-runtime-operation workflow))))
    (setf
     (chidu-runtime-operation-cancel-function
      (chidu-seen-workflow-runtime-operation workflow))
     nil)
    (cond
     ((chidu-result-ok-p result)
      (let ((response (chidu-result-ok-value result)))
        (chidu-seen--settle
         workflow
         (chidu-jmap-seen-response-outcome response)
         (chidu-jmap-seen-response-error-kind response))))
     ((chidu-result-failure-p result)
      (chidu-seen--settle
       workflow
       (chidu-seen--failure-outcome result)
       (symbol-name (chidu-result-failure-kind result))))
     (t (chidu-seen--settle workflow 'unknown "invalid-result")))))

(defun chidu-seen--dispatch-ready (workflow context)
  "Dispatch WORKFLOW's durable intent using Store CONTEXT."
  (let* ((app (chidu-seen--app workflow))
         (runtime (chidu-seen--runtime workflow))
         (intent (chidu-seen-workflow-intent workflow))
         (operation
          (chidu-seen-workflow-runtime-operation workflow))
         (target (chidu-seen-workflow-target workflow))
         (key
          (chidu-seen--request-key
           (chidu-seen-target-account target)
           (chidu-seen-target-local-email-id target))))
    (cond
     ((null runtime) nil)
     ((gethash key (chidu-app-requests app))
      (chidu-seen--finish
       workflow
       (chidu-result-ok-create
        :value
        (chidu-store-seen-change-create
         :endpoint (chidu-store-seen-context-endpoint context)
         :account (chidu-store-seen-context-account context)
         :operation-id (chidu-store-seen-intent-operation-id intent)
         :local-email-id (chidu-store-seen-intent-local-email-id intent)
         :remote-email-id (chidu-store-seen-intent-remote-email-id intent)
         :unread-p (not (chidu-store-seen-intent-desired-seen-p intent))
         :phase 'pending))))
     (t
      (puthash key operation (chidu-app-requests app))
      (setf
       (chidu-runtime-operation-cancel-cleanup-function operation)
       (lambda () (remhash key (chidu-app-requests app))))
      (let (secret)
        (condition-case _error-data
            (setq secret
                  (chidu-runtime--endpoint-secret
                   (chidu-store-seen-context-endpoint context)))
          (error
           (chidu-seen--settle workflow 'rejected "credential-error")))
        (when (and secret
                   (chidu-runtime--operation-current-p runtime operation))
          (condition-case _error-data
              (let ((cancel
                     (chidu-jmap-set-seen
                      context secret
                      (chidu-store-seen-intent-remote-email-id intent)
                      (chidu-store-seen-intent-desired-seen-p intent)
                      (lambda (result)
                        (chidu-seen--after-jmap workflow result)))))
                ;; The adapter owns and clears SECRET after this point.
                (setq secret nil)
                (chidu-runtime--set-operation-cancel
                 runtime operation cancel))
            (error
             (when secret (clear-string secret))
             (chidu-seen--settle
              workflow 'rejected "jmap-request-failed")))))))))

(defun chidu-seen--after-reconciliation (workflow context result)
  "Continue WORKFLOW after authoritative CONTEXT mutable-state RESULT."
  (when-let* ((runtime (chidu-seen--runtime workflow))
              ((chidu-runtime--operation-current-p
                runtime (chidu-seen-workflow-runtime-operation workflow))))
    (setf
     (chidu-runtime-operation-cancel-function
      (chidu-seen-workflow-runtime-operation workflow))
     nil)
    (let* ((target (chidu-seen-workflow-target workflow))
           (app (chidu-seen-target-app target))
           (key
            (chidu-seen--request-key
             (chidu-seen-target-account target)
             (chidu-seen-target-local-email-id target))))
      (remhash key (chidu-app-requests app)))
    (cond
     ((chidu-result-failure-p result)
      (chidu-seen--finish workflow result))
     ((chidu-result-ok-p result)
      (let* ((state (chidu-result-ok-value result))
             (target
              (aref (chidu-jmap-email-mutable-state-targets state) 0))
             (desired (chidu-seen-workflow-desired-seen-p workflow)))
        (cond
         ((not (chidu-jmap-email-mutable-target-found-p target))
          (chidu-seen--settle workflow 'rejected "notFound"))
         ((eq desired
              (chidu-jmap-email-mutable-target-seen-p target))
          (chidu-seen--settle workflow 'succeeded nil))
         (t
          (setf (chidu-seen-workflow-reconciled-p workflow) t)
          (chidu-seen--dispatch-ready workflow context)))))
     (t
      (chidu-seen--finish
       workflow
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result)
        :retryable-p nil))))))

(defun chidu-seen--reconcile (workflow context)
  "Using CONTEXT, reconcile WORKFLOW after serverPartialFail before retry."
  (let* ((app (chidu-seen--app workflow))
         (runtime (chidu-seen--runtime workflow))
         (operation (chidu-seen-workflow-runtime-operation workflow))
         (target (chidu-seen-workflow-target workflow))
         (key
          (chidu-seen--request-key
           (chidu-seen-target-account target)
           (chidu-seen-target-local-email-id target))))
    (cond
     ((null runtime) nil)
     ((gethash key (chidu-app-requests app))
      (chidu-seen--dispatch-ready workflow context))
     (t
      (puthash key operation (chidu-app-requests app))
      (setf
       (chidu-runtime-operation-cancel-cleanup-function operation)
       (lambda () (remhash key (chidu-app-requests app))))
      (let (secret)
        (condition-case _error-data
            (setq secret
                  (chidu-runtime--endpoint-secret
                   (chidu-store-seen-context-endpoint context)))
          (error
           (chidu-seen--finish
            workflow
            (chidu-result-failure-create
             :kind 'credential-error :data nil :retryable-p t))))
        (when secret
          (let ((cancel
                 (chidu-jmap-email-fetch-mutable-state
                  (chidu-store-seen-context-endpoint context)
                  (chidu-store-seen-context-account context)
                  (vector (chidu-seen-target-remote-email-id target))
                  secret
                  (lambda (result)
                    (chidu-seen--after-reconciliation
                     workflow context result)))))
            (clear-string secret)
            (when (chidu-runtime--operation-current-p runtime operation)
              (chidu-runtime--set-operation-cancel
               runtime operation cancel)))))))))

(defun chidu-seen--dispatch (workflow context)
  "Using CONTEXT, reconcile uncertain WORKFLOW state or dispatch its patch."
  (let ((intent (chidu-seen-workflow-intent workflow)))
    (if (and intent
             (eq 'unknown (chidu-store-seen-intent-phase intent))
             (equal "serverPartialFail"
                    (chidu-store-seen-intent-error-kind intent))
             (not (chidu-seen-workflow-reconciled-p workflow)))
        (chidu-seen--reconcile workflow context)
      (chidu-seen--dispatch-ready workflow context))))

(defun chidu-seen--after-accept (workflow result)
  "Continue WORKFLOW after Store accept RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-seen--finish workflow result))
   ((chidu-result-ok-p result)
    (let* ((change (chidu-result-ok-value result))
           (phase (chidu-store-seen-change-phase change))
           (target (chidu-seen-workflow-target workflow))
           (intent-operation-id
            (chidu-store-seen-change-operation-id change)))
      (chidu-seen--emit workflow change)
      (if (eq phase 'unchanged)
          (chidu-seen--finish workflow result)
        (let ((intent
               (chidu-store-seen-intent-create
                :operation-id intent-operation-id
                :local-email-id (chidu-seen-target-local-email-id target)
                :remote-email-id (chidu-seen-target-remote-email-id target)
                :desired-seen-p (chidu-seen-workflow-desired-seen-p workflow)
                :base-unread-p (chidu-seen-target-unread-p target)
                :phase 'pending)))
          (setf (chidu-seen-workflow-intent workflow) intent)
          (unless (chidu-seen-workflow-quiet-p workflow)
            (chidu-seen--message change))
          (chidu-seen--dispatch
           workflow
           (chidu-store-seen-context-create
            :endpoint (chidu-store-seen-change-endpoint change)
            :account (chidu-store-seen-change-account change)))))))
   (t
    (chidu-seen--finish
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-set-seen (target desired-seen-p &optional quiet-p)
  "Set TARGET's $seen keyword to DESIRED-SEEN-P explicitly.

QUIET-P suppresses user feedback.  Return the runtime operation."
  (unless (chidu-seen-target-p target)
    (signal 'wrong-type-argument (list 'chidu-seen-target-p target)))
  (unless (memq desired-seen-p '(nil t))
    (signal 'wrong-type-argument (list 'booleanp desired-seen-p)))
  (let* ((app (chidu-seen-target-app target))
         (runtime (and (appkit-app-live-p app) (chidu-app-runtime app)))
         (requested-operation-id (chidu-store-new-local-id)))
    (unless (chidu-runtime-p runtime)
      (user-error "Chidu runtime is unavailable"))
    (let* ((operation (chidu-runtime--begin-operation runtime))
           (workflow
            (chidu-seen-workflow-create
             :target target
             :desired-seen-p desired-seen-p
             :runtime-operation operation
             :quiet-p quiet-p)))
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-accept-seen-intent-create
        :account-id
        (chidu-store-account-account-id
         (chidu-seen-target-account target))
        :local-email-id (chidu-seen-target-local-email-id target)
        :remote-email-id (chidu-seen-target-remote-email-id target)
        :operation-id requested-operation-id
        :desired-seen-p desired-seen-p
        :current-unread-p (chidu-seen-target-unread-p target))
       (lambda (result)
         (chidu-seen--after-accept workflow result)))
      operation)))

(defun chidu-seen--apply-targets (targets desired-function description)
  "Apply DESIRED-FUNCTION to TARGETS and describe the action with DESCRIPTION.

Each Email retains its own durable intent and request ordering.  Multi-target
commands suppress per-Email minibuffer messages and report one aggregate start."
  (let ((count (length targets))
        operations)
    (dolist (target targets)
      (push
       (chidu-set-seen
        target (funcall desired-function target) (> count 1))
       operations))
    (when (> count 1)
      (message "Chidu: %s %d Emails…" description count))
    (nreverse operations)))

(defun chidu-mark-read ()
  "Explicitly mark selected Emails as read."
  (interactive)
  (chidu-seen--apply-targets
   (chidu-seen-targets) (lambda (_target) t) "marking read"))

(defun chidu-mark-unread ()
  "Explicitly mark selected Emails as unread."
  (interactive)
  (chidu-seen--apply-targets
   (chidu-seen-targets) (lambda (_target) nil) "marking unread"))

(defun chidu-toggle-read ()
  "Explicitly toggle read state for selected Emails."
  (interactive)
  (chidu-seen--apply-targets
   (chidu-seen-targets)
   (lambda (target) (chidu-seen-target-unread-p target))
   "toggling read state for"))

(defun chidu-seen--retry-intent (app context intent)
  "Retry persisted read-state INTENT in APP using CONTEXT."
  (let* ((runtime (chidu-app-runtime app))
         (target
          (chidu-seen-target-create
           :app app
           :account (chidu-store-seen-context-account context)
           :local-email-id (chidu-store-seen-intent-local-email-id intent)
           :remote-email-id (chidu-store-seen-intent-remote-email-id intent)
           :unread-p (not (chidu-store-seen-intent-desired-seen-p intent))))
         (workflow
          (chidu-seen-workflow-create
           :target target
           :desired-seen-p (chidu-store-seen-intent-desired-seen-p intent)
           :intent intent
           :runtime-operation (chidu-runtime--begin-operation runtime)
           :quiet-p t)))
    (chidu-seen--dispatch workflow context)))

(defun chidu-retry-seen-intents (app account)
  "Retry durable explicit read-state intents for ACCOUNT in APP."
  (when (and (appkit-app-live-p app) (chidu-store-account-p account))
    (let ((runtime (chidu-app-runtime app)))
      (chidu-runtime--simple-store-operation
       runtime
       (chidu-store-op-list-seen-intents-create
        :account-id (chidu-store-account-account-id account))
       (lambda (context)
         (cl-loop for intent across (chidu-store-seen-context-intents context)
                  do (chidu-seen--retry-intent app context intent)))
       (lambda (_failure) nil)))))

(provide 'chidu-seen)

;;; chidu-seen.el ends here
