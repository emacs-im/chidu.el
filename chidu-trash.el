;;; chidu-trash.el --- Durable JMAP move-to-Trash workflow -*- lexical-binding: t; -*-

;;; Commentary:

;; Moving an Email to Trash is not an ordinary source-to-destination move.  RFC
;; 8621 requires the complete mailboxIds set to become the unique role=trash
;; Mailbox.  Chidu therefore accepts a separate durable operation, fetches
;; authoritative mailboxIds, records restore evidence, and only then sends
;; bounded Email/set batches.  Permanent Email/destroy is intentionally absent.

;;; Code:

(require 'cl-lib)
(require 'appkit-app)
(require 'chidu-jmap-email)
(require 'chidu-jmap-trash)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-surface-operation)
(require 'chidu-store)

(cl-defstruct (chidu-trash-workflow
               (:constructor chidu-trash-workflow-create))
  "Ephemeral control state for one durable move-to-Trash operation."
  app
  account
  operation-id
  runtime-operation
  reconciled
  phases
  committed
  rejected
  unknown
  quiet-p
  progress-function
  success-function
  error-function)

(defun chidu-trash-role-mailbox (mailboxes)
  "Return the unique writable role=trash Mailbox from MAILBOXES."
  (let ((matches
         (cl-loop
          for mailbox across mailboxes
          when (and (chidu-store-mailbox-available-p mailbox)
                    (equal "trash" (chidu-store-mailbox-role mailbox)))
          collect mailbox)))
    (pcase matches
      (`() (user-error "No Trash Mailbox is available"))
      (`(,mailbox)
       (unless
           (chidu-store-mailbox-rights-may-add-items-p
            (chidu-store-mailbox-rights mailbox))
         (user-error "The Trash Mailbox does not allow adding Email"))
       mailbox)
      (_ (signal 'chidu-invariant-error
                 '("Account has more than one role=trash Mailbox"))))))

(defun chidu-trash-result-for-account-p (result account)
  "Return non-nil when Trash RESULT belongs to ACCOUNT."
  (and
   (chidu-store-trash-result-p result)
   (chidu-store-account-p account)
   (equal
    (chidu-store-account-account-id
     (chidu-store-trash-context-account
      (chidu-store-trash-result-context result)))
    (chidu-store-account-account-id account))))

(defun chidu-trash-result-committed-p (result)
  "Return non-nil when Trash RESULT contains a committed target."
  (and
   (chidu-store-trash-result-p result)
   (cl-loop
    for change across (chidu-store-trash-result-changes result)
    thereis
    (eq 'committed (chidu-store-trash-target-change-phase change)))))

(defun chidu-trash--key (account)
  "Return APP request-table key for ACCOUNT."
  (list 'trash (chidu-store-account-account-id account)))

(defun chidu-trash--runtime (workflow)
  "Return live runtime for WORKFLOW, or nil."
  (let ((app (chidu-trash-workflow-app workflow)))
    (when (appkit-app-live-p app)
      (let ((runtime (chidu-app-runtime app)))
        (and (chidu-runtime-p runtime) runtime)))))

(defun chidu-trash--emit (workflow result)
  "Emit Store Trash RESULT for WORKFLOW's live application."
  (let ((app (chidu-trash-workflow-app workflow)))
    (when (appkit-app-live-p app)
      (chidu-post-app-message app (list 'chidu-trash-changed app result))
      (funcall (chidu-trash-workflow-progress-function workflow) result))))

(defun chidu-trash--record-changes (workflow result)
  "Accumulate latest target phases from Store RESULT in WORKFLOW."
  (let ((phases (chidu-trash-workflow-phases workflow)))
    (cl-loop
     for change across (chidu-store-trash-result-changes result)
     for local-id = (chidu-store-trash-target-change-local-email-id change)
     for phase = (chidu-store-trash-target-change-phase change)
     for previous = (gethash local-id phases)
     do
     (pcase previous
       ('committed (cl-decf (chidu-trash-workflow-committed workflow)))
       ('reverted (cl-decf (chidu-trash-workflow-rejected workflow)))
       ('unknown (cl-decf (chidu-trash-workflow-unknown workflow))))
     do
     (pcase phase
       ('committed (cl-incf (chidu-trash-workflow-committed workflow)))
       ('reverted (cl-incf (chidu-trash-workflow-rejected workflow)))
       ('unknown (cl-incf (chidu-trash-workflow-unknown workflow))))
     do (puthash local-id phase phases))))

(defun chidu-trash--message (workflow)
  "Report aggregate completion for WORKFLOW."
  (unless (chidu-trash-workflow-quiet-p workflow)
    (let ((committed (chidu-trash-workflow-committed workflow))
          (rejected (chidu-trash-workflow-rejected workflow))
          (unknown (chidu-trash-workflow-unknown workflow)))
      (message
       "Chidu: moved %d Email%s to Trash%s%s"
       committed (if (= committed 1) "" "s")
       (if (> rejected 0) (format "; %d rejected" rejected) "")
       (if (> unknown 0) (format "; %d awaiting reconciliation" unknown) "")))))

(defun chidu-trash--finish (workflow result)
  "Finish WORKFLOW exactly once with typed RESULT."
  (let* ((app (chidu-trash-workflow-app workflow))
         (account (chidu-trash-workflow-account workflow))
         (key (chidu-trash--key account))
         (table (chidu-app-requests app))
         (entry (gethash key table))
         (runtime (chidu-trash--runtime workflow)))
    (when (eq workflow (car-safe entry))
      (remhash key table)
      (when-let* ((handle (cdr-safe entry)))
        (appkit-retire-handle handle)))
    (when runtime
      (chidu-runtime--deliver-result
       runtime
       (chidu-trash-workflow-runtime-operation workflow)
       result
       (lambda (value)
         (chidu-trash--message workflow)
         (funcall (chidu-trash-workflow-success-function workflow) value))
       (lambda (failure)
         (funcall (chidu-trash-workflow-error-function workflow) failure))))))

(defun chidu-trash--prefix (vector count)
  "Return first COUNT elements from VECTOR as a vector."
  (if (= count (length vector))
      (copy-sequence vector)
    (cl-subseq vector 0 count)))

(defun chidu-trash--bounded-prefix (vector object-limit byte-limit size-function)
  "Return largest prefix of VECTOR satisfying OBJECT-LIMIT and BYTE-LIMIT.

SIZE-FUNCTION receives a candidate vector and returns encoded request bytes."
  (let ((upper (min object-limit (length vector))))
    (cond
     ((zerop upper) (vector))
     ((<= (funcall size-function (chidu-trash--prefix vector upper)) byte-limit)
      (chidu-trash--prefix vector upper))
     ((> (funcall size-function (chidu-trash--prefix vector 1)) byte-limit)
      (vector))
     (t
      (let ((low 1) (high upper))
        (while (> (- high low) 1)
          (let ((middle (/ (+ low high) 2)))
            (if (<= (funcall size-function
                             (chidu-trash--prefix vector middle))
                    byte-limit)
                (setq low middle)
              (setq high middle))))
        (chidu-trash--prefix vector low))))))

(defun chidu-trash--unhydrated-intents (context)
  "Return CONTEXT intents lacking authoritative membership evidence."
  (vconcat
   (cl-loop
    for intent across (chidu-store-trash-context-intents context)
    unless (vectorp
            (chidu-store-trash-intent-original-remote-mailbox-ids intent))
    collect intent)))

(defun chidu-trash--ready-intents (context)
  "Return pending hydrated Trash intents from CONTEXT."
  (vconcat
   (cl-loop
    for intent across (chidu-store-trash-context-intents context)
    when
    (and (eq 'pending (chidu-store-trash-intent-phase intent))
         (vectorp
          (chidu-store-trash-intent-original-remote-mailbox-ids intent)))
    collect intent)))

(defun chidu-trash--reconciliation-intent (workflow context)
  "Return next uncertain CONTEXT intent not reconciled by WORKFLOW."
  (cl-find-if
   (lambda (intent)
     (and
      (eq 'unknown (chidu-store-trash-intent-phase intent))
      (not
       (gethash (chidu-store-trash-intent-local-email-id intent)
                (chidu-trash-workflow-reconciled workflow)))))
   (chidu-store-trash-context-intents context)))

(defun chidu-trash--remote-ids (intents)
  "Return remote Email ids for INTENTS."
  (vconcat
   (cl-loop for intent across intents
            collect (chidu-store-trash-intent-remote-email-id intent))))

(defun chidu-trash--hydration-batch (context intents)
  "Return largest mutable Email/get batch for CONTEXT and INTENTS."
  (let* ((endpoint (chidu-store-trash-context-endpoint context))
         (account (chidu-store-trash-context-account context))
         (object-limit (chidu-store-endpoint-max-objects-in-get endpoint))
         (byte-limit (chidu-store-endpoint-max-size-request endpoint)))
    (unless (and (integerp object-limit) (> object-limit 0)
                 (integerp byte-limit) (> byte-limit 0))
      (signal 'chidu-invariant-error
              '("JMAP Session has invalid Get request limits")))
    (chidu-trash--bounded-prefix
     intents object-limit byte-limit
     (lambda (candidate)
       (chidu-jmap-email-mutable-request-size
        account (chidu-trash--remote-ids candidate))))))

(defun chidu-trash--set-batch (context intents)
  "Return largest Email/set batch for CONTEXT and hydrated INTENTS."
  (let* ((endpoint (chidu-store-trash-context-endpoint context))
         (object-limit (chidu-store-endpoint-max-objects-in-set endpoint))
         (byte-limit (chidu-store-endpoint-max-size-request endpoint)))
    (unless (and (integerp object-limit) (> object-limit 0)
                 (integerp byte-limit) (> byte-limit 0))
      (signal 'chidu-invariant-error
              '("JMAP Session has invalid Set request limits")))
    (chidu-trash--bounded-prefix
     intents object-limit byte-limit
     (lambda (candidate)
       (chidu-jmap-trash-request-size context candidate)))))

(defun chidu-trash--intent-by-local-id (context local-id)
  "Return LOCAL-ID intent from CONTEXT, or nil."
  (cl-find local-id (chidu-store-trash-context-intents context)
           :key #'chidu-store-trash-intent-local-email-id
           :test #'equal))

(defun chidu-trash--after-store-transition (workflow result)
  "Emit Store RESULT and continue WORKFLOW."
  (cond
   ((chidu-result-failure-p result)
    (chidu-trash--finish workflow result))
   ((chidu-result-ok-p result)
    (let ((trash-result (chidu-result-ok-value result)))
      (unless (chidu-store-trash-result-p trash-result)
        (signal 'chidu-invariant-error
                '("Store returned invalid Trash result")))
      (chidu-trash--record-changes workflow trash-result)
      (chidu-trash--emit workflow trash-result)
      (chidu-trash--dispatch-next
       workflow (chidu-store-trash-result-context trash-result))))
   (t
    (chidu-trash--finish
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-trash--evidence (intents mutable-state)
  "Map MUTABLE-STATE targets to Store evidence for exact INTENTS."
  (let ((targets (chidu-jmap-email-mutable-state-targets mutable-state)))
    (unless (= (length intents) (length targets))
      (signal 'chidu-invariant-error '("Trash hydration coverage mismatch")))
    (vconcat
     (cl-loop
      for intent across intents
      for target across targets
      do
      (unless
          (equal (chidu-store-trash-intent-remote-email-id intent)
                 (chidu-jmap-email-mutable-target-remote-id target))
        (signal 'chidu-invariant-error '("Trash hydration order mismatch")))
      collect
      (chidu-store-trash-target-evidence-create
       :local-email-id (chidu-store-trash-intent-local-email-id intent)
       :remote-email-id (chidu-store-trash-intent-remote-email-id intent)
       :found-p (chidu-jmap-email-mutable-target-found-p target)
       :remote-mailbox-ids
       (or (chidu-jmap-email-mutable-target-remote-mailbox-ids target)
           (vector)))))))

(defun chidu-trash--after-hydration (workflow intents result)
  "Record authoritative hydration RESULT for WORKFLOW INTENTS."
  (when-let* ((runtime (chidu-trash--runtime workflow))
              ((chidu-runtime--operation-current-p
                runtime (chidu-trash-workflow-runtime-operation workflow))))
    (setf
     (chidu-runtime-operation-cancel-function
      (chidu-trash-workflow-runtime-operation workflow))
     nil)
    (cond
     ((chidu-result-failure-p result)
      (chidu-trash--finish workflow result))
     ((chidu-result-ok-p result)
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-record-trash-evidence-create
        :account-id
        (chidu-store-account-account-id
         (chidu-trash-workflow-account workflow))
        :operation-id (chidu-trash-workflow-operation-id workflow)
        :evidence
        (chidu-trash--evidence intents (chidu-result-ok-value result)))
       (lambda (store-result)
         (chidu-trash--after-store-transition workflow store-result))))
     (t
      (chidu-trash--finish
       workflow
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result) :retryable-p nil))))))

(defun chidu-trash--fetch-mutable
    (workflow context intents deliver)
  "Fetch CONTEXT mutable state for WORKFLOW INTENTS and call DELIVER."
  (let ((runtime (chidu-trash--runtime workflow))
        secret)
    (if (null runtime)
        (chidu-trash--finish
         workflow
         (chidu-result-failure-create
          :kind 'runtime-closed :data nil :retryable-p t))
      (condition-case error-data
          (setq secret
                (chidu-runtime--endpoint-secret
                 (chidu-store-trash-context-endpoint context)))
        (error
         (chidu-trash--finish
          workflow
          (chidu-runtime--condition-failure
           'credential-error error-data nil))))
      (when secret
        (condition-case error-data
            (let ((cancel
                   (chidu-jmap-email-fetch-mutable-state
                    (chidu-store-trash-context-endpoint context)
                    (chidu-store-trash-context-account context)
                    (chidu-trash--remote-ids intents)
                    secret deliver)))
              ;; Mutable Email/get leaves credential ownership with the caller.
              (clear-string secret)
              (when (chidu-runtime--operation-current-p
                     runtime (chidu-trash-workflow-runtime-operation workflow))
                (chidu-runtime--set-operation-cancel
                 runtime
                 (chidu-trash-workflow-runtime-operation workflow)
                 cancel)))
          (error
           (clear-string secret)
           (chidu-trash--finish
            workflow
            (chidu-runtime--condition-failure
             'jmap-request-failed error-data nil))))))))

(defun chidu-trash--hydrate (workflow context intents)
  "Fetch authoritative membership for WORKFLOW INTENTS in CONTEXT."
  (chidu-trash--fetch-mutable
   workflow context intents
   (lambda (result)
     (chidu-trash--after-hydration workflow intents result))))

(defun chidu-trash--failure-outcomes (intents failure)
  "Return target outcomes for INTENTS after transport FAILURE."
  (let* ((kind (chidu-result-failure-kind failure))
         (error-kind (symbol-name kind))
         (outcome (if (eq kind 'request-too-large) 'rejected 'unknown)))
    (vconcat
     (cl-loop
      for intent across intents
      collect
      (chidu-store-trash-target-outcome-create
       :local-email-id (chidu-store-trash-intent-local-email-id intent)
       :outcome outcome :error-kind error-kind)))))

(defun chidu-trash--remote-outcomes (intents response)
  "Map exact remote Set RESPONSE outcomes back to local INTENTS."
  (let ((by-remote (make-hash-table :test #'equal)))
    (cl-loop
     for intent across intents
     do (puthash (chidu-store-trash-intent-remote-email-id intent)
                 intent by-remote))
    (vconcat
     (cl-loop
      for result across (chidu-jmap-set-update-response-results response)
      for intent =
      (gethash (chidu-jmap-set-target-result-remote-id result) by-remote)
      do
      (unless intent
        (signal 'chidu-invariant-error
                '("Trash response has no local target")))
      collect
      (chidu-store-trash-target-outcome-create
       :local-email-id (chidu-store-trash-intent-local-email-id intent)
       :outcome (chidu-jmap-set-target-result-outcome result)
       :error-kind (chidu-jmap-set-target-result-error-kind result))))))

(defun chidu-trash--settle (workflow outcomes)
  "Settle WORKFLOW target OUTCOMES in the Store."
  (let ((runtime (chidu-trash--runtime workflow)))
    (if (null runtime)
        (chidu-trash--finish
         workflow
         (chidu-result-failure-create
          :kind 'runtime-closed :data nil :retryable-p t))
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-settle-trash-create
        :account-id
        (chidu-store-account-account-id
         (chidu-trash-workflow-account workflow))
        :operation-id (chidu-trash-workflow-operation-id workflow)
        :outcomes outcomes)
       (lambda (result)
         (chidu-trash--after-store-transition workflow result))))))

(defun chidu-trash--after-jmap (workflow intents result)
  "Settle WORKFLOW INTENTS after remote Set RESULT."
  (condition-case error-data
      (chidu-trash--settle
       workflow
       (cond
        ((chidu-result-ok-p result)
         (chidu-trash--remote-outcomes
          intents (chidu-result-ok-value result)))
        ((chidu-result-failure-p result)
         (chidu-trash--failure-outcomes intents result))
        (t
         (signal 'chidu-invariant-error
                 '("JMAP returned invalid Trash result")))))
    (error
     (chidu-trash--finish
      workflow
      (chidu-result-failure-create
       :kind 'invalid-jmap-response
       :data (list :message (error-message-string error-data))
       :retryable-p nil)))))

(defun chidu-trash--dispatch-set (workflow context intents)
  "Dispatch one move-to-Trash batch for WORKFLOW CONTEXT and INTENTS."
  (let ((runtime (chidu-trash--runtime workflow))
        secret)
    (if (null runtime)
        (chidu-trash--finish
         workflow
         (chidu-result-failure-create
          :kind 'runtime-closed :data nil :retryable-p t))
      (condition-case error-data
          (setq secret
                (chidu-runtime--endpoint-secret
                 (chidu-store-trash-context-endpoint context)))
        (error
         (chidu-trash--finish
          workflow
          (chidu-runtime--condition-failure
           'credential-error error-data nil))))
      (when secret
        (condition-case error-data
            (let ((cancel
                   (chidu-jmap-move-to-trash-batch
                    context secret intents
                    (lambda (result)
                      (chidu-trash--after-jmap workflow intents result)))))
              ;; The Set adapter owns and clears SECRET after this point.
              (setq secret nil)
              (chidu-runtime--set-operation-cancel
               runtime (chidu-trash-workflow-runtime-operation workflow)
               cancel))
          (error
           (when secret (clear-string secret))
           (chidu-trash--finish
            workflow
            (chidu-runtime--condition-failure
             'jmap-request-failed error-data nil))))))))

(defun chidu-trash--after-reconciliation-evidence
    (workflow local-id result)
  "Continue WORKFLOW after recording reconciliation RESULT for LOCAL-ID."
  (cond
   ((chidu-result-failure-p result)
    (chidu-trash--finish workflow result))
   ((chidu-result-ok-p result)
    (let* ((trash-result (chidu-result-ok-value result))
           (context (chidu-store-trash-result-context trash-result))
           (intent (chidu-trash--intent-by-local-id context local-id)))
      (chidu-trash--record-changes workflow trash-result)
      (chidu-trash--emit workflow trash-result)
      (puthash local-id t (chidu-trash-workflow-reconciled workflow))
      (cond
       ((null intent)
        (chidu-trash--dispatch-next workflow context))
       ((eq 'pending (chidu-store-trash-intent-phase intent))
        (chidu-trash--dispatch-next workflow context))
       (t
        ;; notFound after an uncertain Set remains durable.  Do not guess
        ;; success and do not loop in this workflow.
        (chidu-trash--dispatch-next workflow context)))))
   (t
    (chidu-trash--finish
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-trash--after-reconciliation
    (workflow context intent result)
  "Record authoritative reconciliation RESULT for WORKFLOW CONTEXT INTENT."
  (when-let* ((runtime (chidu-trash--runtime workflow))
              ((chidu-runtime--operation-current-p
                runtime (chidu-trash-workflow-runtime-operation workflow))))
    (setf
     (chidu-runtime-operation-cancel-function
      (chidu-trash-workflow-runtime-operation workflow))
     nil)
    (if (chidu-result-failure-p result)
        (chidu-trash--finish workflow result)
      (let ((evidence
             (chidu-trash--evidence
              (vector intent) (chidu-result-ok-value result))))
        (chidu-runtime--store-call
         runtime
         (chidu-store-op-record-trash-evidence-create
          :account-id
          (chidu-store-account-account-id
           (chidu-store-trash-context-account context))
          :operation-id (chidu-store-trash-context-operation-id context)
          :evidence evidence)
         (lambda (store-result)
           (chidu-trash--after-reconciliation-evidence
            workflow (chidu-store-trash-intent-local-email-id intent)
            store-result)))))))

(defun chidu-trash--dispatch-next (workflow context)
  "Hydrate, reconcile, or dispatch WORKFLOW's next CONTEXT batch."
  (let ((reconciliation
         (chidu-trash--reconciliation-intent workflow context))
        (unhydrated (chidu-trash--unhydrated-intents context))
        (ready (chidu-trash--ready-intents context)))
    (cond
     (reconciliation
      ;; notFound after an uncertain Set remains durable instead of being
      ;; treated like a first-attempt rejection.
      (chidu-trash--fetch-mutable
       workflow context (vector reconciliation)
       (lambda (result)
         (chidu-trash--after-reconciliation
          workflow context reconciliation result))))
     ((> (length unhydrated) 0)
      (let ((batch (chidu-trash--hydration-batch context unhydrated)))
        (if (zerop (length batch))
            (chidu-trash--settle
             workflow
             (vector
              (chidu-store-trash-target-outcome-create
               :local-email-id
               (chidu-store-trash-intent-local-email-id
                (aref unhydrated 0))
               :outcome 'rejected :error-kind "request-too-large")))
          (chidu-trash--hydrate workflow context batch))))
     ((> (length ready) 0)
      (let ((batch (chidu-trash--set-batch context ready)))
        (if (zerop (length batch))
            (chidu-trash--settle
             workflow
             (vector
              (chidu-store-trash-target-outcome-create
               :local-email-id
               (chidu-store-trash-intent-local-email-id (aref ready 0))
               :outcome 'rejected :error-kind "request-too-large")))
          (chidu-trash--dispatch-set workflow context batch))))
     (t
      (chidu-trash--finish
       workflow (chidu-result-ok-create :value context))))))

(defun chidu-trash--after-accept (workflow result)
  "Continue WORKFLOW after Store accept RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-trash--finish workflow result))
   ((chidu-result-ok-p result)
    (let ((trash-result (chidu-result-ok-value result)))
      (unless (chidu-store-trash-result-p trash-result)
        (signal 'chidu-invariant-error
                '("Store returned invalid Trash acceptance")))
      (chidu-trash--emit workflow trash-result)
      (chidu-trash--dispatch-next
       workflow (chidu-store-trash-result-context trash-result))))
   (t
    (chidu-trash--finish
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-trash--start-workflow
    (app account operation-id success-function error-function quiet-p
         progress-function)
  "Create and register one Trash workflow in APP.

ACCOUNT and OPERATION-ID identify it; SUCCESS-FUNCTION, ERROR-FUNCTION,
QUIET-P, and PROGRESS-FUNCTION configure completion."
  (let* ((runtime (chidu-app-runtime app))
         (key (chidu-trash--key account))
         (table (chidu-app-requests app)))
    (unless (chidu-runtime-p runtime)
      (user-error "Chidu runtime is unavailable"))
    (when (gethash key table)
      (user-error "A move-to-Trash operation is already active for this Account"))
    (let* ((runtime-operation (chidu-runtime--begin-operation runtime))
           (workflow
            (chidu-trash-workflow-create
             :app app :account account :operation-id operation-id
             :runtime-operation runtime-operation
             :reconciled (make-hash-table :test #'equal)
             :phases (make-hash-table :test #'equal)
             :committed 0 :rejected 0 :unknown 0
             :quiet-p quiet-p
             :progress-function (or progress-function #'ignore)
             :success-function success-function
             :error-function error-function))
           (handle
            (appkit-register-handle
             app 'jmap-operation runtime-operation
             (lambda (operation)
               (chidu-runtime-cancel-operation runtime operation)))))
      (puthash key (cons workflow handle) table)
      (setf
       (chidu-runtime-operation-cancel-cleanup-function runtime-operation)
       (lambda ()
         (when (eq workflow (car-safe (gethash key table)))
           (remhash key table))
         (appkit-retire-handle handle)))
      workflow)))

(defun chidu-trash-emails
    (app account trash-mailbox local-email-ids
         success-function error-function &optional progress-function)
  "Move LOCAL-EMAIL-IDS to TRASH-MAILBOX for ACCOUNT in APP.

The operation is durable before authoritative Email/get or Email/set.  Call
SUCCESS-FUNCTION with the final Store context and ERROR-FUNCTION with a typed
failure."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (and (chidu-store-mailbox-p trash-mailbox)
               (equal "trash" (chidu-store-mailbox-role trash-mailbox)))
    (signal 'wrong-type-argument (list 'trash-mailbox-p trash-mailbox)))
  (unless (and (vectorp local-email-ids) (> (length local-email-ids) 0))
    (user-error "No Email selected"))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let* ((operation-id (chidu-store-new-local-id))
         (workflow
          (chidu-trash--start-workflow
           app account operation-id success-function error-function nil
           progress-function))
         (runtime (chidu-app-runtime app)))
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-accept-trash-create
      :account-id (chidu-store-account-account-id account)
      :operation-id operation-id
      :trash-mailbox-id
      (chidu-store-mailbox-mailbox-id trash-mailbox)
      :local-email-ids local-email-ids)
     (lambda (result) (chidu-trash--after-accept workflow result)))
    (chidu-trash-workflow-runtime-operation workflow)))

(defun chidu-trash--resume (app context)
  "Resume durable Trash CONTEXT in APP."
  (let* ((account (chidu-store-trash-context-account context))
         (operation-id (chidu-store-trash-context-operation-id context)))
    (when (and operation-id
               (> (length (chidu-store-trash-context-intents context)) 0))
      (condition-case nil
          (let ((workflow
                 (chidu-trash--start-workflow
                  app account operation-id #'ignore #'ignore t #'ignore)))
            (chidu-trash--dispatch-next workflow context))
        (user-error nil)))))

(defun chidu-retry-trash-operations (app account)
  "Retry ACCOUNT's unresolved durable move-to-Trash operation in APP."
  (when (and (appkit-app-live-p app) (chidu-store-account-p account))
    (let ((runtime (chidu-app-runtime app)))
      (chidu-runtime--simple-store-operation
       runtime
       (chidu-store-op-get-trash-context-create
        :account-id (chidu-store-account-account-id account))
       (lambda (context) (chidu-trash--resume app context))
       (lambda (_failure) nil)))))

(provide 'chidu-trash)

;;; chidu-trash.el ends here
