;;; chidu-mailbox-move.el --- Durable batched Mailbox moves -*- lexical-binding: t; -*-

;;; Commentary:

;; Accept a complete user operation in the Store before dispatch, then send
;; bounded Email/set batches.  Every target settles independently; uncertain
;; transport outcomes remain durable and are retried after reconnect.  One
;; Account owns at most one unresolved move, which gives simple ordering without
;; inventing a generic mutation scheduler prematurely.

;;; Code:

(require 'cl-lib)
(require 'appkit-app)
(require 'chidu-jmap-email)
(require 'chidu-jmap-mailbox-move)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-surface-operation)
(require 'chidu-store)
(require 'seq)

(cl-defstruct (chidu-mailbox-move-workflow
               (:constructor chidu-mailbox-move-workflow-create))
  "Ephemeral control state for one durable Mailbox move operation."
  app
  account
  operation-id
  runtime-operation
  attempted
  reconciled
  phases
  committed
  rejected
  unknown
  quiet-p
  progress-function
  success-function
  error-function)

(defun chidu-mailbox-move--key (account)
  "Return APP request-table key for ACCOUNT."
  (list 'mailbox-move (chidu-store-account-account-id account)))

(defun chidu-mailbox-move-destinations (mailboxes source)
  "Return writable MAILBOXES other than move SOURCE in display order."
  (let ((source-id (chidu-store-mailbox-mailbox-id source)))
    (vconcat
     (sort
      (cl-loop
       for mailbox across mailboxes
       when
       (and
        (chidu-store-mailbox-available-p mailbox)
        (not
         (equal source-id (chidu-store-mailbox-mailbox-id mailbox)))
        (chidu-store-mailbox-rights-may-add-items-p
         (chidu-store-mailbox-rights mailbox)))
       collect mailbox)
      #'chidu-store-mailbox-less-p))))

(defun chidu-mailbox-move-role-destination (mailboxes source role)
  "Return writable ROLE destination from MAILBOXES other than SOURCE."
  (or
   (cl-find role (chidu-mailbox-move-destinations mailboxes source)
            :key #'chidu-store-mailbox-role :test #'equal)
   (user-error "No writable %s Mailbox is available" role)))

(defun chidu-mailbox-move-read-mailbox (prompt mailboxes)
  "Read one Mailbox from nonempty MAILBOXES using PROMPT."
  (unless (> (length mailboxes) 0)
    (user-error "No writable destination Mailbox is available"))
  (let* ((choices
          (cl-loop
           for mailbox across mailboxes
           collect
           (cons
            (format "%s [%s]"
                    (chidu-store-mailbox-name mailbox)
                    (chidu-store-mailbox-remote-mailbox-id mailbox))
            mailbox)))
         (selected (completing-read prompt choices nil t)))
    (cdr (assoc-string selected choices))))

(defun chidu-mailbox-move-result-for-account-p (result account)
  "Return non-nil when Mailbox move RESULT belongs to ACCOUNT."
  (and
   (chidu-store-mailbox-move-result-p result)
   (chidu-store-account-p account)
   (equal
    (chidu-store-account-account-id
     (chidu-store-mailbox-move-context-account
      (chidu-store-mailbox-move-result-context result)))
    (chidu-store-account-account-id account))))

(defun chidu-mailbox-move-result-affects-mailbox-p (result mailbox)
  "Return non-nil when Mailbox move RESULT may affect MAILBOX."
  (when (and (chidu-store-mailbox-move-result-p result)
             (chidu-store-mailbox-p mailbox))
    (let* ((context (chidu-store-mailbox-move-result-context result))
           (source (chidu-store-mailbox-move-context-source-mailbox context))
           (destination
            (chidu-store-mailbox-move-context-destination-mailbox context))
           (mailbox-id (chidu-store-mailbox-mailbox-id mailbox)))
      (if (or source destination)
          (or
           (and source
                (equal mailbox-id
                       (chidu-store-mailbox-mailbox-id source)))
           (and destination
                (equal mailbox-id
                       (chidu-store-mailbox-mailbox-id destination))))
        ;; A final settlement context no longer retains the operation route.
        ;; Reload conservatively when it contains an effective transition.
        (> (length (chidu-store-mailbox-move-result-changes result)) 0)))))

(defun chidu-mailbox-move-result-committed-p (result)
  "Return non-nil when RESULT contains a committed target transition."
  (and
   (chidu-store-mailbox-move-result-p result)
   (cl-loop
    for change across (chidu-store-mailbox-move-result-changes result)
    thereis
    (eq 'committed
        (chidu-store-mailbox-move-target-change-phase change)))))

(defun chidu-mailbox-move--runtime (workflow)
  "Return live runtime for WORKFLOW, or nil."
  (let ((app (chidu-mailbox-move-workflow-app workflow)))
    (when (appkit-app-live-p app)
      (let ((runtime (chidu-app-runtime app)))
        (and (chidu-runtime-p runtime) runtime)))))

(defun chidu-mailbox-move--emit (workflow result)
  "Emit Store mailbox-move RESULT for WORKFLOW's live app."
  (let ((app (chidu-mailbox-move-workflow-app workflow)))
    (when (appkit-app-live-p app)
      (chidu-post-app-message app (list 'chidu-mailbox-move-changed app result))
      (funcall
       (chidu-mailbox-move-workflow-progress-function workflow)
       result))))

(defun chidu-mailbox-move--record-changes (workflow result)
  "Accumulate latest target phases from Store RESULT in WORKFLOW."
  (let ((phases (chidu-mailbox-move-workflow-phases workflow)))
    (cl-loop
     for change across (chidu-store-mailbox-move-result-changes result)
     for local-id =
     (chidu-store-mailbox-move-target-change-local-email-id change)
     for phase = (chidu-store-mailbox-move-target-change-phase change)
     for previous = (gethash local-id phases)
     do
     (pcase previous
       ('committed
        (cl-decf (chidu-mailbox-move-workflow-committed workflow)))
       ('reverted
        (cl-decf (chidu-mailbox-move-workflow-rejected workflow)))
       ('unknown
        (cl-decf (chidu-mailbox-move-workflow-unknown workflow))))
     do
     (pcase phase
       ('committed
        (cl-incf (chidu-mailbox-move-workflow-committed workflow)))
       ('reverted
        (cl-incf (chidu-mailbox-move-workflow-rejected workflow)))
       ('unknown
        (cl-incf (chidu-mailbox-move-workflow-unknown workflow))))
     do (puthash local-id phase phases))))

(defun chidu-mailbox-move--message (workflow)
  "Report aggregate completion for WORKFLOW."
  (unless (chidu-mailbox-move-workflow-quiet-p workflow)
    (let ((committed (chidu-mailbox-move-workflow-committed workflow))
          (rejected (chidu-mailbox-move-workflow-rejected workflow))
          (unknown (chidu-mailbox-move-workflow-unknown workflow)))
      (message
       "Chidu: moved %d Email%s%s%s"
       committed (if (= committed 1) "" "s")
       (if (> rejected 0) (format "; %d rejected" rejected) "")
       (if (> unknown 0) (format "; %d awaiting retry" unknown) "")))))

(defun chidu-mailbox-move--finish (workflow result)
  "Finish WORKFLOW exactly once with typed RESULT."
  (let* ((app (chidu-mailbox-move-workflow-app workflow))
         (account (chidu-mailbox-move-workflow-account workflow))
         (key (chidu-mailbox-move--key account))
         (table (chidu-app-requests app))
         (entry (gethash key table))
         (runtime (chidu-mailbox-move--runtime workflow)))
    (when (eq workflow (car-safe entry))
      (remhash key table)
      (when-let* ((handle (cdr-safe entry)))
        (appkit-retire-handle handle)))
    (when runtime
      (chidu-runtime--deliver-result
       runtime
       (chidu-mailbox-move-workflow-runtime-operation workflow)
       result
       (lambda (value)
         (chidu-mailbox-move--message workflow)
         (funcall
          (chidu-mailbox-move-workflow-success-function workflow)
          value))
       (lambda (failure)
         (funcall
          (chidu-mailbox-move-workflow-error-function workflow)
          failure))))))

(defun chidu-mailbox-move--unattempted-intents (workflow context)
  "Return CONTEXT intents not yet attempted by WORKFLOW."
  (let ((attempted (chidu-mailbox-move-workflow-attempted workflow)))
    (vconcat
     (cl-loop
      for intent across (chidu-store-mailbox-move-context-intents context)
      unless
      (gethash (chidu-store-mailbox-move-intent-local-email-id intent)
               attempted)
      collect intent))))

(defun chidu-mailbox-move--batch (context intents)
  "Return largest Session-bounded prefix of INTENTS for CONTEXT."
  (let* ((endpoint (chidu-store-mailbox-move-context-endpoint context))
         (object-limit (chidu-store-endpoint-max-objects-in-set endpoint))
         (byte-limit (chidu-store-endpoint-max-size-request endpoint)))
    (unless (and (integerp object-limit) (> object-limit 0)
                 (integerp byte-limit) (> byte-limit 0))
      (signal 'chidu-invariant-error
              '("JMAP Session has invalid Set request limits")))
    (let* ((upper (min object-limit (length intents)))
           (prefix
            (lambda (count)
              (if (= count (length intents))
                  (copy-sequence intents)
                (cl-subseq intents 0 count))))
           (fits
            (lambda (count)
              (<= (chidu-jmap-mailbox-move-request-size
                   context (funcall prefix count))
                  byte-limit))))
      (cond
       ((funcall fits upper) (funcall prefix upper))
       ((not (funcall fits 1)) (vector))
       (t
        (let ((low 1)
              (high upper))
          (while (> (- high low) 1)
            (let ((middle (/ (+ low high) 2)))
              (if (funcall fits middle)
                  (setq low middle)
                (setq high middle))))
          (funcall prefix low)))))))

(defun chidu-mailbox-move--mark-attempted (workflow intents)
  "Record INTENTS as attempted by WORKFLOW."
  (cl-loop
   for intent across intents
   do
   (puthash (chidu-store-mailbox-move-intent-local-email-id intent)
            t
            (chidu-mailbox-move-workflow-attempted workflow))))

(defun chidu-mailbox-move--failure-outcomes (intents failure)
  "Return target outcomes for INTENTS after transport FAILURE."
  (let* ((failure-kind (chidu-result-failure-kind failure))
         (kind (symbol-name failure-kind))
         (outcome (if (eq failure-kind 'request-too-large)
                      'rejected
                    'unknown)))
    (vconcat
     (cl-loop
      for intent across intents
      collect
      (chidu-store-mailbox-move-target-outcome-create
       :local-email-id
       (chidu-store-mailbox-move-intent-local-email-id intent)
       :outcome outcome
       :error-kind kind)))))

(defun chidu-mailbox-move--remote-outcomes (intents response)
  "Map exact remote Set RESPONSE outcomes back to local INTENTS."
  (let ((by-remote (make-hash-table :test #'equal)))
    (cl-loop
     for intent across intents
     do
     (puthash (chidu-store-mailbox-move-intent-remote-email-id intent)
              intent by-remote))
    (vconcat
     (cl-loop
      for result across (chidu-jmap-set-update-response-results response)
      for remote-id = (chidu-jmap-set-target-result-remote-id result)
      for intent = (gethash remote-id by-remote)
      do
      (unless intent
        (signal 'chidu-invariant-error
                '("Mailbox move response has no local target")))
      collect
      (chidu-store-mailbox-move-target-outcome-create
       :local-email-id
       (chidu-store-mailbox-move-intent-local-email-id intent)
       :outcome (chidu-jmap-set-target-result-outcome result)
       :error-kind (chidu-jmap-set-target-result-error-kind result))))))

(defun chidu-mailbox-move--after-settle (workflow result)
  "Continue WORKFLOW after Store settlement RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-mailbox-move--finish workflow result))
   ((chidu-result-ok-p result)
    (let ((move-result (chidu-result-ok-value result)))
      (unless (chidu-store-mailbox-move-result-p move-result)
        (signal 'chidu-invariant-error
                '("Store returned invalid Mailbox move result")))
      (chidu-mailbox-move--record-changes workflow move-result)
      (chidu-mailbox-move--emit workflow move-result)
      (chidu-mailbox-move--dispatch-next
       workflow (chidu-store-mailbox-move-result-context move-result))))
   (t
    (chidu-mailbox-move--finish
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-mailbox-move--settle (workflow intents outcomes)
  "Settle INTENTS using local target OUTCOMES for WORKFLOW."
  (let ((runtime (chidu-mailbox-move--runtime workflow)))
    (if (null runtime)
        (chidu-mailbox-move--finish
         workflow
         (chidu-result-failure-create
          :kind 'runtime-closed :data nil :retryable-p t))
      (unless (= (length intents) (length outcomes))
        (signal 'chidu-invariant-error
                '("Mailbox move settlement coverage mismatch")))
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-settle-mailbox-move-create
        :account-id
        (chidu-store-account-account-id
         (chidu-mailbox-move-workflow-account workflow))
        :operation-id
        (chidu-mailbox-move-workflow-operation-id workflow)
        :outcomes outcomes)
       (lambda (result)
         (chidu-mailbox-move--after-settle workflow result))))))

(defun chidu-mailbox-move--after-jmap (workflow intents result)
  "Settle WORKFLOW's INTENTS batch after JMAP RESULT."
  (condition-case error-data
      (chidu-mailbox-move--settle
       workflow intents
       (cond
        ((chidu-result-ok-p result)
         (chidu-mailbox-move--remote-outcomes
          intents (chidu-result-ok-value result)))
        ((chidu-result-failure-p result)
         (chidu-mailbox-move--failure-outcomes intents result))
        (t
         (signal 'chidu-invariant-error
                 '("JMAP returned invalid Mailbox move result")))))
    (error
     (chidu-mailbox-move--finish
      workflow
      (chidu-result-failure-create
       :kind 'invalid-jmap-response
       :data (list :message (error-message-string error-data))
       :retryable-p nil)))))

(defun chidu-mailbox-move--dispatch-ready (workflow context)
  "Dispatch the next unattempted batch from CONTEXT for WORKFLOW."
  (let ((remaining
         (chidu-mailbox-move--unattempted-intents workflow context)))
    (if (zerop (length remaining))
        (chidu-mailbox-move--finish
         workflow
         (chidu-result-ok-create :value context))
      (let* ((runtime (chidu-mailbox-move--runtime workflow))
             (batch (chidu-mailbox-move--batch context remaining)))
        (cond
         ((null runtime)
          (chidu-mailbox-move--finish
           workflow
           (chidu-result-failure-create
            :kind 'runtime-closed :data nil :retryable-p t)))
         ((zerop (length batch))
          (let* ((intent (aref remaining 0))
                 (local-id
                  (chidu-store-mailbox-move-intent-local-email-id intent)))
            (chidu-mailbox-move--mark-attempted workflow (vector intent))
            (chidu-mailbox-move--settle
             workflow
             (vector intent)
             (vector
              (chidu-store-mailbox-move-target-outcome-create
               :local-email-id local-id
               :outcome 'rejected
               :error-kind "request-too-large")))))
         (t
          (let (secret)
            (condition-case error-data
                (setq secret
                      (chidu-runtime--endpoint-secret
                       (chidu-store-mailbox-move-context-endpoint context)))
              (error
               (chidu-mailbox-move--finish
                workflow
                (chidu-runtime--condition-failure
                 'credential-error error-data nil))))
            (when secret
              (chidu-mailbox-move--mark-attempted workflow batch)
              (condition-case error-data
                  (let ((cancel
                         (chidu-jmap-move-mailbox-batch
                          context secret batch
                          (lambda (result)
                            (chidu-mailbox-move--after-jmap
                             workflow batch result)))))
                    ;; The adapter owns and clears SECRET after this point.
                    (setq secret nil)
                    (chidu-runtime--set-operation-cancel
                     runtime
                     (chidu-mailbox-move-workflow-runtime-operation workflow)
                     cancel))
                (error
                 (when secret (clear-string secret))
                 (chidu-mailbox-move--finish
                  workflow
                  (chidu-runtime--condition-failure
                   'jmap-request-failed error-data nil))))))))))))

(defun chidu-mailbox-move--reconciliation-intent (workflow context)
  "Return WORKFLOW's next uncertain intent from CONTEXT."
  (cl-find-if
   (lambda (intent)
     (and
      (equal "serverPartialFail"
             (chidu-store-mailbox-move-intent-error-kind intent))
      (not
       (gethash
        (chidu-store-mailbox-move-intent-local-email-id intent)
        (chidu-mailbox-move-workflow-reconciled workflow)))))
   (chidu-store-mailbox-move-context-intents context)))

(defun chidu-mailbox-move--after-reconciliation
    (workflow context intent result)
  "Continue WORKFLOW after reconciling CONTEXT INTENT against RESULT."
  (when-let* ((runtime (chidu-mailbox-move--runtime workflow))
              ((chidu-runtime--operation-current-p
                runtime
                (chidu-mailbox-move-workflow-runtime-operation workflow))))
    (setf
     (chidu-runtime-operation-cancel-function
      (chidu-mailbox-move-workflow-runtime-operation workflow))
     nil)
    (cond
     ((chidu-result-failure-p result)
      (chidu-mailbox-move--finish workflow result))
     ((chidu-result-ok-p result)
      (let* ((state (chidu-result-ok-value result))
             (target
              (aref (chidu-jmap-email-mutable-state-targets state) 0))
             (local-id
              (chidu-store-mailbox-move-intent-local-email-id intent))
             (source-id
              (chidu-store-mailbox-remote-mailbox-id
               (chidu-store-mailbox-move-context-source-mailbox context)))
             (destination-id
              (chidu-store-mailbox-remote-mailbox-id
               (chidu-store-mailbox-move-context-destination-mailbox
                context)))
             (mailbox-ids
              (chidu-jmap-email-mutable-target-remote-mailbox-ids target)))
        (puthash local-id t
                 (chidu-mailbox-move-workflow-reconciled workflow))
        (cond
         ((not (chidu-jmap-email-mutable-target-found-p target))
          (chidu-mailbox-move--settle
           workflow (vector intent)
           (vector
            (chidu-store-mailbox-move-target-outcome-create
             :local-email-id local-id
             :outcome 'rejected
             :error-kind "notFound"))))
         ((and
           (seq-contains-p mailbox-ids destination-id #'equal)
           (not (seq-contains-p mailbox-ids source-id #'equal)))
          (chidu-mailbox-move--settle
           workflow (vector intent)
           (vector
            (chidu-store-mailbox-move-target-outcome-create
             :local-email-id local-id :outcome 'succeeded))))
         (t
          (remhash local-id
                   (chidu-mailbox-move-workflow-attempted workflow))
          (chidu-mailbox-move--dispatch-next workflow context)))))
     (t
      (chidu-mailbox-move--finish
       workflow
       (chidu-result-failure-create
        :kind 'invalid-result :data (list :value result)
        :retryable-p nil))))))

(defun chidu-mailbox-move--reconcile
    (workflow context intent)
  "Using CONTEXT, reconcile uncertain INTENT before retrying WORKFLOW."
  (let ((runtime (chidu-mailbox-move--runtime workflow))
        secret)
    (if (null runtime)
        (chidu-mailbox-move--finish
         workflow
         (chidu-result-failure-create
          :kind 'runtime-closed :data nil :retryable-p t))
      (condition-case error-data
          (setq secret
                (chidu-runtime--endpoint-secret
                 (chidu-store-mailbox-move-context-endpoint context)))
        (error
         (chidu-mailbox-move--finish
          workflow
          (chidu-runtime--condition-failure
           'credential-error error-data nil))))
      (when secret
        (let ((cancel
               (chidu-jmap-email-fetch-mutable-state
                (chidu-store-mailbox-move-context-endpoint context)
                (chidu-store-mailbox-move-context-account context)
                (vector
                 (chidu-store-mailbox-move-intent-remote-email-id intent))
                secret
                (lambda (result)
                  (chidu-mailbox-move--after-reconciliation
                   workflow context intent result)))))
          (clear-string secret)
          (when (chidu-runtime--operation-current-p
                 runtime
                 (chidu-mailbox-move-workflow-runtime-operation workflow))
            (chidu-runtime--set-operation-cancel
             runtime
             (chidu-mailbox-move-workflow-runtime-operation workflow)
             cancel)))))))

(defun chidu-mailbox-move--dispatch-next (workflow context)
  "From CONTEXT, reconcile uncertainty or dispatch WORKFLOW's next batch."
  (if-let* ((intent
             (chidu-mailbox-move--reconciliation-intent
              workflow context)))
      (chidu-mailbox-move--reconcile workflow context intent)
    (chidu-mailbox-move--dispatch-ready workflow context)))

(defun chidu-mailbox-move--after-accept (workflow result)
  "Continue WORKFLOW after Store accept RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-mailbox-move--finish workflow result))
   ((chidu-result-ok-p result)
    (let ((move-result (chidu-result-ok-value result)))
      (unless (chidu-store-mailbox-move-result-p move-result)
        (signal 'chidu-invariant-error
                '("Store returned invalid Mailbox move acceptance")))
      (chidu-mailbox-move--emit workflow move-result)
      (chidu-mailbox-move--dispatch-next
       workflow (chidu-store-mailbox-move-result-context move-result))))
   (t
    (chidu-mailbox-move--finish
     workflow
     (chidu-result-failure-create
      :kind 'invalid-result :data (list :value result) :retryable-p nil)))))

(defun chidu-mailbox-move--start-workflow
    (app account operation-id success-function error-function quiet-p
         progress-function)
  "Create and register one Mailbox move workflow in APP.

ACCOUNT and OPERATION-ID identify it; SUCCESS-FUNCTION, ERROR-FUNCTION,
QUIET-P, and PROGRESS-FUNCTION configure its completion policy."
  (let* ((runtime (chidu-app-runtime app))
         (key (chidu-mailbox-move--key account))
         (table (chidu-app-requests app)))
    (unless (chidu-runtime-p runtime)
      (user-error "Chidu runtime is unavailable"))
    (when (gethash key table)
      (user-error "A Mailbox move is already active for this Account"))
    (let* ((runtime-operation (chidu-runtime--begin-operation runtime))
           (workflow
            (chidu-mailbox-move-workflow-create
             :app app
             :account account
             :operation-id operation-id
             :runtime-operation runtime-operation
             :reconciled (make-hash-table :test #'equal)
             :attempted (make-hash-table :test #'equal)
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

(defun chidu-move-emails
    (app account source-mailbox destination-mailbox local-email-ids
         success-function error-function &optional progress-function)
  "Move LOCAL-EMAIL-IDS between Mailboxes for ACCOUNT in APP.

The full operation is durable before any network effect.  SUCCESS-FUNCTION
receives the final Store context; ERROR-FUNCTION receives a typed failure."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (dolist (mailbox (list source-mailbox destination-mailbox))
    (unless (chidu-store-mailbox-p mailbox)
      (signal 'wrong-type-argument (list 'chidu-store-mailbox-p mailbox))))
  (unless (and (vectorp local-email-ids) (> (length local-email-ids) 0))
    (user-error "No Email selected"))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let* ((operation-id (chidu-store-new-local-id))
         (workflow
          (chidu-mailbox-move--start-workflow
           app account operation-id success-function error-function nil
           progress-function))
         (runtime (chidu-app-runtime app)))
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-accept-mailbox-move-create
      :account-id (chidu-store-account-account-id account)
      :operation-id operation-id
      :source-mailbox-id
      (chidu-store-mailbox-mailbox-id source-mailbox)
      :destination-mailbox-id
      (chidu-store-mailbox-mailbox-id destination-mailbox)
      :local-email-ids local-email-ids)
     (lambda (result)
       (chidu-mailbox-move--after-accept workflow result)))
    (chidu-mailbox-move-workflow-runtime-operation workflow)))

(defun chidu-mailbox-move--resume (app context)
  "Resume durable Mailbox move CONTEXT in APP."
  (let* ((account (chidu-store-mailbox-move-context-account context))
         (operation-id
          (chidu-store-mailbox-move-context-operation-id context)))
    (when (and operation-id
               (> (length
                   (chidu-store-mailbox-move-context-intents context))
                  0))
      (condition-case nil
          (let ((workflow
                 (chidu-mailbox-move--start-workflow
                  app account operation-id #'ignore #'ignore t #'ignore)))
            (chidu-mailbox-move--dispatch-next workflow context))
        (user-error nil)))))

(defun chidu-retry-mailbox-moves (app account)
  "Retry ACCOUNT's unresolved durable Mailbox move in APP."
  (when (and (appkit-app-live-p app) (chidu-store-account-p account))
    (let ((runtime (chidu-app-runtime app)))
      (chidu-runtime--simple-store-operation
       runtime
       (chidu-store-op-get-mailbox-move-context-create
        :account-id (chidu-store-account-account-id account))
       (lambda (context)
         (chidu-mailbox-move--resume app context))
       (lambda (_failure) nil)))))

(provide 'chidu-mailbox-move)

;;; chidu-mailbox-move.el ends here
