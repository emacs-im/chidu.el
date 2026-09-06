;;; chidu-email-sync.el --- Canonical Email synchronization -*- lexical-binding: t; -*-

;;; Commentary:

;; Build or resume the canonical local Email index, then keep its active
;; generation current with the same bounded Email/changes reducer.  Initial
;; indexing remains explicit because it may span the entire account; live sync
;; never silently starts it.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-email)
(require 'chidu-jmap-email-changes)
(require 'chidu-jmap-email-catchup)
(require 'chidu-jmap-email-hydration)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)

(defconst chidu-email-metadata-profile-version "metadata-v1"
  "Required Email metadata profile used by the first readable generation.")

(defcustom chidu-email-query-page-size 4096
  "Maximum Email ids requested in one baseline Email/query page."
  :type 'positive-integer
  :group 'chidu)

(defcustom chidu-email-store-chunk-size 256
  "Maximum Email ids committed in one in-process Store transaction."
  :type 'positive-integer
  :group 'chidu)

(defcustom chidu-email-changes-page-size 500
  "Maximum changed Email ids requested in one canonical changes page."
  :type 'positive-integer
  :group 'chidu)

(defcustom chidu-email-hydration-page-size 256
  "Maximum Emails fetched in one homogeneous metadata hydration batch."
  :type 'positive-integer
  :group 'chidu)

(defcustom chidu-email-read-retries 3
  "Number of times to retry a transient canonical Email read failure."
  :type 'natnum
  :group 'chidu)

(defcustom chidu-email-retry-base-delay 0.25
  "Initial seconds before retrying a transient canonical Email read."
  :type 'number
  :group 'chidu)

(defcustom chidu-email-live-return-limit 20
  "Maximum newly active Email rows retained from one live synchronization."
  :type `(integer 1 ,chidu-store-active-email-row-limit)
  :group 'chidu)

(chidu-define-record chidu-email-live-result
    "Completed live canonical Email synchronization."
  context
  (new-emails (vector))
  truncated-p
  changed-p
  rebuilt-p)

(cl-defstruct (chidu-email-run
               (:constructor chidu-email-run-create))
  "Ephemeral control for one durable Email membership synchronization."
  mode
  runtime
  account-state
  generation
  operation
  page-limit
  context
  (new-local-email-ids nil)
  (new-emails (vector))
  truncated-p
  changed-p
  rebuilt-p
  secret
  active-cancel
  (effect-id 0)
  (retry-count 0)
  query-page
  (query-offset 0)
  success-function
  error-function
  completed-p
  canceled-p)

(defun chidu-email-run--current-p (run)
  "Return non-nil when RUN may still apply a result."
  (and (not (chidu-email-run-completed-p run))
       (not (chidu-email-run-canceled-p run))
       (chidu-runtime--account-current-p
        (chidu-email-run-runtime run)
        (chidu-email-run-account-state run)
        (chidu-email-run-generation run)
        (chidu-email-run-operation run))))

(defun chidu-email-run--clear-secret (run)
  "Clear RUN's current credential copy."
  (when-let* ((secret (chidu-email-run-secret run)))
    (clear-string secret)
    (setf (chidu-email-run-secret run) nil)))

(defun chidu-email-run--live-p (run)
  "Return non-nil when RUN performs incremental live synchronization."
  (eq 'live (chidu-email-run-mode run)))

(defun chidu-email-run--record-round (run round)
  "Record canonical Store ROUND in RUN and return its context."
  (unless (chidu-store-email-round-result-p round)
    (signal 'chidu-invariant-error
            (list "Email catch-up Store returned invalid result" round)))
  (let* ((ids
          (append
           (chidu-store-email-round-result-new-local-email-ids round)
           nil))
         (limit
          (min chidu-email-live-return-limit
               chidu-store-active-email-row-limit))
         (retained (chidu-email-run-new-local-email-ids run))
         truncated-p)
    (dolist (local-id ids)
      (unless (member local-id retained)
        (if (< (length retained) limit)
            (setq retained (nconc retained (list local-id)))
          (setq truncated-p t))))
    (setf (chidu-email-run-context run)
          (chidu-store-email-round-result-context round)
          (chidu-email-run-new-local-email-ids run) retained
          (chidu-email-run-truncated-p run)
          (or (chidu-email-run-truncated-p run) truncated-p)
          (chidu-email-run-changed-p run)
          (or (chidu-email-run-changed-p run)
              (chidu-store-email-round-result-changed-p round))))
  (chidu-email-run-context run))

(defun chidu-email-run--public-result (run result)
  "Return public RESULT shape for RUN."
  (if (and (chidu-email-run--live-p run)
           (chidu-result-ok-p result))
      (let ((context (chidu-result-ok-value result)))
        (unless (chidu-store-email-sync-context-p context)
          (signal 'chidu-invariant-error
                  (list "Live Email synchronization returned invalid context"
                        context)))
        (chidu-result-ok-create
         :value
         (chidu-email-live-result-create
          :context context
          :new-emails (copy-sequence (chidu-email-run-new-emails run))
          :truncated-p (and (chidu-email-run-truncated-p run) t)
          :changed-p (and (chidu-email-run-changed-p run) t)
          :rebuilt-p (and (chidu-email-run-rebuilt-p run) t))))
    result))

(defun chidu-email-run--after-active-email-rows
    (run context result)
  "Finish live RUN after targeted active-row RESULT for CONTEXT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let ((rows (chidu-result-ok-value result)))
      (unless (and (vectorp rows)
                   (cl-loop for row across rows
                            always (chidu-store-new-email-row-p row)))
        (signal 'chidu-invariant-error
                (list "Active Email read returned invalid rows" rows)))
      (setf (chidu-email-run-new-emails run) rows)
      (chidu-email-run--finish
       run (chidu-result-ok-create :value context))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--finish-live (run context)
  "Finish live RUN using current canonical rows below CONTEXT."
  (let ((ids (vconcat (chidu-email-run-new-local-email-ids run))))
    (if (zerop (length ids))
        (progn
          (setf (chidu-email-run-new-emails run) (vector))
          (chidu-email-run--finish
           run (chidu-result-ok-create :value context)))
      (setf (chidu-account-runtime-phase
             (chidu-email-run-account-state run))
            'email-reading-new)
      (chidu-runtime--store-call
       (chidu-email-run-runtime run)
       (chidu-store-op-get-active-email-rows-create
        :account-id
        (chidu-store-account-account-id
         (chidu-store-email-sync-context-account context))
        :local-email-ids ids)
       (lambda (result)
         (when (chidu-email-run--current-p run)
           (chidu-email-run--after-active-email-rows
            run context result)))))))

(defun chidu-email-run--finish (run result)
  "Finish current RUN with typed RESULT."
  (when (chidu-email-run--current-p run)
    (setf (chidu-email-run-completed-p run) t
          (chidu-email-run-active-cancel run) nil)
    (chidu-email-run--clear-secret run)
    (chidu-runtime--finish-account-sync
     (chidu-email-run-runtime run)
     (chidu-email-run-account-state run)
     (chidu-email-run-generation run)
     (chidu-email-run-operation run)
     (chidu-email-run--public-result run result)
     (chidu-email-run-success-function run)
     (chidu-email-run-error-function run))))

(defun chidu-email-run--cancel (run)
  "Cancel RUN and clear its ephemeral resources."
  (unless (chidu-email-run-completed-p run)
    (setf (chidu-email-run-canceled-p run) t
          (chidu-email-run-completed-p run) t)
    (when-let* ((cancel (chidu-email-run-active-cancel run)))
      (setf (chidu-email-run-active-cancel run) nil)
      (condition-case-unless-debug _error
          (funcall cancel)
        (error nil)))
    (chidu-email-run--clear-secret run)))

(defun chidu-email-run--failure (kind &optional data retryable-p)
  "Return typed synchronization failure KIND with DATA and RETRYABLE-P."
  (chidu-result-failure-create
   :kind kind :data data :retryable-p retryable-p))

(defun chidu-email-run--retryable-read-p (result)
  "Return non-nil when RESULT is safe to retry as a read."
  (and (chidu-result-failure-p result)
       (chidu-result-failure-retryable-p result)
       (memq (chidu-result-failure-kind result)
             '(network-error transport-unavailable))))

(defun chidu-email-run--schedule
    (run phase delay continuation)
  "Schedule RUN CONTINUATION after DELAY seconds in PHASE."
  (let ((effect-id (cl-incf (chidu-email-run-effect-id run)))
        timer)
    (setf (chidu-account-runtime-phase
           (chidu-email-run-account-state run))
          phase)
    (setq
     timer
     (run-at-time
      delay nil
      (lambda ()
        (when (and (chidu-email-run--current-p run)
                   (= effect-id
                      (chidu-email-run-effect-id run)))
          (setf (chidu-email-run-active-cancel run) nil)
          (funcall continuation)))))
    (let ((cancel
           (lambda ()
             (when (timerp timer)
               (cancel-timer timer)))))
      (setf (chidu-email-run-active-cancel run) cancel
            (chidu-account-runtime-cancel-function
             (chidu-email-run-account-state run))
            cancel))))

(defun chidu-email-run--retry (run result continuation)
  "Retry RUN read CONTINUATION after transient RESULT, or finish."
  (let ((attempt (1+ (chidu-email-run-retry-count run))))
    (if (> attempt chidu-email-read-retries)
        (chidu-email-run--finish run result)
      (setf (chidu-email-run-retry-count run) attempt)
      (chidu-email-run--schedule
       run 'email-retry-wait
       (* chidu-email-retry-base-delay (expt 2 (1- attempt)))
       continuation))))

(defun chidu-email-run--ensure-secret (run)
  "Ensure RUN owns one mutable credential copy."
  (or (chidu-email-run-secret run)
      (condition-case error-data
          (let* ((context (chidu-email-run-context run))
                 (endpoint
                  (chidu-store-email-sync-context-endpoint context))
                 (secret
                  (chidu-runtime--endpoint-secret endpoint)))
            (setf (chidu-email-run-secret run) secret)
            secret)
        (error
         (chidu-email-run--finish
          run
          (chidu-runtime--condition-failure
           'credential-error error-data nil))
         nil))))

(defun chidu-email-run--start-effect
    (run phase starter continuation)
  "For RUN, run STARTER in PHASE and pass its result to CONTINUATION."
  (when (chidu-email-run--current-p run)
    (let ((effect-id (cl-incf (chidu-email-run-effect-id run)))
          cancel)
      (setf (chidu-account-runtime-phase
             (chidu-email-run-account-state run))
            phase
            (chidu-email-run-active-cancel run) nil)
      (condition-case error-data
          (setq
           cancel
           (funcall
            starter
            (lambda (result)
              (when (and (chidu-email-run--current-p run)
                         (= effect-id
                            (chidu-email-run-effect-id run)))
                (setf (chidu-email-run-active-cancel run) nil)
                (funcall continuation result)))))
        (error
         (chidu-email-run--finish
          run
          (chidu-runtime--condition-failure
           'jmap-request-failed error-data nil))))
      ;; A synchronous fake or startup failure may already have advanced the
      ;; workflow.  Never overwrite the next effect's cancellation handle.
      (when (and cancel
                 (chidu-email-run--current-p run)
                 (= effect-id (chidu-email-run-effect-id run)))
        (setf (chidu-email-run-active-cancel run) cancel
              (chidu-account-runtime-cancel-function
               (chidu-email-run-account-state run))
              cancel)))))

(defun chidu-email-run--start-query (run)
  "Fetch the next Email/query page for RUN."
  (when-let* ((secret (chidu-email-run--ensure-secret run)))
    (let ((context (chidu-email-run-context run))
          (limit (chidu-email-run-page-limit run)))
      (chidu-email-run--start-effect
       run 'email-enumerating
       (lambda (deliver)
         (chidu-jmap-email-fetch-query-page
          context secret limit deliver))
       (lambda (result)
         (chidu-email-run--after-query run result))))))

(defun chidu-email-run--after-membership-commit (run result)
  "Continue RUN after committing one membership changes RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let ((context (chidu-result-ok-value result)))
      (unless (chidu-store-email-sync-context-p context)
        (signal 'chidu-invariant-error
                (list "Email changes Store returned invalid context" context)))
      (setf (chidu-email-run-context run) context)
      (pcase (chidu-store-email-sync-context-phase context)
        ('membership-catchup
         (chidu-email-run--start-membership-changes run))
        ('hydrating
         (chidu-email-run--start-hydration-plan run))
        ('metadata-catchup
         (chidu-email-run--start-catchup-changes run))
        (phase
         (chidu-email-run--finish
          run
          (chidu-email-run--failure
           'email-bootstrap-phase-conflict
           (list :phase phase) t))))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--after-membership-changes (run result)
  "Commit one canonical Email/changes RESULT for RUN."
  (cond
   ((chidu-email-run--retryable-read-p result)
    (chidu-email-run--retry
     run result
     (lambda ()
       (chidu-email-run--start-membership-changes run))))
   ((and (chidu-result-failure-p result)
         (eq 'cannot-calculate-changes
             (chidu-result-failure-kind result)))
    ;; The old baseline can no longer be proven.  Observe a fresh Email state
    ;; and replace the entire building generation; the previous generation has
    ;; never been visible to UI.
    (setf (chidu-email-run-retry-count run) 0)
    (chidu-email-run--read-state run t))
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (setf (chidu-email-run-retry-count run) 0)
    (let* ((page (chidu-result-ok-value result))
           (context (chidu-email-run-context run))
           (runtime (chidu-email-run-runtime run))
           (account-id
            (chidu-store-account-account-id
             (chidu-store-email-sync-context-account context))))
      (unless (chidu-jmap-email-changes-page-p page)
        (signal 'chidu-invariant-error
                (list "Email/changes adapter returned invalid page" page)))
      (setf (chidu-account-runtime-phase
             (chidu-email-run-account-state run))
            'email-committing-membership-changes)
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-apply-email-membership-changes-create
        :account-id account-id
        :generation-id
        (chidu-store-email-sync-context-generation-id context)
        :expected-revision
        (chidu-store-email-sync-context-revision context)
        :expected-state (chidu-store-email-sync-context-state context)
        :observation
        (chidu-store-email-changes-observation-create
         :old-state (chidu-jmap-email-changes-page-old-state page)
         :new-state (chidu-jmap-email-changes-page-new-state page)
         :has-more-changes-p
         (chidu-jmap-email-changes-page-has-more-changes-p page)
         :created (chidu-jmap-email-changes-page-created page)
         :updated (chidu-jmap-email-changes-page-updated page)
         :destroyed (chidu-jmap-email-changes-page-destroyed page)))
       (lambda (store-result)
         (when (chidu-email-run--current-p run)
           (chidu-email-run--after-membership-commit
            run store-result))))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--start-membership-changes (run)
  "Fetch the next canonical membership changes page for RUN."
  (when-let* ((secret (chidu-email-run--ensure-secret run)))
    (let ((context (chidu-email-run-context run)))
      (chidu-email-run--start-effect
       run 'email-membership-catchup
       (lambda (deliver)
         (chidu-jmap-email-fetch-changes-page
          context secret chidu-email-changes-page-size deliver))
       (lambda (result)
         (chidu-email-run--after-membership-changes
          run result))))))

(defun chidu-email-run--hydration-limit (run)
  "Return bounded metadata hydration batch size for RUN."
  (let* ((context (chidu-email-run-context run))
         (endpoint (chidu-store-email-sync-context-endpoint context))
         (server-limit (chidu-store-endpoint-max-objects-in-get endpoint)))
    (if server-limit
        (min chidu-email-hydration-page-size server-limit)
      chidu-email-hydration-page-size)))

(defun chidu-email-run--catchup-limit (run)
  "Return a conservative changed-id limit for RUN catch-up."
  (let* ((endpoint
          (chidu-store-email-sync-context-endpoint
           (chidu-email-run-context run)))
         (object-limit
          (or (chidu-store-endpoint-max-objects-in-get endpoint)
              chidu-email-changes-page-size))
         (request-limit
          (or (chidu-store-endpoint-max-size-request endpoint)
              (* chidu-email-changes-page-size 300)))
         ;; JMAP Ids are at most 255 base64url octets.  Reserve fixed envelope
         ;; space and budget 300 bytes per id in the later two-get request.
         (byte-limit (max 1 (/ (max 300 (- request-limit 1024)) 300))))
    (min chidu-email-changes-page-size object-limit byte-limit)))

(defun chidu-email-run--changes-observation (page)
  "Return closed Store changes observation for JMAP PAGE."
  (chidu-store-email-changes-observation-create
   :old-state (chidu-jmap-email-changes-page-old-state page)
   :new-state (chidu-jmap-email-changes-page-new-state page)
   :has-more-changes-p
   (chidu-jmap-email-changes-page-has-more-changes-p page)
   :created (chidu-jmap-email-changes-page-created page)
   :updated (chidu-jmap-email-changes-page-updated page)
   :destroyed (chidu-jmap-email-changes-page-destroyed page)))

(defun chidu-email-run--after-activation (run result)
  "Finish or reject RUN after activation Store RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let ((context (chidu-result-ok-value result)))
      (unless (and (chidu-store-email-sync-context-p context)
                   (eq 'live
                       (chidu-store-email-sync-context-phase context)))
        (signal 'chidu-invariant-error
                (list "Email activation returned invalid context" context)))
      (setf (chidu-email-run-context run) context)
      (if (chidu-email-run--live-p run)
          (chidu-email-run--finish-live run context)
        (chidu-email-run--finish run result))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--activate (run)
  "Atomically publish RUN's state-closed building generation."
  (let* ((context (chidu-email-run-context run))
         (runtime (chidu-email-run-runtime run))
         (account-id
          (chidu-store-account-account-id
           (chidu-store-email-sync-context-account context))))
    (setf (chidu-account-runtime-phase
           (chidu-email-run-account-state run))
          'email-activating)
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-activate-email-generation-create
      :account-id account-id
      :generation-id (chidu-store-email-sync-context-generation-id context)
      :expected-revision (chidu-store-email-sync-context-revision context)
      :expected-state (chidu-store-email-sync-context-state context))
     (lambda (result)
       (when (chidu-email-run--current-p run)
         (chidu-email-run--after-activation run result))))))

(defun chidu-email-run--after-catchup-store (run result)
  "Continue RUN after one catch-up Store RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let* ((round (chidu-result-ok-value result))
           (context (chidu-email-run--record-round run round)))
      (unless (chidu-store-email-sync-context-p context)
        (signal 'chidu-invariant-error
                (list "Email round contains invalid context" context)))
      (pcase (chidu-store-email-sync-context-phase context)
        ('metadata-catchup
         (chidu-email-run--start-catchup-changes run))
        ('activating
         (chidu-email-run--activate run))
        ('live
         (if (chidu-store-email-round-result-closed-p round)
             (if (chidu-email-run--live-p run)
                 (chidu-email-run--finish-live run context)
               (chidu-email-run--finish
                run (chidu-result-ok-create :value context)))
           (chidu-email-run--start-catchup-changes run)))
        (phase
         (chidu-email-run--finish
          run
          (chidu-email-run--failure
           'email-bootstrap-phase-conflict (list :phase phase) t))))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--commit-catchup
    (run page hydration)
  "Commit PAGE and optional profile HYDRATION for RUN."
  (let* ((context (chidu-email-run-context run))
         (runtime (chidu-email-run-runtime run))
         (account-id
          (chidu-store-account-account-id
           (chidu-store-email-sync-context-account context))))
    (setf (chidu-account-runtime-phase
           (chidu-email-run-account-state run))
          'email-committing-catchup)
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-apply-email-catchup-round-create
      :account-id account-id
      :generation-id (chidu-store-email-sync-context-generation-id context)
      :expected-revision (chidu-store-email-sync-context-revision context)
      :expected-state (chidu-store-email-sync-context-state context)
      :observation
      (chidu-store-email-catchup-observation-create
       :changes (chidu-email-run--changes-observation page)
       :full
       (and hydration
            (chidu-jmap-email-catchup-hydration-full hydration))
       :mutable
       (and hydration
            (chidu-jmap-email-catchup-hydration-mutable hydration))))
     (lambda (result)
       (when (chidu-email-run--current-p run)
         (chidu-email-run--after-catchup-store run result))))))

(defun chidu-email-run--after-catchup-hydration
    (run page result)
  "Commit PAGE after catch-up hydration RESULT for RUN."
  (cond
   ((chidu-email-run--retryable-read-p result)
    (chidu-email-run--retry
     run result
     (lambda ()
       (chidu-email-run--start-catchup-hydration run page))))
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (setf (chidu-email-run-retry-count run) 0)
    (let ((hydration (chidu-result-ok-value result)))
      (unless (chidu-jmap-email-catchup-hydration-p hydration)
        (signal 'chidu-invariant-error
                (list "Email catch-up adapter returned invalid hydration"
                      hydration)))
      (chidu-email-run--commit-catchup
       run page hydration)))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--start-catchup-hydration (run page)
  "Fetch PAGE's created/full and updated/mutable targets for RUN."
  (when-let* ((secret (chidu-email-run--ensure-secret run)))
    (let ((context (chidu-email-run-context run))
          (created (chidu-jmap-email-changes-page-created page))
          (updated (chidu-jmap-email-changes-page-updated page)))
      (chidu-email-run--start-effect
       run 'email-catching-up-metadata
       (lambda (deliver)
         (chidu-jmap-email-fetch-catchup-hydration
          context secret created updated deliver))
       (lambda (result)
         (chidu-email-run--after-catchup-hydration
          run page result))))))

(defun chidu-email-run--live-noop-page-p (run page)
  "Return non-nil when PAGE proves RUN is already at current Email state."
  (and (chidu-email-run--live-p run)
       (not (chidu-jmap-email-changes-page-has-more-changes-p page))
       (equal (chidu-jmap-email-changes-page-old-state page)
              (chidu-jmap-email-changes-page-new-state page))
       (zerop (length (chidu-jmap-email-changes-page-created page)))
       (zerop (length (chidu-jmap-email-changes-page-updated page)))
       (zerop (length (chidu-jmap-email-changes-page-destroyed page)))))

(defun chidu-email-run--after-catchup-changes (run result)
  "For RUN, hydrate and commit one canonical Email changes RESULT."
  (cond
   ((chidu-email-run--retryable-read-p result)
    (chidu-email-run--retry
     run result
     (lambda ()
       (chidu-email-run--start-catchup-changes run))))
   ((and (chidu-result-failure-p result)
         (eq 'cannot-calculate-changes
             (chidu-result-failure-kind result)))
    ;; The old active generation remains visible while a replacement baseline
    ;; is built.  An unpublished generation is simply replaced.
    (setf (chidu-email-run-retry-count run) 0
          (chidu-email-run-new-local-email-ids run) nil
          (chidu-email-run-new-emails run) (vector)
          (chidu-email-run-truncated-p run) nil
          (chidu-email-run-rebuilt-p run) t)
    (chidu-email-run--read-state run t))
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (setf (chidu-email-run-retry-count run) 0)
    (let ((page (chidu-result-ok-value result)))
      (unless (chidu-jmap-email-changes-page-p page)
        (signal 'chidu-invariant-error
                (list "Email/changes adapter returned invalid catch-up page"
                      page)))
      (cond
       ((chidu-email-run--live-noop-page-p run page)
        (chidu-email-run--finish-live
         run (chidu-email-run-context run)))
       ((zerop
         (+ (length (chidu-jmap-email-changes-page-created page))
            (length (chidu-jmap-email-changes-page-updated page))))
        (chidu-email-run--commit-catchup run page nil))
       (t
        (chidu-email-run--start-catchup-hydration run page)))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--start-catchup-changes (run)
  "Fetch the next state-bounded metadata catch-up round for RUN."
  (when-let* ((secret (chidu-email-run--ensure-secret run)))
    (let ((context (chidu-email-run-context run))
          (limit (chidu-email-run--catchup-limit run)))
      (chidu-email-run--start-effect
       run 'email-catching-up
       (lambda (deliver)
         (chidu-jmap-email-fetch-changes-page
          context secret limit deliver))
       (lambda (result)
         (chidu-email-run--after-catchup-changes
          run result))))))

(defun chidu-email-run--after-hydration-store (run result)
  "Continue RUN after a hydration Store RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let ((context (chidu-result-ok-value result)))
      (unless (chidu-store-email-sync-context-p context)
        (signal 'chidu-invariant-error
                (list "Email hydration Store returned invalid context" context)))
      (setf (chidu-email-run-context run) context)
      (pcase (chidu-store-email-sync-context-phase context)
        ('hydrating
         (chidu-email-run--start-hydration-plan run))
        ('metadata-catchup
         (chidu-email-run--start-catchup-changes run))
        (phase
         (chidu-email-run--finish
          run
          (chidu-email-run--failure
           'email-bootstrap-phase-conflict
           (list :phase phase) t))))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--after-hydration-fetch
    (run plan result)
  "Commit hydration RESULT for exact PLAN in RUN."
  (cond
   ((chidu-email-run--retryable-read-p result)
    (chidu-email-run--retry
     run result
     (lambda ()
       (chidu-email-run--start-hydration-fetch run plan))))
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (setf (chidu-email-run-retry-count run) 0)
    (let* ((context (chidu-email-run-context run))
           (runtime (chidu-email-run-runtime run))
           (account-id
            (chidu-store-account-account-id
             (chidu-store-email-sync-context-account context))))
      (setf (chidu-account-runtime-phase
             (chidu-email-run-account-state run))
            'email-committing-hydration)
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-apply-email-hydration-create
        :account-id account-id
        :generation-id
        (chidu-store-email-sync-context-generation-id context)
        :expected-revision
        (chidu-store-email-sync-context-revision context)
        :observation (chidu-result-ok-value result))
       (lambda (store-result)
         (when (chidu-email-run--current-p run)
           (chidu-email-run--after-hydration-store
            run store-result))))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--start-hydration-fetch (run plan)
  "Fetch homogeneous metadata PLAN for RUN."
  (when-let* ((secret (chidu-email-run--ensure-secret run)))
    (let ((context (chidu-email-run-context run)))
      (chidu-email-run--start-effect
       run 'email-hydrating
       (lambda (deliver)
         (chidu-jmap-email-fetch-hydration
          context secret plan deliver))
       (lambda (result)
         (chidu-email-run--after-hydration-fetch
          run plan result))))))

(defun chidu-email-run--finish-hydration (run)
  "Close exhausted metadata hydration for RUN."
  (let* ((context (chidu-email-run-context run))
         (runtime (chidu-email-run-runtime run))
         (account-id
          (chidu-store-account-account-id
           (chidu-store-email-sync-context-account context))))
    (setf (chidu-account-runtime-phase
           (chidu-email-run-account-state run))
          'email-finishing-hydration)
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-finish-email-hydration-create
      :account-id account-id
      :generation-id (chidu-store-email-sync-context-generation-id context)
      :expected-revision (chidu-store-email-sync-context-revision context))
     (lambda (result)
       (when (chidu-email-run--current-p run)
         (chidu-email-run--after-hydration-store
          run result))))))

(defun chidu-email-run--after-hydration-plan (run result)
  "Continue RUN after hydration plan Store RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let ((plan (chidu-result-ok-value result)))
      (unless (chidu-store-email-hydration-plan-p plan)
        (signal 'chidu-invariant-error
                (list "Email Store returned invalid hydration plan" plan)))
      (if (zerop
           (length (chidu-store-email-hydration-plan-targets plan)))
          (chidu-email-run--finish-hydration run)
        (chidu-email-run--start-hydration-fetch run plan))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--start-hydration-plan (run)
  "Read the next durable metadata hydration plan for RUN."
  (let* ((context (chidu-email-run-context run))
         (runtime (chidu-email-run-runtime run))
         (account-id
          (chidu-store-account-account-id
           (chidu-store-email-sync-context-account context))))
    (setf (chidu-account-runtime-phase
           (chidu-email-run-account-state run))
          'email-planning-hydration)
    (chidu-runtime--store-call
     runtime
     (chidu-store-op-get-email-hydration-plan-create
      :account-id account-id
      :limit (chidu-email-run--hydration-limit run))
     (lambda (result)
       (when (chidu-email-run--current-p run)
         (chidu-email-run--after-hydration-plan
          run result))))))

(defun chidu-email-run--after-install (run result)
  "Continue RUN after begin or restart Store RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let ((context (chidu-result-ok-value result)))
      (unless (chidu-store-email-sync-context-p context)
        (signal 'chidu-invariant-error
                (list "Email synchronization Store returned invalid context" context)))
      (setf (chidu-email-run-context run) context)
      (chidu-email-run--start-query run)))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--after-state (run restart-p result)
  "Commit Email state RESULT for RUN; replace generation when RESTART-P."
  (cond
   ((chidu-email-run--retryable-read-p result)
    (chidu-email-run--retry
     run result
     (lambda ()
       (chidu-email-run--read-state run restart-p))))
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (setf (chidu-email-run-retry-count run) 0)
    (let* ((runtime (chidu-email-run-runtime run))
           (context (chidu-email-run-context run))
           (account-id
            (chidu-store-account-account-id
             (chidu-store-email-sync-context-account context)))
           (state-token (chidu-result-ok-value result))
           (operation
            (if restart-p
                (chidu-store-op-restart-email-bootstrap-create
                 :account-id account-id
                 :generation-id
                 (chidu-store-email-sync-context-generation-id context)
                 :expected-revision
                 (chidu-store-email-sync-context-revision context)
                 :state state-token
                 :profile-version chidu-email-metadata-profile-version)
              (chidu-store-op-begin-email-bootstrap-create
               :account-id account-id
               :expected-revision
               (chidu-store-email-sync-context-revision context)
               :state state-token
               :profile-version chidu-email-metadata-profile-version))))
      (setf (chidu-account-runtime-phase
             (chidu-email-run-account-state run))
            'email-committing-baseline)
      (chidu-runtime--store-call
       runtime operation
       (lambda (store-result)
         (when (chidu-email-run--current-p run)
           (chidu-email-run--after-install
            run store-result))))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--read-state (run restart-p)
  "Read Email object state for RUN; replace generation when RESTART-P."
  (when-let* ((secret (chidu-email-run--ensure-secret run)))
    (let ((context (chidu-email-run-context run)))
      (chidu-email-run--start-effect
       run 'email-reading-state
       (lambda (deliver)
         (chidu-jmap-email-fetch-state context secret deliver))
       (lambda (result)
         (chidu-email-run--after-state
          run restart-p result))))))

(defun chidu-email-run--after-query-chunk
    (run next-offset result)
  "Continue RUN after a query chunk Store RESULT.

NEXT-OFFSET is the number of ids consumed from the current wire page."
  (cond
   ((and (chidu-result-failure-p result)
         (eq 'query-state-changed
             (chidu-result-failure-kind result)))
    (setf (chidu-email-run-query-page run) nil
          (chidu-email-run-query-offset run) 0)
    (chidu-email-run--read-state run t))
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let* ((context (chidu-result-ok-value result))
           (page (chidu-email-run-query-page run))
           (ids
            (and page
                 (chidu-store-email-query-page-observation-remote-email-ids
                  page))))
      (unless (chidu-store-email-sync-context-p context)
        (signal 'chidu-invariant-error
                (list "Email query commit returned invalid context" context)))
      (setf (chidu-email-run-context run) context
            (chidu-email-run-query-offset run) next-offset)
      (if (and ids (< next-offset (length ids)))
          (chidu-email-run--schedule
           run 'email-committing-page 0
           (lambda ()
             (chidu-email-run--commit-query-chunk run)))
        (setf (chidu-email-run-query-page run) nil
              (chidu-email-run-query-offset run) 0)
        (pcase (chidu-store-email-sync-context-phase context)
          ('enumerating (chidu-email-run--start-query run))
          ('membership-catchup
           (chidu-email-run--start-membership-changes run))
          (phase
           (chidu-email-run--finish
            run
            (chidu-email-run--failure
             'email-bootstrap-phase-conflict
             (list :phase phase) t)))))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--commit-query-chunk (run)
  "Commit the next bounded prefix chunk from RUN's wire query page."
  (when (chidu-email-run--current-p run)
    (let* ((runtime (chidu-email-run-runtime run))
           (context (chidu-email-run-context run))
           (page (chidu-email-run-query-page run))
           (ids
            (chidu-store-email-query-page-observation-remote-email-ids page))
           (offset (chidu-email-run-query-offset run))
           (end
            (if (zerop (length ids))
                0
              (min (length ids)
                   (+ offset chidu-email-store-chunk-size))))
           (chunk-ids
            (if (zerop (length ids))
                (vector)
              (cl-subseq ids offset end)))
           (chunk
            (chidu-store-email-query-page-observation-create
             :query-state
             (chidu-store-email-query-page-observation-query-state page)
             :can-calculate-changes-p
             (chidu-store-email-query-page-observation-can-calculate-changes-p
              page)
             :position
             (+ (chidu-store-email-query-page-observation-position page)
                offset)
             :remote-email-ids chunk-ids))
           (account-id
            (chidu-store-account-account-id
             (chidu-store-email-sync-context-account context))))
      (setf (chidu-account-runtime-phase
             (chidu-email-run-account-state run))
            'email-committing-page)
      (chidu-runtime--store-call
       runtime
       (chidu-store-op-append-email-query-chunk-create
        :account-id account-id
        :generation-id
        (chidu-store-email-sync-context-generation-id context)
        :expected-revision
        (chidu-store-email-sync-context-revision context)
        :observation chunk)
       (lambda (store-result)
         (when (chidu-email-run--current-p run)
           (chidu-email-run--after-query-chunk
            run end store-result)))))))

(defun chidu-email-run--after-query (run result)
  "Commit one validated Email/query RESULT for RUN."
  (cond
   ((chidu-email-run--retryable-read-p result)
    (chidu-email-run--retry
     run result
     (lambda () (chidu-email-run--start-query run))))
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (setf (chidu-email-run-retry-count run) 0
          (chidu-email-run-query-page run)
          (chidu-result-ok-value result)
          (chidu-email-run-query-offset run) 0)
    (chidu-email-run--commit-query-chunk run))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--after-load (run result)
  "Resume RUN from durable Store load RESULT."
  (cond
   ((chidu-result-failure-p result)
    (chidu-email-run--finish run result))
   ((chidu-result-ok-p result)
    (let* ((context (chidu-result-ok-value result))
           (phase
            (and (chidu-store-email-sync-context-p context)
                 (chidu-store-email-sync-context-phase context))))
      (unless phase
        (signal 'chidu-invariant-error
                (list "Email sync load returned invalid context" context)))
      (setf (chidu-email-run-context run) context)
      (when (and (chidu-email-run--live-p run)
                 (memq phase
                       '(enumerating membership-catchup hydrating
                         metadata-catchup activating)))
        (setf (chidu-email-run-rebuilt-p run) t))
      (pcase phase
        ('uninitialized
         (if (chidu-email-run--live-p run)
             (chidu-email-run--finish
              run
              (chidu-email-run--failure
               'email-index-unavailable
               (list :account-id
                     (chidu-store-account-account-id
                      (chidu-store-email-sync-context-account context)))
               nil))
           (chidu-email-run--read-state run nil)))
        ('enumerating
         (if (equal chidu-email-metadata-profile-version
                    (chidu-store-email-sync-context-profile-version context))
             (chidu-email-run--start-query run)
           (chidu-email-run--finish
            run
            (chidu-email-run--failure
             'email-profile-mismatch
             (list :expected chidu-email-metadata-profile-version
                   :actual
                   (chidu-store-email-sync-context-profile-version context))
             nil))))
        ('membership-catchup
         (chidu-email-run--start-membership-changes run))
        ('hydrating
         (chidu-email-run--start-hydration-plan run))
        ('metadata-catchup
         (chidu-email-run--start-catchup-changes run))
        ('activating
         (chidu-email-run--activate run))
        ('live
         (if (chidu-email-run--live-p run)
             (chidu-email-run--start-catchup-changes run)
           (chidu-email-run--finish run result)))
        (_
         (chidu-email-run--finish
          run
          (chidu-email-run--failure
           'email-bootstrap-phase-conflict
           (list :phase phase) t))))))
   (t
    (chidu-email-run--finish
     run
     (chidu-email-run--failure
      'invalid-result (list :value result) nil)))))

(defun chidu-email-run--start
    (mode runtime account success-function error-function page-limit)
  "Start MODE Email synchronization for ACCOUNT in RUNTIME.

MODE is `index' or `live'.  PAGE-LIMIT bounds baseline enumeration if a full
index is needed.  Call SUCCESS-FUNCTION or ERROR-FUNCTION exactly once."
  (chidu-runtime--assert-open runtime)
  (unless (memq mode '(index live))
    (signal 'wrong-type-argument (list '(member index live) mode)))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (unless (and (integerp page-limit) (> page-limit 0))
    (signal 'wrong-type-argument (list 'positive-integer-p page-limit)))
  (let* ((account-id (chidu-store-account-account-id account))
         (state (chidu-runtime--account-state runtime account-id)))
    (if (not (eq 'idle (chidu-account-runtime-phase state)))
        (progn
          (funcall
           error-function
           (chidu-email-run--failure
            'account-busy
            (list :account-id account-id
                  :phase (chidu-account-runtime-phase state))
            t))
          nil)
      (let* ((operation (chidu-runtime--begin-operation runtime))
             (generation (cl-incf (chidu-account-runtime-generation state)))
             (run
              (chidu-email-run-create
               :mode mode
               :runtime runtime
               :account-state state
               :generation generation
               :operation operation
               :page-limit page-limit
               :success-function success-function
               :error-function error-function)))
        (setf (chidu-account-runtime-phase state) 'email-loading
              (chidu-account-runtime-operation-id state)
              (chidu-runtime-operation-id operation)
              (chidu-runtime-operation-cancel-cleanup-function operation)
              (lambda ()
                (when (eql (chidu-account-runtime-operation-id state)
                           (chidu-runtime-operation-id operation))
                  (setf (chidu-account-runtime-phase state) 'idle
                        (chidu-account-runtime-operation-id state) nil
                        (chidu-account-runtime-cancel-function state) nil))))
        (chidu-runtime--set-operation-cancel
         runtime operation
         (lambda () (chidu-email-run--cancel run)))
        (chidu-runtime--store-call
         runtime
         (chidu-store-op-get-email-sync-context-create
          :account-id account-id)
         (lambda (store-result)
           (when (chidu-email-run--current-p run)
             (chidu-email-run--after-load run store-result))))
        operation))))

(defun chidu-email-index-account
    (runtime account success-function error-function &optional page-limit)
  "Build or resume ACCOUNT's canonical local Email index in RUNTIME.

Call SUCCESS-FUNCTION after query enumeration, hydration, state-matched
catch-up, and atomic generation activation reach the durable `live' phase.
PAGE-LIMIT defaults to `chidu-email-query-page-size'."
  (chidu-email-run--start
   'index runtime account success-function error-function
   (or page-limit chidu-email-query-page-size)))

(defun chidu-email-sync-live
    (runtime account success-function error-function)
  "Reconcile ACCOUNT's active Email generation in RUNTIME.

The operation follows bounded `Email/changes' pages, hydrates created and
updated objects, and returns a `chidu-email-live-result'.  It never starts a
full index implicitly.  A server-side changes gap rebuilds a replacement
building generation while the previous active generation remains readable."
  (chidu-email-run--start
   'live runtime account success-function error-function
   chidu-email-query-page-size))

(provide 'chidu-email-sync)

;;; chidu-email-sync.el ends here
