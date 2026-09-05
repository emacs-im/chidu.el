;;; chidu-store-sqlite-trash.el --- SQLite move-to-Trash operations -*- lexical-binding: t; -*-

;;; Commentary:

;; Durable Trash acceptance, authoritative membership evidence,
;; per-target settlement, and projection invalidation.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)
(require 'chidu-store-sqlite-generation)
(require 'chidu-store-sqlite-mutation-support)

(defun chidu-store-sqlite--trash-intents
    (database account-id operation-id)
  "Return DATABASE Trash intents for OPERATION-ID below ACCOUNT-ID."
  (vconcat
   (chidu-sql-map database
       [:select
        [[local-email-id local-email-id]
         [remote-email-id remote-email-id]
         [mailboxes-json original-mailbox-ids-json]
         [phase phase]
         [error-kind error-kind]]
        :from jmap-trash-target
        :where [:and
                [:= account-id [:bind account-id]]
                [:= operation-id [:bind operation-id]]]
        :order-by [[accepted-change-seq :asc] [local-email-id :asc]]]
     (chidu-store-trash-intent-create
      :local-email-id local-email-id
      :remote-email-id remote-email-id
      :original-remote-mailbox-ids
      (and mailboxes-json
           (chidu-store-sqlite--string-vector-from-json
            mailboxes-json "Trash original Mailbox ids"))
      :phase (intern phase)
      :error-kind error-kind))))

(defun chidu-store-sqlite--trash-context (state account-id)
  "Return ACCOUNT-ID's durable Trash context from SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     (t
      (chidu-result-ok-create
       :value
       (or
        (chidu-sql-one database
            [:select
             [[operation-id operation-id]
              [trash-mailbox-id trash-mailbox-id]]
             :from jmap-trash-operation
             :where [:= account-id [:bind account-id]]]
          (let ((trash-mailbox
                 (chidu-store-sqlite--mailbox-by-id
                  database account-id trash-mailbox-id)))
            (unless (and trash-mailbox
                         (equal "trash"
                                (chidu-store-mailbox-role trash-mailbox)))
              (signal
               'chidu-invariant-error
               '("Trash operation references an invalid Trash Mailbox")))
            (chidu-store-trash-context-create
             :endpoint (car location)
             :account (cdr location)
             :operation-id operation-id
             :trash-mailbox trash-mailbox
             :intents
             (chidu-store-sqlite--trash-intents
              database account-id operation-id))))
        (chidu-store-trash-context-create
         :endpoint (car location)
         :account (cdr location))))))))

(defun chidu-store-sqlite--trash-change
    (local-email-id phase &optional error-kind)
  "Return LOCAL-EMAIL-ID Trash change for PHASE and optional ERROR-KIND."
  (chidu-store-trash-target-change-create
   :local-email-id local-email-id :phase phase :error-kind error-kind))

(defun chidu-store-sqlite--trash-result (context changes)
  "Return successful Trash result for CONTEXT and CHANGES."
  (chidu-result-ok-create
   :value
   (chidu-store-trash-result-create
    :context context :changes (vconcat changes))))

(defun chidu-store-sqlite--resolve-trash-targets
    (database account-id local-email-ids)
  "Resolve LOCAL-EMAIL-IDS below ACCOUNT-ID in DATABASE.

Return a typed result whose value is a list of `(local-id . remote-id)' pairs."
  (let (targets missing-local-id)
    (cl-loop
     for local-email-id across local-email-ids
     for remote-email-id =
     (caar
      (chidu-sql-select database
        [:select [remote-email-id]
                 :from jmap-email-record
                 :where [:and
                         [:= account-id [:bind account-id]]
                         [:= local-email-id [:bind local-email-id]]]]))
     do
     (if remote-email-id
         (push (cons local-email-id remote-email-id) targets)
       (unless missing-local-id
         (setq missing-local-id local-email-id))))
    (if missing-local-id
        (chidu-result-failure-create
         :kind 'unknown-email
         :data (list :account-id account-id :local-email-id missing-local-id)
         :retryable-p nil)
      (chidu-result-ok-create :value (nreverse targets)))))

(defun chidu-store-sqlite--insert-trash-operation
    (database account-id operation-id trash-mailbox-id targets)
  "Insert one durable Trash operation and TARGETS into DATABASE.

ACCOUNT-ID owns OPERATION-ID and TRASH-MAILBOX-ID."
  (with-sqlite-transaction database
    (let ((change-seq
           (chidu-store-sqlite--increment-change-seq database)))
      (chidu-sql-execute database
        [:insert :into jmap-trash-operation
                 :row
                 [[operation-id [:bind operation-id]]
                  [account-id [:bind account-id]]
                  [trash-mailbox-id [:bind trash-mailbox-id]]
                  [accepted-change-seq [:bind change-seq]]
                  [updated-change-seq [:bind change-seq]]]])
      (dolist (target targets)
        (chidu-sql-execute database
          [:insert :into jmap-trash-target
                   :row
                   [[operation-id [:bind operation-id]]
                    [account-id [:bind account-id]]
                    [local-email-id [:bind (car target)]]
                    [remote-email-id [:bind (cdr target)]]
                    [original-mailbox-ids-json nil]
                    [phase [:literal "pending"]]
                    [error-kind nil]
                    [accepted-change-seq [:bind change-seq]]
                    [updated-change-seq [:bind change-seq]]]])))))

(defun chidu-store-sqlite--trash-destination-result
    (database account-id trash-mailbox-id)
  "Return validated Trash destination result from DATABASE.

ACCOUNT-ID owns TRASH-MAILBOX-ID."
  (let ((mailbox
         (chidu-store-sqlite--mailbox-by-id
          database account-id trash-mailbox-id)))
    (cond
     ((null mailbox)
      (chidu-result-failure-create
       :kind 'unknown-mailbox
       :data (list :account-id account-id :mailbox-id trash-mailbox-id)
       :retryable-p nil))
     ((not (and (chidu-store-mailbox-available-p mailbox)
                (equal "trash" (chidu-store-mailbox-role mailbox))))
      (chidu-result-failure-create
       :kind 'trash-mailbox-unavailable
       :data (list :account-id account-id :mailbox-id trash-mailbox-id)
       :retryable-p nil))
     ((not
       (chidu-store-mailbox-rights-may-add-items-p
        (chidu-store-mailbox-rights mailbox)))
      (chidu-result-failure-create
       :kind 'trash-destination-forbidden
       :data (list :account-id account-id :mailbox-id trash-mailbox-id)
       :retryable-p nil))
     (t (chidu-result-ok-create :value mailbox)))))

(defun chidu-store-sqlite--trash-accept-conflict
    (database account-id operation-id)
  "Return typed Trash accept conflict in DATABASE, or nil.

ACCOUNT-ID and OPERATION-ID identify the proposed operation."
  (let ((active-operation
         (chidu-store-sqlite--mailbox-mutation-operation-id
          database account-id))
        (operation-owner
         (chidu-store-sqlite--mailbox-mutation-operation-owner
          database operation-id)))
    (cond
     (active-operation
      (chidu-result-failure-create
       :kind 'mailbox-mutation-busy
       :data (list :account-id account-id :operation-id active-operation)
       :retryable-p t))
     (operation-owner
      (chidu-result-failure-create
       :kind 'duplicate-operation-id
       :data (list :operation-id operation-id)
       :retryable-p nil)))))

(defun chidu-store-sqlite--accept-trash (state operation)
  "Accept explicit move-to-Trash OPERATION in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id (chidu-store-op-accept-trash-account-id operation))
         (operation-id (chidu-store-op-accept-trash-operation-id operation))
         (trash-mailbox-id
          (chidu-store-op-accept-trash-trash-mailbox-id operation))
         (local-email-ids
          (chidu-store-sqlite--validate-local-email-ids
           (chidu-store-op-accept-trash-local-email-ids operation)))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((chidu-store-account-read-only-p (cdr location))
      (chidu-result-failure-create
       :kind 'account-read-only :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-local-id-p operation-id))
      (signal 'chidu-invariant-error
              '("Trash operation id must be a canonical local id")))
     ((not (chidu-store-local-id-p trash-mailbox-id))
      (signal 'chidu-invariant-error
              '("Trash requires a canonical local Mailbox id")))
     (t
      (let ((destination-result
             (chidu-store-sqlite--trash-destination-result
              database account-id trash-mailbox-id))
            (conflict
             (chidu-store-sqlite--trash-accept-conflict
              database account-id operation-id)))
        (cond
         ((chidu-result-failure-p destination-result) destination-result)
         (conflict conflict)
         (t
          (let ((targets-result
                 (chidu-store-sqlite--resolve-trash-targets
                  database account-id local-email-ids)))
            (if (chidu-result-failure-p targets-result)
                targets-result
              (chidu-store-sqlite--insert-trash-operation
               database account-id operation-id trash-mailbox-id
               (chidu-result-ok-value targets-result))
              (let* ((context-result
                      (chidu-store-sqlite--trash-context state account-id))
                     (context (chidu-result-ok-value context-result)))
                (chidu-store-sqlite--trash-result
                 context
                 (cl-loop
                  for local-email-id across local-email-ids
                  collect
                  (chidu-store-sqlite--trash-change
                   local-email-id 'pending)))))))))))))

(defun chidu-store-sqlite--trash-evidence (value)
  "Return nonempty unique Trash evidence vector VALUE, or signal."
  (unless (and (vectorp value) (> (length value) 0))
    (signal 'chidu-invariant-error
            '("Trash membership evidence must be nonempty")))
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop
     for item across value
     do
     (unless (chidu-store-trash-target-evidence-p item)
       (signal 'chidu-invariant-error '("Invalid Trash target evidence")))
     (let ((local-id
            (chidu-store-trash-target-evidence-local-email-id item)))
       (when (or (not (chidu-store-local-id-p local-id))
                 (gethash local-id seen))
         (signal 'chidu-invariant-error
                 '("Invalid or duplicate Trash evidence target")))
       (puthash local-id t seen)))
    value))

(defun chidu-store-sqlite--trash-source-forbidden-p
    (database account-id trash-remote-id remote-mailbox-ids)
  "Return non-nil when DATABASE proves a Trash source is not removable.

ACCOUNT-ID owns REMOTE-MAILBOX-IDS; ignore TRASH-REMOTE-ID itself."
  (cl-loop
   for remote-id across remote-mailbox-ids
   unless (equal remote-id trash-remote-id)
   thereis
   (when-let* ((mailbox
                (chidu-store-sqlite--mailbox-by-remote-id
                 database account-id remote-id)))
     (not
      (chidu-store-mailbox-rights-may-remove-items-p
       (chidu-store-mailbox-rights mailbox))))))

(defun chidu-store-sqlite--apply-trash-successes
    (database account-id trash-mailbox local-email-ids change-seq)
  "Apply successful Trash LOCAL-EMAIL-IDS in DATABASE.

ACCOUNT-ID owns TRASH-MAILBOX.  CHANGE-SEQ records invalidated search
materializations; canonical Summary reads observe the generation update
immediately."
  (when local-email-ids
    (let ((trash-remote-id
           (chidu-store-mailbox-remote-mailbox-id trash-mailbox))
          (affected-queries (make-hash-table :test #'equal)))
      (dolist (local-id local-email-ids)
        (chidu-store-sqlite--replace-active-email-mailboxes
         database account-id local-id (vector trash-remote-id))
        (dolist
            (row
             (chidu-sql-select database
               [:select [query-key]
                        :distinct t
                        :from jmap-search-projection-row
                        :where [:and
                                [:= account-id [:bind account-id]]
                                [:= local-email-id [:bind local-id]]]]))
          (puthash (car row) t affected-queries))
        (chidu-sql-execute database
          [:delete :from jmap-search-projection-row
                   :where [:and
                           [:= account-id [:bind account-id]]
                           [:= local-email-id [:bind local-id]]]]))
      (maphash
       (lambda (query-key _)
         (chidu-store-sqlite--compact-search-ordinals
          database account-id query-key))
       affected-queries)
      ;; Moving to Trash may remove or add a server-search hit.
      (chidu-sql-execute database
        [:update jmap-search-projection
                 :set [[is-stale 1]
                       [maybe-more 0]
                       [revision [:+ revision 1]]
                       [observed-change-seq [:bind change-seq]]]
                 :where [:= account-id [:bind account-id]]]))))

(defun chidu-store-sqlite--trash-target-row
    (database account-id operation-id local-email-id)
  "Return DATABASE Trash target row for LOCAL-EMAIL-ID.

ACCOUNT-ID and OPERATION-ID identify the durable operation."
  (car
   (chidu-sql-select database
     [:select [remote-email-id original-mailbox-ids-json phase]
              :from jmap-trash-target
              :where [:and
                      [:= account-id [:bind account-id]]
                      [:= operation-id [:bind operation-id]]
                      [:= local-email-id [:bind local-email-id]]]])))

(defun chidu-store-sqlite--trash-stale-evidence-local-id
    (database account-id operation-id evidence)
  "Return first stale EVIDENCE local id in DATABASE, or nil.

ACCOUNT-ID and OPERATION-ID identify the durable operation."
  (cl-loop
   for item across evidence
   for local-id = (chidu-store-trash-target-evidence-local-email-id item)
   for row =
   (chidu-store-sqlite--trash-target-row
    database account-id operation-id local-id)
   unless
   (and row
        (equal
         (chidu-store-trash-target-evidence-remote-email-id item)
         (nth 0 row)))
   return local-id))

(defun chidu-store-sqlite--delete-trash-target
    (database account-id operation-id local-email-id)
  "Delete LOCAL-EMAIL-ID Trash target from DATABASE.

ACCOUNT-ID and OPERATION-ID identify its operation."
  (chidu-sql-execute database
    [:delete
     :from jmap-trash-target
     :where [:and
             [:= account-id [:bind account-id]]
             [:= operation-id [:bind operation-id]]
             [:= local-email-id [:bind local-email-id]]]]))

(defun chidu-store-sqlite--trash-only-p
    (trash-remote-id remote-mailbox-ids)
  "Return non-nil when REMOTE-MAILBOX-IDS contains only TRASH-REMOTE-ID."
  (and (= 1 (length remote-mailbox-ids))
       (equal trash-remote-id (aref remote-mailbox-ids 0))))

(defun chidu-store-sqlite--apply-trash-evidence-item
    (database account-id operation-id trash-remote-id change-seq item)
  "Apply one Trash evidence ITEM in DATABASE.

ACCOUNT-ID and OPERATION-ID identify the operation; TRASH-REMOTE-ID and
CHANGE-SEQ describe this transition.  Return `(CHANGE . SUCCEEDED-LOCAL-ID)',
where either side may be nil."
  (let* ((local-id
          (chidu-store-trash-target-evidence-local-email-id item))
         (found-p (chidu-store-trash-target-evidence-found-p item))
         (remote-mailbox-ids
          (chidu-store-trash-target-evidence-remote-mailbox-ids item))
         (row
          (chidu-store-sqlite--trash-target-row
           database account-id operation-id local-id))
         (original-json (nth 1 row))
         (previous-phase (intern (nth 2 row))))
    (cond
     ((not found-p)
      (if (or original-json (eq previous-phase 'unknown))
          (progn
            (chidu-sql-execute database
              [:update jmap-trash-target
                       :set [[phase [:literal "unknown"]]
                             [error-kind [:literal "notFound"]]
                             [updated-change-seq [:bind change-seq]]]
                       :where [:and
                               [:= account-id [:bind account-id]]
                               [:= operation-id [:bind operation-id]]
                               [:= local-email-id [:bind local-id]]]])
            (cons
             (chidu-store-sqlite--trash-change
              local-id 'unknown "notFound")
             nil))
        (chidu-store-sqlite--delete-trash-target
         database account-id operation-id local-id)
        (cons
         (chidu-store-sqlite--trash-change
          local-id 'reverted "notFound")
         nil)))
     ((chidu-store-sqlite--trash-source-forbidden-p
       database account-id trash-remote-id remote-mailbox-ids)
      (chidu-store-sqlite--delete-trash-target
       database account-id operation-id local-id)
      (cons
       (chidu-store-sqlite--trash-change
        local-id 'reverted "sourceForbidden")
       nil))
     ((chidu-store-sqlite--trash-only-p
       trash-remote-id remote-mailbox-ids)
      (chidu-store-sqlite--delete-trash-target
       database account-id operation-id local-id)
      (cons
       (chidu-store-sqlite--trash-change local-id 'committed)
       local-id))
     (t
      (chidu-sql-execute database
        [:update jmap-trash-target
                 :set
                 [[original-mailbox-ids-json
                   [:call
                    coalesce original-mailbox-ids-json
                    [:bind
                     (chidu-store-sqlite--string-vector-json
                      remote-mailbox-ids "Trash original Mailbox ids")]]]
                  [phase [:literal "pending"]]
                  [error-kind nil]
                  [updated-change-seq [:bind change-seq]]]
                 :where [:and
                         [:= account-id [:bind account-id]]
                         [:= operation-id [:bind operation-id]]
                         [:= local-email-id [:bind local-id]]]])
      nil))))

(defun chidu-store-sqlite--finish-trash-operation
    (database account-id operation-id change-seq)
  "Advance or remove DATABASE Trash OPERATION-ID below ACCOUNT-ID."
  (if
      (car
       (chidu-sql-select database
         [:select [1]
                  :from jmap-trash-target
                  :where [:and
                          [:= account-id [:bind account-id]]
                          [:= operation-id [:bind operation-id]]]
                  :limit 1]))
      (chidu-sql-execute database
        [:update jmap-trash-operation
                 :set [[updated-change-seq [:bind change-seq]]]
                 :where [:and
                         [:= account-id [:bind account-id]]
                         [:= operation-id [:bind operation-id]]]])
    (chidu-sql-execute database
      [:delete
       :from jmap-trash-operation
       :where [:and
               [:= account-id [:bind account-id]]
               [:= operation-id [:bind operation-id]]]])))

(defun chidu-store-sqlite--trash-transition-result
    (state account-id changes)
  "Return STATE Trash result for ACCOUNT-ID and CHANGES."
  (let* ((next-result
          (chidu-store-sqlite--trash-context state account-id))
         (next-context (chidu-result-ok-value next-result)))
    (chidu-store-sqlite--trash-result next-context changes)))

(defun chidu-store-sqlite--record-trash-evidence (state operation)
  "Record authoritative Trash evidence OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-record-trash-evidence-account-id operation))
         (operation-id
          (chidu-store-op-record-trash-evidence-operation-id operation))
         (evidence
          (chidu-store-sqlite--trash-evidence
           (chidu-store-op-record-trash-evidence-evidence operation)))
         (context-result
          (chidu-store-sqlite--trash-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-operation-id
              (chidu-store-trash-context-operation-id context))
             (stale-local-id
              (and (equal operation-id actual-operation-id)
                   (chidu-store-sqlite--trash-stale-evidence-local-id
                    database account-id operation-id evidence))))
        (cond
         ((not (equal operation-id actual-operation-id))
          (chidu-result-failure-create
           :kind 'stale-operation :data (list :operation-id operation-id)
           :retryable-p nil))
         (stale-local-id
          (chidu-result-failure-create
           :kind 'stale-operation
           :data (list :operation-id operation-id
                       :local-email-id stale-local-id)
           :retryable-p nil))
         (t
          (let (changes succeeded-local-ids)
            (with-sqlite-transaction database
              (let* ((change-seq
                      (chidu-store-sqlite--increment-change-seq database))
                     (trash-mailbox
                      (chidu-store-trash-context-trash-mailbox context))
                     (trash-remote-id
                      (chidu-store-mailbox-remote-mailbox-id trash-mailbox)))
                (cl-loop
                 for item across evidence
                 for transition =
                 (chidu-store-sqlite--apply-trash-evidence-item
                  database account-id operation-id trash-remote-id
                  change-seq item)
                 when (car transition) do (push (car transition) changes)
                 when (cdr transition) do (push (cdr transition)
                                                succeeded-local-ids))
                (chidu-store-sqlite--apply-trash-successes
                 database account-id trash-mailbox
                 (nreverse succeeded-local-ids) change-seq)
                (chidu-store-sqlite--finish-trash-operation
                 database account-id operation-id change-seq)))
            (chidu-store-sqlite--trash-transition-result
             state account-id (nreverse changes)))))))))

(defun chidu-store-sqlite--trash-outcomes (value)
  "Return nonempty unique Trash target outcome vector VALUE, or signal."
  (unless (and (vectorp value) (> (length value) 0))
    (signal 'chidu-invariant-error
            '("Trash settlement requires target outcomes")))
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop
     for item across value
     do
     (unless (chidu-store-trash-target-outcome-p item)
       (signal 'chidu-invariant-error '("Invalid Trash target outcome")))
     (let ((local-id
            (chidu-store-trash-target-outcome-local-email-id item))
           (outcome (chidu-store-trash-target-outcome-outcome item)))
       (when (or (not (chidu-store-local-id-p local-id))
                 (gethash local-id seen)
                 (not (memq outcome '(succeeded rejected unknown))))
         (signal 'chidu-invariant-error
                 '("Invalid or duplicate Trash target outcome")))
       (puthash local-id t seen)))
    value))

(defun chidu-store-sqlite--trash-stale-outcome-local-id
    (database account-id operation-id outcomes)
  "Return first stale OUTCOMES local id in DATABASE, or nil.

ACCOUNT-ID and OPERATION-ID identify the durable operation."
  (cl-loop
   for item across outcomes
   for local-id = (chidu-store-trash-target-outcome-local-email-id item)
   unless
   (chidu-store-sqlite--trash-target-row
    database account-id operation-id local-id)
   return local-id))

(defun chidu-store-sqlite--apply-trash-outcome
    (database account-id operation-id change-seq item)
  "Apply one remote Trash outcome ITEM in DATABASE.

ACCOUNT-ID and OPERATION-ID identify the operation; CHANGE-SEQ records this
transition.  Return `(CHANGE . SUCCEEDED-LOCAL-ID)', where the latter may be
nil."
  (let ((local-id
         (chidu-store-trash-target-outcome-local-email-id item))
        (outcome (chidu-store-trash-target-outcome-outcome item))
        (error-kind (chidu-store-trash-target-outcome-error-kind item)))
    (pcase outcome
      ('succeeded
       (chidu-store-sqlite--delete-trash-target
        database account-id operation-id local-id)
       (cons
        (chidu-store-sqlite--trash-change local-id 'committed)
        local-id))
      ('rejected
       (chidu-store-sqlite--delete-trash-target
        database account-id operation-id local-id)
       (cons
        (chidu-store-sqlite--trash-change
         local-id 'reverted error-kind)
        nil))
      ('unknown
       (chidu-sql-execute database
         [:update jmap-trash-target
                  :set [[phase [:literal "unknown"]]
                        [error-kind [:bind error-kind]]
                        [updated-change-seq [:bind change-seq]]]
                  :where [:and
                          [:= account-id [:bind account-id]]
                          [:= operation-id [:bind operation-id]]
                          [:= local-email-id [:bind local-id]]]])
       (cons
        (chidu-store-sqlite--trash-change
         local-id 'unknown error-kind)
        nil)))))

(defun chidu-store-sqlite--settle-trash (state operation)
  "Settle explicit Trash OPERATION targets in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id (chidu-store-op-settle-trash-account-id operation))
         (operation-id (chidu-store-op-settle-trash-operation-id operation))
         (outcomes
          (chidu-store-sqlite--trash-outcomes
           (chidu-store-op-settle-trash-outcomes operation)))
         (context-result
          (chidu-store-sqlite--trash-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-operation-id
              (chidu-store-trash-context-operation-id context))
             (stale-local-id
              (and (equal operation-id actual-operation-id)
                   (chidu-store-sqlite--trash-stale-outcome-local-id
                    database account-id operation-id outcomes))))
        (cond
         ((not (equal operation-id actual-operation-id))
          (chidu-result-failure-create
           :kind 'stale-operation :data (list :operation-id operation-id)
           :retryable-p nil))
         (stale-local-id
          (chidu-result-failure-create
           :kind 'stale-operation
           :data (list :operation-id operation-id
                       :local-email-id stale-local-id)
           :retryable-p nil))
         (t
          (let (changes succeeded-local-ids)
            (with-sqlite-transaction database
              (let* ((change-seq
                      (chidu-store-sqlite--increment-change-seq database))
                     (trash-mailbox
                      (chidu-store-trash-context-trash-mailbox context)))
                (cl-loop
                 for item across outcomes
                 for transition =
                 (chidu-store-sqlite--apply-trash-outcome
                  database account-id operation-id change-seq item)
                 do (push (car transition) changes)
                 when (cdr transition) do (push (cdr transition)
                                                succeeded-local-ids))
                (chidu-store-sqlite--apply-trash-successes
                 database account-id trash-mailbox
                 (nreverse succeeded-local-ids) change-seq)
                (chidu-store-sqlite--finish-trash-operation
                 database account-id operation-id change-seq)))
            (chidu-store-sqlite--trash-transition-result
             state account-id (nreverse changes)))))))))

(provide 'chidu-store-sqlite-trash)

;;; chidu-store-sqlite-trash.el ends here
