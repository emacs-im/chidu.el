;;; chidu-store-sqlite-mutation.el --- SQLite seen and Mailbox mutations -*- lexical-binding: t; -*-

;;; Commentary:

;; Durable explicit read-state and ordinary Mailbox-move operations.
;; Move-to-Trash remains a separate domain module.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'subr-x)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)
(require 'chidu-store-sqlite-generation)
(require 'chidu-store-sqlite-mutation-support)

(defmacro chidu-store-sqlite--set-projection-unread
    (database table unread account-id local-email-id)
  "Set TABLE unread value for one Email below ACCOUNT-ID in DATABASE."
  (unless (memq table
                '(jmap-search-projection-row
                  jmap-conversation-row))
    (error "Unsupported unread projection table: %S" table))
  `(chidu-sql-execute ,database
     [:update ,table
      :set [[is-unread
             [:bind
              (chidu-store-sqlite--integer-bool ,unread)]]]
      :where [:and
              [:= account-id [:bind ,account-id]]
              [:= local-email-id [:bind ,local-email-id]]]]))

(defun chidu-store-sqlite--seen-intents (database account-id)
  "Return durable explicit read-state intents from DATABASE for ACCOUNT-ID."
  (vconcat
   (chidu-sql-map database
       [:select
        [[operation-id operation-id]
         [local-email-id local-email-id]
         [remote-email-id remote-email-id]
         [desired-seen desired-seen]
         [base-unread base-unread]
         [phase phase]
         [error-kind error-kind]]
        :from jmap-seen-intent
        :where [:= account-id [:bind account-id]]
        :order-by [[accepted-change-seq :asc] [local-email-id :asc]]]
     (chidu-store-seen-intent-create
      :operation-id operation-id
      :local-email-id local-email-id
      :remote-email-id remote-email-id
      :desired-seen-p (chidu-store-sqlite--bool desired-seen)
      :base-unread-p (chidu-store-sqlite--bool base-unread)
      :phase (intern phase)
      :error-kind error-kind))))

(defun chidu-store-sqlite--seen-context (state account-id)
  "Return ACCOUNT-ID explicit read-state context from SQLite STATE."
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
       (chidu-store-seen-context-create
        :endpoint (car location)
        :account (cdr location)
        :intents
        (chidu-store-sqlite--seen-intents database account-id)))))))

(defun chidu-store-sqlite--seen-change
    (location operation-id local-email-id remote-email-id unread-p phase
              &optional error-kind)
  "Build an OPERATION-ID read-state change for LOCATION and one Email identity."
  (chidu-store-seen-change-create
   :endpoint (car location)
   :account (cdr location)
   :operation-id operation-id
   :local-email-id local-email-id
   :remote-email-id remote-email-id
   :unread-p (and unread-p t)
   :phase phase
   :error-kind error-kind))

(defun chidu-store-sqlite--accept-seen-intent (state operation)
  "Accept or supersede explicit read-state OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-accept-seen-intent-account-id operation))
         (local-email-id
          (chidu-store-op-accept-seen-intent-local-email-id operation))
         (remote-email-id
          (chidu-store-op-accept-seen-intent-remote-email-id operation))
         (operation-id
          (chidu-store-op-accept-seen-intent-operation-id operation))
         (desired-seen-p
          (chidu-store-op-accept-seen-intent-desired-seen-p operation))
         (current-unread-p
          (chidu-store-op-accept-seen-intent-current-unread-p operation))
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
     ((not (and (chidu-store-local-id-p local-email-id)
                (chidu-store-local-id-p operation-id)
                (stringp remote-email-id)
                (not (string-empty-p remote-email-id))
                (memq desired-seen-p '(nil t))
                (memq current-unread-p '(nil t))))
      (signal 'chidu-invariant-error
              '("Invalid explicit read-state operation")))
     (t
      (let* ((actual-remote-id
              (caar
               (chidu-sql-select database
                 [:select [remote-email-id]
                  :from jmap-email-record
                  :where [:and
                          [:= account-id [:bind account-id]]
                          [:= local-email-id [:bind local-email-id]]]])))
             (existing
              (car
               (chidu-sql-select database
                 [:select
                  [operation-id remote-email-id desired-seen
                                base-unread phase error-kind]
                  :from jmap-seen-intent
                  :where [:and
                          [:= account-id [:bind account-id]]
                          [:= local-email-id [:bind local-email-id]]]])))
             (operation-owner
              (car
               (chidu-sql-select database
                 [:select [account-id local-email-id]
                  :from jmap-seen-intent
                  :where [:= operation-id [:bind operation-id]]]))))
        (cond
         ((null actual-remote-id)
          (chidu-result-failure-create
           :kind 'unknown-email
           :data (list :account-id account-id
                       :local-email-id local-email-id)
           :retryable-p nil))
         ((not (equal actual-remote-id remote-email-id))
          (chidu-result-failure-create
           :kind 'email-identity-mismatch
           :data (list :account-id account-id
                       :local-email-id local-email-id)
           :retryable-p nil))
         ((and operation-owner
               (not (equal operation-owner
                           (list account-id local-email-id))))
          (chidu-result-failure-create
           :kind 'duplicate-operation-id
           :data (list :operation-id operation-id)
           :retryable-p nil))
         (t
          (let* ((existing-operation-id (and existing (nth 0 existing)))
                 (existing-desired
                  (and existing
                       (chidu-store-sqlite--bool (nth 2 existing))))
                 (base-unread-p
                  (if existing
                      (chidu-store-sqlite--bool (nth 3 existing))
                    current-unread-p))
                 (requested-unread-p (not desired-seen-p)))
            (cond
             ((and (null existing)
                   (eq requested-unread-p current-unread-p))
              (chidu-result-ok-create
               :value
               (chidu-store-sqlite--seen-change
                location operation-id local-email-id remote-email-id
                current-unread-p 'unchanged)))
             ((and existing
                   (eq existing-desired desired-seen-p)
                   (equal "pending" (nth 4 existing)))
              (chidu-result-ok-create
               :value
               (chidu-store-sqlite--seen-change
                location existing-operation-id local-email-id remote-email-id
                requested-unread-p 'pending)))
             (t
              (with-sqlite-transaction database
                (let ((change-seq
                       (chidu-store-sqlite--increment-change-seq database)))
                  (chidu-sql-execute database
                    [:insert :into jmap-seen-intent
                     :row
                     [[account-id [:bind account-id]]
                      [local-email-id [:bind local-email-id]]
                      [remote-email-id [:bind remote-email-id] :update]
                      [operation-id [:bind operation-id] :update]
                      [desired-seen
                       [:bind
                        (chidu-store-sqlite--integer-bool desired-seen-p)]
                       :update]
                      [base-unread
                       [:bind
                        (chidu-store-sqlite--integer-bool base-unread-p)]
                       :update]
                      [phase [:literal "pending"] :update]
                      [error-kind nil :update]
                      [accepted-change-seq [:bind change-seq] :update]
                      [updated-change-seq [:bind change-seq] :update]]
                     :on-conflict [account-id local-email-id]])))
              (chidu-result-ok-create
               :value
               (chidu-store-sqlite--seen-change
                location operation-id local-email-id remote-email-id
                requested-unread-p 'pending))))))))))))

(defun chidu-store-sqlite--settle-seen-intent (state operation)
  "Settle explicit read-state OPERATION in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-settle-seen-intent-account-id operation))
         (local-email-id
          (chidu-store-op-settle-seen-intent-local-email-id operation))
         (operation-id
          (chidu-store-op-settle-seen-intent-operation-id operation))
         (outcome
          (chidu-store-op-settle-seen-intent-outcome operation))
         (error-kind
          (chidu-store-op-settle-seen-intent-error-kind operation))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id))))
    (unless (memq outcome '(succeeded rejected unknown))
      (signal 'chidu-invariant-error
              (list "Invalid read-state outcome" outcome)))
    (when (and error-kind
               (not (and (stringp error-kind)
                         (not (string-empty-p error-kind)))))
      (signal 'chidu-invariant-error
              '("Read-state error kind must be a nonempty string")))
    (if (null location)
        (chidu-result-failure-create
         :kind 'unknown-account :data (list :account-id account-id)
         :retryable-p nil)
      (let ((row
             (car
              (chidu-sql-select database
                [:select
                 [operation-id remote-email-id desired-seen base-unread]
                 :from jmap-seen-intent
                 :where [:and
                         [:= account-id [:bind account-id]]
                         [:= local-email-id [:bind local-email-id]]]]))))
        (cond
         ((or (null row) (not (equal operation-id (nth 0 row))))
          (chidu-result-failure-create
           :kind 'stale-operation
           :data (list :operation-id operation-id)
           :retryable-p nil))
         (t
          (let* ((remote-email-id (nth 1 row))
                 (desired-seen-p
                  (chidu-store-sqlite--bool (nth 2 row)))
                 (base-unread-p
                  (chidu-store-sqlite--bool (nth 3 row)))
                 (effective-unread-p
                  (if (eq outcome 'rejected)
                      base-unread-p
                    (not desired-seen-p)))
                 (phase
                  (pcase outcome
                    ('succeeded 'committed)
                    ('rejected 'reverted)
                    ('unknown 'unknown))))
            (with-sqlite-transaction database
              (let ((change-seq
                     (chidu-store-sqlite--increment-change-seq database)))
                (pcase outcome
                  ('succeeded
                   (chidu-store-sqlite--set-active-email-keyword
                    database account-id local-email-id "$seen" desired-seen-p)
                   (chidu-store-sqlite--set-projection-unread
                    database jmap-search-projection-row
                    effective-unread-p account-id local-email-id)
                   (chidu-store-sqlite--set-projection-unread
                    database jmap-conversation-row
                    effective-unread-p account-id local-email-id)
                   (chidu-sql-execute database
                     [:delete
                      :from jmap-seen-intent
                      :where [:and
                              [:= account-id [:bind account-id]]
                              [:= local-email-id [:bind local-email-id]]]]))
                  ('rejected
                   (chidu-sql-execute database
                     [:delete
                      :from jmap-seen-intent
                      :where [:and
                              [:= account-id [:bind account-id]]
                              [:= local-email-id [:bind local-email-id]]]]))
                  ('unknown
                   (chidu-sql-execute database
                     [:update jmap-seen-intent
                      :set [[phase [:literal "unknown"]]
                            [error-kind [:bind error-kind]]
                            [updated-change-seq [:bind change-seq]]]
                      :where [:and
                              [:= account-id [:bind account-id]]
                              [:= local-email-id [:bind local-email-id]]]])))))
            (chidu-result-ok-create
             :value
             (chidu-store-sqlite--seen-change
              location operation-id local-email-id remote-email-id
              effective-unread-p phase error-kind)))))))))

(defun chidu-store-sqlite--mailbox-move-intents
    (database account-id operation-id)
  "Return DATABASE move intents for OPERATION-ID below ACCOUNT-ID."
  (vconcat
   (chidu-sql-map database
       [:select
        [[local-email-id local-email-id]
         [remote-email-id remote-email-id]
         [phase phase]
         [error-kind error-kind]]
        :from jmap-mailbox-move-target
        :where [:and
                [:= account-id [:bind account-id]]
                [:= operation-id [:bind operation-id]]]
        :order-by [[accepted-change-seq :asc] [local-email-id :asc]]]
     (chidu-store-mailbox-move-intent-create
      :local-email-id local-email-id
      :remote-email-id remote-email-id
      :phase (intern phase)
      :error-kind error-kind))))

(defun chidu-store-sqlite--mailbox-move-context (state account-id)
  "Return ACCOUNT-ID's durable Mailbox move context from SQLite STATE."
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
              [source-mailbox-id source-mailbox-id]
              [destination-mailbox-id destination-mailbox-id]]
             :from jmap-mailbox-move
             :where [:= account-id [:bind account-id]]]
          (let ((source
                 (chidu-store-sqlite--mailbox-by-id
                  database account-id source-mailbox-id))
                (destination
                 (chidu-store-sqlite--mailbox-by-id
                  database account-id destination-mailbox-id)))
            (unless (and source destination)
              (signal 'chidu-invariant-error
                      '("Mailbox move references a missing Mailbox")))
            (chidu-store-mailbox-move-context-create
             :endpoint (car location)
             :account (cdr location)
             :operation-id operation-id
             :source-mailbox source
             :destination-mailbox destination
             :intents
             (chidu-store-sqlite--mailbox-move-intents
              database account-id operation-id))))
        (chidu-store-mailbox-move-context-create
         :endpoint (car location)
         :account (cdr location))))))))

(defun chidu-store-sqlite--mailbox-move-change
    (local-email-id phase &optional error-kind)
  "Return LOCAL-EMAIL-ID move change for PHASE and optional ERROR-KIND."
  (chidu-store-mailbox-move-target-change-create
   :local-email-id local-email-id
   :phase phase
   :error-kind error-kind))

(defun chidu-store-sqlite--mailbox-move-result (context changes)
  "Return successful Mailbox move result for CONTEXT and CHANGES."
  (chidu-result-ok-create
   :value
   (chidu-store-mailbox-move-result-create
    :context context
    :changes (vconcat changes))))

(defun chidu-store-sqlite--accept-mailbox-move (state operation)
  "Accept explicit Mailbox move OPERATION in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-accept-mailbox-move-account-id operation))
         (operation-id
          (chidu-store-op-accept-mailbox-move-operation-id operation))
         (source-mailbox-id
          (chidu-store-op-accept-mailbox-move-source-mailbox-id operation))
         (destination-mailbox-id
          (chidu-store-op-accept-mailbox-move-destination-mailbox-id operation))
         (local-email-ids
          (chidu-store-sqlite--validate-local-email-ids
           (chidu-store-op-accept-mailbox-move-local-email-ids operation)))
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
              '("Mailbox move operation id must be a canonical local id")))
     ((not (and (chidu-store-local-id-p source-mailbox-id)
                (chidu-store-local-id-p destination-mailbox-id)))
      (signal 'chidu-invariant-error
              '("Mailbox move requires canonical local Mailbox ids")))
     ((equal source-mailbox-id destination-mailbox-id)
      (chidu-result-failure-create
       :kind 'mailbox-move-same-mailbox
       :data (list :account-id account-id :mailbox-id source-mailbox-id)
       :retryable-p nil))
     (t
      (let* ((source
              (chidu-store-sqlite--mailbox-by-id
               database account-id source-mailbox-id))
             (destination
              (chidu-store-sqlite--mailbox-by-id
               database account-id destination-mailbox-id))
             (existing-operation
              (chidu-store-sqlite--mailbox-mutation-operation-id
               database account-id))
             (operation-owner
              (chidu-store-sqlite--mailbox-mutation-operation-owner
               database operation-id)))
        (cond
         ((null source)
          (chidu-result-failure-create
           :kind 'unknown-mailbox
           :data (list :account-id account-id :mailbox-id source-mailbox-id)
           :retryable-p nil))
         ((null destination)
          (chidu-result-failure-create
           :kind 'unknown-mailbox
           :data (list :account-id account-id
                       :mailbox-id destination-mailbox-id)
           :retryable-p nil))
         ((not (chidu-store-mailbox-available-p source))
          (chidu-result-failure-create
           :kind 'mailbox-unavailable
           :data (list :account-id account-id :mailbox-id source-mailbox-id)
           :retryable-p nil))
         ((not (chidu-store-mailbox-available-p destination))
          (chidu-result-failure-create
           :kind 'mailbox-unavailable
           :data (list :account-id account-id
                       :mailbox-id destination-mailbox-id)
           :retryable-p nil))
         ((not
           (chidu-store-mailbox-rights-may-remove-items-p
            (chidu-store-mailbox-rights source)))
          (chidu-result-failure-create
           :kind 'mailbox-move-source-forbidden
           :data (list :account-id account-id :mailbox-id source-mailbox-id)
           :retryable-p nil))
         ((not
           (chidu-store-mailbox-rights-may-add-items-p
            (chidu-store-mailbox-rights destination)))
          (chidu-result-failure-create
           :kind 'mailbox-move-destination-forbidden
           :data (list :account-id account-id
                       :mailbox-id destination-mailbox-id)
           :retryable-p nil))
         (existing-operation
          (chidu-result-failure-create
           :kind 'mailbox-mutation-busy
           :data (list :account-id account-id
                       :operation-id existing-operation)
           :retryable-p t))
         (operation-owner
          (chidu-result-failure-create
           :kind 'duplicate-operation-id
           :data (list :operation-id operation-id)
           :retryable-p nil))
         (t
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
                 :data (list :account-id account-id
                             :local-email-id missing-local-id)
                 :retryable-p nil)
              (setq targets (nreverse targets))
              (with-sqlite-transaction database
                (let ((change-seq
                       (chidu-store-sqlite--increment-change-seq database)))
                  (chidu-sql-execute database
                    [:insert :into jmap-mailbox-move
                     :row
                     [[operation-id [:bind operation-id]]
                      [account-id [:bind account-id]]
                      [source-mailbox-id [:bind source-mailbox-id]]
                      [destination-mailbox-id [:bind destination-mailbox-id]]
                      [accepted-change-seq [:bind change-seq]]
                      [updated-change-seq [:bind change-seq]]]])
                  (dolist (target targets)
                    (chidu-sql-execute database
                      [:insert :into jmap-mailbox-move-target
                       :row
                       [[operation-id [:bind operation-id]]
                        [account-id [:bind account-id]]
                        [local-email-id [:bind (car target)]]
                        [remote-email-id [:bind (cdr target)]]
                        [phase [:literal "pending"]]
                        [error-kind nil]
                        [accepted-change-seq [:bind change-seq]]
                        [updated-change-seq [:bind change-seq]]]]))))
              (let* ((context-result
                      (chidu-store-sqlite--mailbox-move-context
                       state account-id))
                     (context (chidu-result-ok-value context-result)))
                (chidu-store-sqlite--mailbox-move-result
                 context
                 (cl-loop
                  for local-email-id across local-email-ids
                  collect
                  (chidu-store-sqlite--mailbox-move-change
                   local-email-id 'pending)))))))))))))

(defun chidu-store-sqlite--mailbox-move-outcomes (value)
  "Validate and return Mailbox move target outcome vector VALUE."
  (unless (and (vectorp value) (> (length value) 0))
    (signal 'chidu-invariant-error
            '("Mailbox move settlement requires target outcomes")))
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop
     for item across value
     do
     (unless (chidu-store-mailbox-move-target-outcome-p item)
       (signal 'chidu-invariant-error
               '("Invalid Mailbox move target outcome")))
     do
     (let ((local-id
            (chidu-store-mailbox-move-target-outcome-local-email-id item))
           (outcome
            (chidu-store-mailbox-move-target-outcome-outcome item))
           (error-kind
            (chidu-store-mailbox-move-target-outcome-error-kind item)))
       (unless (and (chidu-store-local-id-p local-id)
                    (memq outcome '(succeeded rejected unknown))
                    (or (null error-kind)
                        (and (stringp error-kind)
                             (not (string-empty-p error-kind))))
                    (not (gethash local-id seen)))
         (signal 'chidu-invariant-error
                 '("Invalid or duplicate Mailbox move target outcome")))
       (puthash local-id t seen))))
  value)

(defun chidu-store-sqlite--apply-mailbox-move-successes
    (database account-id source destination local-email-ids change-seq)
  "Apply successful Mailbox move LOCAL-EMAIL-IDS in DATABASE.

ACCOUNT-ID owns SOURCE and DESTINATION.  CHANGE-SEQ records invalidated search
materializations; canonical Summary reads observe the generation update
immediately."
  (when local-email-ids
    (let ((source-remote-id
           (chidu-store-mailbox-remote-mailbox-id source))
          (destination-remote-id
           (chidu-store-mailbox-remote-mailbox-id destination))
          (query-keys (make-hash-table :test #'equal)))
      (dolist (local-email-id local-email-ids)
        (chidu-store-sqlite--move-active-email
         database account-id local-email-id
         source-remote-id destination-remote-id)
        (dolist
            (row
             (chidu-sql-select database
               [:select [query-key]
                :distinct t
                :from jmap-search-projection-row
                :where [:and
                        [:= account-id [:bind account-id]]
                        [:= local-email-id [:bind local-email-id]]]]))
          (puthash (car row) t query-keys))
        (chidu-sql-execute database
          [:delete :from jmap-search-projection-row
           :where [:and
                   [:= account-id [:bind account-id]]
                   [:= local-email-id [:bind local-email-id]]]]))
      (maphash
       (lambda (query-key _)
         (chidu-store-sqlite--compact-search-ordinals
          database account-id query-key))
       query-keys)
      ;; Query membership and snippets are server-defined.  A move may remove
      ;; or add a hit, so every materialized query lease becomes stale.
      (chidu-sql-execute database
        [:update jmap-search-projection
         :set [[is-stale 1]
               [maybe-more 0]
               [revision [:+ revision 1]]
               [observed-change-seq [:bind change-seq]]]
         :where [:= account-id [:bind account-id]]]))))

(defun chidu-store-sqlite--settle-mailbox-move (state operation)
  "Settle explicit Mailbox move OPERATION targets in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-settle-mailbox-move-account-id operation))
         (operation-id
          (chidu-store-op-settle-mailbox-move-operation-id operation))
         (outcomes
          (chidu-store-sqlite--mailbox-move-outcomes
           (chidu-store-op-settle-mailbox-move-outcomes operation)))
         (context-result
          (chidu-store-sqlite--mailbox-move-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-operation-id
              (chidu-store-mailbox-move-context-operation-id context)))
        (cond
         ((null actual-operation-id)
          (chidu-result-failure-create
           :kind 'stale-operation :data (list :operation-id operation-id)
           :retryable-p nil))
         ((not (equal operation-id actual-operation-id))
          (chidu-result-failure-create
           :kind 'stale-operation :data (list :operation-id operation-id)
           :retryable-p nil))
         (t
          (let ((source-mailbox
                 (chidu-store-mailbox-move-context-source-mailbox context))
                (destination-mailbox
                 (chidu-store-mailbox-move-context-destination-mailbox
                  context))
                missing-local-id)
            (cl-loop
             for item across outcomes
             for local-id =
             (chidu-store-mailbox-move-target-outcome-local-email-id item)
             unless
             (car
              (chidu-sql-select database
                [:select [1]
                 :from jmap-mailbox-move-target
                 :where [:and
                         [:= account-id [:bind account-id]]
                         [:= operation-id [:bind operation-id]]
                         [:= local-email-id [:bind local-id]]]]))
             do (unless missing-local-id (setq missing-local-id local-id)))
            (if missing-local-id
                (chidu-result-failure-create
                 :kind 'stale-operation
                 :data (list :operation-id operation-id
                             :local-email-id missing-local-id)
                 :retryable-p nil)
              (let (changes succeeded-local-ids)
                (with-sqlite-transaction database
                  (let ((change-seq
                         (chidu-store-sqlite--increment-change-seq database)))
                    (cl-loop
                     for item across outcomes
                     for local-id =
                     (chidu-store-mailbox-move-target-outcome-local-email-id
                      item)
                     for outcome =
                     (chidu-store-mailbox-move-target-outcome-outcome item)
                     for error-kind =
                     (chidu-store-mailbox-move-target-outcome-error-kind item)
                     do
                     (pcase outcome
                       ('succeeded
                        (push local-id succeeded-local-ids)
                        (chidu-sql-execute database
                          [:delete
                           :from jmap-mailbox-move-target
                           :where [:and
                                   [:= account-id [:bind account-id]]
                                   [:= operation-id [:bind operation-id]]
                                   [:= local-email-id [:bind local-id]]]])
                        (push
                         (chidu-store-sqlite--mailbox-move-change
                          local-id 'committed)
                         changes))
                       ('rejected
                        (chidu-sql-execute database
                          [:delete
                           :from jmap-mailbox-move-target
                           :where [:and
                                   [:= account-id [:bind account-id]]
                                   [:= operation-id [:bind operation-id]]
                                   [:= local-email-id [:bind local-id]]]])
                        (push
                         (chidu-store-sqlite--mailbox-move-change
                          local-id 'reverted error-kind)
                         changes))
                       ('unknown
                        (chidu-sql-execute database
                          [:update jmap-mailbox-move-target
                           :set [[phase [:literal "unknown"]]
                                 [error-kind [:bind error-kind]]
                                 [updated-change-seq [:bind change-seq]]]
                           :where [:and
                                   [:= account-id [:bind account-id]]
                                   [:= operation-id [:bind operation-id]]
                                   [:= local-email-id [:bind local-id]]]])
                        (push
                         (chidu-store-sqlite--mailbox-move-change
                          local-id 'unknown error-kind)
                         changes))))
                    (chidu-store-sqlite--apply-mailbox-move-successes
                     database account-id source-mailbox destination-mailbox
                     (nreverse succeeded-local-ids) change-seq)
                    (if
                        (car
                         (chidu-sql-select database
                           [:select [1]
                            :from jmap-mailbox-move-target
                            :where [:and
                                    [:= account-id [:bind account-id]]
                                    [:= operation-id [:bind operation-id]]]
                            :limit 1]))
                        (chidu-sql-execute database
                          [:update jmap-mailbox-move
                           :set [[updated-change-seq [:bind change-seq]]]
                           :where [:and
                                   [:= account-id [:bind account-id]]
                                   [:= operation-id [:bind operation-id]]]])
                      (chidu-sql-execute database
                        [:delete
                         :from jmap-mailbox-move
                         :where [:and
                                 [:= account-id [:bind account-id]]
                                 [:= operation-id [:bind operation-id]]]]))))
                (let* ((next-result
                        (chidu-store-sqlite--mailbox-move-context
                         state account-id))
                       (next-context (chidu-result-ok-value next-result)))
                  (chidu-store-sqlite--mailbox-move-result
                   next-context (nreverse changes))))))))))))

(provide 'chidu-store-sqlite-mutation)

;;; chidu-store-sqlite-mutation.el ends here
