;;; chidu-store-sqlite-draft.el --- Server Draft publication state -*- lexical-binding: t; -*-

;;; Commentary:

;; Durable control state for publishing one ComposeWorkspace revision as a
;; JMAP Draft Email.  A pending create is known not to have run; an unknown
;; create must be reconciled before retry.  A confirmed replacement becomes
;; the workspace's remote head before predecessor cleanup begins.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'subr-x)
(require 'chidu-result)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)
(require 'chidu-store-sqlite-compose)
(require 'chidu-store-sqlite-compose-resource)

(cl-defstruct (chidu-store-sqlite--draft-publish-row
               (:constructor chidu-store-sqlite--make-draft-publish-row)
               (:copier nil))
  attempt-id
  workspace-id
  account-id
  identity-id
  drafts-mailbox-id
  revision
  message-id
  predecessor-remote-email-id
  predecessor-remote-blob-id
  phase
  error-kind)

(defun chidu-store-sqlite--decode-draft-publish-row (row)
  "Decode one SQLite Draft publication ROW."
  (pcase-let
      ((`(,attempt-id ,workspace-id ,account-id ,identity-id
                      ,drafts-mailbox-id ,revision ,message-id
                      ,predecessor-remote-email-id
                      ,predecessor-remote-blob-id ,phase ,error-kind)
        row))
    (chidu-store-sqlite--make-draft-publish-row
     :attempt-id attempt-id
     :workspace-id workspace-id
     :account-id account-id
     :identity-id identity-id
     :drafts-mailbox-id drafts-mailbox-id
     :revision revision
     :message-id message-id
     :predecessor-remote-email-id predecessor-remote-email-id
     :predecessor-remote-blob-id predecessor-remote-blob-id
     :phase (intern phase)
     :error-kind error-kind)))

(defun chidu-store-sqlite--select-draft-publish-row
    (database &optional workspace-id attempt-id)
  "Return DATABASE publication row for WORKSPACE-ID or ATTEMPT-ID."
  (unless (or workspace-id attempt-id)
    (signal 'chidu-invariant-error
            '("Draft publication lookup needs an identity")))
  (let ((row
         (car
          (if workspace-id
              (chidu-sql-select database
                [:select
                 [attempt-id workspace-id account-id identity-id
                             drafts-mailbox-id revision message-id
                             predecessor-remote-email-id predecessor-remote-blob-id
                             phase error-kind]
                 :from chidu-draft-publish-attempt
                 :where
                 [:and
                  [:= workspace-id [:bind workspace-id]]
                  [:in phase
                       [[:literal "pending"] [:literal "unknown"]]]]
                 :order-by [[created-change-seq :asc] [attempt-id :asc]]
                 :limit 1])
            (chidu-sql-select database
              [:select
               [attempt-id workspace-id account-id identity-id
                           drafts-mailbox-id revision message-id
                           predecessor-remote-email-id predecessor-remote-blob-id
                           phase error-kind]
               :from chidu-draft-publish-attempt
               :where [:= attempt-id [:bind attempt-id]]])))))
    (and row (chidu-store-sqlite--decode-draft-publish-row row))))

(defun chidu-store-sqlite--draft-publish-attempt-from-row (row)
  "Return typed publication attempt decoded from SQLite ROW."
  (chidu-store-draft-publish-attempt-create
   :attempt-id (chidu-store-sqlite--draft-publish-row-attempt-id row)
   :workspace-id (chidu-store-sqlite--draft-publish-row-workspace-id row)
   :account-id (chidu-store-sqlite--draft-publish-row-account-id row)
   :identity-id (chidu-store-sqlite--draft-publish-row-identity-id row)
   :drafts-mailbox-id
   (chidu-store-sqlite--draft-publish-row-drafts-mailbox-id row)
   :revision (chidu-store-sqlite--draft-publish-row-revision row)
   :message-id (chidu-store-sqlite--draft-publish-row-message-id row)
   :predecessor-remote-email-id
   (chidu-store-sqlite--draft-publish-row-predecessor-remote-email-id row)
   :predecessor-remote-blob-id
   (chidu-store-sqlite--draft-publish-row-predecessor-remote-blob-id row)
   :phase (chidu-store-sqlite--draft-publish-row-phase row)
   :error-kind (chidu-store-sqlite--draft-publish-row-error-kind row)))

(defun chidu-store-sqlite--draft-publish-attempt (database workspace-id)
  "Return active DATABASE publication attempt for WORKSPACE-ID, or nil."
  (when-let* ((row
               (chidu-store-sqlite--select-draft-publish-row
                database workspace-id nil)))
    (chidu-store-sqlite--draft-publish-attempt-from-row row)))

(defun chidu-store-sqlite--draft-publish-context (state row)
  "Return SQLite STATE Compose context for publication ROW."
  (chidu-store-sqlite--compose-context
   state (chidu-store-sqlite--draft-publish-row-workspace-id row)))

(defun chidu-store-sqlite--draft-cleanup-attempts (database workspace-id)
  "Return DATABASE cleanup attempts for WORKSPACE-ID in durable order."
  (vconcat
   (mapcar
    #'chidu-store-sqlite--draft-publish-attempt-from-row
    (mapcar
     #'chidu-store-sqlite--decode-draft-publish-row
     (chidu-sql-select database
       [:select
        [attempt-id workspace-id account-id identity-id
                    drafts-mailbox-id revision message-id
                    predecessor-remote-email-id predecessor-remote-blob-id
                    phase error-kind]
        :from chidu-draft-publish-attempt
        :where [:and
                [:= workspace-id [:bind workspace-id]]
                [:= phase [:literal "cleanup-pending"]]]
        :order-by [[created-change-seq :asc] [attempt-id :asc]]])))))

(defun chidu-store-sqlite--draft-publish-text-p (value)
  "Return non-nil when VALUE is safe non-empty publication text."
  (and (stringp value)
       (not (string-empty-p value))
       (not (string-match-p "[\0\r\n]" value))))

(defun chidu-store-sqlite--drafts-mailbox-writable-p (mailbox)
  "Return non-nil when MAILBOX accepts a new server Draft revision."
  (and
   (chidu-store-mailbox-p mailbox)
   (chidu-store-mailbox-available-p mailbox)
   (equal "drafts" (chidu-store-mailbox-role mailbox))
   (chidu-store-mailbox-rights-may-add-items-p
    (chidu-store-mailbox-rights mailbox))))

(defun chidu-store-sqlite--draft-resource-failure
    (database row document)
  "Return DATABASE publication resource failure for ROW and DOCUMENT, or nil."
  (let* ((workspace-id
          (chidu-store-sqlite--compose-row-workspace-id row))
         (resource-result
          (chidu-store-sqlite--compose-document-resources-result
           database workspace-id document t)))
    (if (chidu-result-failure-p resource-result)
        resource-result
      (let* ((resources (chidu-result-ok-value resource-result))
             (account-id
              (chidu-store-sqlite--compose-row-account-id row))
             (location
              (chidu-store-sqlite--account-location database account-id))
             (maximum
              (and location
                   (chidu-store-account-max-size-attachments-per-email
                    (cdr location))))
             (total
              (cl-loop for resource across resources
                       sum (chidu-store-compose-resource-size resource))))
        (when (and maximum (> total maximum))
          (chidu-result-failure-create
           :kind 'compose-attachments-too-large
           :data (list :workspace-id workspace-id
                       :actual-bytes total
                       :max-size-attachments-per-email maximum)
           :retryable-p nil))))))

(defun chidu-store-sqlite--draft-publish-revision-failure
    (workspace-id expected actual)
  "Return publication revision failure for WORKSPACE-ID EXPECTED and ACTUAL."
  (chidu-result-failure-create
   :kind 'revision-conflict
   :data (list :workspace-id workspace-id
               :expected expected :actual actual)
   :retryable-p t))

(defun chidu-store-sqlite--checkpoint-for-draft-publish
    (database row identity-id expected revision document change-seq)
  "Checkpoint DATABASE workspace ROW for a Draft publication.

IDENTITY-ID, EXPECTED, REVISION, DOCUMENT, and CHANGE-SEQ describe the exact
captured workspace value."
  (when (> revision expected)
    (chidu-store-sqlite--update-compose-document
     database row identity-id revision document change-seq)))

(defun chidu-store-sqlite--accept-draft-publish (state operation)
  "Checkpoint and accept Draft publication OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (workspace-id
          (chidu-store-op-accept-draft-publish-workspace-id operation))
         (identity-id
          (chidu-store-op-accept-draft-publish-identity-id operation))
         (expected
          (chidu-store-op-accept-draft-publish-expected-revision operation))
         (revision
          (chidu-store-op-accept-draft-publish-revision operation))
         (document
          (chidu-store-op-accept-draft-publish-document operation))
         (attempt-id
          (chidu-store-op-accept-draft-publish-attempt-id operation))
         (message-id
          (chidu-store-op-accept-draft-publish-message-id operation))
         (row
          (and (chidu-store-local-id-p workspace-id)
               (chidu-store-sqlite--select-compose-row
                database workspace-id))))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-compose-workspace
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((chidu-store-sqlite--select-draft-publish-row
       database workspace-id nil)
      (chidu-result-failure-create
       :kind 'draft-publish-active
       :data (list :workspace-id workspace-id) :retryable-p t))
     ((not (chidu-store-sqlite--compose-document-p document))
      (chidu-result-failure-create
       :kind 'invalid-compose-document :data nil :retryable-p nil))
     ((not (and (integerp expected) (>= expected 0)
                (integerp revision) (>= revision expected)))
      (chidu-result-failure-create
       :kind 'invalid-compose-revision
       :data (list :workspace-id workspace-id
                   :expected expected :revision revision)
       :retryable-p nil))
     ((chidu-store-sqlite--draft-resource-failure
       database row document))
     ((/= expected (chidu-store-sqlite--compose-row-revision row))
      (chidu-store-sqlite--draft-publish-revision-failure
       workspace-id expected
       (chidu-store-sqlite--compose-row-revision row)))
     ((and (= revision expected)
           (not
            (and
             (equal identity-id
                    (chidu-store-sqlite--compose-row-identity-id row))
             (equal document
                    (chidu-store-sqlite--compose-row-document row)))))
      (chidu-result-failure-create
       :kind 'invalid-compose-revision
       :data (list :workspace-id workspace-id :revision revision)
       :retryable-p nil))
     ((not
       (equal
        (chidu-store-compose-document-resource-ids document)
        (chidu-store-compose-document-resource-ids
         (chidu-store-sqlite--compose-row-document row))))
      (chidu-result-failure-create
       :kind 'compose-resource-membership-conflict
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((not (and (chidu-store-local-id-p attempt-id)
                (chidu-store-sqlite--draft-publish-text-p message-id)))
      (chidu-result-failure-create
       :kind 'invalid-draft-publish-identity :data nil :retryable-p nil))
     (t
      (let* ((account-id
              (chidu-store-sqlite--compose-row-account-id row))
             (owner-failure
              (chidu-store-sqlite--compose-owner-failure
               database account-id identity-id t))
             (drafts-mailbox
              (chidu-store-sqlite--compose-drafts-mailbox
               database account-id))
             (predecessor
              (chidu-store-sqlite--compose-row-base-remote-email-id row))
             (predecessor-blob
              (chidu-store-sqlite--compose-row-base-remote-blob-id row)))
        (cond
         (owner-failure owner-failure)
         ((not
           (chidu-store-sqlite--drafts-mailbox-writable-p drafts-mailbox))
          (chidu-result-failure-create
           :kind 'drafts-mailbox-unavailable
           :data (list :account-id account-id) :retryable-p nil))
         (t
          (with-sqlite-transaction database
            (let ((change-seq
                   (chidu-store-sqlite--increment-change-seq database)))
              (chidu-store-sqlite--checkpoint-for-draft-publish
               database row identity-id expected revision document change-seq)
              (chidu-sql-execute database
                [:insert :into chidu-draft-publish-attempt
                         :row
                         [[attempt-id [:bind attempt-id]]
                          [workspace-id [:bind workspace-id]]
                          [account-id [:bind account-id]]
                          [identity-id [:bind identity-id]]
                          [drafts-mailbox-id
                           [:bind (chidu-store-mailbox-mailbox-id drafts-mailbox)]]
                          [revision [:bind revision]]
                          [message-id [:bind message-id]]
                          [predecessor-remote-email-id [:bind predecessor]]
                          [predecessor-remote-blob-id [:bind predecessor-blob]]
                          [phase [:literal "pending"]]
                          [error-kind nil]
                          [created-change-seq [:bind change-seq]]
                          [updated-change-seq [:bind change-seq]]]])))
          (chidu-store-sqlite--compose-context state workspace-id))))))))

(defun chidu-store-sqlite--mark-draft-publish-unknown (state operation)
  "Fence pending Draft create OPERATION as unknown in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (attempt-id
          (chidu-store-op-mark-draft-publish-unknown-attempt-id operation))
         (row
          (chidu-store-sqlite--select-draft-publish-row
           database nil attempt-id)))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-draft-publish-attempt
       :data (list :attempt-id attempt-id) :retryable-p nil))
     ((eq 'unknown (chidu-store-sqlite--draft-publish-row-phase row))
      (chidu-store-sqlite--draft-publish-context state row))
     ((not (eq 'pending
               (chidu-store-sqlite--draft-publish-row-phase row)))
      (chidu-result-failure-create
       :kind 'draft-publish-phase-conflict
       :data (list :attempt-id attempt-id
                   :phase
                   (chidu-store-sqlite--draft-publish-row-phase row))
       :retryable-p t))
     (t
      (with-sqlite-transaction database
        (let ((change-seq
               (chidu-store-sqlite--increment-change-seq database)))
          (chidu-sql-execute database
            [:update chidu-draft-publish-attempt
                     :set [[phase [:literal "unknown"]]
                           [error-kind nil]
                           [updated-change-seq [:bind change-seq]]]
                     :where [:= attempt-id [:bind attempt-id]]])))
      (chidu-store-sqlite--draft-publish-context state row)))))

(defun chidu-store-sqlite--retry-draft-publish-create (state operation)
  "Return reconciled absent Draft create OPERATION to pending in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (attempt-id
          (chidu-store-op-retry-draft-publish-create-attempt-id operation))
         (row
          (chidu-store-sqlite--select-draft-publish-row
           database nil attempt-id)))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-draft-publish-attempt
       :data (list :attempt-id attempt-id) :retryable-p nil))
     ((not (eq 'unknown
               (chidu-store-sqlite--draft-publish-row-phase row)))
      (chidu-result-failure-create
       :kind 'draft-publish-phase-conflict
       :data (list :attempt-id attempt-id
                   :phase (chidu-store-sqlite--draft-publish-row-phase row))
       :retryable-p t))
     (t
      (with-sqlite-transaction database
        (let ((change-seq
               (chidu-store-sqlite--increment-change-seq database)))
          (chidu-sql-execute database
            [:update chidu-draft-publish-attempt
                     :set [[phase [:literal "pending"]]
                           [error-kind nil]
                           [updated-change-seq [:bind change-seq]]]
                     :where [:= attempt-id [:bind attempt-id]]])))
      (chidu-store-sqlite--draft-publish-context state row)))))

(defun chidu-store-sqlite--settle-draft-publish-create (state operation)
  "Settle Draft create OPERATION from exact remote evidence in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (attempt-id
          (chidu-store-op-settle-draft-publish-create-attempt-id operation))
         (outcome
          (chidu-store-op-settle-draft-publish-create-outcome operation))
         (remote-email-id
          (chidu-store-op-settle-draft-publish-create-remote-email-id
           operation))
         (remote-blob-id
          (chidu-store-op-settle-draft-publish-create-remote-blob-id
           operation))
         (error-kind
          (chidu-store-op-settle-draft-publish-create-error-kind operation))
         (row
          (chidu-store-sqlite--select-draft-publish-row
           database nil attempt-id)))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-draft-publish-attempt
       :data (list :attempt-id attempt-id) :retryable-p nil))
     ((not (eq 'unknown
               (chidu-store-sqlite--draft-publish-row-phase row)))
      (chidu-result-failure-create
       :kind 'draft-publish-phase-conflict
       :data (list :attempt-id attempt-id
                   :phase (chidu-store-sqlite--draft-publish-row-phase row))
       :retryable-p t))
     ((not (memq outcome '(succeeded rejected unknown)))
      (chidu-result-failure-create
       :kind 'invalid-draft-publish-outcome
       :data (list :outcome outcome) :retryable-p nil))
     ((and (eq outcome 'succeeded)
           (not
            (and
             (chidu-store-sqlite--draft-publish-text-p remote-email-id)
             (chidu-store-sqlite--draft-publish-text-p remote-blob-id))))
      (chidu-result-failure-create
       :kind 'invalid-remote-email-evidence :data nil :retryable-p nil))
     (t
      (let ((workspace-id
             (chidu-store-sqlite--draft-publish-row-workspace-id row)))
        (with-sqlite-transaction database
          (let ((change-seq
                 (chidu-store-sqlite--increment-change-seq database)))
            (pcase outcome
              ('succeeded
               (let ((predecessor
                      (chidu-store-sqlite--draft-publish-row-predecessor-remote-email-id
                       row)))
                 (chidu-sql-execute database
                   [:update chidu-compose-workspace
                            :set [[base-remote-email-id [:bind remote-email-id]]
                                  [base-remote-blob-id [:bind remote-blob-id]]
                                  [published-revision
                                   [:bind
                                    (chidu-store-sqlite--draft-publish-row-revision row)]]
                                  [updated-change-seq [:bind change-seq]]]
                            :where [:= workspace-id [:bind workspace-id]]])
                 (if (and predecessor
                          (not (equal predecessor remote-email-id)))
                     (chidu-sql-execute database
                       [:update chidu-draft-publish-attempt
                                :set [[phase [:literal "cleanup-pending"]]
                                      [error-kind nil]
                                      [updated-change-seq [:bind change-seq]]]
                                :where [:= attempt-id [:bind attempt-id]]])
                   (chidu-sql-execute database
                     [:delete :from chidu-draft-publish-attempt
                              :where [:= attempt-id [:bind attempt-id]]]))))
              ('rejected
               (chidu-sql-execute database
                 [:delete :from chidu-draft-publish-attempt
                          :where [:= attempt-id [:bind attempt-id]]]))
              ('unknown
               (chidu-sql-execute database
                 [:update chidu-draft-publish-attempt
                          :set [[phase [:literal "unknown"]]
                                [error-kind [:bind error-kind]]
                                [updated-change-seq [:bind change-seq]]]
                          :where [:= attempt-id [:bind attempt-id]]])))))
        (chidu-store-sqlite--compose-context state workspace-id))))))

(defun chidu-store-sqlite--settle-draft-publish-cleanup (state operation)
  "Settle predecessor cleanup OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (attempt-id
          (chidu-store-op-settle-draft-publish-cleanup-attempt-id operation))
         (outcome
          (chidu-store-op-settle-draft-publish-cleanup-outcome operation))
         (error-kind
          (chidu-store-op-settle-draft-publish-cleanup-error-kind operation))
         (row
          (chidu-store-sqlite--select-draft-publish-row
           database nil attempt-id)))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-draft-publish-attempt
       :data (list :attempt-id attempt-id) :retryable-p nil))
     ((not (eq 'cleanup-pending
               (chidu-store-sqlite--draft-publish-row-phase row)))
      (chidu-result-failure-create
       :kind 'draft-publish-phase-conflict
       :data (list :attempt-id attempt-id
                   :phase (chidu-store-sqlite--draft-publish-row-phase row))
       :retryable-p t))
     ((not (memq outcome '(succeeded rejected unknown)))
      (chidu-result-failure-create
       :kind 'invalid-draft-publish-outcome
       :data (list :outcome outcome) :retryable-p nil))
     (t
      (with-sqlite-transaction database
        (let ((change-seq
               (chidu-store-sqlite--increment-change-seq database)))
          (if (memq outcome '(succeeded rejected))
              (chidu-sql-execute database
                [:delete :from chidu-draft-publish-attempt
                         :where [:= attempt-id [:bind attempt-id]]])
            (chidu-sql-execute database
              [:update chidu-draft-publish-attempt
                       :set [[error-kind [:bind error-kind]]
                             [updated-change-seq [:bind change-seq]]]
                       :where [:= attempt-id [:bind attempt-id]]]))))
      (chidu-store-sqlite--draft-publish-context state row)))))

(provide 'chidu-store-sqlite-draft)

;;; chidu-store-sqlite-draft.el ends here
