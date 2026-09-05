;;; chidu-store-sqlite-compose.el --- Local Compose workspaces -*- lexical-binding: t; -*-

;;; Commentary:

;; Persist structured local Compose workspaces.  A workspace is a checkout and
;; recovery journal for a client-owned ComposeDocument; it is not a JMAP Draft
;; Email.  Remote Draft publication and Submission use separate durable
;; operations.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'subr-x)
(require 'chidu-result)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)

(declare-function chidu-store-sqlite--draft-publish-attempt
                  "chidu-store-sqlite-draft" (database workspace-id))
(declare-function chidu-store-sqlite--draft-cleanup-attempts
                  "chidu-store-sqlite-draft" (database workspace-id))
(declare-function chidu-store-sqlite--compose-resources
                  "chidu-store-sqlite-compose-resource"
                  (database workspace-id document))
(declare-function chidu-store-sqlite--compose-document-resources-result
                  "chidu-store-sqlite-compose-resource"
                  (database workspace-id document &optional require-remote-p))
(declare-function chidu-store-sqlite--update-compose-document
                  "chidu-store-sqlite-compose-resource"
                  (database row identity-id revision document change-seq))

(cl-defstruct (chidu-store-sqlite--compose-row
               (:constructor chidu-store-sqlite--make-compose-row)
               (:copier nil))
  workspace-id
  account-id
  identity-id
  kind
  document
  base-remote-email-id
  base-remote-blob-id
  published-revision
  revision)

(defconst chidu-store-sqlite--compose-kinds
  '(new reply-sender reply-all reply-list forward draft)
  "Closed Compose workspace kinds understood by the current Store format.")

(defun chidu-store-sqlite--compose-kind-p (value)
  "Return non-nil when VALUE is a supported Compose workspace kind."
  (memq value chidu-store-sqlite--compose-kinds))

(defun chidu-store-sqlite--compose-resource-ids-p (value)
  "Return non-nil when VALUE is a unique vector of local resource ids."
  (condition-case nil
      (let ((ids (chidu-store-validate-string-vector
                  value "Compose resource ids"))
            (seen (make-hash-table :test #'equal)))
        (cl-loop
         for id across ids
         always
         (and (chidu-store-local-id-p id)
              (not (prog1 (gethash id seen)
                     (puthash id t seen))))))
    (error nil)))

(defun chidu-store-sqlite--compose-document-p (document)
  "Return non-nil when DOCUMENT is a valid current ComposeDocument."
  (and
   (chidu-store-compose-document-p document)
   (chidu-store-sqlite--compose-resource-ids-p
    (chidu-store-compose-document-resource-ids document))
   (cl-loop
    for value in
    (list
     (chidu-store-compose-document-to document)
     (chidu-store-compose-document-cc document)
     (chidu-store-compose-document-bcc document)
     (chidu-store-compose-document-reply-to document)
     (chidu-store-compose-document-subject document)
     (chidu-store-compose-document-body document))
    always (and (stringp value) (not (string-match-p "\0" value))))))

(defun chidu-store-sqlite--compose-identity (account identity-id)
  "Return ACCOUNT Identity named by local IDENTITY-ID, or nil."
  (cl-find identity-id
           (chidu-store-account-identities account)
           :key #'chidu-store-identity-identity-id
           :test #'equal))

(defun chidu-store-sqlite--decode-compose-row (row)
  "Decode one SQLite Compose workspace ROW."
  (pcase-let
      ((`(,workspace-id ,account-id ,identity-id ,kind
                        ,to-value ,cc-value ,bcc-value ,reply-to-value
                        ,subject ,body ,resource-ids-json ,base-remote-email-id
                        ,base-remote-blob-id ,published-revision ,revision)
        row))
    (chidu-store-sqlite--make-compose-row
     :workspace-id workspace-id
     :account-id account-id
     :identity-id identity-id
     :kind (intern kind)
     :document
     (chidu-store-compose-document-create
      :to to-value
      :cc cc-value
      :bcc bcc-value
      :reply-to reply-to-value
      :subject subject
      :body body
      :resource-ids
      (chidu-store-sqlite--string-vector-from-json
       resource-ids-json "Compose document resource ids"))
     :base-remote-email-id base-remote-email-id
     :base-remote-blob-id base-remote-blob-id
     :published-revision published-revision
     :revision revision)))

(defun chidu-store-sqlite--select-compose-row (database workspace-id)
  "Return decoded DATABASE row for WORKSPACE-ID, or nil."
  (when-let* ((row
               (car
                (chidu-sql-select database
                  [:select
                   [workspace-id account-id identity-id kind
                                 to-value cc-value bcc-value reply-to-value
                                 subject body resource-ids-json base-remote-email-id
                                 base-remote-blob-id published-revision revision]
                   :from chidu-compose-workspace
                   :where [:= workspace-id [:bind workspace-id]]]))))
    (chidu-store-sqlite--decode-compose-row row)))

(defun chidu-store-sqlite--select-compose-row-by-remote
    (database account-id remote-email-id)
  "Return DATABASE checkout for ACCOUNT-ID REMOTE-EMAIL-ID, or nil."
  (when-let* ((row
               (car
                (chidu-sql-select database
                  [:select
                   [workspace-id account-id identity-id kind
                                 to-value cc-value bcc-value reply-to-value
                                 subject body resource-ids-json base-remote-email-id
                                 base-remote-blob-id published-revision revision]
                   :from chidu-compose-workspace
                   :where [:and
                           [:= account-id [:bind account-id]]
                           [:= base-remote-email-id [:bind remote-email-id]]]]))))
    (chidu-store-sqlite--decode-compose-row row)))

(defun chidu-store-sqlite--insert-compose-workspace
    (database workspace-id account-id identity-id kind document
              base-remote-email-id base-remote-blob-id
              published-revision revision)
  "Insert one Compose workspace into DATABASE.

WORKSPACE-ID, ACCOUNT-ID, IDENTITY-ID, KIND, DOCUMENT, BASE-REMOTE-EMAIL-ID,
BASE-REMOTE-BLOB-ID, PUBLISHED-REVISION, and REVISION are validated."
  (let ((change-seq
         (chidu-store-sqlite--increment-change-seq database)))
    (chidu-sql-execute database
      [:insert :into chidu-compose-workspace
               :row
               [[workspace-id [:bind workspace-id]]
                [account-id [:bind account-id]]
                [identity-id [:bind identity-id]]
                [kind [:bind (symbol-name kind)]]
                [to-value [:bind (chidu-store-compose-document-to document)]]
                [cc-value [:bind (chidu-store-compose-document-cc document)]]
                [bcc-value [:bind (chidu-store-compose-document-bcc document)]]
                [reply-to-value
                 [:bind (chidu-store-compose-document-reply-to document)]]
                [subject [:bind (chidu-store-compose-document-subject document)]]
                [body [:bind (chidu-store-compose-document-body document)]]
                [resource-ids-json
                 [:bind
                  (chidu-store-sqlite--string-vector-json
                   (chidu-store-compose-document-resource-ids document)
                   "Compose document resource ids")]]
                [base-remote-email-id [:bind base-remote-email-id]]
                [base-remote-blob-id [:bind base-remote-blob-id]]
                [published-revision [:bind published-revision]]
                [revision [:bind revision]]
                [created-change-seq [:bind change-seq]]
                [updated-change-seq [:bind change-seq]]]])
    change-seq))

(defun chidu-store-sqlite--compose-drafts-mailbox (database account-id)
  "Return ACCOUNT-ID's unique available Drafts Mailbox from DATABASE."
  (let ((matches
         (cl-loop
          for mailbox across (chidu-store-sqlite--mailboxes database account-id)
          when (and (chidu-store-mailbox-available-p mailbox)
                    (equal "drafts" (chidu-store-mailbox-role mailbox)))
          collect mailbox)))
    (pcase matches
      (`() nil)
      (`(,mailbox) mailbox)
      (_
       (signal 'chidu-invariant-error
               (list "Account has multiple available Drafts Mailboxes"
                     :account-id account-id))))))

(defun chidu-store-sqlite--compose-context-value (database row)
  "Return one Compose context decoded from internal DATABASE ROW."
  (let* ((workspace-id
          (chidu-store-sqlite--compose-row-workspace-id row))
         (account-id
          (chidu-store-sqlite--compose-row-account-id row))
         (identity-id
          (chidu-store-sqlite--compose-row-identity-id row))
         (location
          (chidu-store-sqlite--account-location database account-id))
         (account (cdr location))
         (identity
          (and account
               (chidu-store-sqlite--compose-identity account identity-id))))
    (unless (and location identity)
      (signal 'chidu-invariant-error
              (list "Compose workspace references unavailable ownership"
                    :workspace-id workspace-id
                    :account-id account-id
                    :identity-id identity-id)))
    (chidu-store-compose-context-create
     :endpoint (car location)
     :account account
     :identity identity
     :drafts-mailbox
     (chidu-store-sqlite--compose-drafts-mailbox database account-id)
     :resources
     (chidu-store-sqlite--compose-resources
      database workspace-id
      (chidu-store-sqlite--compose-row-document row))
     :publish-attempt
     (chidu-store-sqlite--draft-publish-attempt database workspace-id)
     :cleanup-attempts
     (chidu-store-sqlite--draft-cleanup-attempts database workspace-id)
     :workspace
     (chidu-store-compose-workspace-create
      :workspace-id workspace-id
      :account-id account-id
      :identity-id identity-id
      :kind (chidu-store-sqlite--compose-row-kind row)
      :document (chidu-store-sqlite--compose-row-document row)
      :base-remote-email-id
      (chidu-store-sqlite--compose-row-base-remote-email-id row)
      :base-remote-blob-id
      (chidu-store-sqlite--compose-row-base-remote-blob-id row)
      :published-revision
      (chidu-store-sqlite--compose-row-published-revision row)
      :revision (chidu-store-sqlite--compose-row-revision row)))))

(defun chidu-store-sqlite--compose-context (state workspace-id)
  "Read WORKSPACE-ID context from SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (row
          (and (chidu-store-local-id-p workspace-id)
               (chidu-store-sqlite--select-compose-row
                database workspace-id))))
    (if row
        (chidu-result-ok-create
         :value (chidu-store-sqlite--compose-context-value database row))
      (chidu-result-failure-create
       :kind 'unknown-compose-workspace
       :data (list :workspace-id workspace-id)
       :retryable-p nil))))

(defun chidu-store-sqlite--list-compose-workspaces (state)
  "Return every local Compose workspace from SQLite STATE."
  (let ((database (chidu-store-sqlite--assert-open state)))
    (chidu-result-ok-create
     :value
     (vconcat
      (mapcar
       (lambda (row)
         (chidu-store-sqlite--compose-context-value
          database (chidu-store-sqlite--decode-compose-row row)))
       (chidu-sql-select database
         [:select
          [workspace-id account-id identity-id kind
                        to-value cc-value bcc-value reply-to-value
                        subject body resource-ids-json base-remote-email-id
                        base-remote-blob-id published-revision revision]
          :from chidu-compose-workspace
          :order-by [[updated-change-seq :desc] [workspace-id :asc]]]))))))

(defun chidu-store-sqlite--compose-owner-failure
    (database account-id identity-id &optional require-available-p)
  "Return ownership failure for ACCOUNT-ID and IDENTITY-ID in DATABASE.

When REQUIRE-AVAILABLE-P is non-nil, both projections must currently be
available.  Return nil when ownership is valid."
  (let* ((location
          (and (chidu-store-local-id-p account-id)
               (chidu-store-sqlite--account-location database account-id)))
         (account (cdr location))
         (identity
          (and account
               (chidu-store-sqlite--compose-identity account identity-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((null identity)
      (chidu-result-failure-create
       :kind 'unknown-identity
       :data (list :account-id account-id :identity-id identity-id)
       :retryable-p nil))
     ((and require-available-p
           (not (chidu-store-account-available-p account)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((and require-available-p
           (not (chidu-store-identity-available-p identity)))
      (chidu-result-failure-create
       :kind 'identity-unavailable
       :data (list :account-id account-id :identity-id identity-id)
       :retryable-p nil)))))

(defun chidu-store-sqlite--compose-revision-conflict
    (workspace-id expected actual)
  "Return Compose revision conflict for WORKSPACE-ID EXPECTED and ACTUAL."
  (chidu-result-failure-create
   :kind 'revision-conflict
   :data (list :workspace-id workspace-id
               :expected expected :actual actual)
   :retryable-p t))

(defun chidu-store-sqlite--create-compose-workspace (state operation)
  "Create Compose workspace OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (workspace-id
          (chidu-store-op-create-compose-workspace-workspace-id operation))
         (account-id
          (chidu-store-op-create-compose-workspace-account-id operation))
         (identity-id
          (chidu-store-op-create-compose-workspace-identity-id operation))
         (kind (chidu-store-op-create-compose-workspace-kind operation))
         (document
          (chidu-store-op-create-compose-workspace-document operation)))
    (cond
     ((not (chidu-store-local-id-p workspace-id))
      (chidu-result-failure-create
       :kind 'invalid-workspace-id
       :data (list :workspace-id workspace-id)
       :retryable-p nil))
     ((not (chidu-store-sqlite--compose-kind-p kind))
      (chidu-result-failure-create
       :kind 'invalid-compose-kind :data (list :kind kind)
       :retryable-p nil))
     ((not (chidu-store-sqlite--compose-document-p document))
      (chidu-result-failure-create
       :kind 'invalid-compose-document :data nil :retryable-p nil))
     ((> (length (chidu-store-compose-document-resource-ids document)) 0)
      (chidu-result-failure-create
       :kind 'unknown-compose-resource :data (list :workspace-id workspace-id)
       :retryable-p nil))
     ((chidu-store-sqlite--compose-owner-failure
       database account-id identity-id t))
     ((chidu-store-sqlite--select-compose-row database workspace-id)
      (chidu-result-failure-create
       :kind 'compose-workspace-conflict
       :data (list :workspace-id workspace-id)
       :retryable-p nil))
     (t
      (with-sqlite-transaction database
        (chidu-store-sqlite--insert-compose-workspace
         database workspace-id account-id identity-id kind document
         nil nil nil 0))
      (chidu-store-sqlite--compose-context state workspace-id)))))

(defun chidu-store-sqlite--checkpoint-compose-workspace (state operation)
  "CAS-checkpoint Compose workspace OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (workspace-id
          (chidu-store-op-checkpoint-compose-workspace-workspace-id operation))
         (identity-id
          (chidu-store-op-checkpoint-compose-workspace-identity-id operation))
         (expected
          (chidu-store-op-checkpoint-compose-workspace-expected-revision
           operation))
         (revision
          (chidu-store-op-checkpoint-compose-workspace-revision operation))
         (document
          (chidu-store-op-checkpoint-compose-workspace-document operation))
         (row
          (and (chidu-store-local-id-p workspace-id)
               (chidu-store-sqlite--select-compose-row
                database workspace-id))))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-compose-workspace
       :data (list :workspace-id workspace-id)
       :retryable-p nil))
     ((not (chidu-store-sqlite--compose-document-p document))
      (chidu-result-failure-create
       :kind 'invalid-compose-document
       :data (list :workspace-id workspace-id)
       :retryable-p nil))
     ((not (and (integerp expected) (>= expected 0)
                (integerp revision) (> revision expected)))
      (chidu-result-failure-create
       :kind 'invalid-compose-revision
       :data (list :workspace-id workspace-id
                   :expected expected :revision revision)
       :retryable-p nil))
     ((not
       (equal
        (chidu-store-compose-document-resource-ids document)
        (chidu-store-compose-document-resource-ids
         (chidu-store-sqlite--compose-row-document row))))
      (chidu-result-failure-create
       :kind 'compose-resource-membership-conflict
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((let ((resource-result
             (chidu-store-sqlite--compose-document-resources-result
              database workspace-id document)))
        (and (chidu-result-failure-p resource-result) resource-result)))
     ((/= expected (chidu-store-sqlite--compose-row-revision row))
      (chidu-store-sqlite--compose-revision-conflict
       workspace-id expected
       (chidu-store-sqlite--compose-row-revision row)))
     (t
      (let ((owner-failure
             (chidu-store-sqlite--compose-owner-failure
              database
              (chidu-store-sqlite--compose-row-account-id row)
              identity-id)))
        (if owner-failure
            owner-failure
          (with-sqlite-transaction database
            (let ((change-seq
                   (chidu-store-sqlite--increment-change-seq database)))
              (chidu-store-sqlite--update-compose-document
               database row identity-id revision document change-seq)))
          (chidu-store-sqlite--compose-context state workspace-id)))))))

(defun chidu-store-sqlite--discard-compose-workspace (state operation)
  "CAS-discard Compose workspace OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (workspace-id
          (chidu-store-op-discard-compose-workspace-workspace-id operation))
         (expected
          (chidu-store-op-discard-compose-workspace-expected-revision
           operation))
         (row
          (and (chidu-store-local-id-p workspace-id)
               (chidu-store-sqlite--select-compose-row
                database workspace-id))))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-compose-workspace
       :data (list :workspace-id workspace-id)
       :retryable-p nil))
     ((not (and (integerp expected) (>= expected 0)))
      (chidu-result-failure-create
       :kind 'invalid-compose-revision
       :data (list :workspace-id workspace-id :expected expected)
       :retryable-p nil))
     ((/= expected (chidu-store-sqlite--compose-row-revision row))
      (chidu-store-sqlite--compose-revision-conflict
       workspace-id expected
       (chidu-store-sqlite--compose-row-revision row)))
     ((or
       (chidu-store-sqlite--compose-row-base-remote-email-id row)
       (chidu-store-sqlite--draft-publish-attempt database workspace-id)
       (> (length
           (chidu-store-sqlite--draft-cleanup-attempts
            database workspace-id))
          0))
      (chidu-result-failure-create
       :kind 'compose-workspace-has-remote-draft
       :data (list :workspace-id workspace-id)
       :retryable-p nil))
     (t
      (with-sqlite-transaction database
        (chidu-store-sqlite--increment-change-seq database)
        (chidu-sql-execute database
          [:delete :from chidu-compose-workspace
                   :where
                   [:and
                    [:= workspace-id [:bind workspace-id]]
                    [:= revision [:bind expected]]]]))
      (chidu-result-ok-create :value workspace-id)))))

(provide 'chidu-store-sqlite-compose)

;;; chidu-store-sqlite-compose.el ends here
