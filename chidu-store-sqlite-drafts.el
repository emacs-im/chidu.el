;;; chidu-store-sqlite-drafts.el --- Canonical Drafts and checkout -*- lexical-binding: t; -*-

;;; Commentary:

;; Derive a bounded Drafts view directly from the active Email generation and
;; create idempotent local Compose checkouts for exact canonical Draft Emails.
;; Server content remains immutable; a checkout is only the editable local head.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'chidu-result)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-compose)
(require 'chidu-store-sqlite-compose-resource)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)
(require 'chidu-store-sqlite-generation)

(defun chidu-store-sqlite--draft-workspace-overlays (database account-id)
  "Return remote Email keyed checkout overlays from DATABASE for ACCOUNT-ID."
  (let ((phases (make-hash-table :test #'equal))
        (overlays (make-hash-table :test #'equal)))
    (dolist (row
             (chidu-sql-select database
               [:select [workspace-id phase]
                :from chidu-draft-publish-attempt
                :where [:and
                        [:= account-id [:bind account-id]]
                        [:in phase
                             [[:literal "pending"] [:literal "unknown"]]]]
                :order-by [[created-change-seq :asc] [attempt-id :asc]]]))
      (pcase-let ((`(,workspace-id ,phase) row))
        (when (gethash workspace-id phases)
          (signal 'chidu-invariant-error
                  (list "Compose workspace has multiple active Draft attempts"
                        :workspace-id workspace-id)))
        (puthash workspace-id (intern phase) phases)))
    (dolist (row
             (chidu-sql-select database
               [:select
                [workspace-id base-remote-email-id revision published-revision]
                :from chidu-compose-workspace
                :where [:and
                        [:= account-id [:bind account-id]]
                        [:is-not base-remote-email-id nil]]
                :order-by [[workspace-id :asc]]]))
      (pcase-let ((`(,workspace-id ,remote-email-id ,revision ,published) row))
        (when (gethash remote-email-id overlays)
          (signal 'chidu-invariant-error
                  (list "Remote Draft has multiple local checkouts"
                        :account-id account-id
                        :remote-email-id remote-email-id)))
        (puthash
         remote-email-id
         (list :workspace-id workspace-id
               :revision revision
               :published-revision published
               :publish-phase (gethash workspace-id phases))
         overlays)))
    overlays))

(defun chidu-store-sqlite--draft-records
    (database account-id generation-id mailbox limit)
  "Return LIMIT plus one canonical Draft records from DATABASE.

ACCOUNT-ID and GENERATION-ID identify active Email state.  MAILBOX must be the
Account's Drafts Mailbox."
  (let* ((remote-mailbox-id
          (chidu-store-mailbox-remote-mailbox-id mailbox))
         (overlays
          (chidu-store-sqlite--draft-workspace-overlays database account-id)))
    (chidu-sql-map database
        [:select
         [[local-id membership:local-email-id]
          [remote-id email:remote-email-id]
          [thread-id metadata:remote-thread-id]
          [received-at metadata:received-at]
          [from-json metadata:from-json]
          [to-json metadata:to-json]
          [cc-json metadata:cc-json]
          [bcc-json metadata:bcc-json]
          [subject metadata:subject]
          [preview [:call coalesce preview:value [:literal ""]]]
          [has-attachment metadata:has-attachment]]
         :from [:as jmap-email-generation-mailbox membership]
         :joins
         [[:inner [:as jmap-email-generation-keyword draft]
           :on [:and
                [:= draft:account-id membership:account-id]
                [:= draft:generation-id membership:generation-id]
                [:= draft:local-email-id membership:local-email-id]
                [:= draft:keyword [:literal "$draft"]]]]
          [:inner [:as jmap-email-record email]
           :on [:and
                [:= email:account-id membership:account-id]
                [:= email:local-email-id membership:local-email-id]]]
          [:inner [:as jmap-email-metadata metadata]
           :on [:and
                [:= metadata:account-id membership:account-id]
                [:= metadata:local-email-id membership:local-email-id]]]
          [:left [:as jmap-email-preview preview]
           :on [:and
                [:= preview:account-id membership:account-id]
                [:= preview:local-email-id membership:local-email-id]]]]
         :where [:and
                 [:= membership:account-id [:bind account-id]]
                 [:= membership:generation-id [:bind generation-id]]
                 [:= membership:remote-mailbox-id [:bind remote-mailbox-id]]]
         :order-by [[metadata:received-at :desc]
                    [membership:local-email-id :desc]]
         :limit [:bind (1+ limit)]]
      (let* ((summary
              (chidu-store-sqlite--canonical-summary-row
               local-id remote-id thread-id received-at
               from-json subject preview nil nil nil has-attachment))
             (overlay (gethash remote-id overlays)))
        (chidu-store-draft-row-create
         :summary-row summary
         :recipients
         (vconcat
          (chidu-store-sqlite--email-address-vector-from-json
           to-json "Canonical Draft To recipients")
          (chidu-store-sqlite--email-address-vector-from-json
           cc-json "Canonical Draft Cc recipients")
          (chidu-store-sqlite--email-address-vector-from-json
           bcc-json "Canonical Draft Bcc recipients"))
         :workspace-id (plist-get overlay :workspace-id)
         :workspace-revision (plist-get overlay :revision)
         :published-revision (plist-get overlay :published-revision)
         :publish-phase (plist-get overlay :publish-phase))))))

(defun chidu-store-sqlite--canonical-draft-p
    (database account-id generation-id mailbox local-email-id remote-email-id)
  "Return whether DATABASE identifies the exact canonical Draft.

ACCOUNT-ID, GENERATION-ID, MAILBOX, LOCAL-EMAIL-ID, and REMOTE-EMAIL-ID supply
the required identity evidence."
  (chidu-sql-one database
      [:select [[present 1]]
       :from [:as jmap-email-generation-member member]
       :joins
       [[:inner [:as jmap-email-record email]
         :on [:and
              [:= email:account-id member:account-id]
              [:= email:local-email-id member:local-email-id]]]]
       :where
       [:and
        [:= member:account-id [:bind account-id]]
        [:= member:generation-id [:bind generation-id]]
        [:= member:local-email-id [:bind local-email-id]]
        [:= email:remote-email-id [:bind remote-email-id]]
        [:exists
         [:select [1]
          :from [:as jmap-email-generation-mailbox membership]
          :where [:and
                  [:= membership:account-id member:account-id]
                  [:= membership:generation-id member:generation-id]
                  [:= membership:local-email-id member:local-email-id]
                  [:= membership:remote-mailbox-id
                      [:bind (chidu-store-mailbox-remote-mailbox-id mailbox)]]]]]
        [:exists
         [:select [1]
          :from [:as jmap-email-generation-keyword keyword]
          :where [:and
                  [:= keyword:account-id member:account-id]
                  [:= keyword:generation-id member:generation-id]
                  [:= keyword:local-email-id member:local-email-id]
                  [:= keyword:keyword [:literal "$draft"]]]]]]
       :limit 1]
    present))

(defun chidu-store-sqlite--drafts-context
    (state account-id mailbox-id limit)
  "Return ACCOUNT-ID MAILBOX-ID canonical Drafts from SQLite STATE."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'positive-integer-p limit)))
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id)))
         (mailbox
          (and location (stringp mailbox-id)
               (chidu-store-sqlite--mailbox-by-id
                database account-id mailbox-id)))
         (generation-id
          (and location
               (chidu-store-sqlite--active-email-generation-id
                database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((null mailbox)
      (chidu-result-failure-create
       :kind 'unknown-mailbox
       :data (list :account-id account-id :mailbox-id mailbox-id)
       :retryable-p nil))
     ((not (and (chidu-store-mailbox-available-p mailbox)
                (equal "drafts" (chidu-store-mailbox-role mailbox))))
      (chidu-result-failure-create
       :kind 'mailbox-not-drafts
       :data (list :account-id account-id :mailbox-id mailbox-id)
       :retryable-p nil))
     ((not (chidu-store-mailbox-rights-may-read-items-p
            (chidu-store-mailbox-rights mailbox)))
      (chidu-result-failure-create
       :kind 'mailbox-read-forbidden
       :data (list :account-id account-id :mailbox-id mailbox-id)
       :retryable-p nil))
     ((null generation-id)
      (chidu-result-failure-create
       :kind 'email-index-unavailable
       :data (list :account-id account-id) :retryable-p nil))
     (t
      (let* ((records
              (chidu-store-sqlite--draft-records
               database account-id generation-id mailbox limit))
             (count (length records)))
        (chidu-result-ok-create
         :value
         (chidu-store-drafts-context-create
          :endpoint (car location)
          :account (cdr location)
          :mailbox mailbox
          :revision (chidu-store-sqlite--change-seq database)
          :maybe-more-p (> count limit)
          :rows (vconcat (cl-subseq records 0 (min count limit))))))))))

(defun chidu-store-sqlite--checkout-draft (state operation)
  "Create or recover Draft checkout OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (workspace-id
          (chidu-store-op-checkout-draft-workspace-id operation))
         (account-id (chidu-store-op-checkout-draft-account-id operation))
         (identity-id (chidu-store-op-checkout-draft-identity-id operation))
         (mailbox-id
          (chidu-store-op-checkout-draft-drafts-mailbox-id operation))
         (local-email-id
          (chidu-store-op-checkout-draft-local-email-id operation))
         (remote-email-id
          (chidu-store-op-checkout-draft-remote-email-id operation))
         (remote-blob-id
          (chidu-store-op-checkout-draft-remote-blob-id operation))
         (document (chidu-store-op-checkout-draft-document operation))
         (resources (chidu-store-op-checkout-draft-resources operation))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id)))
         (mailbox
          (and location (stringp mailbox-id)
               (chidu-store-sqlite--mailbox-by-id
                database account-id mailbox-id)))
         (generation-id
          (and location
               (chidu-store-sqlite--active-email-generation-id
                database account-id)))
         (existing
          (and location (stringp remote-email-id)
               (chidu-store-sqlite--select-compose-row-by-remote
                database account-id remote-email-id))))
    (cond
     (existing
      (chidu-store-sqlite--compose-context
       state (chidu-store-sqlite--compose-row-workspace-id existing)))
     ((not (chidu-store-local-id-p workspace-id))
      (chidu-result-failure-create
       :kind 'invalid-workspace-id :data (list :workspace-id workspace-id)
       :retryable-p nil))
     ((not (chidu-store-local-id-p local-email-id))
      (chidu-result-failure-create
       :kind 'invalid-local-email-id :data (list :local-email-id local-email-id)
       :retryable-p nil))
     ((not (and (stringp remote-blob-id)
                (not (string-empty-p remote-blob-id))
                (not (string-match-p "\0" remote-blob-id))))
      (chidu-result-failure-create
       :kind 'invalid-remote-blob-id
       :data (list :remote-blob-id remote-blob-id)
       :retryable-p nil))
     ((not (chidu-store-sqlite--compose-document-p document))
      (chidu-result-failure-create
       :kind 'invalid-compose-document :data nil :retryable-p nil))
     ((not (chidu-store-sqlite--compose-resource-observations-match-p
            resources document t t))
      (chidu-result-failure-create
       :kind 'invalid-compose-resource
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((chidu-store-sqlite--compose-owner-failure
       database account-id identity-id t))
     ((null mailbox)
      (chidu-result-failure-create
       :kind 'unknown-mailbox
       :data (list :account-id account-id :mailbox-id mailbox-id)
       :retryable-p nil))
     ((not (and (chidu-store-mailbox-available-p mailbox)
                (equal "drafts" (chidu-store-mailbox-role mailbox))))
      (chidu-result-failure-create
       :kind 'mailbox-not-drafts
       :data (list :account-id account-id :mailbox-id mailbox-id)
       :retryable-p nil))
     ((null generation-id)
      (chidu-result-failure-create
       :kind 'email-index-unavailable
       :data (list :account-id account-id) :retryable-p nil))
     ((not (chidu-store-sqlite--canonical-draft-p
            database account-id generation-id mailbox
            local-email-id remote-email-id))
      (chidu-result-failure-create
       :kind 'draft-no-longer-canonical
       :data (list :account-id account-id
                   :local-email-id local-email-id
                   :remote-email-id remote-email-id)
       :retryable-p t))
     ((chidu-store-sqlite--select-compose-row database workspace-id)
      (chidu-result-failure-create
       :kind 'compose-workspace-conflict
       :data (list :workspace-id workspace-id) :retryable-p nil))
     (t
      (with-sqlite-transaction database
        (let ((change-seq
               (chidu-store-sqlite--insert-compose-workspace
                database workspace-id account-id identity-id 'draft document
                remote-email-id remote-blob-id 0 0)))
          (cl-loop
           for resource across resources
           do
           (chidu-store-sqlite--insert-compose-resource-observation
            database workspace-id resource change-seq))))
      (chidu-store-sqlite--compose-context state workspace-id)))))

(provide 'chidu-store-sqlite-drafts)

;;; chidu-store-sqlite-drafts.el ends here
