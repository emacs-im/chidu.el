;;; chidu-store-sqlite.el --- SQLite Store backend -*- lexical-binding: t; -*-

;;; Commentary:

;; Thin composition root for Chidu's sole Store implementation.
;; The explicit exhaustive dispatch remains centralized here.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)
(require 'chidu-store-sqlite-compose)
(require 'chidu-store-sqlite-compose-resource)
(require 'chidu-store-sqlite-draft)
(require 'chidu-store-sqlite-drafts)
(require 'chidu-store-sqlite-schema)
(require 'chidu-store-sqlite-sync)
(require 'chidu-store-sqlite-hydration)
(require 'chidu-store-sqlite-catchup)
(require 'chidu-store-sqlite-generation)
(require 'chidu-store-sqlite-mutation)
(require 'chidu-store-sqlite-trash)
(require 'chidu-store-sqlite-materialization)

(defun chidu-store-sqlite-create (data-root)
  "Open Chidu's persistent SQLite Store at DATA-ROOT."
  (unless (sqlite-available-p)
    (signal 'chidu-invariant-error '("SQLite is unavailable")))
  (let* ((root (chidu-store-sqlite--prepare-root data-root))
         (candidate-path (expand-file-name "store.sqlite3" root))
         (fresh-p (not (file-exists-p candidate-path)))
         (owner-path (chidu-store-sqlite--prepare-owner-database root))
         (owner-database (chidu-store-sqlite--acquire-owner owner-path))
         (database-path (chidu-store-sqlite--prepare-database root))
         database
         state)
    (condition-case error-data
        (progn
          (setq database (sqlite-open database-path))
          (chidu-store-sqlite--configure-database database)
          (if fresh-p
              (with-sqlite-transaction database
                (chidu-store-sqlite--initialize-schema database)
                (chidu-store-sqlite--initialize-metadata database))
            (chidu-store-sqlite--assert-current-schema database))
          (let* ((metadata (chidu-store-sqlite--metadata database))
                 (store-id (nth 0 metadata)))
            (unless (chidu-store-local-id-p store-id)
              (signal 'chidu-invariant-error
                      (list "SQLite store id is not a canonical UUID")))
            (unless (chidu-store-sqlite--private-file-p database-path)
              (signal 'chidu-invariant-error
                      '("SQLite database changed owner or mode")))
            (setq state
                  (chidu-store-sqlite-state-create
                   :root root
                   :owner-path owner-path
                   :owner-connection owner-database
                   :database-path database-path
                   :connection database
                   :store-id store-id))))
      (error
       (when database (ignore-errors (sqlite-close database)))
       (chidu-store-sqlite--release-owner owner-database)
       (signal (car error-data) (cdr error-data))))
    (chidu-store-capability-create
     :name 'sqlite
     :invoke-function
     (lambda (operation deliver)
       (unless (functionp deliver)
         (signal 'wrong-type-argument (list 'functionp deliver)))
       (let ((result
              (pcase-exhaustive operation
                ((cl-struct chidu-store-op-runtime)
                 (chidu-result-ok-create
                  :value (chidu-store-sqlite--runtime state)))
                ((cl-struct chidu-store-op-list-endpoints)
                 (chidu-result-ok-create
                  :value
                  (chidu-store-sqlite--list-endpoints
                   (chidu-store-sqlite--assert-open state))))
                ((cl-struct chidu-store-op-list-compose-workspaces)
                 (chidu-store-sqlite--list-compose-workspaces state))
                ((cl-struct chidu-store-op-get-compose-workspace
                            (workspace-id workspace-id))
                 (chidu-store-sqlite--compose-context state workspace-id))
                ((cl-struct chidu-store-op-get-drafts
                            (account-id account-id)
                            (mailbox-id mailbox-id)
                            (limit limit))
                 (chidu-store-sqlite--drafts-context
                  state account-id mailbox-id limit))
                ((cl-struct chidu-store-op-checkout-draft)
                 (chidu-store-sqlite--checkout-draft state operation))
                ((cl-struct chidu-store-op-create-compose-workspace)
                 (chidu-store-sqlite--create-compose-workspace
                  state operation))
                ((cl-struct chidu-store-op-add-compose-resource)
                 (chidu-store-sqlite--add-compose-resource state operation))
                ((cl-struct chidu-store-op-remove-compose-resource)
                 (chidu-store-sqlite--remove-compose-resource state operation))
                ((cl-struct chidu-store-op-set-compose-resource-blob)
                 (chidu-store-sqlite--set-compose-resource-blob
                  state operation))
                ((cl-struct chidu-store-op-checkpoint-compose-workspace)
                 (chidu-store-sqlite--checkpoint-compose-workspace
                  state operation))
                ((cl-struct chidu-store-op-accept-draft-publish)
                 (chidu-store-sqlite--accept-draft-publish state operation))
                ((cl-struct chidu-store-op-mark-draft-publish-unknown)
                 (chidu-store-sqlite--mark-draft-publish-unknown
                  state operation))
                ((cl-struct chidu-store-op-retry-draft-publish-create)
                 (chidu-store-sqlite--retry-draft-publish-create
                  state operation))
                ((cl-struct chidu-store-op-settle-draft-publish-create)
                 (chidu-store-sqlite--settle-draft-publish-create
                  state operation))
                ((cl-struct chidu-store-op-settle-draft-publish-cleanup)
                 (chidu-store-sqlite--settle-draft-publish-cleanup
                  state operation))
                ((cl-struct chidu-store-op-discard-compose-workspace)
                 (chidu-store-sqlite--discard-compose-workspace
                  state operation))
                ((cl-struct chidu-store-op-configure-endpoint)
                 (chidu-result-ok-create
                  :value (chidu-store-sqlite--configure state operation)))
                ((cl-struct chidu-store-op-observe-session)
                 (chidu-store-sqlite--observe-session state operation))
                ((cl-struct chidu-store-op-get-mailbox-sync-context
                            (account-id account-id))
                 (chidu-store-sqlite--mailbox-context state account-id))
                ((cl-struct chidu-store-op-list-mailboxes
                            (account-id account-id))
                 (chidu-store-sqlite--mailbox-context state account-id))
                ((cl-struct chidu-store-op-observe-mailbox-snapshot)
                 (chidu-store-sqlite--observe-mailbox-snapshot
                  state operation))
                ((cl-struct chidu-store-op-get-email-sync-context
                            (account-id account-id))
                 (chidu-store-sqlite--email-context state account-id))
                ((cl-struct chidu-store-op-begin-email-bootstrap)
                 (chidu-store-sqlite--begin-email-bootstrap state operation))
                ((cl-struct chidu-store-op-append-email-query-chunk)
                 (chidu-store-sqlite--append-email-query-chunk
                  state operation))
                ((cl-struct chidu-store-op-restart-email-bootstrap)
                 (chidu-store-sqlite--restart-email-bootstrap
                  state operation))
                ((cl-struct chidu-store-op-apply-email-membership-changes)
                 (chidu-store-sqlite--apply-email-membership-changes
                  state operation))
                ((cl-struct chidu-store-op-get-email-hydration-plan
                            (account-id account-id)
                            (limit limit))
                 (chidu-store-sqlite--email-hydration-plan
                  state account-id limit))
                ((cl-struct chidu-store-op-apply-email-hydration)
                 (chidu-store-sqlite--apply-email-hydration
                  state operation))
                ((cl-struct chidu-store-op-finish-email-hydration)
                 (chidu-store-sqlite--finish-email-hydration
                  state operation))
                ((cl-struct chidu-store-op-apply-email-catchup-round)
                 (chidu-store-sqlite--apply-email-catchup-round
                  state operation))
                ((cl-struct chidu-store-op-activate-email-generation)
                 (chidu-store-sqlite--activate-email-generation
                  state operation))
                ((cl-struct chidu-store-op-get-active-email-rows
                            (account-id account-id)
                            (local-email-ids local-email-ids))
                 (chidu-store-sqlite--active-email-rows
                  state account-id local-email-ids))
                ((cl-struct chidu-store-op-get-mailbox-summary
                            (account-id account-id)
                            (mailbox-id mailbox-id)
                            (limit limit))
                 (chidu-store-sqlite--mailbox-summary-context
                  state account-id mailbox-id limit))
                ((cl-struct chidu-store-op-get-search
                            (account-id account-id)
                            (query-key query-key))
                 (chidu-store-sqlite--search-context
                  state account-id query-key))
                ((cl-struct chidu-store-op-replace-search)
                 (chidu-store-sqlite--replace-search state operation))
                ((cl-struct chidu-store-op-append-search)
                 (chidu-store-sqlite--append-search state operation))
                ((cl-struct chidu-store-op-list-seen-intents
                            (account-id account-id))
                 (chidu-store-sqlite--seen-context state account-id))
                ((cl-struct chidu-store-op-accept-seen-intent)
                 (chidu-store-sqlite--accept-seen-intent state operation))
                ((cl-struct chidu-store-op-settle-seen-intent)
                 (chidu-store-sqlite--settle-seen-intent state operation))
                ((cl-struct chidu-store-op-get-mailbox-move-context
                            (account-id account-id))
                 (chidu-store-sqlite--mailbox-move-context
                  state account-id))
                ((cl-struct chidu-store-op-accept-mailbox-move)
                 (chidu-store-sqlite--accept-mailbox-move state operation))
                ((cl-struct chidu-store-op-settle-mailbox-move)
                 (chidu-store-sqlite--settle-mailbox-move state operation))
                ((cl-struct chidu-store-op-get-trash-context
                            (account-id account-id))
                 (chidu-store-sqlite--trash-context state account-id))
                ((cl-struct chidu-store-op-accept-trash)
                 (chidu-store-sqlite--accept-trash state operation))
                ((cl-struct chidu-store-op-record-trash-evidence)
                 (chidu-store-sqlite--record-trash-evidence state operation))
                ((cl-struct chidu-store-op-settle-trash)
                 (chidu-store-sqlite--settle-trash state operation))
                ((cl-struct chidu-store-op-get-email-body
                            (account-id account-id)
                            (local-email-id local-email-id)
                            (remote-email-id remote-email-id))
                 (chidu-store-sqlite--email-body-context
                  state account-id local-email-id remote-email-id))
                ((cl-struct chidu-store-op-replace-email-body)
                 (chidu-store-sqlite--replace-email-body
                  state operation))
                ((cl-struct chidu-store-op-get-parsed-blob
                            (account-id account-id)
                            (blob-id blob-id)
                            (profile-version profile-version))
                 (chidu-store-sqlite--parsed-blob-context
                  state account-id blob-id profile-version))
                ((cl-struct chidu-store-op-replace-parsed-blob)
                 (chidu-store-sqlite--replace-parsed-blob
                  state operation))
                ((cl-struct chidu-store-op-get-conversation
                            (account-id account-id)
                            (remote-thread-id remote-thread-id))
                 (chidu-store-sqlite--conversation-context
                  state account-id remote-thread-id))
                ((cl-struct chidu-store-op-replace-conversation)
                 (chidu-store-sqlite--replace-conversation
                  state operation)))))
         (funcall deliver result))
       nil)
     :inspect-function
     (lambda ()
       (let ((runtime (chidu-store-sqlite--runtime state)))
         (list :backend 'sqlite
               :root (chidu-store-sqlite-state-root state)
               :store-id (chidu-store-runtime-store-id runtime)
               :change-seq (chidu-store-runtime-change-seq runtime)
               :owner-lock 'sqlite-exclusive-transaction
               :strict-native-open-p nil)))
     :close-function
     (lambda () (chidu-store-sqlite--close state)))))

(provide 'chidu-store-sqlite)

;;; chidu-store-sqlite.el ends here
