;;; chidu-mailbox-move-test.el --- Mailbox move contract tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'ert)
(require 'chidu)
(require 'chidu-jmap-email)
(require 'chidu-jmap-mailbox-move)
(require 'chidu-mailbox-move)
(require 'chidu-test-support)

(defun chidu-mailbox-move-test--mailbox-observation
    (remote-id name role)
  "Return one writable Mailbox observation."
  (chidu-store-mailbox-observation-create
   :remote-mailbox-id remote-id
   :name name
   :role role
   :sort-order 0
   :total-emails 3 :unread-emails 3
   :total-threads 3 :unread-threads 3
   :rights
   (chidu-store-mailbox-rights-create
    :may-read-items-p t :may-add-items-p t :may-remove-items-p t
    :may-set-seen-p t :may-set-keywords-p t
    :may-create-child-p t :may-rename-p t :may-delete-p t
    :may-submit-p t)
   :subscribed-p t))

(defun chidu-mailbox-move-test--summary-row (remote-id timestamp)
  "Return one Summary observation row for REMOTE-ID and TIMESTAMP."
  (chidu-store-email-summary-observation-row-create
   :remote-email-id remote-id
   :remote-thread-id (concat "thread-" remote-id)
   :received-at timestamp
   :from-name "Alice" :from-email "alice@example.test"
   :subject remote-id :preview "preview"
   :unread-p t :flagged-p nil :has-attachment-p nil))

(defun chidu-mailbox-move-test--setup (store)
  "Create Account, Mailboxes, canonical Email state, and Search data."
  (let* ((account-id (chidu-store-test--prepare-mailbox-account store))
         (initial
          (chidu-store-test--value
           store
           (chidu-store-op-get-mailbox-sync-context-create
            :account-id account-id)))
         (mailbox-context
          (chidu-store-test--value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id account-id
            :expected-revision
            (chidu-store-mailbox-sync-context-revision initial)
            :observation
            (chidu-store-mailbox-snapshot-observation-create
             :state "mailbox/move"
             :mailboxes
             (vector
              (chidu-mailbox-move-test--mailbox-observation
               "inbox" "Inbox" "inbox")
              (chidu-mailbox-move-test--mailbox-observation
               "archive" "Archive" "archive"))))))
         (account (chidu-store-mailbox-sync-context-account mailbox-context))
         (mailboxes (chidu-store-mailbox-sync-context-mailboxes mailbox-context))
         (inbox
          (cl-find "inbox" mailboxes
                   :key #'chidu-store-mailbox-remote-mailbox-id
                   :test #'equal))
         (archive
          (cl-find "archive" mailboxes
                   :key #'chidu-store-mailbox-remote-mailbox-id
                   :test #'equal))
         (entries
          (vector
           (chidu-store-test--email-entry
            "email-1" "2026-08-25T03:00:00Z"
            :subject "email-1" :preview "preview")
           (chidu-store-test--email-entry
            "email-2" "2026-08-25T02:00:00Z"
            :subject "email-2" :preview "preview")
           (chidu-store-test--email-entry
            "email-3" "2026-08-25T01:00:00Z"
            :subject "email-3" :preview "preview")))
         (_generation
          (chidu-store-test--activate-email-generation
           store account-id entries))
         (summary
          (chidu-store-test--canonical-summary store account-id inbox))
         (summary-rows
          (chidu-store-mailbox-summary-context-rows summary))
         (query-key "move-search")
         (_search
          (chidu-store-test--value
           store
           (chidu-store-op-replace-search-create
            :account-id account-id :query-key query-key
            :expected-revision 0
            :observation
            (chidu-store-search-observation-create
             :query-key query-key :query-text "move"
             :filter-json "{\"text\":\"move\"}"
             :query-state "search/query/move"
             :email-state "search/email/move"
             :cursor-remote-email-id "email-3"
             :maybe-more-p t
             :rows
             (vconcat
              (cl-loop
               for row across
               (vector
                (chidu-mailbox-move-test--summary-row
                 "email-1" "2026-08-25T03:00:00Z")
                (chidu-mailbox-move-test--summary-row
                 "email-2" "2026-08-25T02:00:00Z")
                (chidu-mailbox-move-test--summary-row
                 "email-3" "2026-08-25T01:00:00Z"))
               collect
               (chidu-store-search-observation-row-create
                :summary-row row
                :remote-mailbox-ids (vector "inbox")
                :snippet nil))))))))
    (list :account-id account-id :account account
          :inbox inbox :archive archive :query-key query-key
          :local-ids
          (vconcat
           (cl-loop for row across summary-rows
                    collect
                    (chidu-store-email-summary-row-local-email-id row))))))

(ert-deftest chidu-jmap-mailbox-move-is-one-atomic-per-key-patch ()
  (let* ((intents
          (vector
           (chidu-store-mailbox-move-intent-create
            :local-email-id "11111111-1111-4111-8111-111111111111"
            :remote-email-id "email-1" :phase 'pending)
           (chidu-store-mailbox-move-intent-create
            :local-email-id "22222222-2222-4222-8222-222222222222"
            :remote-email-id "email-2" :phase 'pending)))
         (request
          (chidu-jmap-mailbox-move--request
           "account" "inbox" "archive" intents))
         (arguments (aref (aref (plist-get request :methodCalls) 0) 1))
         (updates (plist-get arguments :update)))
    (should (= 2 (hash-table-count updates)))
    (dolist (remote-id '("email-1" "email-2"))
      (let ((patch (gethash remote-id updates)))
        (should (= 2 (hash-table-count patch)))
        (should (eq t (gethash "mailboxIds/archive" patch)))
        (should (eq :json-null (gethash "mailboxIds/inbox" patch)))))
    (should
     (equal "a~0b~1c"
            (chidu-jmap-patch-path-component "a~b/c" "test path")))))

(ert-deftest chidu-jmap-set-decodes-partial-mailbox-move-exactly ()
  (let ((updated (make-hash-table :test #'equal))
        (not-updated (make-hash-table :test #'equal)))
    (puthash "email-1" :json-null updated)
    (puthash "email-2"
             (list :type "forbidden" :description "denied")
             not-updated)
    (let* ((response
            (chidu-jmap-set-validate-update-response
             (chidu-store-test--method-response
              "Email/set" "mailbox-move"
              (list :accountId "account" :oldState :json-null
                    :newState "state-1"
                    :updated updated :notUpdated not-updated))
             "Email/set" "mailbox-move" "account"
             (vector "email-1" "email-2")))
           (results (chidu-jmap-set-update-response-results response)))
      (should (eq 'succeeded
                  (chidu-jmap-set-target-result-outcome (aref results 0))))
      (should (eq 'rejected
                  (chidu-jmap-set-target-result-outcome (aref results 1))))
      (should (equal "forbidden"
                     (chidu-jmap-set-target-result-error-kind
                      (aref results 1)))))))

(ert-deftest chidu-jmap-set-rejects-malformed-normal-response-shapes ()
  (let ((updated (make-hash-table :test #'equal)))
    (puthash "email-1" :json-null updated)
    (dolist
        (arguments
         (list
          (list :accountId "account" :newState "state-1"
                :updated updated)
          (list :accountId "account" :oldState :json-null
                :updated updated)
          (list :accountId "account" :oldState :json-null
                :newState :json-null :updated updated)
          (list :accountId "account" :oldState :json-null
                :newState "state-1" :updated
                (let ((invalid (make-hash-table :test #'equal)))
                  (puthash "email-1" "invalid" invalid)
                  invalid))))
      (should-error
       (chidu-jmap-set-validate-update-response
        (chidu-store-test--method-response
         "Email/set" "mailbox-move" arguments)
        "Email/set" "mailbox-move" "account" (vector "email-1"))
       :type 'chidu-jmap-error))))

(ert-deftest chidu-jmap-email-mutable-reconciliation-is-exact ()
  (let ((email (make-hash-table :test #'equal))
        (mailboxes (make-hash-table :test #'equal))
        (keywords (make-hash-table :test #'equal)))
    (puthash "inbox" t mailboxes)
    (puthash "$seen" t keywords)
    (puthash "id" "email-1" email)
    (puthash "mailboxIds" mailboxes email)
    (puthash "keywords" keywords email)
    (let* ((state
            (chidu-jmap-email--validate-mutable-state
             (chidu-store-test--method-response
              "Email/get" "email-mutable"
              (list :accountId "account" :state "state-1"
                    :list (vector email) :notFound (vector "email-2")))
             "account" (vector "email-1" "email-2")))
           (targets (chidu-jmap-email-mutable-state-targets state))
           (found (aref targets 0))
           (missing (aref targets 1)))
      (should (equal "state-1"
                     (chidu-jmap-email-mutable-state-state state)))
      (should (chidu-jmap-email-mutable-target-found-p found))
      (should (chidu-jmap-email-mutable-target-seen-p found))
      (should
       (equal (vector "inbox")
              (chidu-jmap-email-mutable-target-remote-mailbox-ids found)))
      (should-not (chidu-jmap-email-mutable-target-found-p missing)))))

(ert-deftest chidu-mailbox-move-batch-obeys-max-objects-in-set ()
  (let* ((endpoint
          (chidu-store-endpoint-create
           :endpoint-id "endpoint" :session-url "https://example.test/jmap"
           :login "me@example.test" :authentication 'basic
           :api-url "https://example.test/api"
           :max-size-request 1048576
           :max-objects-in-get 10 :max-objects-in-set 2))
         (account
          (chidu-store-account-create
           :account-id "account" :remote-account-id "remote-account"
           :name "Mail" :available-p t))
         (mailbox
          (lambda (local remote)
            (chidu-store-mailbox-create
             :mailbox-id local :remote-mailbox-id remote
             :name remote :available-p t)))
         (context
          (chidu-store-mailbox-move-context-create
           :endpoint endpoint :account account :operation-id "operation"
           :source-mailbox (funcall mailbox "source" "inbox")
           :destination-mailbox (funcall mailbox "destination" "archive")))
         (intents
          (vconcat
           (cl-loop for index from 1 to 3
                    collect
                    (chidu-store-mailbox-move-intent-create
                     :local-email-id (format "local-%d" index)
                     :remote-email-id (format "email-%d" index)
                     :phase 'pending)))))
    (should (= 2 (length (chidu-mailbox-move--batch context intents))))
    (let* ((one (cl-subseq intents 0 1))
           (one-size
            (chidu-jmap-mailbox-move-request-size context one))
           (one-context
            (chidu-store-mailbox-move-context-with
             context
             :endpoint
             (chidu-store-endpoint-with
              endpoint :max-size-request one-size)))
           (too-small-context
            (chidu-store-mailbox-move-context-with
             context
             :endpoint
             (chidu-store-endpoint-with
              endpoint :max-size-request (1- one-size)))))
      (should (= 1
                 (length
                  (chidu-mailbox-move--batch one-context intents))))
      (should (zerop
               (length
                (chidu-mailbox-move--batch
                 too-small-context intents)))))))

(ert-deftest chidu-store-mailbox-move-is-optimistic-partial-and-durable ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-mailbox-move-" t)) store fixture)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root)
                fixture (chidu-mailbox-move-test--setup store))
          (let* ((account-id (plist-get fixture :account-id))
                 (inbox (plist-get fixture :inbox))
                 (archive (plist-get fixture :archive))
                 (query-key (plist-get fixture :query-key))
                 (ids (plist-get fixture :local-ids))
                 (operation-id (chidu-store-new-local-id))
                 (accepted
                  (chidu-store-test--value
                   store
                   (chidu-store-op-accept-mailbox-move-create
                    :account-id account-id :operation-id operation-id
                    :source-mailbox-id
                    (chidu-store-mailbox-mailbox-id inbox)
                    :destination-mailbox-id
                    (chidu-store-mailbox-mailbox-id archive)
                    :local-email-ids (cl-subseq ids 0 2))))
                 (pending-summary
                  (chidu-store-test--canonical-summary
                   store account-id inbox))
                 (pending-search
                  (chidu-store-test--value
                   store
                   (chidu-store-op-get-search-create
                    :account-id account-id :query-key query-key))))
            (should
             (= 2
                (length
                 (chidu-store-mailbox-move-context-intents
                  (chidu-store-mailbox-move-result-context accepted)))))
            (should
             (equal (list (aref ids 2))
                    (cl-loop
                     for row across
                     (chidu-store-mailbox-summary-context-rows
                      pending-summary)
                     collect
                     (chidu-store-email-summary-row-local-email-id row))))
            (should (= 1 (length (chidu-store-search-context-rows
                                  pending-search))))
            (let* ((settled
                    (chidu-store-test--value
                     store
                     (chidu-store-op-settle-mailbox-move-create
                      :account-id account-id :operation-id operation-id
                      :outcomes
                      (vector
                       (chidu-store-mailbox-move-target-outcome-create
                        :local-email-id (aref ids 0) :outcome 'succeeded)
                       (chidu-store-mailbox-move-target-outcome-create
                        :local-email-id (aref ids 1) :outcome 'rejected
                        :error-kind "forbidden")))))
                   (inbox-summary
                    (chidu-store-test--canonical-summary
                     store account-id inbox))
                   (archive-summary
                    (chidu-store-test--canonical-summary
                     store account-id archive))
                   (search
                    (chidu-store-test--value
                     store
                     (chidu-store-op-get-search-create
                      :account-id account-id :query-key query-key))))
              (should-not
               (chidu-store-mailbox-move-context-operation-id
                (chidu-store-mailbox-move-result-context settled)))
              (should (= 2 (length
                            (chidu-store-mailbox-move-result-changes
                             settled))))
              (should
               (equal (list (aref ids 1) (aref ids 2))
                      (cl-loop
                       for row across
                       (chidu-store-mailbox-summary-context-rows
                        inbox-summary)
                       collect
                       (chidu-store-email-summary-row-local-email-id row))))
              (should
               (equal (list (aref ids 0))
                      (cl-loop
                       for row across
                       (chidu-store-mailbox-summary-context-rows
                        archive-summary)
                       collect
                       (chidu-store-email-summary-row-local-email-id row))))
              (should (chidu-store-search-context-stale-p search))
              (should
               (equal (list (aref ids 1) (aref ids 2))
                      (cl-loop
                       for row across (chidu-store-search-context-rows search)
                       collect
                       (chidu-store-email-summary-row-local-email-id
                        (chidu-store-search-row-summary-row row))))))
            ;; Unknown settlement remains hidden and survives Store reopen.
            (let ((unknown-operation (chidu-store-new-local-id)))
              (chidu-store-test--value
               store
               (chidu-store-op-accept-mailbox-move-create
                :account-id account-id :operation-id unknown-operation
                :source-mailbox-id
                (chidu-store-mailbox-mailbox-id inbox)
                :destination-mailbox-id
                (chidu-store-mailbox-mailbox-id archive)
                :local-email-ids (vector (aref ids 1))))
              (chidu-store-test--value
               store
               (chidu-store-op-settle-mailbox-move-create
                :account-id account-id :operation-id unknown-operation
                :outcomes
                (vector
                 (chidu-store-mailbox-move-target-outcome-create
                  :local-email-id (aref ids 1) :outcome 'unknown
                  :error-kind "transport-unavailable"))))
              (chidu-store-close store)
              (setq store (chidu-store-sqlite-create root))
              (let ((context
                     (chidu-store-test--value
                      store
                      (chidu-store-op-get-mailbox-move-context-create
                       :account-id account-id))))
                (should
                 (equal unknown-operation
                        (chidu-store-mailbox-move-context-operation-id
                         context)))
                (should
                 (= 1
                    (length
                     (chidu-store-mailbox-move-context-intents context))))
                (should
                 (eq 'unknown
                     (chidu-store-mailbox-move-intent-phase
                      (aref
                       (chidu-store-mailbox-move-context-intents context)
                       0))))
                (should
                 (equal (list (aref ids 2))
                        (cl-loop
                         for row across
                         (chidu-store-mailbox-summary-context-rows
                          (chidu-store-test--canonical-summary
                           store account-id inbox))
                         collect
                         (chidu-store-email-summary-row-local-email-id
                          row))))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-mailbox-move-workflow-is-owned-by-app-lifecycle ()
  (let* ((store
          (chidu-store-capability-create
           :name 'fake :invoke-function #'ignore
           :inspect-function #'ignore :close-function #'ignore))
         (runtime (chidu-runtime-open :store store))
         (account (chidu-store-account-create :account-id "account"))
         app)
    (unwind-protect
        (progn
          (setq app
                (appkit-app-start
                 chidu--app-type :identity (make-symbol "mailbox-move-owner")
                 :input (chidu--state-create)))
          (appkit-app-send app (list :runtime runtime))
          (chidu-mailbox-move--start-workflow
           app account "operation" #'ignore #'ignore t nil)
          (let ((table (chidu-app-requests app)))
            (appkit-app-close app)
            (should (zerop (hash-table-count table)))
            (should (chidu-runtime-closed-p runtime))))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (unless (chidu-runtime-closed-p runtime)
        (chidu-runtime-close runtime)))))

(provide 'chidu-mailbox-move-test)

;;; chidu-mailbox-move-test.el ends here
