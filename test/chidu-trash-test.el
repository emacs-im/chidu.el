;;; chidu-trash-test.el --- Move-to-Trash contract tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'ert)
(require 'chidu)
(require 'chidu-jmap-trash)
(require 'chidu-test-support)
(require 'chidu-trash)

(defun chidu-trash-test--rights (may-add-p may-remove-p)
  "Return fake Mailbox rights with MAY-ADD-P and MAY-REMOVE-P."
  (chidu-store-mailbox-rights-create
   :may-read-items-p t
   :may-add-items-p may-add-p
   :may-remove-items-p may-remove-p
   :may-set-seen-p t
   :may-set-keywords-p t
   :may-create-child-p t
   :may-rename-p t
   :may-delete-p t
   :may-submit-p t))

(defun chidu-trash-test--mailbox
    (remote-id name role may-add-p may-remove-p)
  "Return writable-shape Mailbox observation for REMOTE-ID, NAME, and ROLE."
  (chidu-store-mailbox-observation-create
   :remote-mailbox-id remote-id
   :name name
   :role role
   :sort-order 0
   :total-emails 3 :unread-emails 3
   :total-threads 3 :unread-threads 3
   :rights (chidu-trash-test--rights may-add-p may-remove-p)
   :subscribed-p t))

(defun chidu-trash-test--summary-row (remote-id timestamp)
  "Return Summary observation row for REMOTE-ID and TIMESTAMP."
  (chidu-store-email-summary-observation-row-create
   :remote-email-id remote-id
   :remote-thread-id (concat "thread-" remote-id)
   :received-at timestamp
   :from-name "Alice" :from-email "alice@example.test"
   :subject remote-id :preview "preview"
   :unread-p t :flagged-p nil :has-attachment-p nil))

(cl-defun chidu-trash-test--setup
    (store &key (trash-add-p t) (custom-remove-p t))
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
             :state "mailbox/trash"
             :mailboxes
             (vector
              (chidu-trash-test--mailbox
               "inbox" "Inbox" "inbox" t t)
              (chidu-trash-test--mailbox
               "custom" "Custom" nil t custom-remove-p)
              (chidu-trash-test--mailbox
               "trash" "Trash" "trash" trash-add-p t))))))
         (account (chidu-store-mailbox-sync-context-account mailbox-context))
         (mailboxes (chidu-store-mailbox-sync-context-mailboxes mailbox-context))
         (inbox
          (cl-find "inbox" mailboxes
                   :key #'chidu-store-mailbox-remote-mailbox-id :test #'equal))
         (custom
          (cl-find "custom" mailboxes
                   :key #'chidu-store-mailbox-remote-mailbox-id :test #'equal))
         (trash
          (cl-find "trash" mailboxes
                   :key #'chidu-store-mailbox-remote-mailbox-id :test #'equal))
         (entries
          (vector
           (chidu-store-test--email-entry
            "email-1" "2026-08-25T03:00:00Z"
            :subject "email-1" :preview "preview"
            :mailbox-ids (vector "inbox" "custom"))
           (chidu-store-test--email-entry
            "email-2" "2026-08-25T02:00:00Z"
            :subject "email-2" :preview "preview"
            :mailbox-ids (vector "inbox" "custom"))
           (chidu-store-test--email-entry
            "email-3" "2026-08-25T01:00:00Z"
            :subject "email-3" :preview "preview"
            :mailbox-ids (vector "inbox"))))
         (_generation
          (chidu-store-test--activate-email-generation
           store account-id entries))
         (summary
          (chidu-store-test--canonical-summary store account-id inbox))
         (local-ids
          (vconcat
           (cl-loop
            for row across (chidu-store-mailbox-summary-context-rows summary)
            collect (chidu-store-email-summary-row-local-email-id row))))
         (rows
          (vector
           (chidu-trash-test--summary-row
            "email-1" "2026-08-25T03:00:00Z")
           (chidu-trash-test--summary-row
            "email-2" "2026-08-25T02:00:00Z")
           (chidu-trash-test--summary-row
            "email-3" "2026-08-25T01:00:00Z")))
         (query-key "trash-search")
         (_search
          (chidu-store-test--value
           store
           (chidu-store-op-replace-search-create
            :account-id account-id :query-key query-key
            :expected-revision 0
            :observation
            (chidu-store-search-observation-create
             :query-key query-key :query-text "trash"
             :filter-json "{\"text\":\"trash\"}"
             :query-state "search/query/trash"
             :email-state "search/email/trash"
             :cursor-remote-email-id "email-3"
             :maybe-more-p t
             :rows
             (vconcat
              (cl-loop
               for row across rows
               for index from 0
               collect
               (chidu-store-search-observation-row-create
                :summary-row row
                :remote-mailbox-ids
                (if (< index 2)
                    (vector "inbox" "custom")
                  (vector "inbox"))
                :snippet nil))))))))
    (list :account-id account-id :account account
          :inbox inbox :custom custom :trash trash
          :query-key query-key :local-ids local-ids)))

(defun chidu-trash-test--evidence (local-id remote-id mailboxes)
  "Return found Trash evidence for LOCAL-ID, REMOTE-ID, and MAILBOXES."
  (chidu-store-trash-target-evidence-create
   :local-email-id local-id
   :remote-email-id remote-id
   :found-p t
   :remote-mailbox-ids mailboxes))

(ert-deftest chidu-jmap-trash-replaces-complete-mailbox-set-and-bounds-batches ()
  (let* ((endpoint
          (chidu-store-endpoint-create
           :endpoint-id "endpoint"
           :session-url "https://example.test/jmap"
           :login "me@example.test" :authentication 'basic
           :api-url "https://example.test/api"
           :max-size-request 1048576
           :max-objects-in-get 2 :max-objects-in-set 2))
         (account
          (chidu-store-account-create
           :account-id "account" :remote-account-id "remote-account"
           :name "Mail" :available-p t))
         (trash
          (chidu-store-mailbox-create
           :mailbox-id "trash-local" :remote-mailbox-id "trash/remote"
           :name "Trash" :role "trash" :available-p t))
         (intents
          (vconcat
           (cl-loop
            for index from 1 to 3
            collect
            (chidu-store-trash-intent-create
             :local-email-id (format "local-%d" index)
             :remote-email-id (format "email-%d" index)
             :original-remote-mailbox-ids
             (vector "inbox" "custom")
             :phase 'pending))))
         (context
          (chidu-store-trash-context-create
           :endpoint endpoint :account account
           :operation-id "operation" :trash-mailbox trash
           :intents intents))
         (request
           (chidu-jmap-trash--request
            "remote-account" "trash/remote" intents))
         (arguments (aref (aref (plist-get request :methodCalls) 0) 1))
         (updates (plist-get arguments :update)))
    (should (= 3 (hash-table-count updates)))
    (maphash
     (lambda (_remote-id patch)
       (should (= 1 (hash-table-count patch)))
       (let ((mailboxes (gethash "mailboxIds" patch)))
         (should (hash-table-p mailboxes))
         (should (= 1 (hash-table-count mailboxes)))
         (should (eq t (gethash "trash/remote" mailboxes)))))
     updates)
    (should (= 2 (length (chidu-trash--set-batch context intents))))
    (should (= 2 (length (chidu-trash--hydration-batch context intents))))
    (let* ((one (cl-subseq intents 0 1))
           (one-size (chidu-jmap-trash-request-size context one))
           (small-context
            (chidu-store-trash-context-with
             context
             :endpoint
             (chidu-store-endpoint-with
              endpoint :max-size-request (1- one-size)))))
      (should (zerop
               (length (chidu-trash--set-batch small-context intents)))))))

(ert-deftest chidu-store-trash-is-optimistic-partial-and-durable ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-trash-" t)) store fixture)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root)
                fixture (chidu-trash-test--setup store))
          (let* ((account-id (plist-get fixture :account-id))
                 (inbox (plist-get fixture :inbox))
                 (custom (plist-get fixture :custom))
                 (trash (plist-get fixture :trash))
                 (query-key (plist-get fixture :query-key))
                 (ids (plist-get fixture :local-ids))
                 (operation-id (chidu-store-new-local-id))
                 (accepted
                  (chidu-store-test--value
                   store
                   (chidu-store-op-accept-trash-create
                    :account-id account-id :operation-id operation-id
                    :trash-mailbox-id
                    (chidu-store-mailbox-mailbox-id trash)
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
                 (chidu-store-trash-context-intents
                  (chidu-store-trash-result-context accepted)))))
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
            (let* ((evidenced
                    (chidu-store-test--value
                     store
                     (chidu-store-op-record-trash-evidence-create
                      :account-id account-id :operation-id operation-id
                      :evidence
                      (vector
                       (chidu-trash-test--evidence
                        (aref ids 0) "email-1"
                        (vector "inbox" "custom"))
                       (chidu-trash-test--evidence
                        (aref ids 1) "email-2"
                        (vector "inbox" "custom"))))))
                   (intents
                    (chidu-store-trash-context-intents
                     (chidu-store-trash-result-context evidenced))))
              (should
               (equal (vector "inbox" "custom")
                      (chidu-store-trash-intent-original-remote-mailbox-ids
                       (aref intents 0)))))
            (let* ((settled
                    (chidu-store-test--value
                     store
                     (chidu-store-op-settle-trash-create
                      :account-id account-id :operation-id operation-id
                      :outcomes
                      (vector
                       (chidu-store-trash-target-outcome-create
                        :local-email-id (aref ids 0) :outcome 'succeeded)
                       (chidu-store-trash-target-outcome-create
                        :local-email-id (aref ids 1) :outcome 'rejected
                        :error-kind "forbidden")))))
                   (inbox-summary
                    (chidu-store-test--canonical-summary
                     store account-id inbox))
                   (custom-summary
                    (chidu-store-test--canonical-summary
                     store account-id custom))
                   (trash-summary
                    (chidu-store-test--canonical-summary
                     store account-id trash))
                   (search
                    (chidu-store-test--value
                     store
                     (chidu-store-op-get-search-create
                      :account-id account-id :query-key query-key))))
              (should-not
               (chidu-store-trash-context-operation-id
                (chidu-store-trash-result-context settled)))
              (should
               (equal (list (aref ids 1) (aref ids 2))
                      (cl-loop
                       for row across
                       (chidu-store-mailbox-summary-context-rows
                        inbox-summary)
                       collect
                       (chidu-store-email-summary-row-local-email-id row))))
              (should
               (equal (list (aref ids 1))
                      (cl-loop
                       for row across
                       (chidu-store-mailbox-summary-context-rows
                        custom-summary)
                       collect
                       (chidu-store-email-summary-row-local-email-id row))))
              (should
               (equal (list (aref ids 0))
                      (cl-loop
                       for row across
                       (chidu-store-mailbox-summary-context-rows
                        trash-summary)
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
            ;; An uncertain target remains hidden across Store restart.
            (let ((unknown-operation (chidu-store-new-local-id)))
              (chidu-store-test--value
               store
               (chidu-store-op-accept-trash-create
                :account-id account-id :operation-id unknown-operation
                :trash-mailbox-id
                (chidu-store-mailbox-mailbox-id trash)
                :local-email-ids (vector (aref ids 1))))
              (chidu-store-test--value
               store
               (chidu-store-op-record-trash-evidence-create
                :account-id account-id :operation-id unknown-operation
                :evidence
                (vector
                 (chidu-trash-test--evidence
                  (aref ids 1) "email-2"
                  (vector "inbox" "custom")))))
              (chidu-store-test--value
               store
               (chidu-store-op-settle-trash-create
                :account-id account-id :operation-id unknown-operation
                :outcomes
                (vector
                 (chidu-store-trash-target-outcome-create
                  :local-email-id (aref ids 1) :outcome 'unknown
                  :error-kind "serverPartialFail"))))
              (chidu-store-close store)
              (setq store (chidu-store-sqlite-create root))
              (let* ((context
                      (chidu-store-test--value
                       store
                       (chidu-store-op-get-trash-context-create
                        :account-id account-id)))
                     (intent
                      (aref (chidu-store-trash-context-intents context) 0)))
                (should
                 (equal unknown-operation
                        (chidu-store-trash-context-operation-id context)))
                (should (eq 'unknown
                            (chidu-store-trash-intent-phase intent)))
                (should
                 (equal (vector "inbox" "custom")
                        (chidu-store-trash-intent-original-remote-mailbox-ids
                         intent)))
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

(ert-deftest chidu-store-trash-enforces-destination-and-known-source-rights ()
  (when (sqlite-available-p)
    (let ((store (chidu-test-store-create)))
      (unwind-protect
          (let* ((fixture
                  (chidu-trash-test--setup
                   store :trash-add-p nil :custom-remove-p nil))
                 (account-id (plist-get fixture :account-id))
                 (trash (plist-get fixture :trash))
                 (ids (plist-get fixture :local-ids))
                 (forbidden
                  (chidu-store-test--store-call
                   store
                   (chidu-store-op-accept-trash-create
                    :account-id account-id
                    :operation-id (chidu-store-new-local-id)
                    :trash-mailbox-id
                    (chidu-store-mailbox-mailbox-id trash)
                    :local-email-ids (vector (aref ids 0))))))
            (should (chidu-result-failure-p forbidden))
            (should (eq 'trash-destination-forbidden
                        (chidu-result-failure-kind forbidden))))
        (chidu-store-close store)))
    (let ((store (chidu-test-store-create)))
      (unwind-protect
          (let* ((fixture
                  (chidu-trash-test--setup store :custom-remove-p nil))
                 (account-id (plist-get fixture :account-id))
                 (trash (plist-get fixture :trash))
                 (ids (plist-get fixture :local-ids))
                 (operation-id (chidu-store-new-local-id))
                 (_accepted
                  (chidu-store-test--store-call
                   store
                   (chidu-store-op-accept-trash-create
                    :account-id account-id :operation-id operation-id
                    :trash-mailbox-id
                    (chidu-store-mailbox-mailbox-id trash)
                    :local-email-ids (vector (aref ids 0)))))
                 (evidenced
                  (chidu-result-ok-value
                   (chidu-store-test--store-call
                    store
                    (chidu-store-op-record-trash-evidence-create
                     :account-id account-id :operation-id operation-id
                     :evidence
                     (vector
                      (chidu-trash-test--evidence
                       (aref ids 0) "email-1"
                       (vector "inbox" "custom"))))))))
            (should-not
             (chidu-store-trash-context-operation-id
              (chidu-store-trash-result-context evidenced)))
            (let ((change (aref (chidu-store-trash-result-changes evidenced) 0)))
              (should (eq 'reverted
                          (chidu-store-trash-target-change-phase change)))
              (should (equal "sourceForbidden"
                             (chidu-store-trash-target-change-error-kind
                              change)))))
        (chidu-store-close store)))))

(ert-deftest chidu-trash-server-partial-fail-reconciles-before-retry ()
  (let* ((store (chidu-test-store-create))
         (fixture (chidu-trash-test--setup store))
         (account (plist-get fixture :account))
         (trash (plist-get fixture :trash))
         (local-id (aref (plist-get fixture :local-ids) 0))
         (runtime (chidu-runtime-open :store store))
         events callbacks completed failure)
    (unwind-protect
        (let ((app (appkit-app-start chidu--app-type :input (chidu--state-create) :identity (make-symbol "trash-test"))))
          (unwind-protect
              (progn
                (appkit-app-send app (list :runtime runtime))
                (cl-letf
                    (((symbol-function 'auth-source-search)
                      (lambda (&rest _arguments)
                        (list (list :secret (lambda () "secret")))))
                     ((symbol-function 'chidu-jmap-email-fetch-mutable-state)
                      (lambda (_endpoint _account remote-ids _secret deliver)
                        (setq events
                              (append events
                                      (list (list 'get (aref remote-ids 0))))
                              callbacks (append callbacks (list deliver)))
                        #'ignore))
                     ((symbol-function 'chidu-jmap-move-to-trash-batch)
                      (lambda (_context secret intents deliver)
                        (clear-string secret)
                        (setq events
                              (append
                               events
                               (list
                                (list
                                 'set
                                 (chidu-store-trash-intent-remote-email-id
                                  (aref intents 0)))))
                              callbacks (append callbacks (list deliver)))
                        #'ignore)))
                  (chidu-trash-emails
                   app account trash (vector local-id)
                   (lambda (_context) (setq completed t))
                   (lambda (error) (setq failure error)))
                  (let ((get-callback (pop callbacks)))
                    (funcall
                     get-callback
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-email-mutable-state-create
                       :state "preflight"
                       :targets
                       (vector
                        (chidu-jmap-email-mutable-target-create
                         :remote-id "email-1" :found-p t
                         :remote-mailbox-ids (vector "inbox" "custom")
                         :seen-p nil))))))
                  (let ((set-callback (pop callbacks)))
                    (funcall
                     set-callback
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-set-update-response-create
                       :results
                       (vector
                        (chidu-jmap-set-target-result-create
                         :remote-id "email-1" :outcome 'unknown
                         :error-kind "serverPartialFail"))))))
                  (let ((get-callback (pop callbacks)))
                    (funcall
                     get-callback
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-email-mutable-state-create
                       :state "reconciled"
                       :targets
                       (vector
                        (chidu-jmap-email-mutable-target-create
                         :remote-id "email-1" :found-p t
                         :remote-mailbox-ids (vector "inbox" "custom")
                         :seen-p nil))))))
                  (should
                   (equal '((get "email-1") (set "email-1")
                            (get "email-1") (set "email-1"))
                          events))
                  (let ((set-callback (pop callbacks)))
                    (funcall
                     set-callback
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-set-update-response-create
                       :results
                       (vector
                        (chidu-jmap-set-target-result-create
                         :remote-id "email-1" :outcome 'succeeded))))))
                  (should completed)
                  (should-not failure)
                  (let ((context
                         (chidu-result-ok-value
                          (chidu-store-test--store-call
                           store
                           (chidu-store-op-get-trash-context-create
                            :account-id
                            (chidu-store-account-account-id account))))))
                    (should-not (chidu-store-trash-context-operation-id context))))
                (when (appkit-app-live-p app) (appkit-app-close app))))
          (when runtime (chidu-runtime-close runtime))))))

(ert-deftest chidu-sqlite-does-not-implicitly-migrate-missing-trash-tables ()
  (when (sqlite-available-p)
    (let* ((root (make-temp-file "chidu-trash-format-" t))
           (path (expand-file-name "store.sqlite3" root))
           (store nil)
           database)
      (set-file-modes root #o700)
      (unwind-protect
          (progn
            (setq store (chidu-store-sqlite-create root))
            (chidu-store-close store)
            (setq store nil
                  database (sqlite-open path))
            (sqlite-execute database "PRAGMA foreign_keys = OFF")
            (sqlite-execute database "DROP TABLE jmap_trash_target")
            (sqlite-close database)
            (setq database nil)
            (should-error
             (chidu-store-sqlite-create root)
             :type 'chidu-invariant-error)
            (setq database (sqlite-open path))
            (should-not
             (sqlite-select
              database
              "SELECT 1 FROM sqlite_master
                WHERE type = 'table' AND name = 'jmap_trash_target'")))
        (when store (chidu-store-close store))
        (when database (sqlite-close database))
        (when (file-directory-p root) (delete-directory root t))))))

(ert-deftest chidu-trash-workflow-is-owned-by-app-lifecycle ()
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
                 chidu--app-type :identity (make-symbol "trash-owner")
                 :input (chidu--state-create)))
          (appkit-app-send app (list :runtime runtime))
          (chidu-trash--start-workflow
           app account "operation" #'ignore #'ignore t nil)
          (let ((table (chidu-app-requests app)))
            (appkit-app-close app)
            (should (zerop (hash-table-count table)))
            (should (chidu-runtime-closed-p runtime))))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (unless (chidu-runtime-closed-p runtime)
        (chidu-runtime-close runtime)))))

(provide 'chidu-trash-test)

;;; chidu-trash-test.el ends here
