;;; chidu-email-catchup-test.el --- Canonical Email catch-up tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'ert)
(require 'sqlite)
(require 'chidu-jmap-email-catchup)
(require 'chidu-email-sync)
(require 'chidu-store-sqlite)
(require 'chidu-test-support)

(defun chidu-email-catchup-test--value (store operation)
  "Return successful STORE OPERATION value."
  (let ((result (chidu-store-test--store-call store operation)))
    (should (chidu-result-ok-p result))
    (chidu-result-ok-value result)))

(defun chidu-email-catchup-test--mailbox-snapshot ()
  "Return one readable Inbox snapshot."
  (chidu-store-mailbox-snapshot-observation-create
   :state "mailboxes"
   :mailboxes
   (vector
    (chidu-store-mailbox-observation-create
     :remote-mailbox-id "inbox" :name "Inbox" :role "inbox"
     :sort-order 10 :total-emails 4 :unread-emails 1
     :total-threads 4 :unread-threads 1
     :rights
     (chidu-store-mailbox-rights-create
      :may-read-items-p t :may-add-items-p t :may-remove-items-p t
      :may-set-seen-p t :may-set-keywords-p t
      :may-create-child-p t :may-rename-p t :may-delete-p t
      :may-submit-p t)
     :subscribed-p t)
    (chidu-store-mailbox-observation-create
     :remote-mailbox-id "archive" :name "Archive" :role "archive"
     :sort-order 20 :total-emails 0 :unread-emails 0
     :total-threads 0 :unread-threads 0
     :rights
     (chidu-store-mailbox-rights-create
      :may-read-items-p t :may-add-items-p t :may-remove-items-p t
      :may-set-seen-p t :may-set-keywords-p t
      :may-create-child-p t :may-rename-p t :may-delete-p t
      :may-submit-p t)
     :subscribed-p t))))

(defun chidu-email-catchup-test--hydration
    (kind state remote-ids &optional seen-id remote-mailbox-id)
  "Return KIND hydration at STATE for REMOTE-IDS.

When SEEN-ID matches a target, include the $seen keyword.  REMOTE-MAILBOX-ID
defaults to Inbox."
  (chidu-store-email-hydration-observation-create
   :kind kind :state state
   :results
   (vconcat
    (cl-loop
     for remote-id across remote-ids
     collect
     (chidu-store-email-hydration-result-create
      :remote-email-id remote-id :found-p t
      :metadata
      (and (eq kind 'full)
           (chidu-store-test--email-metadata remote-id))
      :preview (and (eq kind 'full) (concat "Preview " remote-id))
      :remote-mailbox-ids (vector (or remote-mailbox-id "inbox"))
      :keywords (if (equal remote-id seen-id)
                    (vector "$seen")
                  (vector)))))))

(defun chidu-email-catchup-test--setup (store)
  "Return metadata-catchup context in STORE with three hydrated Emails."
  (let* ((account-id (chidu-store-test--prepare-mailbox-account store))
         (_mailboxes
          (chidu-email-catchup-test--value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id account-id :expected-revision 0
            :observation (chidu-email-catchup-test--mailbox-snapshot))))
         (context
          (chidu-email-catchup-test--value
           store
           (chidu-store-op-begin-email-bootstrap-create
            :account-id account-id :expected-revision 0
            :state "e0" :profile-version "metadata-v1")))
         (generation-id
          (chidu-store-email-sync-context-generation-id context))
         (baseline (vector "email-keep" "email-update" "email-destroy")))
    (dolist (ids (list baseline (vector)))
      (setq
       context
       (chidu-email-catchup-test--value
        store
        (chidu-store-op-append-email-query-chunk-create
         :account-id account-id :generation-id generation-id
         :expected-revision
         (chidu-store-email-sync-context-revision context)
         :observation
         (chidu-store-email-query-page-observation-create
          :query-state "query" :can-calculate-changes-p t
          :position (chidu-store-email-sync-context-committed-count context)
          :remote-email-ids ids)))))
    (setq
     context
     (chidu-email-catchup-test--value
      store
      (chidu-store-op-apply-email-membership-changes-create
       :account-id account-id :generation-id generation-id
       :expected-revision
       (chidu-store-email-sync-context-revision context)
       :expected-state "e0"
       :observation
       (chidu-store-email-changes-observation-create
        :old-state "e0" :new-state "e1"))))
    (let ((plan
           (chidu-email-catchup-test--value
            store
            (chidu-store-op-get-email-hydration-plan-create
             :account-id account-id :limit 10))))
      (setq
       context
       (chidu-email-catchup-test--value
        store
        (chidu-store-op-apply-email-hydration-create
         :account-id account-id :generation-id generation-id
         :expected-revision
         (chidu-store-email-sync-context-revision context)
         :observation
         (chidu-store-test--hydration-observation plan "e1")))))
    (chidu-email-catchup-test--value
     store
     (chidu-store-op-finish-email-hydration-create
      :account-id account-id :generation-id generation-id
      :expected-revision
      (chidu-store-email-sync-context-revision context)))))

(ert-deftest chidu-jmap-email-catchup-batches-full-and-mutable-get ()
  (let* ((created (vector "created"))
         (updated (vector "updated"))
         (request
           (chidu-jmap-email-catchup--request
            "remote-account" created updated))
         (calls (plist-get request :methodCalls))
         (mailboxes (make-hash-table :test #'equal))
         (keywords (make-hash-table :test #'equal)))
    (puthash "inbox" t mailboxes)
    (puthash "$seen" t keywords)
    (should (= 2 (length calls)))
    (should
     (equal chidu-jmap-email-hydration-full-properties
            (plist-get (aref (aref calls 0) 1) :properties)))
    (should
     (equal chidu-jmap-email-hydration-mutable-properties
            (plist-get (aref (aref calls 1) 1) :properties)))
    (let* ((bytes
            (chidu-store-test--payload
             `(:sessionState "session"
               :methodResponses
               [["Email/get"
                 (:accountId "remote-account" :state "e3"
                  :list
                  [(:id "created" :blobId "blob-created"
                    :threadId "thread-created"
                    :mailboxIds ,mailboxes :keywords ,keywords :size 42
                    :receivedAt "2026-08-26T12:00:00Z"
                    :from [(:name "Alice" :email "alice@example.test")]
                    :subject "Subject created"
                    :messageId ["mid-created"]
                    :hasAttachment :json-false)]
                  :notFound [])
                 "email-catchup-created"]
                ["Email/get"
                 (:accountId "remote-account" :state "e2"
                  :list [(:id "updated" :mailboxIds ,mailboxes
                          :keywords ,keywords)]
                  :notFound [])
                 "email-catchup-updated"]])))
           (decoded
            (chidu-jmap-email-catchup--decode
             bytes "remote-account" created updated))
           (full (chidu-jmap-email-catchup-hydration-full decoded))
           (mutable (chidu-jmap-email-catchup-hydration-mutable decoded)))
      (should (equal "e3"
                     (chidu-store-email-hydration-observation-state full)))
      (should (equal "e2"
                     (chidu-store-email-hydration-observation-state mutable)))
      (should
       (equal (chidu-store-test--email-metadata "created")
              (chidu-store-email-hydration-result-metadata
               (aref
                (chidu-store-email-hydration-observation-results full) 0))))
      (should
       (equal (vector "$seen")
              (chidu-store-email-hydration-result-keywords
               (aref
                (chidu-store-email-hydration-observation-results mutable)
                0)))))))

(ert-deftest chidu-store-email-catchup-closes-by-state-and-activates ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-email-catchup-" t)) store account-id)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (let* ((context (chidu-email-catchup-test--setup store))
                 (_account-id
                  (setq account-id
                        (chidu-store-account-account-id
                         (chidu-store-email-sync-context-account context))))
                 (generation-id
                  (chidu-store-email-sync-context-generation-id context)))
            ;; Full get is already at e3 while this changes page only closes e2.
            ;; Commit its cache values, but do not publish the generation yet.
            (setq
             context
             (chidu-store-email-round-result-context
              (chidu-email-catchup-test--value
               store
               (chidu-store-op-apply-email-catchup-round-create
                :account-id account-id :generation-id generation-id
                :expected-revision
                (chidu-store-email-sync-context-revision context)
                :expected-state "e1"
                :observation
                (chidu-store-email-catchup-observation-create
                 :changes
                 (chidu-store-email-changes-observation-create
                  :old-state "e1" :new-state "e2"
                  :created (vector "email-created")
                  :updated (vector "email-update")
                  :destroyed (vector "email-destroy"))
                 :full
                 (chidu-email-catchup-test--hydration
                  'full "e3" (vector "email-created"))
                 :mutable
                 (chidu-email-catchup-test--hydration
                  'mutable "e2" (vector "email-update") "email-update"))))))
            (should (eq 'metadata-catchup
                        (chidu-store-email-sync-context-phase context)))
            (should (equal "e2"
                           (chidu-store-email-sync-context-state context)))
            ;; The durable checkpoint, not an in-memory page, owns resume.
            (chidu-store-close store)
            (setq store (chidu-store-sqlite-create root)
                  context
                  (chidu-email-catchup-test--value
                   store
                   (chidu-store-op-get-email-sync-context-create
                    :account-id account-id)))
            (setq
             context
             (chidu-store-email-round-result-context
              (chidu-email-catchup-test--value
               store
               (chidu-store-op-apply-email-catchup-round-create
                :account-id account-id :generation-id generation-id
                :expected-revision
                (chidu-store-email-sync-context-revision context)
                :expected-state "e2"
                :observation
                (chidu-store-email-catchup-observation-create
                 :changes
                 (chidu-store-email-changes-observation-create
                  :old-state "e2" :new-state "e3"))))))
            (should (eq 'activating
                        (chidu-store-email-sync-context-phase context)))
            (setq
             context
             (chidu-email-catchup-test--value
              store
              (chidu-store-op-activate-email-generation-create
               :account-id account-id :generation-id generation-id
               :expected-revision
               (chidu-store-email-sync-context-revision context)
               :expected-state "e3")))
            (should (eq 'live
                        (chidu-store-email-sync-context-phase context)))
            (should (equal "e3"
                           (chidu-store-email-sync-context-state context)))
            (let ((database
                   (sqlite-open (expand-file-name "store.sqlite3" root))))
              (unwind-protect
                  (progn
                    (should
                     (equal
                      '("active")
                      (mapcar
                       #'car
                       (sqlite-select
                        database
                        "SELECT lifecycle FROM jmap_email_generation
                           WHERE generation_id = ?"
                        (list generation-id)))))
                    (should
                     (equal
                      '("email-created" "email-keep" "email-update")
                      (mapcar
                       #'car
                       (sqlite-select
                        database
                        "SELECT email.remote_email_id
                           FROM jmap_email_generation_member AS member
                           JOIN jmap_email_record AS email
                             ON email.account_id = member.account_id
                            AND email.local_email_id = member.local_email_id
                          WHERE member.generation_id = ?
                          ORDER BY email.remote_email_id"
                        (list generation-id)))))
                    (should
                     (= 1
                        (caar
                         (sqlite-select
                          database
                          "SELECT count(*)
                             FROM jmap_email_generation_keyword AS keyword
                             JOIN jmap_email_record AS email
                               ON email.account_id = keyword.account_id
                              AND email.local_email_id = keyword.local_email_id
                            WHERE keyword.generation_id = ?
                              AND email.remote_email_id = 'email-update'
                              AND keyword.keyword = '$seen'"
                          (list generation-id))))))
                (sqlite-close database)))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-email-live-gap-drops-unprovable-notification-candidates ()
  (let ((run
         (chidu-email-run-create
          :mode 'live
          :new-local-email-ids (list "candidate")
          :new-emails (vector 'candidate)
          :truncated-p t))
        restarted)
    (cl-letf (((symbol-function 'chidu-email-run--read-state)
               (lambda (actual-run restart-p)
                 (should (eq run actual-run))
                 (setq restarted restart-p))))
      (chidu-email-run--after-catchup-changes
       run
       (chidu-result-failure-create
        :kind 'cannot-calculate-changes :retryable-p nil)))
    (should restarted)
    (should-not (chidu-email-run-new-local-email-ids run))
    (should (equal (vector) (chidu-email-run-new-emails run)))
    (should-not (chidu-email-run-truncated-p run))
    (should (chidu-email-run-rebuilt-p run))))

(ert-deftest chidu-email-live-sync-returns-final-canonical-new-mail-state ()
  (skip-unless (sqlite-available-p))
  (let ((store (chidu-test-store-create)) runtime result failure
        (changes-calls 0) (hydration-calls 0))
    (unwind-protect
        (let* ((account-id
                (chidu-store-test--prepare-mailbox-account store))
               (mailbox-context
                (chidu-email-catchup-test--value
                 store
                 (chidu-store-op-observe-mailbox-snapshot-create
                  :account-id account-id :expected-revision 0
                  :observation
                  (chidu-email-catchup-test--mailbox-snapshot))))
               (account
                (chidu-store-mailbox-sync-context-account mailbox-context))
               (mailboxes
                (chidu-store-mailbox-sync-context-mailboxes mailbox-context))
               (inbox
                (cl-find "inbox" mailboxes
                         :key #'chidu-store-mailbox-remote-mailbox-id
                         :test #'equal))
               (archive
                (cl-find "archive" mailboxes
                         :key #'chidu-store-mailbox-remote-mailbox-id
                         :test #'equal))
               (_active
                (chidu-store-test--activate-email-generation
                 store account-id
                 (vector
                  (chidu-store-test--email-entry
                   "email-existing" "2026-08-26T10:00:00Z"))
                 "e0"))
               (_search
                (chidu-email-catchup-test--value
                 store
                 (chidu-store-op-replace-search-create
                  :account-id account-id :query-key "live-search"
                  :expected-revision 0
                  :observation
                  (chidu-store-search-observation-create
                   :query-key "live-search" :query-text "live"
                   :filter-json "{}" :query-state "q0"
                   :email-state "e0" :maybe-more-p nil))))
               (new-metadata
                (chidu-store-email-metadata-with
                 (chidu-store-test--email-metadata "email-new")
                 :received-at "2026-08-27T01:02:03Z"))
               (gone-metadata
                (chidu-store-email-metadata-with
                 (chidu-store-test--email-metadata "email-gone")
                 :received-at "2026-08-27T01:01:03Z")))
          (setq runtime (chidu-runtime-open :store store))
          (cl-letf
              (((symbol-function 'auth-source-search)
                (lambda (&rest _arguments)
                  (list (list :secret (lambda () "live-secret")))))
               ((symbol-function 'chidu-jmap-email-fetch-changes-page)
                (lambda (context _secret _limit deliver)
                  (cl-incf changes-calls)
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value
                    (pcase changes-calls
                      (1
                       (should
                        (equal "e0"
                               (chidu-store-email-sync-context-state context)))
                       (chidu-jmap-email-changes-page-create
                        :session-state "session" :old-state "e0"
                        :new-state "e1" :has-more-changes-p t
                        :created (vector "email-new" "email-gone")))
                      (2
                       (should
                        (equal "e1"
                               (chidu-store-email-sync-context-state context)))
                       (chidu-jmap-email-changes-page-create
                        :session-state "session" :old-state "e1"
                        :new-state "e2" :has-more-changes-p nil
                        :updated (vector "email-new")
                        :destroyed (vector "email-gone")))
                      (_ (ert-fail "unexpected live Email/changes request")))))
                  #'ignore))
               ((symbol-function
                 'chidu-jmap-email-fetch-catchup-hydration)
                (lambda (_context _secret created updated deliver)
                  (cl-incf hydration-calls)
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value
                    (pcase hydration-calls
                      (1
                       (should
                        (equal (vector "email-new" "email-gone") created))
                       (should (zerop (length updated)))
                       (chidu-jmap-email-catchup-hydration-create
                        :full
                        (chidu-store-email-hydration-observation-create
                         :kind 'full :state "e1"
                         :results
                         (vector
                          (chidu-store-email-hydration-result-create
                           :remote-email-id "email-new" :found-p t
                           :metadata new-metadata :preview "new preview"
                           :remote-mailbox-ids (vector "inbox")
                           :keywords (vector))
                          (chidu-store-email-hydration-result-create
                           :remote-email-id "email-gone" :found-p t
                           :metadata gone-metadata :preview "gone preview"
                           :remote-mailbox-ids (vector "inbox")
                           :keywords (vector))))))
                      (2
                       (should (zerop (length created)))
                       (should (equal (vector "email-new") updated))
                       (chidu-jmap-email-catchup-hydration-create
                        :mutable
                        (chidu-email-catchup-test--hydration
                         'mutable "e2" (vector "email-new")
                         "email-new" "archive")))
                      (_ (ert-fail "unexpected live Email/get request")))))
                  #'ignore)))
            (chidu-email-sync-live
             runtime account
             (lambda (value) (setq result value))
             (lambda (value) (setq failure value))))
          (should-not failure)
          (should (chidu-email-live-result-p result))
          (should (= 2 changes-calls))
          (should (= 2 hydration-calls))
          (should-not (chidu-email-live-result-truncated-p result))
          (should (chidu-email-live-result-changed-p result))
          (should-not (chidu-email-live-result-rebuilt-p result))
          (let* ((context (chidu-email-live-result-context result))
                 (rows (chidu-email-live-result-new-emails result))
                 (new-row (aref rows 0))
                 (summary (chidu-store-new-email-row-summary-row new-row))
                 (inbox-summary
                  (chidu-store-test--canonical-summary
                   store account-id inbox))
                 (archive-summary
                  (chidu-store-test--canonical-summary
                   store account-id archive))
                 (search
                  (chidu-email-catchup-test--value
                   store
                   (chidu-store-op-get-search-create
                    :account-id account-id :query-key "live-search"))))
            (should (eq 'live
                        (chidu-store-email-sync-context-phase context)))
            (should (equal "e2"
                           (chidu-store-email-sync-context-state context)))
            (should (= 2
                       (chidu-store-email-sync-context-committed-count
                        context)))
            (should (= 1 (length rows)))
            (should (equal "email-new"
                           (chidu-store-email-summary-row-remote-email-id
                            summary)))
            (should-not
             (chidu-store-email-summary-row-unread-p summary))
            (should
             (equal (vector "archive")
                    (chidu-store-new-email-row-remote-mailbox-ids new-row)))
            (should
             (equal '("email-existing")
                    (cl-loop
                     for row across
                     (chidu-store-mailbox-summary-context-rows inbox-summary)
                     collect
                     (chidu-store-email-summary-row-remote-email-id row))))
            (should
             (equal '("email-new")
                    (cl-loop
                     for row across
                     (chidu-store-mailbox-summary-context-rows archive-summary)
                     collect
                     (chidu-store-email-summary-row-remote-email-id row))))
            (should (chidu-store-search-context-stale-p search))))
      (when runtime (chidu-runtime-close runtime)))))

(ert-deftest chidu-live-gap-keeps-old-active-generation-readable ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-live-gap-" t)) store account-id mailbox)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root)
                account-id
                (chidu-store-test--prepare-mailbox-account store))
          (let* ((mailbox-context
                  (chidu-email-catchup-test--value
                   store
                   (chidu-store-op-observe-mailbox-snapshot-create
                    :account-id account-id :expected-revision 0
                    :observation
                    (chidu-email-catchup-test--mailbox-snapshot))))
                 (_mailbox
                  (setq mailbox
                        (cl-find
                         "inbox"
                         (chidu-store-mailbox-sync-context-mailboxes
                          mailbox-context)
                         :key #'chidu-store-mailbox-remote-mailbox-id
                         :test #'equal)))
                 (live
                  (chidu-store-test--activate-email-generation
                   store account-id
                   (vector
                    (chidu-store-test--email-entry
                     "email-still-visible" "2026-08-27T01:00:00Z"))
                   "e0"))
                 (old-generation
                  (chidu-store-email-sync-context-generation-id live))
                 (rebuilding
                  (chidu-email-catchup-test--value
                   store
                   (chidu-store-op-restart-email-bootstrap-create
                    :account-id account-id
                    :generation-id old-generation
                    :expected-revision
                    (chidu-store-email-sync-context-revision live)
                    :state "fresh-state"
                    :profile-version "metadata-v1"))))
            (should (eq 'enumerating
                        (chidu-store-email-sync-context-phase rebuilding)))
            (should-not
             (equal old-generation
                    (chidu-store-email-sync-context-generation-id rebuilding)))
            (should
             (equal '("email-still-visible")
                    (cl-loop
                     for row across
                     (chidu-store-mailbox-summary-context-rows
                      (chidu-store-test--canonical-summary
                       store account-id mailbox))
                     collect
                     (chidu-store-email-summary-row-remote-email-id row))))
            (chidu-store-close store)
            (setq store (chidu-store-sqlite-create root))
            (should
             (equal '("email-still-visible")
                    (cl-loop
                     for row across
                     (chidu-store-mailbox-summary-context-rows
                      (chidu-store-test--canonical-summary
                       store account-id mailbox))
                     collect
                     (chidu-store-email-summary-row-remote-email-id row))))
            (let ((database
                   (sqlite-open (expand-file-name "store.sqlite3" root))))
              (unwind-protect
                  (should
                   (equal '(("active" 1) ("building" 1))
                          (sqlite-select
                           database
                           "SELECT lifecycle, count(*)
                              FROM jmap_email_generation
                             WHERE account_id = ?
                             GROUP BY lifecycle
                             ORDER BY lifecycle"
                           (list account-id))))
                (sqlite-close database)))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(provide 'chidu-email-catchup-test)

;;; chidu-email-catchup-test.el ends here
