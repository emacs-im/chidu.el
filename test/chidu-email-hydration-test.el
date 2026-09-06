;;; chidu-email-hydration-test.el --- Canonical Email hydration tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'ert)
(require 'sqlite)
(require 'chidu-jmap-email-hydration)
(require 'chidu-store-sqlite)
(require 'chidu-test-support)

(defun chidu-email-hydration-test--value (store operation)
  "Return successful STORE OPERATION value."
  (let ((result (chidu-store-test--store-call store operation)))
    (should (chidu-result-ok-p result))
    (chidu-result-ok-value result)))

(defun chidu-email-hydration-test--setup (store)
  "Return hydrating canonical Email context from STORE."
  (let* ((account-id (chidu-store-test--prepare-mailbox-account store))
         (context
          (chidu-email-hydration-test--value
           store
           (chidu-store-op-begin-email-bootstrap-create
            :account-id account-id :expected-revision 0
            :state "email-0" :profile-version "metadata-v1")))
         (generation-id
          (chidu-store-email-sync-context-generation-id context)))
    (dolist (ids (list (vector "email-1" "email-2") (vector)))
      (setq
       context
       (chidu-email-hydration-test--value
        store
        (chidu-store-op-append-email-query-chunk-create
         :account-id account-id
         :generation-id generation-id
         :expected-revision
         (chidu-store-email-sync-context-revision context)
         :observation
         (chidu-store-email-query-page-observation-create
          :query-state "query-0"
          :can-calculate-changes-p t
          :position
          (chidu-store-email-sync-context-committed-count context)
          :remote-email-ids ids)))))
    (chidu-email-hydration-test--value
     store
     (chidu-store-op-apply-email-membership-changes-create
      :account-id account-id
      :generation-id generation-id
      :expected-revision
      (chidu-store-email-sync-context-revision context)
      :expected-state "email-0"
      :observation
      (chidu-store-email-changes-observation-create
       :old-state "email-0" :new-state "email-1")))))

(ert-deftest chidu-jmap-email-hydration-is-homogeneous-and-plan-ordered ()
  (let* ((targets
          (vector
           (chidu-store-email-hydration-target-create
            :local-email-id "local-1" :remote-email-id "email-1")
           (chidu-store-email-hydration-target-create
            :local-email-id "local-2" :remote-email-id "email-2")))
         (plan
          (chidu-store-email-hydration-plan-create
           :kind 'full :targets targets))
         (mailboxes (make-hash-table :test #'equal))
         (keywords (make-hash-table :test #'equal)))
    (puthash "inbox" t mailboxes)
    (puthash "$seen" t keywords)
    (let* ((request
             (chidu-jmap-email-hydration--request
              "remote-account" 'full (vector "email-1" "email-2")))
           (arguments (aref (aref (plist-get request :methodCalls) 0) 1))
           (bytes
            (chidu-store-test--method-response
             "Email/get" "email-hydration"
             `(:accountId "remote-account" :state "email-1"
               :list
               [(:id "email-1" :blobId "blob-email-1"
                 :threadId "thread-email-1"
                 :mailboxIds ,mailboxes :keywords ,keywords :size 42
                 :receivedAt "2026-08-26T12:00:00Z"
                 :from [(:name "Alice" :email "alice@example.test")]
                 :subject "Subject email-1"
                 :messageId ["mid-email-1"]
                 :hasAttachment :json-false)]
               :notFound ["email-2"])))
           (observation
            (chidu-jmap-email-hydration--decode
             bytes "remote-account" 'full
             (vector "email-1" "email-2") "email-hydration"))
           (results
            (chidu-store-email-hydration-observation-results observation)))
      (should
       (equal chidu-jmap-email-hydration-full-properties
              (plist-get arguments :properties)))
      (should
       (equal "email-1"
              (chidu-store-email-hydration-observation-state observation)))
      (should (equal "email-1"
                     (chidu-store-email-hydration-result-remote-email-id
                      (aref results 0))))
      (should
       (equal (chidu-store-test--email-metadata "email-1")
              (chidu-store-email-hydration-result-metadata
               (aref results 0))))
      (should (equal ""
                     (chidu-store-email-hydration-result-preview
                      (aref results 0))))
      (should-not
       (chidu-store-email-hydration-result-found-p (aref results 1))))
    (let* ((mutable-plan
            (chidu-store-email-hydration-plan-create
             :kind 'mutable
             :targets (vector (aref targets 0))))
           (request
             (chidu-jmap-email-hydration--request
              "remote-account" 'mutable (vector "email-1")))
           (arguments (aref (aref (plist-get request :methodCalls) 0) 1))
           (bytes
            (chidu-store-test--method-response
             "Email/get" "email-hydration"
             `(:accountId "remote-account" :state "email-2"
               :list [(:id "email-1" :mailboxIds ,mailboxes
                       :keywords ,keywords)]
               :notFound [])))
           (result
            (aref
             (chidu-store-email-hydration-observation-results
              (chidu-jmap-email-hydration--decode
               bytes "remote-account" 'mutable
               (vector "email-1") "email-hydration"))
             0)))
      (should
       (equal chidu-jmap-email-hydration-mutable-properties
              (plist-get arguments :properties)))
      (should-not (chidu-store-email-hydration-result-metadata result))
      (should (equal (vector "$seen")
                     (chidu-store-email-hydration-result-keywords result))))))

(ert-deftest chidu-store-email-hydration-plans-by-coverage-and-resumes ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-email-hydration-" t))
        store account-id)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (let* ((context (chidu-email-hydration-test--setup store))
                 (_account-id
                  (setq account-id
                        (chidu-store-account-account-id
                         (chidu-store-email-sync-context-account context))))
                 (generation-id
                  (chidu-store-email-sync-context-generation-id context))
                 (initial-plan
                  (chidu-email-hydration-test--value
                   store
                   (chidu-store-op-get-email-hydration-plan-create
                    :account-id account-id :limit 10)))
                 (first-target
                  (aref
                   (chidu-store-email-hydration-plan-targets initial-plan)
                   0))
                 (database
                  (sqlite-open (expand-file-name "store.sqlite3" root))))
            (unwind-protect
                (chidu-store-sqlite--insert-email-metadata
                 database account-id
                 (chidu-store-email-hydration-target-local-email-id
                  first-target)
                 "metadata-v1"
                 (chidu-store-test--email-metadata "email-1")
                 "Warm preview" 0)
              (sqlite-close database))
            (let* ((mutable-plan
                    (chidu-email-hydration-test--value
                     store
                     (chidu-store-op-get-email-hydration-plan-create
                      :account-id account-id :limit 10)))
                   (mutable-target
                    (aref
                     (chidu-store-email-hydration-plan-targets mutable-plan)
                     0)))
              (should (eq 'mutable
                          (chidu-store-email-hydration-plan-kind mutable-plan)))
              (should (= 1
                         (length
                          (chidu-store-email-hydration-plan-targets
                           mutable-plan))))
              (setq
               context
               (chidu-email-hydration-test--value
                store
                (chidu-store-op-apply-email-hydration-create
                 :account-id account-id :generation-id generation-id
                 :expected-revision
                 (chidu-store-email-sync-context-revision context)
                 :observation
                 (chidu-store-email-hydration-observation-create
                  :kind 'mutable
                  :results
                  (vector
                   (chidu-store-email-hydration-result-create
                    :remote-email-id
                    (chidu-store-email-hydration-target-remote-email-id
                     mutable-target)
                    :found-p t
                    :remote-mailbox-ids (vector "inbox")
                    :keywords (vector "$seen"))))))))
            ;; Resume from the durable local-id cursor, not from an in-memory
            ;; copy of the issued plan.
            (chidu-store-close store)
            (setq store (chidu-store-sqlite-create root)
                  context
                  (chidu-email-hydration-test--value
                   store
                   (chidu-store-op-get-email-sync-context-create
                    :account-id account-id)))
            (let* ((full-plan
                    (chidu-email-hydration-test--value
                     store
                     (chidu-store-op-get-email-hydration-plan-create
                      :account-id account-id :limit 10)))
                   (full-target
                    (aref
                     (chidu-store-email-hydration-plan-targets full-plan)
                     0)))
              (should (eq 'full
                          (chidu-store-email-hydration-plan-kind full-plan)))
              (setq
               context
               (chidu-email-hydration-test--value
                store
                (chidu-store-op-apply-email-hydration-create
                 :account-id account-id :generation-id generation-id
                 :expected-revision
                 (chidu-store-email-sync-context-revision context)
                 :observation
                 (chidu-store-email-hydration-observation-create
                  :kind 'full
                  :results
                  (vector
                   (chidu-store-email-hydration-result-create
                    :remote-email-id
                    (chidu-store-email-hydration-target-remote-email-id
                     full-target)
                    :found-p nil)))))))
            (let ((empty-plan
                   (chidu-email-hydration-test--value
                    store
                    (chidu-store-op-get-email-hydration-plan-create
                     :account-id account-id :limit 10))))
              (should (zerop
                       (length
                        (chidu-store-email-hydration-plan-targets
                         empty-plan)))))
            (setq
             context
             (chidu-email-hydration-test--value
              store
              (chidu-store-op-finish-email-hydration-create
               :account-id account-id :generation-id generation-id
               :expected-revision
               (chidu-store-email-sync-context-revision context))))
            (should (eq 'metadata-catchup
                        (chidu-store-email-sync-context-phase context)))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(provide 'chidu-email-hydration-test)

;;; chidu-email-hydration-test.el ends here
