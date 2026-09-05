;;; chidu-compose-test.el --- Structured Compose tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-compose)
(require 'appkit-core)
(require 'chidu)
(require 'chidu-compose)
(require 'chidu-compose-resource)
(require 'chidu-jmap-upload)
(require 'chidu-draft)
(require 'chidu-jmap-draft)
(require 'chidu-jmap-compose)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)
(require 'chidu-store-sqlite)
(require 'json)

(defun chidu-compose-test--payload (value)
  "Encode JSON VALUE as an unibyte JMAP test payload."
  (encode-coding-string
   (json-serialize value :null-object :json-null :false-object :json-false)
   'utf-8-unix t))

(defun chidu-compose-test--method-response (method call-id arguments)
  "Return one encoded JMAP METHOD response for CALL-ID and ARGUMENTS."
  (chidu-compose-test--payload
   `(:sessionState "session"
                   :methodResponses [[,method ,arguments ,call-id]])))

(defun chidu-compose-test--store-call (store operation)
  "Synchronously invoke STORE OPERATION."
  (let (result)
    (chidu-store-call store operation (lambda (value) (setq result value)))
    result))

(defun chidu-compose-test--value (store operation)
  "Return successful STORE OPERATION value, or signal."
  (let ((result (chidu-compose-test--store-call store operation)))
    (unless (chidu-result-ok-p result)
      (error "Compose test Store operation failed: %S" result))
    (chidu-result-ok-value result)))

(defun chidu-compose-test--account-observation (identities)
  "Return one writable Account observation with IDENTITIES."
  (chidu-store-account-observation-create
   :remote-account-id "remote-account"
   :name "Mail"
   :personal-p t
   :read-only-p nil
   :primary-mail-p t
   :primary-submission-p t
   :identity-state "identity-compose"
   :capabilities
   (vector chidu-jmap-mail-capability
           chidu-jmap-submission-capability)
   :identities identities))

(defun chidu-compose-test--session-observation (account)
  "Return one Session observation carrying ACCOUNT."
  (chidu-store-session-observation-create
   :username "me@example.test"
   :state "session-compose"
   :api-url "https://mail.example.test/jmap/api"
   :download-url
   "https://mail.example.test/jmap/download/{accountId}/{blobId}/{name}?type={type}"
   :upload-url "https://mail.example.test/jmap/upload/{accountId}"
   :event-source-url
   "https://mail.example.test/jmap/eventsource/?types={types}"
   :max-size-request 1048576
   :max-objects-in-get 256
   :max-objects-in-set 128
   :capabilities
   (vector chidu-jmap-core-capability
           chidu-jmap-mail-capability
           chidu-jmap-submission-capability)
   :accounts (vector account)))

(defun chidu-compose-test--directory ()
  "Return one private temporary Store directory."
  (let ((directory (make-temp-file "chidu-compose-test-" t)))
    (set-file-modes directory #o700)
    directory))

(defun chidu-compose-test--drafts-snapshot ()
  "Return one writable Drafts Mailbox snapshot."
  (chidu-store-mailbox-snapshot-observation-create
   :state "mailboxes-compose"
   :mailboxes
   (vector
    (chidu-store-mailbox-observation-create
     :remote-mailbox-id "drafts" :name "Drafts" :role "drafts"
     :sort-order 10 :total-emails 0 :unread-emails 0
     :total-threads 0 :unread-threads 0
     :rights
     (chidu-store-mailbox-rights-create
      :may-read-items-p t :may-add-items-p t :may-remove-items-p nil
      :may-set-seen-p t :may-set-keywords-p t
      :may-create-child-p nil :may-rename-p t :may-delete-p t
      :may-submit-p t)
     :subscribed-p t))))

(defun chidu-compose-test--connected-context (store)
  "Create one connected Account and Identity in STORE."
  (let* ((endpoint
          (chidu-compose-test--value
           store
           (chidu-store-op-configure-endpoint-create
            :session-url "https://mail.example.test/.well-known/jmap"
            :login "me@example.test"
            :authentication 'basic)))
         (identity
          (chidu-store-identity-observation-create
           :remote-identity-id "remote-identity"
           :name "Me"
           :email "me@example.test"))
         (alternate-identity
          (chidu-store-identity-observation-create
           :remote-identity-id "remote-identity-alternate"
           :name "Me Alternate"
           :email "alternate@example.test"))
         (connected
          (chidu-compose-test--value
           store
           (chidu-store-op-observe-session-create
            :endpoint-id (chidu-store-endpoint-endpoint-id endpoint)
            :observation
            (chidu-compose-test--session-observation
             (chidu-compose-test--account-observation
              (vector identity alternate-identity))))))
         (account (aref (chidu-store-endpoint-accounts connected) 0)))
    (chidu-compose-test--value
     store
     (chidu-store-op-observe-mailbox-snapshot-create
      :account-id (chidu-store-account-account-id account)
      :expected-revision 0
      :observation (chidu-compose-test--drafts-snapshot)))
    (list connected account
          (aref (chidu-store-account-identities account) 0))))

(defun chidu-compose-test--create-workspace
    (store account identity workspace-id document)
  "Create WORKSPACE-ID with DOCUMENT in STORE for ACCOUNT and IDENTITY."
  (chidu-compose-test--value
   store
   (chidu-store-op-create-compose-workspace-create
    :workspace-id workspace-id
    :account-id (chidu-store-account-account-id account)
    :identity-id (chidu-store-identity-identity-id identity)
    :kind 'new
    :document document)))

(ert-deftest chidu-compose-workspace-structured-cas-survives-restart ()
  "Structured document revisions should survive restart with exact CAS."
  (let ((root (chidu-compose-test--directory))
        store)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (initial
                        (chidu-store-compose-document-create
                         :to "alice@example.test"
                         :subject "Initial"
                         :body "Body"))
                       (created
                        (chidu-compose-test--create-workspace
                         store account identity workspace-id initial))
                       (workspace
                        (chidu-store-compose-context-workspace created)))
            (should (= 0 (chidu-store-compose-workspace-revision workspace)))
            (should
             (equal initial
                    (chidu-store-compose-workspace-document workspace)))
            (let* ((updated
                    (chidu-store-compose-document-with
                     initial :subject "Updated" :body "Body\nMore"))
                   (context
                    (chidu-compose-test--value
                     store
                     (chidu-store-op-checkpoint-compose-workspace-create
                      :workspace-id workspace-id
                      :identity-id
                      (chidu-store-identity-identity-id identity)
                      :expected-revision 0
                      :revision 3
                      :document updated))))
              (should
               (= 3
                  (chidu-store-compose-workspace-revision
                   (chidu-store-compose-context-workspace context))))
              (let ((stale
                     (chidu-compose-test--store-call
                      store
                      (chidu-store-op-checkpoint-compose-workspace-create
                       :workspace-id workspace-id
                       :identity-id
                       (chidu-store-identity-identity-id identity)
                       :expected-revision 0
                       :revision 4
                       :document initial))))
                (should (chidu-result-failure-p stale))
                (should
                 (eq 'revision-conflict
                     (chidu-result-failure-kind stale))))
              (chidu-store-close store)
              (setq store (chidu-store-sqlite-create root))
              (let* ((reopened
                      (chidu-compose-test--value
                       store
                       (chidu-store-op-get-compose-workspace-create
                        :workspace-id workspace-id)))
                     (reopened-workspace
                      (chidu-store-compose-context-workspace reopened)))
                (should (= 3
                           (chidu-store-compose-workspace-revision
                            reopened-workspace)))
                (should
                 (equal updated
                        (chidu-store-compose-workspace-document
                         reopened-workspace))))
              (chidu-compose-test--value
               store
               (chidu-store-op-discard-compose-workspace-create
                :workspace-id workspace-id :expected-revision 3))
              (should
               (= 0
                  (length
                   (chidu-compose-test--value
                    store
                    (chidu-store-op-list-compose-workspaces-create))))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest chidu-compose-mode-edits-one-structured-document ()
  "Dedicated Compose mode should checkpoint typed state without a text mirror."
  (let ((root (chidu-compose-test--directory))
        store runtime app compose-buffer)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (initial
                        (chidu-store-compose-document-create
                         :body "Opening body"))
                       (context
                        (chidu-compose-test--create-workspace
                         store account identity workspace-id initial)))
            (setq runtime (chidu-runtime-open :data-root root :store store)
                  store nil
                  app (appkit-app-start
                       chidu--app-type :input (chidu--state-create) :identity (make-symbol "compose-test")))
            (appkit-app-send app (list :runtime runtime))
            (chidu-compose--open-context app context)
            (setq compose-buffer (current-buffer))
            (should (derived-mode-p 'chidu-compose-mode 'text-mode))
            (should-not (derived-mode-p 'message-mode))
            (should (equal initial (chidu-compose--document)))
            (should-not (string-match-p "^To:" (buffer-string)))
            (chidu-compose--goto-field 'to)
            (insert "alice@example.test")
            (chidu-compose--goto-field 'subject)
            (insert "Topic\ncontinued")
            (goto-char (point-max))
            (insert "\nSecond paragraph")
            (let* ((capture (appkit-compose-capture))
                   (generation (plist-get capture :generation))
                   (document (plist-get capture :value))
                   (view chidu-compose--view))
              (should (> generation 0))
              (should
               (equal "alice@example.test"
                      (chidu-store-compose-document-to document)))
              (should
               (equal "Topic continued"
                      (chidu-store-compose-document-subject document)))
              (should
               (equal "Opening body\nSecond paragraph"
                      (chidu-store-compose-document-body document)))
              (chidu-compose--open-context app context)
              (should (eq compose-buffer (current-buffer)))
              (should (eq view chidu-compose--view))
              (should (equal document (chidu-compose--document)))
              (chidu-compose-checkpoint)
              (should-not (chidu-compose--dirty-p))
              (let* ((saved
                      (chidu-compose-test--value
                       (chidu-runtime-store runtime)
                       (chidu-store-op-get-compose-workspace-create
                        :workspace-id workspace-id)))
                     (workspace
                      (chidu-store-compose-context-workspace saved)))
                (should
                 (= generation
                    (chidu-store-compose-workspace-revision workspace)))
                (should
                 (equal document
                        (chidu-store-compose-workspace-document workspace)))))))
      (when (buffer-live-p compose-buffer)
        (with-current-buffer compose-buffer
          (setq-local chidu-compose--closing-p t))
        (kill-buffer compose-buffer))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest chidu-jmap-compose-compiles-a-native-draft-email ()
  "ComposeDocument should compile directly to one JMAP Email/set create shape."
  (let* ((document
          (chidu-store-compose-document-create
           :to "Alice <alice@example.test>, unfinished"
           :cc "review@example.test"
           :subject "Structured Draft"
           :body "Plain text body"))
         (identity
          (chidu-store-identity-create
           :identity-id "local-identity"
           :remote-identity-id "remote-identity"
           :name "Me"
           :email "me@example.test"
           :available-p t))
         (email
          (chidu-jmap-compose-draft-email
           document identity "drafts-mailbox"))
         (from (aref (gethash "from" email) 0))
         (body-structure (gethash "bodyStructure" email))
         (body-values (gethash "bodyValues" email)))
    (should (equal "Alice <alice@example.test>, unfinished"
                   (gethash "header:To" email)))
    (should (equal "review@example.test" (gethash "header:Cc" email)))
    (should-not (gethash "to" email))
    (should (eq t (gethash "drafts-mailbox" (gethash "mailboxIds" email))))
    (should (eq t (gethash "$draft" (gethash "keywords" email))))
    (should (eq t (gethash "$seen" (gethash "keywords" email))))
    (should (equal "me@example.test" (gethash "email" from)))
    (should (equal "text/plain" (gethash "type" body-structure)))
    (should
     (equal "Plain text body"
            (gethash "value" (gethash "text" body-values))))))

(ert-deftest chidu-draft-publication-state-survives-unknown-and-does-not-block ()
  "A cleanup intent must not block publishing a newer workspace revision."
  (let ((root (chidu-compose-test--directory)) store)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (initial
                        (chidu-store-compose-document-create
                         :subject "Initial" :body "Body"))
                       (context
                        (chidu-compose-test--create-workspace
                         store account identity workspace-id initial))
                       (attempt-1 (chidu-store-new-local-id)))
            (setq
             context
             (chidu-compose-test--value
              store
              (chidu-store-op-accept-draft-publish-create
               :workspace-id workspace-id
               :identity-id (chidu-store-identity-identity-id identity)
               :expected-revision 0 :revision 0 :document initial
               :attempt-id attempt-1
               :message-id "chidu.first@example.test")))
            (should
             (eq 'pending
                 (chidu-store-draft-publish-attempt-phase
                  (chidu-store-compose-context-publish-attempt context))))
            (let ((premature
                   (chidu-compose-test--store-call
                    store
                    (chidu-store-op-settle-draft-publish-create-create
                     :attempt-id attempt-1 :outcome 'succeeded
                     :remote-email-id "draft-premature"))))
              (should (chidu-result-failure-p premature))
              (should
               (eq 'draft-publish-phase-conflict
                   (chidu-result-failure-kind premature))))
            (setq
             context
             (chidu-compose-test--value
              store
              (chidu-store-op-mark-draft-publish-unknown-create
               :attempt-id attempt-1)))
            (should
             (eq 'unknown
                 (chidu-store-draft-publish-attempt-phase
                  (chidu-store-compose-context-publish-attempt context))))
            (chidu-store-close store)
            (setq store (chidu-store-sqlite-create root)
                  context
                  (chidu-compose-test--value
                   store
                   (chidu-store-op-get-compose-workspace-create
                    :workspace-id workspace-id)))
            (should
             (eq 'unknown
                 (chidu-store-draft-publish-attempt-phase
                  (chidu-store-compose-context-publish-attempt context))))
            (setq
             context
             (chidu-compose-test--value
              store
              (chidu-store-op-settle-draft-publish-create-create
               :attempt-id attempt-1 :outcome 'succeeded
               :remote-email-id "draft-1"
               :remote-blob-id "blob-1")))
            (let ((workspace
                   (chidu-store-compose-context-workspace context)))
              (should
               (equal "draft-1"
                      (chidu-store-compose-workspace-base-remote-email-id
                       workspace)))
              (should
               (equal "blob-1"
                      (chidu-store-compose-workspace-base-remote-blob-id
                       workspace)))
              (should (= 0
                         (chidu-store-compose-workspace-published-revision
                          workspace))))
            (should-not
             (chidu-store-compose-context-publish-attempt context))
            (let ((discard
                   (chidu-compose-test--store-call
                    store
                    (chidu-store-op-discard-compose-workspace-create
                     :workspace-id workspace-id :expected-revision 0))))
              (should (chidu-result-failure-p discard))
              (should
               (eq 'compose-workspace-has-remote-draft
                   (chidu-result-failure-kind discard))))
            (let* ((updated
                    (chidu-store-compose-document-with
                     initial :subject "Updated"))
                   (attempt-2 (chidu-store-new-local-id)))
              (setq
               context
               (chidu-compose-test--value
                store
                (chidu-store-op-accept-draft-publish-create
                 :workspace-id workspace-id
                 :identity-id (chidu-store-identity-identity-id identity)
                 :expected-revision 0 :revision 1 :document updated
                 :attempt-id attempt-2
                 :message-id "chidu.second@example.test")))
              (chidu-compose-test--value
               store
               (chidu-store-op-mark-draft-publish-unknown-create
                :attempt-id attempt-2))
              (setq
               context
               (chidu-compose-test--value
                store
                (chidu-store-op-settle-draft-publish-create-create
                 :attempt-id attempt-2 :outcome 'succeeded
                 :remote-email-id "draft-2"
                 :remote-blob-id "blob-2")))
              (should (= 1
                         (length
                          (chidu-store-compose-context-cleanup-attempts
                           context))))
              (should
               (equal
                "draft-1"
                (chidu-store-draft-publish-attempt-predecessor-remote-email-id
                 (aref
                  (chidu-store-compose-context-cleanup-attempts context)
                  0))))
              (should
               (equal
                "blob-1"
                (chidu-store-draft-publish-attempt-predecessor-remote-blob-id
                 (aref
                  (chidu-store-compose-context-cleanup-attempts context)
                  0))))
              (let ((cleanup-attempt
                     (aref
                      (chidu-store-compose-context-cleanup-attempts context)
                      0)))
                (setq
                 context
                 (chidu-compose-test--value
                  store
                  (chidu-store-op-settle-draft-publish-cleanup-create
                   :attempt-id
                   (chidu-store-draft-publish-attempt-attempt-id
                    cleanup-attempt)
                   :outcome 'rejected
                   :error-kind "remoteConflict")))
                (should
                 (zerop
                  (length
                   (chidu-store-compose-context-cleanup-attempts context)))))
              ;; A terminal predecessor conflict is removed from the cleanup
              ;; queue and cannot block revision 2.
              (let ((third
                     (chidu-store-compose-document-with
                      updated :subject "Third")))
                (setq
                 context
                 (chidu-compose-test--value
                  store
                  (chidu-store-op-accept-draft-publish-create
                   :workspace-id workspace-id
                   :identity-id
                   (chidu-store-identity-identity-id identity)
                   :expected-revision 1 :revision 2 :document third
                   :attempt-id (chidu-store-new-local-id)
                   :message-id "chidu.third@example.test")))
                (should
                 (eq 'pending
                     (chidu-store-draft-publish-attempt-phase
                      (chidu-store-compose-context-publish-attempt
                       context))))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-jmap-draft-decodes-create-reconcile-and-cleanup ()
  "Draft wire adapters should settle exact create and cleanup evidence."
  (let ((created (make-hash-table :test #'equal))
        (mailboxes (make-hash-table :test #'equal))
        (keywords (make-hash-table :test #'equal))
        (not-destroyed (make-hash-table :test #'equal)))
    (puthash "create-1"
             '(:id "draft-1" :blobId "blob-1" :threadId "thread-1"
                   :size 81)
             created)
    (puthash "drafts" t mailboxes)
    (puthash "$draft" t keywords)
    (let ((result
           (chidu-jmap-draft-validate-create-response
            (chidu-compose-test--method-response
             "Email/set" "draft-create"
             `(:accountId "account" :oldState "e0" :newState "e1"
                          :created ,created :notCreated :json-null))
            "account" "create-1")))
      (should (eq 'succeeded
                  (chidu-jmap-draft-create-result-outcome result)))
      (should (equal "draft-1"
                     (chidu-jmap-draft-create-result-remote-email-id result)))
      (should (equal "blob-1"
                     (chidu-jmap-draft-create-result-remote-blob-id result))))
    (let ((extra-created (make-hash-table :test #'equal)))
      (puthash "other"
               '(:id "other" :blobId "blob-other" :threadId "thread-other"
                     :size 1)
               extra-created)
      (should-error
       (chidu-jmap-draft-validate-create-response
        (chidu-compose-test--method-response
         "Email/set" "draft-create"
         `(:accountId "account" :oldState "e0" :newState "e1"
                      :created ,extra-created :notCreated :json-null))
        "account" "create-1")
       :type 'chidu-jmap-error))
    (should-error
     (chidu-jmap-draft-validate-reconcile-response
      (chidu-compose-test--payload
       '(:sessionState "session"
                       :methodResponses
                       [["Email/query"
                         (:accountId "account" :queryState "q1"
                                     :canCalculateChanges t :position 0
                                     :ids ["draft-1"] :total 1)
                         "draft-query"]
                        ["Email/get"
                         (:accountId "account" :state "e1"
                                     :list [] :notFound [])
                         "draft-get"]]))
      "account" "drafts" "chidu.first@example.test")
     :type 'chidu-jmap-error)
    (let ((matches
           (chidu-jmap-draft-validate-reconcile-response
            (chidu-compose-test--payload
             `(:sessionState "session"
                             :methodResponses
                             [["Email/query"
                               (:accountId "account" :queryState "q1"
                                           :canCalculateChanges t :position 0
                                           :ids ["draft-1"] :total 1)
                               "draft-query"]
                              ["Email/get"
                               (:accountId "account" :state "e1"
                                           :list
                                           [(:id "draft-1"
                                                 :blobId "blob-1"
                                                 :messageId ["chidu.first@example.test"]
                                                 :mailboxIds ,mailboxes :keywords ,keywords)]
                                           :notFound [])
                               "draft-get"]]))
            "account" "drafts" "chidu.first@example.test")))
      (should (= 1 (length matches)))
      (should
       (equal "draft-1"
              (chidu-jmap-draft-reconcile-match-remote-email-id
               (aref matches 0))))
      (should
       (equal "blob-1"
              (chidu-jmap-draft-reconcile-match-remote-blob-id
               (aref matches 0)))))
    (let ((evidence
           (chidu-jmap-draft-validate-cleanup-get-response
            (chidu-compose-test--method-response
             "Email/get" "draft-cleanup-get"
             `(:accountId "account" :state "e2"
                          :list
                          [(:id "draft-1" :blobId "blob-1"
                                :mailboxIds ,mailboxes :keywords ,keywords)]
                          :notFound []))
            "account" "draft-1")))
      (should (chidu-jmap-draft-cleanup-evidence-found-p evidence))
      (should (equal "e2"
                     (chidu-jmap-draft-cleanup-evidence-state evidence)))
      (should (equal "blob-1"
                     (chidu-jmap-draft-cleanup-evidence-remote-blob-id
                      evidence))))
    (let* ((request
            (chidu-jmap-draft-cleanup-request
             "account" "draft-1" "e2"))
           (arguments (aref (aref (plist-get request :methodCalls) 0) 1)))
      (should (equal "e2" (plist-get arguments :ifInState))))
    (puthash "draft-1" '(:type "notFound") not-destroyed)
    (let ((cleanup
           (chidu-jmap-draft-validate-cleanup-response
            (chidu-compose-test--method-response
             "Email/set" "draft-cleanup"
             `(:accountId "account" :oldState "e1" :newState "e1"
                          :destroyed [] :notDestroyed ,not-destroyed))
            "account" "draft-1")))
      (should
       (eq 'succeeded
           (chidu-jmap-draft-cleanup-result-outcome cleanup))))))

(ert-deftest chidu-draft-cleanup-conflict-and-state-mismatch-are-safe ()
  (let* ((mailbox
          (chidu-store-mailbox-create
           :mailbox-id "local-drafts"
           :remote-mailbox-id "drafts"
           :role "drafts"
           :available-p t))
         (attempt
          (chidu-store-draft-publish-attempt-create
           :attempt-id "attempt"
           :predecessor-remote-email-id "draft-old"
           :predecessor-remote-blob-id "blob-old"
           :phase 'cleanup-pending))
         (context
          (chidu-store-compose-context-create
           :drafts-mailbox mailbox
           :cleanup-attempts (vector attempt)))
         (workflow (chidu-draft-workflow-create :context context))
         settled
         destroyed-state)
    (cl-letf (((symbol-function 'chidu-draft--settle-cleanup)
               (lambda (_workflow outcome error-kind)
                 (setq settled (list outcome error-kind))))
              ((symbol-function 'chidu-draft--dispatch-cleanup-destroy)
               (lambda (_workflow state)
                 (setq destroyed-state state))))
      (chidu-draft--after-cleanup-read
       workflow
       (chidu-result-ok-create
        :value
        (chidu-jmap-draft-cleanup-evidence-create
         :state "s1"
         :found-p t
         :remote-blob-id "changed-blob"
         :remote-mailbox-ids ["drafts"]
         :keywords ["$draft" "$seen"])))
      (should (equal '(rejected "remoteConflict") settled))
      (should-not destroyed-state)
      (setq settled nil)
      (chidu-draft--after-cleanup-read
       workflow
       (chidu-result-ok-create
        :value
        (chidu-jmap-draft-cleanup-evidence-create
         :state "s2"
         :found-p t
         :remote-blob-id "blob-old"
         :remote-mailbox-ids ["drafts" "label"]
         :keywords ["$draft" "$flagged"])))
      (should-not settled)
      (should (equal "s2" destroyed-state))
      (setq settled nil)
      (chidu-draft--after-cleanup-jmap
       workflow
       (chidu-result-ok-create
        :value
        (chidu-jmap-draft-cleanup-result-create
         :outcome 'rejected :error-kind "stateMismatch")))
      (should (equal '(unknown "stateMismatch") settled)))))

(ert-deftest chidu-draft-cleanup-sync-settlement-keeps-newest-cancel ()
  (let* ((store
          (chidu-store-capability-create
           :name 'fake
           :invoke-function #'ignore
           :inspect-function #'ignore
           :close-function #'ignore))
         (runtime (chidu-runtime-open :store store))
         (operation (chidu-runtime--begin-operation runtime))
         (context
          (chidu-store-compose-context-create
           :endpoint (chidu-store-endpoint-create)))
         (workflow
          (chidu-draft-workflow-create
           :runtime runtime
           :runtime-operation operation
           :context context))
         (first-cancel-count 0)
         (second-cancel-count 0))
    (unwind-protect
        (cl-letf
            (((symbol-function 'chidu-runtime--endpoint-secret)
              (lambda (_endpoint) (copy-sequence "secret"))))
          (chidu-draft--start-cleanup-effect
           workflow
           (lambda (_secret deliver)
             (funcall deliver (chidu-result-ok-create :value 'first))
             (lambda () (cl-incf first-cancel-count)))
           (lambda (current _result)
             (chidu-draft--start-cleanup-effect
              current
              (lambda (_secret _deliver)
                (lambda () (cl-incf second-cancel-count)))
              #'ignore)))
          (should
           (functionp
            (chidu-runtime-operation-cancel-function operation)))
          (chidu-runtime-cancel-operation runtime operation)
          (should (zerop first-cancel-count))
          (should (= 1 second-cancel-count)))
      (chidu-runtime-close runtime))))

(ert-deftest chidu-draft-workflow-reconciles-a-lost-create-response ()
  "A later Save should reconcile a lost create response before any retry."
  (let ((root (chidu-compose-test--directory))
        store runtime (create-count 0) (reconcile-count 0)
        first-result recovered-result)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (document
                        (chidu-store-compose-document-create
                         :subject "Uncertain" :body "Body"))
                       (context
                        (chidu-compose-test--create-workspace
                         store account identity workspace-id document)))
            (setq runtime (chidu-runtime-open :data-root root :store store)
                  store nil)
            (cl-letf
                (((symbol-function 'auth-source-search)
                  (lambda (&rest _arguments)
                    (list (list :secret
                                (lambda () (copy-sequence "secret"))))))
                 ((symbol-function 'chidu-jmap-draft-create)
                  (lambda (&rest arguments)
                    (cl-incf create-count)
                    (funcall
                     (car (last arguments))
                     (chidu-result-failure-create
                      :kind 'response-lost :data nil :retryable-p t))
                    #'ignore))
                 ((symbol-function 'chidu-jmap-draft-reconcile)
                  (lambda (_context attempt _secret deliver)
                    (cl-incf reconcile-count)
                    (should
                     (eq 'unknown
                         (chidu-store-draft-publish-attempt-phase attempt)))
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (vector
                       (chidu-jmap-draft-reconcile-match-create
                        :remote-email-id "draft-recovered"
                        :remote-blob-id "blob-draft-recovered"))))
                    #'ignore)))
              (chidu-publish-draft
               runtime context identity 0 document
               (lambda (value) (setq first-result value))
               (lambda (failure) (ert-fail (format "%S" failure))))
              (should (= 1 create-count))
              (should (= 0 reconcile-count))
              (should
               (eq 'unknown
                   (chidu-draft-publish-result-status first-result)))
              (let ((uncertain-context
                     (chidu-draft-publish-result-context first-result)))
                (chidu-publish-draft
                 runtime uncertain-context identity 0 document
                 (lambda (value) (setq recovered-result value))
                 (lambda (failure) (ert-fail (format "%S" failure)))))
              (should (= 1 create-count))
              (should (= 1 reconcile-count))
              (should
               (eq 'saved
                   (chidu-draft-publish-result-status recovered-result)))
              (let* ((final-context
                      (chidu-draft-publish-result-context recovered-result))
                     (workspace
                      (chidu-store-compose-context-workspace final-context)))
                (should
                 (equal "draft-recovered"
                        (chidu-store-compose-workspace-base-remote-email-id
                         workspace)))
                (should-not
                 (chidu-store-compose-context-publish-attempt
                  final-context))))))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-draft-workflow-publishes-and-replaces-server-head ()
  "The vertical workflow should create a Draft and clean its predecessor."
  (let ((root (chidu-compose-test--directory))
        store runtime events first second)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (initial
                        (chidu-store-compose-document-create
                         :subject "First" :body "Body"))
                       (context
                        (chidu-compose-test--create-workspace
                         store account identity workspace-id initial))
                       (alternate-identity
                        (aref (chidu-store-account-identities account) 1)))
            (setq runtime (chidu-runtime-open :data-root root :store store)
                  store nil)
            (cl-letf
                (((symbol-function 'auth-source-search)
                  (lambda (&rest _arguments)
                    (list (list :secret
                                (lambda () (copy-sequence "secret"))))))
                 ((symbol-function 'chidu-jmap-draft-create)
                  (lambda (actual-context _document _creation-id _message-id
                                          _secret deliver)
                    (should
                     (equal
                      (chidu-store-identity-identity-id alternate-identity)
                      (chidu-store-identity-identity-id
                       (chidu-store-compose-context-identity actual-context))))
                    (let ((remote-id
                           (format "draft-%d"
                                   (1+ (cl-count 'create events)))))
                      (setq events (append events (list 'create)))
                      (funcall
                       deliver
                       (chidu-result-ok-create
                        :value
                        (chidu-jmap-draft-create-result-create
                         :outcome 'succeeded
                         :remote-email-id remote-id
                         :remote-blob-id (concat "blob-" remote-id)))))
                    #'ignore))
                 ((symbol-function 'chidu-jmap-draft-cleanup-read)
                  (lambda (actual-context attempt secret deliver)
                    (clear-string secret)
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-draft-cleanup-evidence-create
                       :state "cleanup-state"
                       :found-p t
                       :remote-blob-id
                       (chidu-store-draft-publish-attempt-predecessor-remote-blob-id
                        attempt)
                       :remote-mailbox-ids
                       (vector
                        (chidu-store-mailbox-remote-mailbox-id
                         (chidu-store-compose-context-drafts-mailbox
                          actual-context)))
                       :keywords ["$draft" "$seen"])))
                    #'ignore))
                 ((symbol-function 'chidu-jmap-draft-cleanup-destroy)
                  (lambda (_context attempt state secret deliver)
                    (clear-string secret)
                    (should (equal "cleanup-state" state))
                    (setq events
                          (append
                           events
                           (list
                            (list
                             'cleanup
                             (chidu-store-draft-publish-attempt-predecessor-remote-email-id
                              attempt)))))
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-draft-cleanup-result-create
                       :outcome 'succeeded)))
                    #'ignore)))
              (chidu-publish-draft
               runtime context alternate-identity 1 initial
               (lambda (value) (setq first value))
               (lambda (failure) (ert-fail (format "%S" failure))))
              (should (eq 'saved
                          (chidu-draft-publish-result-status first)))
              (let* ((first-context
                      (chidu-draft-publish-result-context first))
                     (updated
                      (chidu-store-compose-document-with
                       initial :subject "Second")))
                (chidu-publish-draft
                 runtime first-context alternate-identity 2 updated
                 (lambda (value) (setq second value))
                 (lambda (failure) (ert-fail (format "%S" failure))))
                (should (eq 'saved
                            (chidu-draft-publish-result-status second)))
                (should
                 (equal '(create create (cleanup "draft-1")) events))
                (let* ((final-context
                        (chidu-draft-publish-result-context second))
                       (workspace
                        (chidu-store-compose-context-workspace final-context)))
                  (should
                   (equal "draft-2"
                          (chidu-store-compose-workspace-base-remote-email-id
                           workspace)))
                  (should (= 2
                             (chidu-store-compose-workspace-published-revision
                              workspace)))
                  (should (= 0
                             (length
                              (chidu-store-compose-context-cleanup-attempts
                               final-context)))))))))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-compose-address-capf-completes-account-identities ()
  "Address fields should expose Account identities through standard CAPF."
  (let ((root (chidu-compose-test--directory))
        store runtime app compose-buffer)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (context
                        (chidu-compose-test--value
                         store
                         (chidu-store-op-create-compose-workspace-create
                          :workspace-id workspace-id
                          :account-id
                          (chidu-store-account-account-id account)
                          :identity-id
                          (chidu-store-identity-identity-id identity)
                          :kind 'new
                          :document
                          (chidu-store-compose-document-create)))) )
            (setq runtime (chidu-runtime-open :data-root root :store store)
                  store nil
                  app (appkit-app-start
                       chidu--app-type :input (chidu--state-create) :identity (make-symbol "compose-capf-test")))
            (appkit-app-send app (list :runtime runtime))
            (chidu-compose--open-context app context)
            (setq compose-buffer (current-buffer))
            (chidu-compose--goto-field 'to)
            (setq-local
             chidu-compose--address-candidates
             (vector
              (chidu-store-email-address-create
               :name "Me" :email "me@example.test")))
            (let ((completion (chidu-compose-completion-at-point)))
              (should completion)
              (should
               (member
                "Me <me@example.test>"
                (all-completions "" (nth 2 completion))))
              )))
      (when (buffer-live-p compose-buffer)
        (with-current-buffer compose-buffer
          (setq-local chidu-compose--closing-p t))
        (kill-buffer compose-buffer))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-compose-resource-freezes-private-exact-bytes ()
  "A selected file should become immutable private CAS data."
  (let* ((root (chidu-compose-test--directory))
         (source (expand-file-name "mutable.bin" root))
         observation target original)
    (unwind-protect
        (progn
          (setq original (unibyte-string 0 1 2 127 128 255))
          (let ((coding-system-for-write 'binary))
            (write-region original nil source nil 'silent))
          (setq observation
                (chidu-compose-resource-import root source 1024)
                target
                (chidu-compose-resource-path
                 root
                 (chidu-store-compose-resource-observation-digest
                  observation)))
          (should (= (length original)
                     (chidu-store-compose-resource-observation-size
                      observation)))
          (should (= #o600 (logand (file-modes target) #o777)))
          (let ((coding-system-for-read 'binary))
            (should
             (equal original
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally target)
                      (buffer-string)))))
          (with-temp-file source (insert "changed"))
          (let ((coding-system-for-read 'binary))
            (should
             (equal original
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally target)
                      (buffer-string))))))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-store-compose-resource-membership-is-atomic ()
  "Only dedicated CAS operations may change Compose resource membership."
  (let ((root (chidu-compose-test--directory)) store)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (resource-id (chidu-store-new-local-id))
                       (initial (chidu-store-compose-document-create
                                 :body "Body"))
                       (_created
                        (chidu-compose-test--create-workspace
                         store account identity workspace-id initial))
                       (resource
                        (chidu-store-compose-resource-observation-create
                         :resource-id resource-id :name "note.txt"
                         :media-type "text/plain" :size 4
                         :digest (make-string 64 ?a)
                         :disposition "attachment"))
                       (attached-document
                        (chidu-store-compose-document-with
                         initial :resource-ids (vector resource-id)))
                       (attached
                        (chidu-compose-test--value
                         store
                         (chidu-store-op-add-compose-resource-create
                          :workspace-id workspace-id
                          :identity-id
                          (chidu-store-identity-identity-id identity)
                          :expected-revision 0 :revision 1
                          :document attached-document
                          :resource resource))))
            (should (= 1 (length
                          (chidu-store-compose-context-resources attached))))
            (should
             (equal (vector resource-id)
                    (chidu-store-compose-document-resource-ids
                     (chidu-store-compose-workspace-document
                      (chidu-store-compose-context-workspace attached)))))
            (let ((smuggled
                   (chidu-compose-test--store-call
                    store
                    (chidu-store-op-checkpoint-compose-workspace-create
                     :workspace-id workspace-id
                     :identity-id
                     (chidu-store-identity-identity-id identity)
                     :expected-revision 1 :revision 2
                     :document initial))))
              (should (chidu-result-failure-p smuggled))
              (should
               (eq 'compose-resource-membership-conflict
                   (chidu-result-failure-kind smuggled))))
            (setq
             attached
             (chidu-compose-test--value
              store
              (chidu-store-op-set-compose-resource-blob-create
               :workspace-id workspace-id :resource-id resource-id
               :remote-blob-id "blob-note")))
            (should
             (equal "blob-note"
                    (chidu-store-compose-resource-remote-blob-id
                     (aref
                      (chidu-store-compose-context-resources attached) 0))))
            (let ((removed
                   (chidu-compose-test--value
                    store
                    (chidu-store-op-remove-compose-resource-create
                     :workspace-id workspace-id
                     :identity-id
                     (chidu-store-identity-identity-id identity)
                     :expected-revision 1 :revision 2
                     :document initial :resource-id resource-id))))
              (should (zerop
                       (length
                        (chidu-store-compose-context-resources removed))))
              (should (= 2
                         (chidu-store-compose-workspace-revision
                          (chidu-store-compose-context-workspace removed)))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-jmap-compose-and-upload-preserve-resource-evidence ()
  "JMAP upload and Draft compilation should use exact resource evidence."
  (let* ((endpoint
          (chidu-store-endpoint-create
           :endpoint-id "endpoint"
           :session-url "https://mail.example.test/.well-known/jmap"
           :login "me@example.test" :authentication 'basic
           :upload-url "https://upload.example.test/{accountId}"
           :max-size-upload 1024))
         (identity
          (chidu-store-identity-create
           :identity-id "identity" :remote-identity-id "remote-identity"
           :name "Me" :email "me@example.test" :available-p t))
         (resource-id (chidu-store-new-local-id))
         (resource
          (chidu-store-compose-resource-create
           :resource-id resource-id :workspace-id "workspace"
           :name "note.txt" :media-type "text/plain" :size 4
           :digest (make-string 64 ?b) :remote-blob-id "blob-note"
           :charset "utf-8" :disposition "attachment"
           :language ["en"]))
         (document
          (chidu-store-compose-document-create
           :body "Body" :resource-ids (vector resource-id)))
         (email
          (chidu-jmap-compose-draft-email
           document identity "drafts" nil (vector resource)))
         (structure (gethash "bodyStructure" email))
         (parts (gethash "subParts" structure))
         (attachment (aref parts 1)))
    (should
     (equal "https://upload.example.test/remote-account"
            (chidu-jmap-upload-url endpoint "remote-account")))
    (let ((result
           (chidu-jmap-upload-validate-response
            (chidu-compose-test--payload
             '(:accountId "remote-account" :blobId "blob-note"
                          :type "text/plain" :size 4))
            "remote-account" "text/plain" 4)))
      (should (equal "blob-note" (chidu-jmap-upload-result-blob-id result))))
    (should (equal "multipart/mixed" (gethash "type" structure)))
    (should (= 2 (length parts)))
    (should (equal "text/plain" (gethash "type" (aref parts 0))))
    (should (equal "blob-note" (gethash "blobId" attachment)))
    (should (equal "note.txt" (gethash "name" attachment)))
    (should (equal ["en"] (gethash "language" attachment)))))

(ert-deftest chidu-draft-workflow-uploads-local-resources-before-create ()
  "Draft publication should upload exact local bytes before Email/set."
  (let ((root (chidu-compose-test--directory))
        store runtime result events source)
    (unwind-protect
        (progn
          (setq source (expand-file-name "note.txt" root))
          (with-temp-file source (insert "note"))
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (initial (chidu-store-compose-document-create
                                 :body "Body"))
                       (_created
                        (chidu-compose-test--create-workspace
                         store account identity workspace-id initial))
                       (observation
                        (chidu-compose-resource-import root source 1024))
                       (resource-id
                        (chidu-store-compose-resource-observation-resource-id
                         observation))
                       (document
                        (chidu-store-compose-document-with
                         initial :resource-ids (vector resource-id)))
                       (context
                        (chidu-compose-test--value
                         store
                         (chidu-store-op-add-compose-resource-create
                          :workspace-id workspace-id
                          :identity-id
                          (chidu-store-identity-identity-id identity)
                          :expected-revision 0 :revision 1
                          :document document :resource observation))))
            (setq runtime (chidu-runtime-open :data-root root :store store)
                  store nil)
            (cl-letf
                (((symbol-function 'auth-source-search)
                  (lambda (&rest _arguments)
                    (list (list :secret
                                (lambda () (copy-sequence "secret"))))))
                 ((symbol-function 'chidu-jmap-upload-compose-resource)
                  (lambda (_endpoint _account resource file _secret deliver)
                    (setq events (append events '(upload)))
                    (should
                     (equal "note"
                            (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string))))
                    (should-not
                     (chidu-store-compose-resource-remote-blob-id resource))
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-upload-result-create
                       :blob-id "blob-note" :media-type "text/plain"
                       :size 4)))
                    #'ignore))
                 ((symbol-function 'chidu-jmap-draft-create)
                  (lambda (actual-context _document _creation-id _message-id
                                          _secret deliver)
                    (setq events (append events '(create)))
                    (should
                     (equal
                      "blob-note"
                      (chidu-store-compose-resource-remote-blob-id
                       (aref
                        (chidu-store-compose-context-resources
                         actual-context)
                        0))))
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-draft-create-result-create
                       :outcome 'succeeded
                       :remote-email-id "draft-note"
                       :remote-blob-id "blob-draft-note")))
                    #'ignore)))
              (chidu-publish-draft
               runtime context identity 1 document
               (lambda (value) (setq result value))
               (lambda (failure) (ert-fail (format "%S" failure))))
              (should (eq 'saved
                          (chidu-draft-publish-result-status result)))
              (should (equal '(upload create) events))
              (should
               (equal
                "blob-note"
                (chidu-store-compose-resource-remote-blob-id
                 (aref
                  (chidu-store-compose-context-resources
                   (chidu-draft-publish-result-context result))
                  0)))))))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-compose-ui-attachment-edits-share-one-durable-frontier ()
  "Attach/remove should settle one semantic and one durable revision each."
  (let ((root (chidu-compose-test--directory))
        store runtime app buffer source)
    (unwind-protect
        (progn
          (setq source (expand-file-name "ui-note.txt" root))
          (with-temp-file source (insert "stable bytes"))
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity)
                        (chidu-compose-test--connected-context store))
                       (workspace-id (chidu-store-new-local-id))
                       (context
                        (chidu-compose-test--create-workspace
                         store account identity workspace-id
                         (chidu-store-compose-document-create
                          :body "Body"))))
            (setq runtime (chidu-runtime-open :data-root root :store store)
                  store nil
                  app (appkit-app-start
                       chidu--app-type :input (chidu--state-create) :identity (make-symbol "compose-resource-ui")))
            (appkit-app-send app (list :runtime runtime))
            (chidu-compose--open-context app context)
            (setq buffer (current-buffer))
            (chidu-compose-attach-file source)
            (should (= 1 (length chidu-compose--resources)))
            (should-not (chidu-compose--dirty-p))
            (should
             (string-match-p
              "ui-note.txt"
              (or (overlay-get chidu-compose--resource-overlay
                               'after-string)
                  "")))
            (let* ((workspace (chidu-compose--workspace))
                   (resource (aref chidu-compose--resources 0)))
              (should
               (= (appkit-compose-generation)
                  (chidu-store-compose-workspace-revision workspace)))
              (should
               (equal
                (vector
                 (chidu-store-compose-resource-resource-id resource))
                (chidu-store-compose-document-resource-ids
                 (chidu-compose--document))))
              (chidu-compose-remove-attachment resource))
            (should (zerop (length chidu-compose--resources)))
            (should-not (chidu-compose--dirty-p))
            (should
             (= (appkit-compose-generation)
                (chidu-store-compose-workspace-revision
                 (chidu-compose--workspace))))
            (should
             (zerop
              (length
               (chidu-store-compose-document-resource-ids
                (chidu-compose--document)))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local chidu-compose--closing-p t))
        (kill-buffer buffer))
      (when (and app (appkit-app-live-p app)) (appkit-app-close app))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(provide 'chidu-compose-test)

;;; chidu-compose-test.el ends here
