;;; chidu-test.el --- In-process application tests for Chidu -*- lexical-binding: t; -*-

;;; Code:

(add-to-list
 'load-path
 (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'chidu)
(require 'chidu-email-sync)
(require 'chidu-live)
(require 'chidu-notify)
(require 'chidu-search)
(require 'chidu-test-support)
(require 'chidu-conversation)
(require 'chidu-message)

(defun chidu-test--store-call (store operation)
  "Synchronously invoke STORE OPERATION in a test."
  (let (result)
    (chidu-store-call store operation (lambda (value) (setq result value)))
    result))

(defun chidu-test--store-value (store operation)
  "Return successful STORE OPERATION value, or fail the current test."
  (let ((result (chidu-test--store-call store operation)))
    (unless (chidu-result-ok-p result)
      (ert-fail (format "Store operation failed: %S" result)))
    (chidu-result-ok-value result)))

(defun chidu-test--private-directory ()
  "Return a new current-uid mode-0700 test directory."
  (let ((directory (make-temp-file "chidu-runtime-test-" t)))
    (set-file-modes directory #o700)
    directory))

(defun chidu-test--identity-observation ()
  "Return one fake Identity observation."
  (chidu-store-identity-observation-create
   :remote-identity-id "remote-identity"
   :name "Me"
   :email "me@example.test"))

(defun chidu-test--account-observation (&optional identities)
  "Return one fake mail-capable Account observation."
  (chidu-store-account-observation-create
   :remote-account-id "remote-account"
   :name "Mail"
   :personal-p t
   :read-only-p nil
   :primary-mail-p t
   :primary-submission-p t
   :identity-state "identity-state"
   :capabilities
   (vector chidu-jmap-mail-capability
           chidu-jmap-submission-capability)
   :identities (or identities (vector))))

(defun chidu-test--session-observation ()
  "Return one fake Session observation with an Account and Identity."
  (chidu-store-session-observation-create
   :username "me@example.test"
   :state "session-state"
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
   :accounts
   (vector
    (chidu-test--account-observation
     (vector (chidu-test--identity-observation))))))

(defun chidu-test--configure-endpoint (store)
  "Configure one fake Endpoint in STORE and return it."
  (chidu-test--store-value
   store
   (chidu-store-op-configure-endpoint-create
    :session-url "https://mail.example.test/.well-known/jmap"
    :login "me@example.test"
    :authentication 'basic)))

(defun chidu-test--prepare-connected-account (store)
  "Create connected Endpoint state in STORE and return (ENDPOINT ACCOUNT)."
  (let* ((configured (chidu-test--configure-endpoint store))
         (connected
          (chidu-test--store-value
           store
           (chidu-store-op-observe-session-create
            :endpoint-id (chidu-store-endpoint-endpoint-id configured)
            :observation (chidu-test--session-observation))))
         (account (aref (chidu-store-endpoint-accounts connected) 0)))
    (list connected account)))

(defun chidu-test--mailbox-rights ()
  "Return permissive fake Mailbox rights."
  (chidu-store-mailbox-rights-create
   :may-read-items-p t
   :may-add-items-p t
   :may-remove-items-p t
   :may-set-seen-p t
   :may-set-keywords-p t
   :may-create-child-p t
   :may-rename-p t
   :may-delete-p t
   :may-submit-p t))

(defun chidu-test--mailbox-snapshot (&optional state)
  "Return one complete fake Mailbox snapshot using STATE."
  (chidu-store-mailbox-snapshot-observation-create
   :state (or state "mailbox-state")
   :mailboxes
   (vector
    (chidu-store-mailbox-observation-create
     :remote-mailbox-id "inbox"
     :name "Inbox"
     :parent-remote-mailbox-id nil
     :role "inbox"
     :sort-order 10
     :total-emails 4
     :unread-emails 1
     :total-threads 3
     :unread-threads 1
     :rights (chidu-test--mailbox-rights)
     :subscribed-p t))))

(cl-defmacro chidu-test--with-app ((app state) &rest body)
  "Evaluate BODY with APP owning STATE and clean all attached buffers."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(let* ((,app
           (appkit-app-start
            chidu--app-type :identity (make-symbol "chidu-test")
            :input ,state))
          (buffer-name (generate-new-buffer-name "*Chidu test*"))
          (chidu-home-buffer-name buffer-name))
     (unwind-protect
         (progn ,@body)
       (let (buffers)
         (maphash (lambda (_identity entry)
                    (push (appkit-surface-buffer (cdr entry)) buffers))
                  (appkit-app-surfaces ,app))
         (appkit-app-close ,app)
         (dolist (buffer buffers)
           (when (buffer-live-p buffer) (kill-buffer buffer))))
       (when-let* ((buffer (get-buffer buffer-name)))
         (kill-buffer buffer)))))

(ert-deftest chidu-runtime-open-is-purely-local-even-with-plz-loaded ()
  (let ((directory (chidu-test--private-directory))
        (store (chidu-test-store-create))
        runtime
        listed
        http-called)
    (unwind-protect
        (cl-letf (((symbol-function 'chidu-jmap-http-request)
                   (lambda (&rest _arguments)
                     (setq http-called t)
                     (ert-fail "runtime open must not perform HTTP"))))
          (setq runtime
                (chidu-runtime-open :data-root directory :store store))
          (chidu-runtime-list-endpoints
           runtime (lambda (value) (setq listed value)) #'ignore)
          (should (vectorp listed))
          (should-not http-called))
      (when runtime (chidu-runtime-close runtime))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest chidu-runtime-types-plz-startup-failure ()
  (let ((directory (chidu-test--private-directory))
        (store (chidu-test-store-create))
        runtime
        endpoint
        failure)
    (unwind-protect
        (progn
          (setq endpoint (chidu-test--configure-endpoint store)
                runtime (chidu-runtime-open :data-root directory :store store))
          (cl-letf (((symbol-function 'auth-source-search)
                     (lambda (&rest _arguments)
                       (list (list :secret (lambda () "secret")))))
                    ((symbol-function 'plz)
                     (lambda (&rest _arguments)
                       (signal 'file-missing '("curl executable is unavailable")))))
            (chidu-runtime-connect-endpoint
             runtime endpoint #'ignore (lambda (value) (setq failure value))))
          (should (chidu-result-failure-p failure))
          (should (eq 'transport-unavailable
                      (chidu-result-failure-kind failure))))
      (when runtime (chidu-runtime-close runtime))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest chidu-runtime-does-not-swallow-consumer-callback-faults ()
  (let ((directory (chidu-test--private-directory))
        (store (chidu-test-store-create))
        runtime)
    (unwind-protect
        (progn
          (setq runtime
                (chidu-runtime-open
                 :data-root directory
                 :store store))
          (should-error
           (chidu-runtime-list-endpoints
            runtime
            (lambda (_value) (error "consumer callback fault"))
            #'ignore)
           :type 'error))
      (when runtime (chidu-runtime-close runtime))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest chidu-runtime-configures-and-lists-endpoints-directly ()
  (let ((directory (chidu-test--private-directory))
        (store (chidu-test-store-create))
        runtime
        configured
        listed
        failure)
    (unwind-protect
        (progn
          (setq runtime
                (chidu-runtime-open
                 :data-root directory
                 :store store))
          (chidu-runtime-configure-endpoint
           runtime
           "https://mail.example.test/.well-known/jmap"
           "me@example.test"
           'basic
           (lambda (value) (setq configured value))
           (lambda (value) (setq failure value)))
          (should (chidu-store-endpoint-p configured))
          (should-not failure)
          (chidu-runtime-list-endpoints
           runtime
           (lambda (value) (setq listed value))
           (lambda (value) (setq failure value)))
          (should (= 1 (length listed)))
          (should
           (equal
            (chidu-store-endpoint-endpoint-id configured)
            (chidu-store-endpoint-endpoint-id (aref listed 0))))
          (let ((metrics (chidu-runtime-store-metrics runtime)))
            (should (= 2 (plist-get metrics :count)))
            (should (numberp (plist-get metrics :max-seconds)))
            (should
             (eq 'chidu-store-op-list-endpoints
                 (plist-get metrics :last-operation))))
          (let ((spec
                 (chidu--normalize-endpoint-spec
                  '(:host "mail.example.test" :user "me@example.test"))))
            (should
             (equal "https://mail.example.test/.well-known/jmap"
                    (plist-get spec :session-url)))
            (should (= 443 (plist-get spec :port)))
            (should (eq 'basic (plist-get spec :authentication)))))
      (when runtime (chidu-runtime-close runtime))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest chidu-runtime-sqlite-persists-through-interactive-restart ()
  (skip-unless (sqlite-available-p))
  (let ((root (chidu-test--private-directory))
        first-runtime
        second-runtime
        endpoint-id
        listed
        failure)
    (unwind-protect
        (progn
          (setq first-runtime
                (chidu-runtime-open
                 :data-root root))
          (chidu-runtime-configure-endpoint
           first-runtime
           "https://mail.example.test/.well-known/jmap"
           "me@example.test"
           'basic
           (lambda (endpoint)
             (setq endpoint-id
                   (chidu-store-endpoint-endpoint-id endpoint)))
           (lambda (value) (setq failure value)))
          (should (stringp endpoint-id))
          (should-not failure)
          (chidu-runtime-close first-runtime)
          (setq first-runtime nil
                second-runtime
                (chidu-runtime-open
                 :data-root root))
          (chidu-runtime-list-endpoints
           second-runtime
           (lambda (value) (setq listed value))
           (lambda (value) (setq failure value)))
          (should-not failure)
          (should (= 1 (length listed)))
          (should
           (equal endpoint-id
                  (chidu-store-endpoint-endpoint-id (aref listed 0))))
          (should
           (eq 'sqlite
               (plist-get
                (chidu-store-inspect
                 (chidu-runtime-store second-runtime))
                :backend))))
      (when first-runtime (chidu-runtime-close first-runtime))
      (when second-runtime (chidu-runtime-close second-runtime))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-runtime-connects-without-local-rpc-or-staging-file ()
  (let ((directory (chidu-test--private-directory))
        (store (chidu-test-store-create))
        runtime
        endpoint
        connected
        failure
        secret-reference
        secret-seen)
    (unwind-protect
        (progn
          (setq endpoint (chidu-test--configure-endpoint store)
                runtime
                (chidu-runtime-open
                 :data-root directory
                 :store store))
          (cl-letf
              (((symbol-function 'auth-source-search)
                (lambda (&rest _arguments)
                  (list
                   (list :user "me@example.test"
                         :secret (lambda () "top-secret")))))
               ((symbol-function 'chidu-jmap-discover)
                (lambda (actual-endpoint secret deliver)
                  (should (eq endpoint actual-endpoint))
                  (setq secret-reference secret
                        secret-seen (equal secret "top-secret"))
                  (clear-string secret)
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value (chidu-test--session-observation)))
                  nil)))
            (chidu-runtime-connect-endpoint
             runtime endpoint
             (lambda (value) (setq connected value))
             (lambda (value) (setq failure value))))
          (should secret-seen)
          (should-not (equal secret-reference "top-secret"))
          (should-not failure)
          (should (chidu-store-endpoint-p connected))
          (should
           (equal "session-state"
                  (chidu-store-endpoint-session-state connected)))
          (should-not
           (string-match-p "top-secret" (prin1-to-string runtime)))
          (should-not
           (directory-files directory nil "credential\|ticket")))
      (when runtime (chidu-runtime-close runtime))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest chidu-runtime-mailbox-sync-is-a-direct-closed-workflow ()
  (let ((directory (chidu-test--private-directory))
        (store (chidu-test-store-create))
        runtime
        account
        result
        failure
        fetched)
    (unwind-protect
        (progn
          (setq account
                (cadr (chidu-test--prepare-connected-account store))
                runtime
                (chidu-runtime-open
                 :data-root directory
                 :store store))
          (cl-letf
              (((symbol-function 'auth-source-search)
                (lambda (&rest _arguments)
                  (list (list :secret (lambda () "mailbox-secret")))))
               ((symbol-function 'chidu-jmap-fetch-mailboxes)
                (lambda (context secret deliver)
                  (should
                   (equal
                    (chidu-store-account-account-id account)
                    (chidu-store-account-account-id
                     (chidu-store-mailbox-sync-context-account context))))
                  (setq fetched t)
                  (clear-string secret)
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value (chidu-test--mailbox-snapshot)))
                  nil)))
            (chidu-runtime-sync-mailboxes
             runtime account
             (lambda (value) (setq result value))
             (lambda (value) (setq failure value))))
          (should fetched)
          (should-not failure)
          (should (chidu-store-mailbox-sync-context-p result))
          (should (= 1 (chidu-store-mailbox-sync-context-revision result)))
          (should
           (equal "mailbox-state"
                  (chidu-store-mailbox-sync-context-state result)))
          (should
           (= 1
              (length
               (chidu-store-mailbox-sync-context-mailboxes result))))
          (let ((account-state
                 (gethash
                  (chidu-store-account-account-id account)
                  (chidu-runtime-accounts runtime))))
            (should (eq 'idle (chidu-account-runtime-phase account-state)))))
      (when runtime (chidu-runtime-close runtime))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest chidu-account-sync-is-mailbox-only ()
  (let* ((store (chidu-test-store-create))
         (prepared (chidu-test--prepare-connected-account store))
         (endpoint (car prepared))
         (account (cadr prepared))
         (account-id (chidu-store-account-account-id account))
         (mailbox-context
          (chidu-test--store-value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id account-id :expected-revision 0
            :observation (chidu-test--mailbox-snapshot))))
         (store-info
          (chidu-test--store-value store (chidu-store-op-runtime-create)))
         (accounts (make-hash-table :test #'equal))
         (runtime (chidu-runtime-open :store store))
         mailbox-synced
         indexed)
    (puthash account-id
             (chidu--account-ui-state-create :account account)
             accounts)
    (unwind-protect
        (chidu-test--with-app
            (app
             (chidu--state-create
              :phase 'ready :store-info store-info
              :endpoints (vector endpoint) :accounts accounts))
          (appkit-app-send app (list :runtime runtime))
          (let ((chidu--app app))
            (cl-letf
                (((symbol-function 'chidu-runtime-sync-mailboxes)
                  (lambda (actual-runtime actual-account success _error)
                    (should (eq runtime actual-runtime))
                    (should (eq account actual-account))
                    (setq mailbox-synced t)
                    (funcall success mailbox-context)
                    nil))
                 ((symbol-function 'chidu-email-index-account)
                  (lambda (&rest _arguments) (setq indexed t))))
              (chidu-sync-account account))
            (should mailbox-synced)
            (should-not indexed)
            (let ((ui (gethash account-id (chidu--state-accounts (chidu-app-state app)))))
              (should (eq 'idle (chidu--account-ui-state-phase ui)))
              (should
               (eq mailbox-context
                   (chidu--account-ui-state-mailbox-context ui)))
              )))
      (when runtime (chidu-runtime-close runtime)))))

(ert-deftest chidu-email-live-sync-never-starts-an-initial-index ()
  (let* ((store (chidu-test-store-create))
         (account
          (cadr (chidu-test--prepare-connected-account store)))
         (runtime (chidu-runtime-open :store store))
         result failure network-called)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chidu-jmap-email-fetch-state)
              (lambda (&rest _arguments)
                (setq network-called t)
                (ert-fail "live sync must not start an initial Email index"))))
          (chidu-email-sync-live
           runtime account
           (lambda (value) (setq result value))
           (lambda (value) (setq failure value)))
          (should-not result)
          (should-not network-called)
          (should (chidu-result-failure-p failure))
          (should (eq 'email-index-unavailable
                      (chidu-result-failure-kind failure))))
      (when runtime (chidu-runtime-close runtime)))))

(ert-deftest chidu-email-index-is-an-explicit-account-operation ()
  (let* ((store (chidu-test-store-create))
         (prepared (chidu-test--prepare-connected-account store))
         (endpoint (car prepared))
         (account (cadr prepared))
         (account-id (chidu-store-account-account-id account))
         (store-info
          (chidu-test--store-value store (chidu-store-op-runtime-create)))
         (accounts (make-hash-table :test #'equal))
         (runtime (chidu-runtime-open :store store))
         indexed
         mailbox-synced)
    (puthash account-id
             (chidu--account-ui-state-create :account account)
             accounts)
    (unwind-protect
        (chidu-test--with-app
            (app
             (chidu--state-create
              :phase 'ready :store-info store-info
              :endpoints (vector endpoint) :accounts accounts))
          (appkit-app-send app (list :runtime runtime))
          (let ((chidu--app app))
            (cl-letf
                (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'chidu-runtime-sync-mailboxes)
                  (lambda (&rest _arguments) (setq mailbox-synced t)))
                 ((symbol-function 'chidu-email-index-account)
                  (lambda (actual-runtime actual-account success _error
                                          &optional _limit)
                    (should (eq runtime actual-runtime))
                    (should (eq account actual-account))
                    (setq indexed t)
                    (funcall success nil)
                    (chidu-runtime-operation-create :id 99))))
              (chidu-index-account account))
            (should indexed)
            (should-not mailbox-synced)
            (should-not
             (gethash (chidu--email-index-key account)
                      (chidu-app-requests app)))
            (should
             (eq 'idle
                 (chidu--account-ui-state-phase
                  (gethash account-id (chidu--state-accounts (chidu-app-state app))))))))
      (when runtime (chidu-runtime-close runtime)))))

(ert-deftest chidu-email-bootstrap-resumes-through-query-state-drift ()
  (let ((store (chidu-test-store-create))
        runtime
        account
        result
        failure
        secret-reference
        (state-calls 0)
        (query-calls 0)
        (changes-calls 0)
        (hydration-calls 0))
    (unwind-protect
        (progn
          (setq account
                (cadr (chidu-test--prepare-connected-account store))
                runtime (chidu-runtime-open :store store))
          (chidu-test--store-value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id (chidu-store-account-account-id account)
            :expected-revision 0
            :observation (chidu-test--mailbox-snapshot)))
          (cl-letf
              (((symbol-function 'auth-source-search)
                (lambda (&rest _arguments)
                  (list (list :secret (lambda () "email-secret")))))
               ((symbol-function 'chidu-jmap-email-fetch-state)
                (lambda (_context secret deliver)
                  (if secret-reference
                      (should (eq secret-reference secret))
                    (setq secret-reference secret))
                  (cl-incf state-calls)
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value (if (= state-calls 1) "email-0" "email-1")))
                  #'ignore))
               ((symbol-function 'chidu-jmap-email-fetch-query-page)
                (lambda (context secret limit deliver)
                  (should (eq secret-reference secret))
                  (should (= 2 limit))
                  (cl-incf query-calls)
                  (pcase-let
                      ((`(,query-state ,position ,ids ,expected-anchor)
                        (pcase query-calls
                          (1 '("query-1" 0 ["email-1" "email-2"] nil))
                          (2 '("query-2" 2 ["email-3"] "email-2"))
                          (3 '("query-2" 0 ["email-1" "email-3"] nil))
                          (4 '("query-2" 2 [] "email-3"))
                          (_ (ert-fail "unexpected Email/query request")))))
                    (should
                     (equal expected-anchor
                            (chidu-store-email-sync-context-anchor-remote-email-id
                             context)))
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (chidu-store-email-query-page-observation-create
                       :query-state query-state
                       :can-calculate-changes-p t
                       :position position
                       :remote-email-ids ids))))
                  #'ignore))
               ((symbol-function 'chidu-jmap-email-fetch-changes-page)
                (lambda (context secret limit deliver)
                  (should (eq secret-reference secret))
                  (cl-incf changes-calls)
                  (pcase changes-calls
                    (1
                     (should (= chidu-email-changes-page-size limit))
                     (should
                      (equal "email-1"
                             (chidu-store-email-sync-context-state context)))
                     (funcall
                      deliver
                      (chidu-result-ok-create
                       :value
                       (chidu-jmap-email-changes-page-create
                        :session-state "session"
                        :old-state "email-1" :new-state "email-2"
                        :has-more-changes-p nil
                        :created (vector "email-4")
                        :destroyed (vector "email-1")))))
                    (2
                     (should (= 256 limit))
                     (should
                      (equal "email-2"
                             (chidu-store-email-sync-context-state context)))
                     (funcall
                      deliver
                      (chidu-result-ok-create
                       :value
                       (chidu-jmap-email-changes-page-create
                        :session-state "session"
                        :old-state "email-2" :new-state "email-2"
                        :has-more-changes-p nil))))
                    (_ (ert-fail "unexpected Email/changes request")))
                  #'ignore))
               ((symbol-function 'chidu-jmap-email-fetch-hydration)
                (lambda (_context secret plan deliver)
                  (should (eq secret-reference secret))
                  (cl-incf hydration-calls)
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value
                    (chidu-store-test--hydration-observation plan)))
                  #'ignore)))
            (chidu-email-index-account
             runtime account
             (lambda (value) (setq result value))
             (lambda (value) (setq failure value))
             2))
          (should-not failure)
          (should (chidu-store-email-sync-context-p result))
          (should (eq 'live
                      (chidu-store-email-sync-context-phase result)))
          (should (equal "email-2"
                         (chidu-store-email-sync-context-state result)))
          (should (= 2 (chidu-store-email-sync-context-committed-count result)))
          (should (= 10 (chidu-store-email-sync-context-revision result)))
          (should (= 2 state-calls))
          (should (= 4 query-calls))
          (should (= 2 changes-calls))
          (should (= 1 hydration-calls))
          (should-not (equal "email-secret" secret-reference))
          (let ((account-state
                 (gethash
                  (chidu-store-account-account-id account)
                  (chidu-runtime-accounts runtime))))
            (should (eq 'idle (chidu-account-runtime-phase account-state)))))
      (when runtime (chidu-runtime-close runtime)))))

(ert-deftest chidu-email-bootstrap-restarts-on-cannot-calculate-changes ()
  (let ((store (chidu-test-store-create))
        runtime account result failure
        (state-calls 0) (query-calls 0) (changes-calls 0)
        first-generation second-generation)
    (unwind-protect
        (progn
          (setq account
                (cadr (chidu-test--prepare-connected-account store))
                runtime (chidu-runtime-open :store store))
          (chidu-test--store-value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id (chidu-store-account-account-id account)
            :expected-revision 0
            :observation (chidu-test--mailbox-snapshot)))
          (cl-letf
              (((symbol-function 'auth-source-search)
                (lambda (&rest _arguments)
                  (list (list :secret (lambda () "email-secret")))))
               ((symbol-function 'chidu-jmap-email-fetch-state)
                (lambda (_context _secret deliver)
                  (cl-incf state-calls)
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value (if (= state-calls 1) "email-0" "email-1")))
                  #'ignore))
               ((symbol-function 'chidu-jmap-email-fetch-query-page)
                (lambda (context _secret _limit deliver)
                  (cl-incf query-calls)
                  (when (= query-calls 1)
                    (setq first-generation
                          (chidu-store-email-sync-context-generation-id
                           context)))
                  (when (= query-calls 3)
                    (setq second-generation
                          (chidu-store-email-sync-context-generation-id
                           context)))
                  (let* ((first-page (memq query-calls '(1 3)))
                         (id (if (= query-calls 1) "email-old" "email-new")))
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (chidu-store-email-query-page-observation-create
                       :query-state
                       (if (< query-calls 3) "query-0" "query-1")
                       :can-calculate-changes-p t
                       :position
                       (chidu-store-email-sync-context-committed-count context)
                       :remote-email-ids
                       (if first-page (vector id) (vector))))))
                  #'ignore))
               ((symbol-function 'chidu-jmap-email-fetch-changes-page)
                (lambda (context _secret _limit deliver)
                  (cl-incf changes-calls)
                  (funcall
                   deliver
                   (pcase changes-calls
                     (1
                      (chidu-result-failure-create
                       :kind 'cannot-calculate-changes
                       :data (list :method "Email/changes")
                       :retryable-p nil))
                     (2
                      (chidu-result-ok-create
                       :value
                       (chidu-jmap-email-changes-page-create
                        :session-state "session"
                        :old-state
                        (chidu-store-email-sync-context-state context)
                        :new-state "email-2"
                        :has-more-changes-p nil)))
                     (3
                      (chidu-result-ok-create
                       :value
                       (chidu-jmap-email-changes-page-create
                        :session-state "session"
                        :old-state "email-2" :new-state "email-2"
                        :has-more-changes-p nil)))
                     (_ (ert-fail "unexpected Email/changes request"))))
                  #'ignore))
               ((symbol-function 'chidu-jmap-email-fetch-hydration)
                (lambda (_context _secret plan deliver)
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value
                    (chidu-store-test--hydration-observation plan)))
                  #'ignore)))
            (chidu-email-index-account
             runtime account
             (lambda (value) (setq result value))
             (lambda (value) (setq failure value))
             2))
          (should-not failure)
          (should (chidu-store-email-sync-context-p result))
          (should (eq 'live
                      (chidu-store-email-sync-context-phase result)))
          (should (equal "email-2"
                         (chidu-store-email-sync-context-state result)))
          (should (= 2 state-calls))
          (should (= 4 query-calls))
          (should (= 3 changes-calls))
          (should first-generation)
          (should second-generation)
          (should-not (equal first-generation second-generation)))
      (when runtime (chidu-runtime-close runtime)))))

(ert-deftest chidu-runtime-account-cancel-restores-idle-admission ()
  (let ((directory (chidu-test--private-directory))
        (store (chidu-test-store-create))
        runtime
        account
        first
        second
        busy
        (cancel-count 0))
    (unwind-protect
        (progn
          (setq account
                (cadr (chidu-test--prepare-connected-account store))
                runtime
                (chidu-runtime-open
                 :data-root directory
                 :store store))
          (cl-letf
              (((symbol-function 'auth-source-search)
                (lambda (&rest _arguments)
                  (list (list :secret (lambda () "mailbox-secret")))))
               ((symbol-function 'chidu-jmap-fetch-mailboxes)
                (lambda (_context secret _deliver)
                  (clear-string secret)
                  (lambda () (cl-incf cancel-count)))))
            (setq first
                  (chidu-runtime-sync-mailboxes
                   runtime account #'ignore (lambda (value) (setq busy value))))
            (should (chidu-runtime-operation-p first))
            (setq second
                  (chidu-runtime-sync-mailboxes
                   runtime account #'ignore (lambda (value) (setq busy value))))
            (should-not second)
            (should (chidu-result-failure-p busy))
            (should (eq 'account-busy (chidu-result-failure-kind busy)))
            (should (chidu-runtime-cancel-operation runtime first))
            (should (= 1 cancel-count))
            (let ((state
                   (gethash
                    (chidu-store-account-account-id account)
                    (chidu-runtime-accounts runtime))))
              (should (eq 'idle (chidu-account-runtime-phase state))))
            (setq busy nil
                  second
                  (chidu-runtime-sync-mailboxes
                   runtime account #'ignore (lambda (value) (setq busy value))))
            (should (chidu-runtime-operation-p second))
            (should-not busy)
            (should (chidu-runtime-cancel-operation runtime second))
            (should (= 2 cancel-count))))
      (when runtime (chidu-runtime-close runtime))
      (when (file-directory-p directory) (delete-directory directory t)))))

(ert-deftest chidu-runtime-close-cancels-active-effects ()
  (let ((store (chidu-test-store-create))
        runtime
        endpoint
        operation
        canceled
        cleaned)
    (setq endpoint (chidu-test--configure-endpoint store))
    (cl-letf
        (((symbol-function 'auth-source-search)
          (lambda (&rest _arguments)
            (list (list :secret (lambda () "close-secret")))))
         ((symbol-function 'chidu-jmap-discover)
          (lambda (_endpoint secret _deliver)
            (clear-string secret)
            (lambda () (setq canceled t)))))
      (setq runtime (chidu-runtime-open :store store)
            operation
            (chidu-runtime-connect-endpoint
             runtime endpoint #'ignore #'ignore))
      (should (chidu-runtime-operation-p operation))
      (setf
       (chidu-runtime-operation-cancel-cleanup-function operation)
       (lambda () (setq cleaned t)))
      (should (chidu-runtime-close runtime))
      (should canceled)
      (should cleaned)
      (should (chidu-runtime-closed-p runtime)))))

(ert-deftest chidu-root-summary-and-conversation-form-one-local-ui-path ()
  (cl-labels
      ((sender-name
         (remote-id)
         (pcase remote-id
           ("email-1" "Kai Ma")
           ("email-2" "Eli Zaretskii")
           ("email-3" "Johan Myréen")
           (_ "Stefan Monnier")))
       (sender-email
         (remote-id)
         (pcase remote-id
           ("email-1" "kai@example.test")
           ("email-2" "eli@example.test")
           ("email-3" "johan@example.test")
           (_ "stefan@example.test")))
       (summary-row
         (remote-id subject)
         (chidu-store-email-summary-observation-row-create
          :remote-email-id remote-id :remote-thread-id "thread-1"
          :received-at "2026-08-25T01:02:03Z"
          :from-name (sender-name remote-id)
          :from-email (sender-email remote-id)
          :subject subject :preview (concat "preview-" remote-id)
          :unread-p nil :flagged-p nil
          :has-attachment-p (equal remote-id "email-2")))
       (conversation-row
         (remote-id message-id reply references)
         (chidu-store-conversation-observation-row-create
          :summary-row (summary-row remote-id remote-id)
          :sent-at nil
          :message-ids (vector message-id)
          :in-reply-to (if reply (vector reply) (vector))
          :references (vconcat references))))
    (let* ((appkit-discussion-connector-style 'text)
           (store (chidu-test-store-create))
           (prepared (chidu-test--prepare-connected-account store))
           (endpoint (car prepared))
           (account (cadr prepared))
           (account-id (chidu-store-account-account-id account))
           (mailbox-context
            (chidu-test--store-value
             store
             (chidu-store-op-observe-mailbox-snapshot-create
              :account-id account-id :expected-revision 0
              :observation (chidu-test--mailbox-snapshot))))
           (mailbox
            (aref (chidu-store-mailbox-sync-context-mailboxes mailbox-context) 0))
           (mailbox-id (chidu-store-mailbox-mailbox-id mailbox))
           (_generation
            (chidu-store-test--activate-email-generation
             store account-id
             (vector
              (chidu-store-test--email-entry
               "email-1" "2026-08-25T04:00:00Z"
               :thread-id "thread-1"
               :from-name "Kai Ma" :from-email "kai@example.test"
               :subject "Root" :preview "preview-email-1"
               :keywords (vector "$seen"))
              (chidu-store-test--email-entry
               "email-2" "2026-08-25T03:00:00Z"
               :thread-id "thread-1"
               :from-name "Eli Zaretskii" :from-email "eli@example.test"
               :subject "Reply" :preview "preview-email-2"
               :keywords (vector "$seen") :has-attachment-p t)
              (chidu-store-test--email-entry
               "email-3" "2026-08-25T02:00:00Z"
               :thread-id "thread-1"
               :from-name "Johan Myréen" :from-email "johan@example.test"
               :subject "Nested" :preview "preview-email-3"
               :keywords (vector "$seen"))
              (chidu-store-test--email-entry
               "email-4" "2026-08-25T01:00:00Z"
               :thread-id "thread-1"
               :from-name "Stefan Monnier" :from-email "stefan@example.test"
               :subject "Sibling" :preview "preview-email-4"
               :keywords (vector "$seen")))))
           (summary
            (chidu-store-test--canonical-summary
             store account-id mailbox 50))
           (root-row
            (aref (chidu-store-mailbox-summary-context-rows summary) 0))
           (selected
            (aref (chidu-store-mailbox-summary-context-rows summary) 1))
           (nested-row
            (aref (chidu-store-mailbox-summary-context-rows summary) 2))
           (sibling-row
            (aref (chidu-store-mailbox-summary-context-rows summary) 3))
           (_conversation
            (chidu-test--store-value
             store
             (chidu-store-op-replace-conversation-create
              :account-id account-id :remote-thread-id "thread-1"
              :expected-revision 0
              :observation
              (chidu-store-conversation-observation-create
               :remote-thread-id "thread-1" :thread-state "thread"
               :email-state "conversation-email" :complete-p t
               :rows
               (vector
                (conversation-row "email-1" "m1" "missing-root" '("missing-root"))
                (conversation-row "email-2" "m2" "m1" '("m1"))
                (conversation-row "email-3" "m3" nil '("m1" "m2"))
                (conversation-row "email-4" "m4" "m1" '("m1")))))))
           (_body
            (chidu-test--store-value
             store
             (chidu-store-op-replace-email-body-create
              :account-id account-id
              :local-email-id
              (chidu-store-email-summary-row-local-email-id selected)
              :remote-email-id "email-2" :expected-revision 0
              :observation
              (chidu-store-email-body-observation-create
               :remote-email-id "email-2" :email-state "body"
               :text-content
               "Hi Eli,

> Kai Ma wrote:
>> Eli Zaretskii replied:

Selected full reply body"
               :html-content ""
               :truncated-p nil :encoding-problem-p nil
               :attachments
               (vector
                (chidu-store-email-attachment-create
                 :part-id "attachment-1" :blob-id "blob-attachment-1"
                 :size 128 :name "notes.txt" :media-type "text/plain"
                 :charset "utf-8" :disposition "attachment"
                 :language (vector)))))))
           (search-spec
            (chidu-search-query-compile
             "emoji width" (vector mailbox) mailbox))
           (_search
            (chidu-test--store-value
             store
             (chidu-store-op-replace-search-create
              :account-id account-id
              :query-key (chidu-search-spec-query-key search-spec)
              :expected-revision 0
              :observation
              (chidu-store-search-observation-create
               :query-key (chidu-search-spec-query-key search-spec)
               :query-text (chidu-search-spec-query-text search-spec)
               :filter-json (chidu-search-spec-filter-json search-spec)
               :query-state "search-query"
               :email-state "search-email"
               :cursor-remote-email-id "email-2"
               :maybe-more-p t
               :rows
               (vector
                (chidu-store-search-observation-row-create
                 :summary-row (summary-row "email-2" "Reply")
                 :remote-mailbox-ids (vector "inbox")
                 :snippet
                 (chidu-store-search-snippet-create
                  :subject "<mark>Emoji</mark> width"
                  :preview "matching <mark>emoji</mark> text")))))))
           (store-info
            (chidu-test--store-value store (chidu-store-op-runtime-create)))
           (accounts (make-hash-table :test #'equal))
           (runtime
            (chidu-runtime-open
             :data-root (plist-get (chidu-store-inspect store) :root)
             :store store)))
      (puthash
       account-id
       (chidu--account-ui-state-create
        :account account :mailbox-context mailbox-context)
       accounts)
      (chidu-test--with-app
          (app
           (chidu--state-create
            :phase 'ready :store-info store-info
            :endpoints (vector endpoint) :accounts accounts))
        (appkit-app-send app (list :runtime runtime))
        (let* ((home (chidu--open-home app))
               (home-buffer (appkit-surface-buffer home))
               summary-buffer
               conversation-buffer
               search-buffer
               message-buffer)
          (unwind-protect
              (progn
                (with-current-buffer home-buffer
                  (should (string-match-p "Inbox" (buffer-string)))
                  (should (string-match-p "1 unread" (buffer-string)))
                  (goto-char (point-min))
                  (search-forward "Inbox")
                  (beginning-of-line)
                  (chidu--home-activate-mailbox
                   nil (appkit-directory-entry-at-point)))
                (let ((summary-view
                       (appkit-app-surface
                        app (list 'summary account-id mailbox-id))))
                  (chidu-test-drain summary-view)
                  (setq summary-buffer (appkit-surface-buffer summary-view)))
                (with-current-buffer summary-buffer
                  (goto-char (point-min))
                  (let ((match
                         (text-property-search-forward
                          'chidu-summary-email-id
                          (chidu-store-email-summary-row-local-email-id
                           selected)
                          #'equal)))
                    (should match)
                    (goto-char (prop-match-beginning match))
                    (chidu-summary-open-conversation)))
                (let ((conversation-view
                       (appkit-app-surface
                        app (list 'conversation account-id "thread-1"))))
                  (chidu-test-drain conversation-view)
                  (setq conversation-buffer
                        (appkit-surface-buffer conversation-view)))
                (with-current-buffer conversation-buffer
                  (let* ((view (appkit-current-surface))
                         (root-id
                          (chidu-store-email-summary-row-local-email-id root-row))
                         (selected-id
                          (chidu-store-email-summary-row-local-email-id selected))
                         (nested-id
                          (chidu-store-email-summary-row-local-email-id nested-row))
                         (sibling-id
                          (chidu-store-email-summary-row-local-email-id sibling-row)))
                    (cl-labels
                        ((row-position
                           (local-id)
                           (save-excursion
                             (goto-char (point-min))
                             (when-let* ((match
                                          (text-property-search-forward
                                           'chidu-conversation-email-id
                                           local-id #'equal)))
                               (prop-match-beginning match))))
                         (goto-row
                           (local-id)
                           (let ((position (row-position local-id)))
                             (should position)
                             (goto-char position))))
                      (should
                       (string-match-p
                        "Selected full reply body" (buffer-string)))
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "notes.txt")
                        (let* ((card (appkit-media-card-context-at-point))
                               (payload (plist-get card :payload))
                               (attachment
                                (plist-get payload :attachment)))
                          (should card)
                          (should
                           (equal "notes.txt"
                                  (chidu-store-email-attachment-name
                                   attachment)))))
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "Hi Eli,")
                        (should
                         (equal "mail:eli@example.test"
                                (get-text-property
                                 (- (point) 4) 'chidu-person-identity))))
                      ;; Sender headings and exact names inside the body share
                      ;; one thread-local identity colour.  Quote markers remain
                      ;; literal source while Appkit projects copy-safe bars and
                      ;; depth-specific background blocks.
                      (save-excursion
                        (goto-row selected-id)
                        (let ((line-end (line-end-position)))
                          (search-forward "Eli Zaretskii" line-end)
                          (should
                           (equal "mail:eli@example.test"
                                  (get-text-property
                                   (match-beginning 0)
                                   'chidu-person-identity)))))
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "> Kai Ma wrote:")
                        (let ((line-start (line-beginning-position))
                              (line-end (line-end-position)))
                          (should (= 1 (get-text-property
                                        line-start
                                        'chidu-message-quote-depth)))
                          (should (get-text-property line-start 'display))
                          (should (get-text-property
                                   line-start 'appkit-ui-source-line-marker))
                          (search-backward "Kai Ma" line-start)
                          (should
                           (equal "mail:kai@example.test"
                                  (get-text-property
                                   (point) 'chidu-person-identity)))
                          (should
                           (equal "> Kai Ma wrote:"
                                  (substring-no-properties
                                   (filter-buffer-substring
                                    line-start line-end))))))
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward ">> Eli Zaretskii replied:")
                        (should (= 2 (get-text-property
                                      (line-beginning-position)
                                      'chidu-message-quote-depth))))
                      (dolist (preview '("preview-email-1" "preview-email-3"))
                        (should-not (string-match-p preview (buffer-string))))
                      ;; The focused view contains only ancestors + focus +
                      ;; descendants.  A sibling branch in the same JMAP Thread
                      ;; is not part of this Conversation scope.
                      (should-not (row-position sibling-id))
                      (let ((ghost
                             (save-excursion
                               (goto-char (point-min))
                               (text-property-search-forward
                                'chidu-conversation-missing-message-id
                                "missing-root" #'equal))))
                        (should ghost)
                        (should
                         (eq 'missing
                             (get-text-property
                              (prop-match-beginning ghost)
                              'chidu-conversation-role))))
                      ;; Canonical depth stays intact, but the focused
                      ;; presentation follows Chirp: ancestors + focus share a
                      ;; depth-0 spine; only descendants below the focus indent.
                      (should (= 0 (get-text-property
                                    (row-position root-id)
                                    'chidu-conversation-actual-depth)))
                      (should (= 1 (get-text-property
                                    (row-position selected-id)
                                    'chidu-conversation-actual-depth)))
                      (should (= 2 (get-text-property
                                    (row-position nested-id)
                                    'chidu-conversation-actual-depth)))
                      (should
                       (equal '(0 0 1)
                              (mapcar
                               (lambda (id)
                                 (get-text-property
                                  (row-position id)
                                  'chidu-conversation-visual-depth))
                               (list root-id selected-id nested-id))))
                      (should
                       (equal '(chain focus tree)
                              (mapcar
                               (lambda (id)
                                 (get-text-property
                                  (row-position id)
                                  'chidu-conversation-role))
                               (list root-id selected-id nested-id))))
                      (should
                       (equal '(0 0 1)
                              (mapcar
                               (lambda (id)
                                 (get-text-property
                                  (row-position id)
                                  appkit-discussion-depth-property))
                               (list root-id selected-id nested-id))))
                      (should
                       (equal
                        (list 'email root-id)
                        (get-text-property
                         (row-position selected-id)
                         appkit-discussion-parent-key-property)))
                      (should
                       (equal
                        (list 'email selected-id)
                        (get-text-property
                         (row-position nested-id)
                         appkit-discussion-parent-key-property)))
                      (should
                       (string-prefix-p
                        "│ "
                        (or (get-text-property
                             (row-position root-id) 'line-prefix)
                            "")))
                      (should
                       (string-prefix-p
                        "│ "
                        (or (get-text-property
                             (row-position selected-id) 'line-prefix)
                            "")))
                      (should
                       (= 3
                          (string-width
                           (or (get-text-property
                                (row-position nested-id) 'line-prefix)
                               ""))))
                      (should-not (string-match-p "reply to" (buffer-string)))
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "Selected full reply body")
                        (beginning-of-line)
                        (should
                         (<= (string-width
                              (or (get-text-property (point) 'line-prefix) ""))
                             2)))
                      ;; `v' changes body visibility without changing focus.
                      (goto-row selected-id)
                      (chidu-conversation-toggle-body)
                      (chidu-test-drain view)
                      (should-not
                       (string-match-p "Selected full reply body" (buffer-string)))
                      (should-not (string-match-p "preview-email-2" (buffer-string)))
                      (goto-row selected-id)
                      (chidu-conversation-toggle-body)
                      (chidu-test-drain view)
                      (should
                       (string-match-p
                        "Selected full reply body" (buffer-string)))
                      ;; RET re-roots the focused scope, but does not change any
                      ;; body fold.  Focusing the root admits its sibling branch;
                      ;; focusing the reply narrows the view again.
                      (goto-row root-id)
                      (chidu-conversation-focus)
                      (chidu-test-drain view)
                      (should (row-position sibling-id))
                      (should
                       (gethash
                        selected-id
                        (chidu-conversation-state-visible-bodies
                         (appkit-surface-model view))))
                      (goto-row selected-id)
                      (chidu-conversation-focus)
                      (chidu-test-drain view)
                      (should-not (row-position sibling-id))
                      (should
                       (equal selected-id
                              (chidu-conversation-state-focus-local-email-id
                               (appkit-surface-model view))))
                      (goto-row root-id)
                      (forward-char 2)
                      (let ((column (current-column)))
                        (chidu-conversation-close-replies)
                        (chidu-test-drain view)
                        (should (equal root-id
                                       (chidu-conversation--local-id-at-point)))
                        (should (= column (current-column))))
                      (should (row-position root-id))
                      (should-not (row-position selected-id))
                      (should-not (row-position nested-id))
                      (should
                       (string-match-p "2 replies hidden" (buffer-string)))
                      (should-not
                       (string-match-p "Selected full reply body" (buffer-string)))
                      (goto-row root-id)
                      (forward-char 2)
                      (let ((column (current-column)))
                        (chidu-conversation-open-replies)
                        (chidu-test-drain view)
                        (should (equal root-id
                                       (chidu-conversation--local-id-at-point)))
                        (should (= column (current-column))))
                      (should (row-position selected-id))
                      (should (row-position nested-id))
                      (should
                       (string-match-p
                        "Selected full reply body" (buffer-string))))))
                ;; Search is the same Store-first UI path: the committed result
                ;; renders without network I/O and can immediately reopen the
                ;; existing Conversation surface.
                (setq search-buffer
                      (chidu-search-open
                       app account (vector mailbox) search-spec nil))
                (with-current-buffer search-buffer
                  (let ((view (appkit-current-surface)))
                    (chidu-test-drain view)
                    (should (string-match-p "emoji width" (buffer-string)))
                    (should (string-match-p (regexp-quote "more available") (buffer-string)))
                    (goto-char (point-min))
                    (let ((case-fold-search nil))
                      (search-forward "Emoji"))
                    (should
                     (memq 'match
                           (ensure-list
                            (get-text-property
                             (- (point) (length "Emoji")) 'face))))
                    (goto-char (point-min))
                    (let ((match
                           (text-property-search-forward
                            'chidu-search-email-id
                            (chidu-store-email-summary-row-local-email-id
                             selected)
                            #'equal)))
                      (should match)
                      (goto-char (prop-match-beginning match))
                      (chidu-search-open-conversation))))
                ;; Merely opening or focusing an Email never changes `$seen'.
                ;; Only the explicit commands call the mutation workflow.
                (let (seen-call)
                  (cl-letf (((symbol-function 'chidu-set-seen)
                             (lambda (target desired &optional _quiet)
                               (setq seen-call (list target desired)))))
                    (setq message-buffer
                          (chidu-message-open
                           app account mailbox selected nil))
                    (chidu-test-drain app)
                    (with-current-buffer message-buffer
                      (should (string-match-p "notes.txt" (buffer-string)))
                      (goto-char (point-min))
                      (search-forward "notes.txt")
                      (should (appkit-media-card-context-at-point)))
                    (with-current-buffer conversation-buffer
                      (goto-char (point-min))
                      (let ((match
                             (text-property-search-forward
                              'chidu-conversation-email-id
                              (chidu-store-email-summary-row-local-email-id
                               selected)
                              #'equal)))
                        (should match)
                        (goto-char (prop-match-beginning match))
                        (chidu-conversation-focus)))
                    (should-not seen-call)
                    (with-current-buffer summary-buffer
                      (goto-char (point-min))
                      (let ((match
                             (text-property-search-forward
                              'chidu-summary-email-id
                              (chidu-store-email-summary-row-local-email-id
                               selected)
                              #'equal)))
                        (should match)
                        (goto-char (prop-match-beginning match))
                        (chidu-mark-unread)))
                    (should seen-call)
                    (should-not (cadr seen-call))
                    (should
                     (equal
                      (chidu-store-email-summary-row-local-email-id selected)
                      (chidu-seen-target-local-email-id (car seen-call))))))
                ;; One optimistic Store transition is projected into every live
                ;; surface without a network read in any render path.
                (let ((change
                       (chidu-store-seen-change-create
                        :endpoint endpoint :account account
                        :operation-id (chidu-store-new-local-id)
                        :local-email-id
                        (chidu-store-email-summary-row-local-email-id selected)
                        :remote-email-id "email-2"
                        :unread-p t :phase 'pending)))
                  (chidu--seen-changed app change)
                  (dolist (view
                           (list
                            (appkit-app-surface
                             app (list 'summary account-id mailbox-id))
                            (appkit-app-surface
                             app
                             (list 'search account-id
                                   (chidu-search-spec-query-key search-spec)))
                            (appkit-app-surface
                             app (list 'conversation account-id "thread-1"))
                            (appkit-app-surface
                             app (list 'message account-id
                                       (chidu-store-email-summary-row-local-email-id
                                        selected)))))
                    (when view (chidu-test-drain view)))
                  (let* ((summary-view
                          (appkit-app-surface
                           app (list 'summary account-id mailbox-id)))
                         (summary-state (appkit-surface-model summary-view))
                         (summary-row
                          (cl-find
                           (chidu-store-email-summary-row-local-email-id selected)
                           (chidu-store-mailbox-summary-context-rows
                            (chidu-summary-state-context summary-state))
                           :key #'chidu-store-email-summary-row-local-email-id
                           :test #'equal))
                         (search-state
                          (appkit-surface-model
                           (appkit-app-surface
                            app
                            (list 'search account-id
                                  (chidu-search-spec-query-key search-spec)))))
                         (search-row
                          (aref
                           (chidu-store-search-context-rows
                            (chidu-search-state-context search-state))
                           0))
                         (conversation-state
                          (appkit-surface-model
                           (appkit-app-surface
                            app (list 'conversation account-id "thread-1"))))
                         (conversation-row
                          (chidu-conversation--row-for-local-id
                           conversation-state
                           (chidu-store-email-summary-row-local-email-id
                            selected)))
                         (message-state
                          (appkit-surface-model
                           (appkit-app-surface
                            app
                            (list 'message account-id
                                  (chidu-store-email-summary-row-local-email-id
                                   selected))))))
                    (should (chidu-store-email-summary-row-unread-p summary-row))
                    (should
                     (chidu-store-email-summary-row-unread-p
                      (chidu-store-search-row-summary-row search-row)))
                    (should
                     (chidu-store-email-summary-row-unread-p
                      (chidu-store-conversation-row-summary-row
                       conversation-row)))
                    (should
                     (chidu-store-email-summary-row-unread-p
                      (chidu-message-state-row message-state)))))
                (should
                 (appkit-app-surface
                  app (list 'conversation account-id "thread-1"))))
            (dolist
                (buffer
                 (list summary-buffer conversation-buffer search-buffer
                       message-buffer))
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))

(ert-deftest chidu-explicit-seen-serializes-opposite-server-requests ()
  (let* ((store (chidu-test-store-create))
         (prepared (chidu-test--prepare-connected-account store))
         (endpoint (car prepared))
         (account (cadr prepared))
         (account-id (chidu-store-account-account-id account))
         (mailbox-context
          (chidu-test--store-value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id account-id :expected-revision 0
            :observation (chidu-test--mailbox-snapshot))))
         (mailbox
          (aref (chidu-store-mailbox-sync-context-mailboxes mailbox-context) 0))
         (_generation
          (chidu-store-test--activate-email-generation
           store account-id
           (vector
            (chidu-store-test--email-entry
             "email-seen" "2026-08-25T01:02:03Z"
             :thread-id "thread-seen"
             :subject "Seen" :preview ""))))
         (summary
          (chidu-store-test--canonical-summary
           store account-id mailbox 50))
         (row (aref (chidu-store-mailbox-summary-context-rows summary) 0))
         (runtime (chidu-runtime-open :store store))
         callbacks
         events
         requests)
    (unwind-protect
        (chidu-test--with-app
            (app
             (chidu--state-create
              :phase 'ready :endpoints (vector endpoint)
              :accounts (make-hash-table :test #'equal)))
          (appkit-app-send app (list :runtime runtime))
          (let ((target
                 (chidu-seen-target-create
                  :app app :account account
                  :local-email-id
                  (chidu-store-email-summary-row-local-email-id row)
                  :remote-email-id "email-seen"
                  :unread-p t)))
            (cl-letf
                (((symbol-function 'chidu--refresh-account-mailboxes) #'ignore)
                 ((symbol-function 'auth-source-search)
                  (lambda (&rest _arguments)
                    (list (list :secret (lambda () "secret")))))
                 ((symbol-function 'chidu-jmap-set-seen)
                  (lambda (_context secret _remote-email-id
                                    desired-seen-p deliver)
                    (clear-string secret)
                    (setq requests
                          (append requests (list desired-seen-p))
                          events
                          (append events (list (list 'set desired-seen-p)))
                          callbacks (append callbacks (list deliver)))
                    #'ignore))
                 ((symbol-function 'chidu-jmap-email-fetch-mutable-state)
                  (lambda (_endpoint _account remote-ids secret deliver)
                    (clear-string secret)
                    (setq events
                          (append events
                                  (list (list 'get (aref remote-ids 0)))))
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-email-mutable-state-create
                       :state "reconciled"
                       :targets
                       (vector
                        (chidu-jmap-email-mutable-target-create
                         :remote-id (aref remote-ids 0)
                         :found-p t
                         :remote-mailbox-ids (vector "inbox")
                         :seen-p nil)))))
                    #'ignore)))
              (chidu-set-seen target t t)
              (should (equal requests '(t)))
              (should (= 1 (length callbacks)))
              ;; The opposite intent becomes visible locally but waits for the
              ;; first server request to settle; requests never race per Email.
              (chidu-set-seen
               (chidu-seen-target-with target :unread-p nil) nil t)
              (should (equal requests '(t)))
              (funcall
               (nth 0 callbacks)
               (chidu-result-ok-create
                :value
                (chidu-jmap-seen-response-create :outcome 'succeeded)))
              (should (equal requests '(t nil)))
              (should (= 2 (length callbacks)))
              (funcall
               (nth 1 callbacks)
               (chidu-result-ok-create
                :value
                (chidu-jmap-seen-response-create :outcome 'succeeded)))
              ;; A method-level partial failure must perform Email/get before
              ;; retrying the idempotent absolute-value patch.
              (chidu-set-seen target t t)
              (should (equal requests '(t nil t)))
              (funcall
               (nth 2 callbacks)
               (chidu-result-ok-create
                :value
                (chidu-jmap-seen-response-create
                 :outcome 'unknown :error-kind "serverPartialFail")))
              (should (equal requests '(t nil t t)))
              (should
               (equal
                '((set t) (set nil) (set t)
                  (get "email-seen") (set t))
                events))
              (funcall
               (nth 3 callbacks)
               (chidu-result-ok-create
                :value
                (chidu-jmap-seen-response-create :outcome 'succeeded)))
              (let ((context
                     (chidu-test--store-value
                      store
                      (chidu-store-op-list-seen-intents-create
                       :account-id account-id))))
                (should
                 (= 0 (length (chidu-store-seen-context-intents context))))))))
      (when runtime (chidu-runtime-close runtime)))))

(ert-deftest chidu-start-app-opens-one-in-process-runtime ()
  (let ((fake-runtime (list :runtime))
        (store-info
         (chidu-store-runtime-create
          :store-id "store-test" :change-seq "0"))
        opened-root
        closed)
    (cl-letf
        (((symbol-function 'chidu-runtime-open)
          (lambda (&rest keys)
            (setq opened-root (plist-get keys :data-root))
            fake-runtime))
         ((symbol-function 'chidu-runtime-info)
          (lambda (runtime success-function _error-function)
            (should (eq fake-runtime runtime))
            (funcall success-function store-info)))
         ((symbol-function 'chidu-runtime-list-endpoints)
          (lambda (runtime success-function _error-function)
            (should (eq fake-runtime runtime))
            (funcall success-function (vector))))
         ((symbol-function 'chidu-runtime-close)
          (lambda (runtime)
            (should (eq fake-runtime runtime))
            (setq closed t))))
      (let* ((chidu-data-root (make-temp-name "/tmp/chidu-app-test-"))
             (app (chidu--start-app)))
        (unwind-protect
            (progn
              (should (equal (expand-file-name chidu-data-root) opened-root))
              (should (eq fake-runtime (chidu-app-runtime app)))
              (should (eq 'ready
                          (chidu--state-phase (chidu-app-state app))))
              (should
               (eq store-info
                   (chidu--state-store-info (chidu-app-state app)))))
          (when (appkit-app-live-p app) (appkit-app-close app)))
        (should closed)))))

(ert-deftest chidu-live-watcher-does-not-depend-on-desktop-notifications ()
  (let* ((store (chidu-test-store-create))
         (prepared (chidu-test--prepare-connected-account store))
         (endpoint (car prepared))
         scheduled
         event-source-started)
    (unwind-protect
        (chidu-test--with-app
            (app
             (chidu--state-create
              :phase 'ready :endpoints (vector endpoint)
              :accounts (make-hash-table :test #'equal)))
          (let ((chidu-new-mail-notifications nil))
            (cl-letf
                (((symbol-function 'chidu-live--schedule-all)
                  (lambda (_watcher) (setq scheduled t)))
                 ((symbol-function 'chidu-live--start-event-source)
                  (lambda (_watcher) (setq event-source-started t))))
              (should (chidu-live-watcher-p
                       (chidu-live-watch-endpoint app endpoint)))
              (should scheduled)
              (should event-source-started))))
      (chidu-store-close store))))

(ert-deftest chidu-new-mail-policy-notifies-and-opens-one-inbox-email ()
  (let* ((store (chidu-test-store-create))
         (prepared (chidu-test--prepare-connected-account store))
         (endpoint (car prepared))
         (account (cadr prepared))
         (account-id (chidu-store-account-account-id account))
         (mailbox-context
          (chidu-test--store-value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id account-id :expected-revision 0
            :observation (chidu-test--mailbox-snapshot))))
         (inbox
          (aref (chidu-store-mailbox-sync-context-mailboxes mailbox-context) 0))
         (summary
          (chidu-store-email-summary-row-create
           :local-email-id "local-new"
           :remote-email-id "remote-new"
           :remote-thread-id "thread-new"
           :received-at "2026-08-25T12:34:56Z"
           :from-name "Alice" :from-email "alice@example.test"
           :subject "Delivered" :preview "new mail"
           :unread-p t :flagged-p nil :has-attachment-p nil))
         (row
          (chidu-store-new-email-row-create
           :summary-row summary :remote-mailbox-ids (vector "inbox")))
         (result
          (chidu-email-live-result-create
           :new-emails (vector row) :changed-p t))
         (state
          (let ((accounts (make-hash-table :test #'equal)))
            (puthash
             account-id
             (chidu--account-ui-state-create
              :account account :mailbox-context mailbox-context)
             accounts)
            (chidu--state-create
             :phase 'ready :endpoints (vector endpoint) :accounts accounts)))
         notification
         opened)
    (unwind-protect
        (chidu-test--with-app (app state)
          (let ((chidu-notification-function
                 (lambda (&rest arguments)
                   (setq notification arguments))))
            (cl-letf (((symbol-function 'chidu-notify--open-email)
                       (lambda (selected-app selected-account selected-inbox
                                             selected-summary)
                         (setq opened
                               (list selected-app selected-account
                                     selected-inbox selected-summary)))))
              (chidu-notify-present app account mailbox-context result)
              (should (equal "Alice" (plist-get notification :title)))
              (should (string-match-p "Delivered"
                                      (plist-get notification :body)))
              (funcall (plist-get notification :on-action) nil "default")
              (should (eq app (nth 0 opened)))
              (should (eq account (nth 1 opened)))
              (should (eq inbox (nth 2 opened)))
              (should (eq summary (nth 3 opened)))
              (setq notification nil)
              (cl-letf (((symbol-function 'chidu-notify--visible-p)
                         (lambda (&rest _arguments) t)))
                (chidu-notify-present
                 app account mailbox-context result))
              (should-not notification))))
      (chidu-store-close store))))

(provide 'chidu-test)

;;; chidu-test.el ends here
