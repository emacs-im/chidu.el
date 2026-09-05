;;; chidu-drafts-test.el --- Canonical server Draft tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'cl-lib)
(require 'ert)
(require 'appkit-core)
(require 'chidu)
(require 'chidu-compose)
(require 'chidu-compose-resource)
(require 'chidu-draft-checkout)
(require 'chidu-drafts)
(require 'chidu-jmap-draft-checkout)
(require 'chidu-test-support)

(defun chidu-drafts-test--root ()
  "Return one private temporary Store root."
  (let ((root (make-temp-file "chidu-drafts-test-" t)))
    (set-file-modes root #o700)
    root))

(defun chidu-drafts-test--address (name email)
  "Return one immutable Email address from NAME and EMAIL."
  (chidu-store-email-address-create :name name :email email))

(defun chidu-drafts-test--mailbox-rights ()
  "Return writable mail item rights for a Drafts test Mailbox."
  (chidu-store-mailbox-rights-create
   :may-read-items-p t :may-add-items-p t :may-remove-items-p t
   :may-set-seen-p t :may-set-keywords-p t
   :may-create-child-p nil :may-rename-p t :may-delete-p t
   :may-submit-p t))

(defun chidu-drafts-test--mailbox-snapshot ()
  "Return Inbox and Drafts Mailboxes for Draft tests."
  (chidu-store-mailbox-snapshot-observation-create
   :state "mailboxes/drafts"
   :mailboxes
   (vector
    (chidu-store-mailbox-observation-create
     :remote-mailbox-id "inbox" :name "Inbox" :role "inbox"
     :sort-order 0 :total-emails 1 :unread-emails 0
     :total-threads 1 :unread-threads 0
     :rights (chidu-drafts-test--mailbox-rights)
     :subscribed-p t)
    (chidu-store-mailbox-observation-create
     :remote-mailbox-id "drafts" :name "Drafts" :role "drafts"
     :sort-order 10 :total-emails 3 :unread-emails 0
     :total-threads 3 :unread-threads 0
     :rights (chidu-drafts-test--mailbox-rights)
     :subscribed-p t))))

(defun chidu-drafts-test--fixture (store)
  "Create a connected Draft-capable fixture in STORE.

Return (ENDPOINT ACCOUNT IDENTITY DRAFTS-MAILBOX)."
  (let* ((endpoint
          (chidu-store-test--value
           store
           (chidu-store-op-configure-endpoint-create
            :session-url "https://mail.example.test/.well-known/jmap"
            :login "me@example.test"
            :authentication 'basic)))
         (identity-observation
          (chidu-store-identity-observation-create
           :remote-identity-id "identity-main"
           :name "Me" :email "me@example.test"))
         (alternate-observation
          (chidu-store-identity-observation-create
           :remote-identity-id "identity-alt"
           :name "Alternate" :email "alternate@example.test"))
         (connected
          (chidu-store-test--value
           store
           (chidu-store-op-observe-session-create
            :endpoint-id (chidu-store-endpoint-endpoint-id endpoint)
            :observation
            (chidu-store-test--session-observation-with
             "session/drafts"
             (vector
              (chidu-store-test--account-observation
               (vector identity-observation alternate-observation)
               "identity/drafts"))))))
         (account (aref (chidu-store-endpoint-accounts connected) 0))
         (mailbox-context
          (chidu-store-test--value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id (chidu-store-account-account-id account)
            :expected-revision 0
            :observation (chidu-drafts-test--mailbox-snapshot))))
         (current-account
          (chidu-store-mailbox-sync-context-account mailbox-context))
         (identity
          (cl-find "me@example.test"
                   (chidu-store-account-identities current-account)
                   :key #'chidu-store-identity-email :test #'equal))
         (drafts
          (cl-find "drafts"
                   (chidu-store-mailbox-sync-context-mailboxes mailbox-context)
                   :key #'chidu-store-mailbox-role :test #'equal)))
    (list connected current-account identity drafts)))

(defun chidu-drafts-test--activate (store account)
  "Activate canonical Draft fixtures for ACCOUNT in STORE."
  (let ((alice
         (chidu-drafts-test--address "Alice" "alice@example.test"))
        (bob
         (chidu-drafts-test--address "Bob" "bob@example.test"))
        (carol
         (chidu-drafts-test--address nil "carol@example.test")))
    (chidu-store-test--activate-email-generation
     store (chidu-store-account-account-id account)
     (vector
      (chidu-store-test--email-entry
       "draft-new" "2026-08-27T12:00:00Z"
       :from-name "Me" :from-email "me@example.test"
       :to (vector alice) :cc (vector bob)
       :subject "Newest Draft" :preview "Newest body"
       :mailbox-ids ["drafts"] :keywords ["$draft" "$seen"])
      (chidu-store-test--email-entry
       "ordinary-in-drafts" "2026-08-27T11:00:00Z"
       :from-name "Me" :from-email "me@example.test"
       :to (vector alice)
       :subject "Not a Draft"
       :mailbox-ids ["drafts"] :keywords ["$seen"])
      (chidu-store-test--email-entry
       "draft-in-inbox" "2026-08-27T10:00:00Z"
       :from-name "Me" :from-email "me@example.test"
       :to (vector alice)
       :subject "Wrong Mailbox"
       :mailbox-ids ["inbox"] :keywords ["$draft" "$seen"])
      (chidu-store-test--email-entry
       "draft-old" "2026-08-27T09:00:00Z"
       :from-name "Me" :from-email "me@example.test"
       :bcc (vector carol)
       :subject "Older Draft" :preview "Older body"
       :mailbox-ids ["drafts"] :keywords ["$draft" "$seen"])))))

(defun chidu-drafts-test--drafts (store account mailbox &optional limit)
  "Return ACCOUNT MAILBOX canonical Drafts from STORE."
  (chidu-store-test--value
   store
   (chidu-store-op-get-drafts-create
    :account-id (chidu-store-account-account-id account)
    :mailbox-id (chidu-store-mailbox-mailbox-id mailbox)
    :limit (or limit 50))))

(defun chidu-drafts-test--row (context remote-id)
  "Return CONTEXT Draft row for REMOTE-ID."
  (cl-find
   remote-id (chidu-store-drafts-context-rows context)
   :key
   (lambda (row)
     (chidu-store-email-summary-row-remote-email-id
      (chidu-store-draft-row-summary-row row)))
   :test #'equal))

(defun chidu-drafts-test--remote-resource
    (resource-id blob-id size name)
  "Return remote RESOURCE-ID BLOB-ID SIZE NAME attachment observation."
  (chidu-store-compose-resource-observation-create
   :resource-id resource-id
   :name name
   :media-type "text/plain"
   :size size
   :digest nil
   :remote-blob-id blob-id
   :charset "utf-8"
   :disposition "attachment"
   :cid nil
   :language ["en"]
   :location nil))

(defun chidu-drafts-test--write-bytes (file bytes)
  "Write exact unibyte BYTES to FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'binary))
      (write-region (point-min) (point-max) file nil 'silent))))

(cl-defun chidu-drafts-test--snapshot-create
    (&key remote-email-id remote-blob-id from document
          (resources (vector)))
  "Return REMOTE-EMAIL-ID REMOTE-BLOB-ID FROM DOCUMENT fixture.

RESOURCES defaults to an empty vector."
  (unless (and (vectorp from) (= 1 (length from)))
    (error "Snapshot fixture needs one originator"))
  (let ((shape
         (chidu-draft-editable-shape-from-observations
          (aref from 0) document resources)))
    (chidu-draft-editable-snapshot-create
     :remote-email-id remote-email-id
     :remote-blob-id remote-blob-id
     :shape shape)))

(ert-deftest chidu-store-drafts-are-canonical-bounded-and-checkout-idempotent ()
  "Drafts must require both Drafts membership and the $draft keyword."
  (let ((root (chidu-drafts-test--root)) store)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity ,mailbox)
                        (chidu-drafts-test--fixture store)))
            (chidu-drafts-test--activate store account)
            (let* ((bounded
                    (chidu-drafts-test--drafts store account mailbox 1))
                   (bounded-rows
                    (chidu-store-drafts-context-rows bounded)))
              (should (= 1 (length bounded-rows)))
              (should (chidu-store-drafts-context-maybe-more-p bounded))
              (should
               (equal "draft-new"
                      (chidu-store-email-summary-row-remote-email-id
                       (chidu-store-draft-row-summary-row
                        (aref bounded-rows 0))))))
            (let* ((context
                    (chidu-drafts-test--drafts store account mailbox 10))
                   (rows (chidu-store-drafts-context-rows context))
                   (new-row (chidu-drafts-test--row context "draft-new"))
                   (old-row (chidu-drafts-test--row context "draft-old"))
                   (summary (chidu-store-draft-row-summary-row new-row))
                   (document
                    (chidu-store-compose-document-create
                     :to "Alice <alice@example.test>"
                     :subject "Newest Draft"
                     :body "Newest body"))
                   (workspace-id (chidu-store-new-local-id)))
              (should (= 2 (length rows)))
              (should new-row)
              (should old-row)
              (should (= 2 (length
                            (chidu-store-draft-row-recipients new-row))))
              (should
               (equal "carol@example.test"
                      (chidu-store-email-address-email
                       (aref (chidu-store-draft-row-recipients old-row) 0))))
              (let ((rejected
                     (chidu-store-test--store-call
                      store
                      (chidu-store-op-checkout-draft-create
                       :workspace-id (chidu-store-new-local-id)
                       :account-id (chidu-store-account-account-id account)
                       :identity-id (chidu-store-identity-identity-id identity)
                       :drafts-mailbox-id
                       (chidu-store-mailbox-mailbox-id mailbox)
                       :local-email-id
                       (chidu-store-email-summary-row-local-email-id summary)
                       :remote-email-id "wrong-remote-id"
                       :remote-blob-id "blob-draft"
                       :document document))))
                (should (chidu-result-failure-p rejected))
                (should (eq 'draft-no-longer-canonical
                            (chidu-result-failure-kind rejected))))
              (let* ((created
                      (chidu-store-test--value
                       store
                       (chidu-store-op-checkout-draft-create
                        :workspace-id workspace-id
                        :account-id (chidu-store-account-account-id account)
                        :identity-id (chidu-store-identity-identity-id identity)
                        :drafts-mailbox-id
                        (chidu-store-mailbox-mailbox-id mailbox)
                        :local-email-id
                        (chidu-store-email-summary-row-local-email-id summary)
                        :remote-email-id "draft-new"
                        :remote-blob-id "blob-draft"
                        :document document)))
                     (workspace
                      (chidu-store-compose-context-workspace created)))
                (should
                 (eq 'draft
                     (chidu-store-compose-workspace-kind workspace)))
                (should
                 (equal "draft-new"
                        (chidu-store-compose-workspace-base-remote-email-id
                         workspace)))
                (should
                 (equal "blob-draft"
                        (chidu-store-compose-workspace-base-remote-blob-id
                         workspace)))
                (should
                 (= 0
                    (chidu-store-compose-workspace-published-revision
                     workspace))))
              (let* ((projected
                      (chidu-drafts-test--drafts store account mailbox 10))
                     (projected-row
                      (chidu-drafts-test--row projected "draft-new")))
                (should (equal workspace-id
                               (chidu-store-draft-row-workspace-id
                                projected-row))))
              (chidu-store-close store)
              (setq store (chidu-store-sqlite-create root))
              (let* ((reopened
                      (chidu-drafts-test--drafts store account mailbox 10))
                     (reopened-row
                      (chidu-drafts-test--row reopened "draft-new"))
                     (retried
                      (chidu-store-test--value
                       store
                       (chidu-store-op-checkout-draft-create
                        :workspace-id (chidu-store-new-local-id)
                        :account-id (chidu-store-account-account-id account)
                        :identity-id (chidu-store-identity-identity-id identity)
                        :drafts-mailbox-id
                        (chidu-store-mailbox-mailbox-id mailbox)
                        :local-email-id
                        (chidu-store-email-summary-row-local-email-id
                         (chidu-store-draft-row-summary-row reopened-row))
                        :remote-email-id "draft-new"
                        :remote-blob-id "blob-draft"
                        :document document))))
                (should
                 (equal workspace-id
                        (chidu-store-compose-workspace-workspace-id
                         (chidu-store-compose-context-workspace retried))))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-drafts-view-checks-out-once-then-resumes-store-first ()
  "Opening a canonical Draft should fetch once and then resume its workspace."
  (let ((root (chidu-drafts-test--root))
        store runtime app buffer opened jmap-calls)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,endpoint ,account ,identity ,mailbox)
                        (chidu-drafts-test--fixture store)))
            (chidu-drafts-test--activate store account)
            (setq runtime (chidu-runtime-open :data-root root :store store)
                  store nil
                  app (chidu-test-app-create nil))
            (appkit-app-send app (list :runtime runtime))
            (cl-letf
                (((symbol-function 'auth-source-search)
                  (lambda (&rest _arguments)
                    (list (list :secret
                                (lambda () (copy-sequence "secret"))))))
                 ((symbol-function 'chidu-jmap-draft-checkout)
                  (lambda (_endpoint actual-account actual-mailbox remote-id
                                     _secret deliver)
                    (setq jmap-calls (1+ (or jmap-calls 0)))
                    (should (eq actual-account account))
                    (should (eq actual-mailbox mailbox))
                    (should (equal "draft-new" remote-id))
                    (funcall
                     deliver
                     (chidu-result-ok-create
                      :value
                      (chidu-drafts-test--snapshot-create
                       :remote-email-id remote-id
                       :remote-blob-id "blob-draft"
                       :from
                       (vector
                        (chidu-store-email-address-create
                         :name "Me" :email "me@example.test"))
                       :document
                       (chidu-store-compose-document-create
                        :to "Alice <alice@example.test>"
                        :subject "Newest Draft"
                        :body "Newest body"))))
                    #'ignore))
                 ((symbol-function 'chidu-compose-open-context)
                  (lambda (actual-app context)
                    (should (eq actual-app app))
                    (setq opened context))))
              (setq buffer (chidu-drafts-open app account mailbox nil))
              (with-current-buffer buffer
                (chidu-test-drain (appkit-current-surface))
                (should (eq major-mode 'chidu-drafts-mode))
                (should (string-match-p "Alice, Bob" (buffer-string)))
                (should (string-match-p "Newest Draft" (buffer-string)))
                (goto-char (point-min))
                (let ((match
                       (text-property-search-forward
                        'chidu-draft-email-id)))
                  (should match)
                  (goto-char (prop-match-beginning match)))
                (chidu-drafts-open-draft)
                (chidu-test-drain app))
              (should (= 1 jmap-calls))
              (should (chidu-store-compose-context-p opened))
              (should
               (equal
                (chidu-store-identity-identity-id identity)
                (chidu-store-identity-identity-id
                 (chidu-store-compose-context-identity opened))))
              (should
               (eq 'draft
                   (chidu-store-compose-workspace-kind
                    (chidu-store-compose-context-workspace opened))))
              (setq opened nil)
              (with-current-buffer buffer
                (chidu-test-drain (appkit-current-surface))
                (goto-char (point-min))
                (let ((match
                       (text-property-search-forward
                        'chidu-draft-email-id)))
                  (should match)
                  (goto-char (prop-match-beginning match)))
                (chidu-drafts-open-draft)
                (chidu-test-drain app))
              (should (= 1 jmap-calls))
              (should (chidu-store-compose-context-p opened)))))
      (when (and app (appkit-app-live-p app)) (appkit-app-close app)
            )
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-store-draft-checkout-requires-local-resource-evidence ()
  "A remote-only Blob cannot become a durable editable attachment."
  (let ((root (chidu-drafts-test--root))
        store)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,_endpoint ,account ,identity ,mailbox)
                        (chidu-drafts-test--fixture store)))
            (chidu-drafts-test--activate store account)
            (let* ((row
                    (chidu-drafts-test--row
                     (chidu-drafts-test--drafts store account mailbox)
                     "draft-new"))
                   (summary (chidu-store-draft-row-summary-row row))
                   (resource-id (chidu-store-new-local-id))
                   (document
                    (chidu-store-compose-document-create
                     :body "Body" :resource-ids (vector resource-id)))
                   (remote
                    (chidu-drafts-test--remote-resource
                     resource-id "blob-note" 4 "note.txt"))
                   (rejected
                    (chidu-store-test--store-call
                     store
                     (chidu-store-op-checkout-draft-create
                      :workspace-id (chidu-store-new-local-id)
                      :account-id
                      (chidu-store-account-account-id account)
                      :identity-id
                      (chidu-store-identity-identity-id identity)
                      :drafts-mailbox-id
                      (chidu-store-mailbox-mailbox-id mailbox)
                      :local-email-id
                      (chidu-store-email-summary-row-local-email-id summary)
                      :remote-email-id "draft-new"
                      :remote-blob-id "blob-draft"
                      :document document
                      :resources (vector remote)))))
              (should (chidu-result-failure-p rejected))
              (should
               (eq 'invalid-compose-resource
                   (chidu-result-failure-kind rejected)))
              (let* ((digest (make-string 64 ?a))
                     (created
                      (chidu-store-test--value
                       store
                       (chidu-store-op-checkout-draft-create
                        :workspace-id (chidu-store-new-local-id)
                        :account-id
                        (chidu-store-account-account-id account)
                        :identity-id
                        (chidu-store-identity-identity-id identity)
                        :drafts-mailbox-id
                        (chidu-store-mailbox-mailbox-id mailbox)
                        :local-email-id
                        (chidu-store-email-summary-row-local-email-id summary)
                        :remote-email-id "draft-new"
                        :remote-blob-id "blob-draft"
                        :document document
                        :resources
                        (vector
                         (chidu-store-compose-resource-observation-with
                          remote :digest digest)))))
                     (resource
                      (aref
                       (chidu-store-compose-context-resources created) 0)))
                (should
                 (equal digest
                        (chidu-store-compose-resource-digest resource)))
                (should
                 (equal "blob-note"
                        (chidu-store-compose-resource-remote-blob-id
                         resource)))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-draft-checkout-rejects-mismatched-remote-plan ()
  "A checkout plan for another Email must not reach materialization or Store."
  (let ((root (chidu-drafts-test--root))
        store runtime context failure)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,endpoint ,account ,_identity ,mailbox)
                        (chidu-drafts-test--fixture store)))
            (chidu-drafts-test--activate store account)
            (let ((row
                   (chidu-drafts-test--row
                    (chidu-drafts-test--drafts store account mailbox)
                    "draft-new")))
              (setq runtime
                    (chidu-runtime-open :data-root root :store store)
                    store nil)
              (cl-letf
                  (((symbol-function 'auth-source-search)
                    (lambda (&rest _arguments)
                      (list
                       (list :secret
                             (lambda () (copy-sequence "secret"))))))
                   ((symbol-function 'chidu-jmap-draft-checkout)
                    (lambda (_endpoint _account _mailbox _remote-id
                                       _secret deliver)
                      (funcall
                       deliver
                       (chidu-result-ok-create
                        :value
                        (chidu-drafts-test--snapshot-create
                         :remote-email-id "another-draft"
                         :remote-blob-id "another-blob"
                         :from
                         (vector
                          (chidu-store-email-address-create
                           :name "Me" :email "me@example.test"))
                         :document
                         (chidu-store-compose-document-create))))
                      #'ignore)))
                (chidu-checkout-draft
                 runtime endpoint account mailbox row
                 (lambda (value) (setq context value))
                 (lambda (value) (setq failure value))))
              (should-not context)
              (should (chidu-result-failure-p failure))
              (should (eq 'invalid-result
                          (chidu-result-failure-kind failure)))
              (should
               (= 0
                  (length
                   (chidu-store-test--value
                    (chidu-runtime-store runtime)
                    (chidu-store-op-list-compose-workspaces-create))))))))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-draft-checkout-materializes-attachments-in-wire-order ()
  "Checkout downloads every Blob before one atomic workspace creation."
  (let ((root (chidu-drafts-test--root))
        store runtime context failure calls)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,endpoint ,account ,_identity ,mailbox)
                        (chidu-drafts-test--fixture store)))
            (chidu-drafts-test--activate store account)
            (let* ((row
                    (chidu-drafts-test--row
                     (chidu-drafts-test--drafts store account mailbox)
                     "draft-new"))
                   (first-id (chidu-store-new-local-id))
                   (second-id (chidu-store-new-local-id))
                   (first
                    (chidu-drafts-test--remote-resource
                     first-id "blob-one" 3 "one.txt"))
                   (second
                    (chidu-drafts-test--remote-resource
                     second-id "blob-two" 4 "two.txt"))
                   (document
                    (chidu-store-compose-document-create
                     :body "Body"
                     :resource-ids (vector first-id second-id)))
                   (snapshot
                    (chidu-drafts-test--snapshot-create
                     :remote-email-id "draft-new"
                     :remote-blob-id "blob-draft"
                     :from
                     (vector
                      (chidu-store-email-address-create
                       :name "Me" :email "me@example.test"))
                     :document document
                     :resources (vector first second))))
              (setq runtime
                    (chidu-runtime-open :data-root root :store store)
                    store nil)
              (cl-letf
                  (((symbol-function 'auth-source-search)
                    (lambda (&rest _arguments)
                      (list
                       (list :secret
                             (lambda () (copy-sequence "secret"))))))
                   ((symbol-function 'chidu-jmap-draft-checkout)
                    (lambda (_endpoint _account _mailbox _remote-id
                                       _secret deliver)
                      (setq calls (append calls '(email)))
                      (funcall
                       deliver
                       (chidu-result-ok-create :value snapshot))
                      (lambda () (setq calls (append calls '(stale-email))))))
                   ((symbol-function 'chidu-jmap-http-download-file)
                    (lambda (url _login _authentication _secret file deliver
                                 &rest _arguments)
                      (let ((entry
                             (cond
                              ((string-match-p "blob-one" url)
                               '(blob-one . "one"))
                              ((string-match-p "blob-two" url)
                               '(blob-two . "two!"))
                              (t (ert-fail (format "unexpected Blob URL %s"
                                                   url))))))
                        (setq calls (append calls (list (car entry))))
                        (chidu-drafts-test--write-bytes file (cdr entry))
                        (funcall
                         deliver
                         (chidu-result-ok-create :value file))
                        nil))))
                (chidu-checkout-draft
                 runtime endpoint account mailbox row
                 (lambda (value) (setq context value))
                 (lambda (value) (setq failure value))))
              (should-not failure)
              (should (chidu-store-compose-context-p context))
              (should (equal '(email blob-one blob-two) calls))
              (let* ((resources
                      (chidu-store-compose-context-resources context))
                     (stored-document
                      (chidu-store-compose-workspace-document
                       (chidu-store-compose-context-workspace context)))
                     (stored-ids
                      (vconcat
                       (cl-loop
                        for resource across resources
                        collect
                        (chidu-store-compose-resource-resource-id resource)))))
                (should (= 2 (length resources)))
                (should
                 (equal stored-ids
                        (chidu-store-compose-document-resource-ids
                         stored-document)))
                (should (cl-every #'chidu-store-local-id-p stored-ids))
                (should-not (equal stored-ids (vector first-id second-id)))
                (should
                 (equal ["blob-one" "blob-two"]
                        (vconcat
                         (cl-loop
                          for resource across resources
                          collect
                          (chidu-store-compose-resource-remote-blob-id
                           resource)))))
                (should
                 (equal (secure-hash 'sha256 "one")
                        (chidu-store-compose-resource-digest
                         (aref resources 0))))
                (should
                 (equal (secure-hash 'sha256 "two!")
                        (chidu-store-compose-resource-digest
                         (aref resources 1))))
                (dolist (resource (append resources nil))
                  (should
                   (file-regular-p
                    (chidu-compose-resource-local-file root resource)))))
              (let* ((projected
                      (chidu-drafts-test--drafts
                       (chidu-runtime-store runtime) account mailbox))
                     (projected-row
                      (chidu-drafts-test--row projected "draft-new")))
                (should
                 (equal
                  (chidu-store-compose-workspace-workspace-id
                   (chidu-store-compose-context-workspace context))
                  (chidu-store-draft-row-workspace-id projected-row)))))))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-draft-checkout-failure-leaves-no-partial-workspace ()
  "A later Blob failure may leave CAS bytes but never a checkout row."
  (let ((root (chidu-drafts-test--root))
        store runtime context failure calls)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,endpoint ,account ,_identity ,mailbox)
                        (chidu-drafts-test--fixture store)))
            (chidu-drafts-test--activate store account)
            (let* ((row
                    (chidu-drafts-test--row
                     (chidu-drafts-test--drafts store account mailbox)
                     "draft-new"))
                   (first-id (chidu-store-new-local-id))
                   (second-id (chidu-store-new-local-id))
                   (first
                    (chidu-drafts-test--remote-resource
                     first-id "blob-one" 3 "one.txt"))
                   (second
                    (chidu-drafts-test--remote-resource
                     second-id "blob-two" 4 "two.txt"))
                   (snapshot
                    (chidu-drafts-test--snapshot-create
                     :remote-email-id "draft-new"
                     :remote-blob-id "blob-draft"
                     :from
                     (vector
                      (chidu-store-email-address-create
                       :name "Me" :email "me@example.test"))
                     :document
                     (chidu-store-compose-document-create
                      :body "Body"
                      :resource-ids (vector first-id second-id))
                     :resources (vector first second))))
              (setq runtime
                    (chidu-runtime-open :data-root root :store store)
                    store nil)
              (cl-letf
                  (((symbol-function 'auth-source-search)
                    (lambda (&rest _arguments)
                      (list
                       (list :secret
                             (lambda () (copy-sequence "secret"))))))
                   ((symbol-function 'chidu-jmap-draft-checkout)
                    (lambda (_endpoint _account _mailbox _remote-id
                                       _secret deliver)
                      (funcall
                       deliver
                       (chidu-result-ok-create :value snapshot))
                      #'ignore))
                   ((symbol-function 'chidu-jmap-http-download-file)
                    (lambda (url _login _authentication _secret file deliver
                                 &rest _arguments)
                      (if (string-match-p "blob-one" url)
                          (progn
                            (setq calls (append calls '(blob-one)))
                            (chidu-drafts-test--write-bytes file "one")
                            (funcall
                             deliver
                             (chidu-result-ok-create :value file)))
                        (setq calls (append calls '(blob-two)))
                        (funcall
                         deliver
                         (chidu-result-failure-create
                          :kind 'network-error
                          :data '(simulated-second-blob-failure)
                          :retryable-p t)))
                      nil)))
                (chidu-checkout-draft
                 runtime endpoint account mailbox row
                 (lambda (value) (setq context value))
                 (lambda (value) (setq failure value))))
              (should-not context)
              (should (chidu-result-failure-p failure))
              (should (eq 'network-error
                          (chidu-result-failure-kind failure)))
              (should (equal '(blob-one blob-two) calls))
              (should
               (file-regular-p
                (chidu-compose-resource-path
                 root (secure-hash 'sha256 "one"))))
              (should
               (= 0
                  (length
                   (chidu-store-test--value
                    (chidu-runtime-store runtime)
                    (chidu-store-op-list-compose-workspaces-create)))))
              (let ((projected-row
                     (chidu-drafts-test--row
                      (chidu-drafts-test--drafts
                       (chidu-runtime-store runtime) account mailbox)
                      "draft-new")))
                (should-not
                 (chidu-store-draft-row-workspace-id projected-row))))))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-draft-checkout-sync-settlement-keeps-newest-cancel ()
  "A synchronous stage must not overwrite the next live stage's cancel."
  (let ((root (chidu-drafts-test--root))
        store runtime operation second-deliver
        stale-remote stale-first current-second succeeded failed)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (pcase-let* ((`(,endpoint ,account ,_identity ,mailbox)
                        (chidu-drafts-test--fixture store)))
            (chidu-drafts-test--activate store account)
            (let* ((row
                    (chidu-drafts-test--row
                     (chidu-drafts-test--drafts store account mailbox)
                     "draft-new"))
                   (first-id (chidu-store-new-local-id))
                   (second-id (chidu-store-new-local-id))
                   (first
                    (chidu-drafts-test--remote-resource
                     first-id "blob-one" 3 "one.txt"))
                   (second
                    (chidu-drafts-test--remote-resource
                     second-id "blob-two" 4 "two.txt"))
                   (snapshot
                    (chidu-drafts-test--snapshot-create
                     :remote-email-id "draft-new"
                     :remote-blob-id "blob-draft"
                     :from
                     (vector
                      (chidu-store-email-address-create
                       :name "Me" :email "me@example.test"))
                     :document
                     (chidu-store-compose-document-create
                      :resource-ids (vector first-id second-id))
                     :resources (vector first second)))
                   (download-index 0))
              (setq runtime
                    (chidu-runtime-open :data-root root :store store)
                    store nil)
              (cl-letf
                  (((symbol-function 'auth-source-search)
                    (lambda (&rest _arguments)
                      (list
                       (list :secret
                             (lambda () (copy-sequence "secret"))))))
                   ((symbol-function 'chidu-jmap-draft-checkout)
                    (lambda (_endpoint _account _mailbox _remote-id
                                       _secret deliver)
                      (funcall
                       deliver
                       (chidu-result-ok-create :value snapshot))
                      (lambda ()
                        (setq stale-remote (1+ (or stale-remote 0))))))
                   ((symbol-function 'chidu-jmap-download-compose-resource)
                    (lambda (_endpoint _account resource _root _secret deliver)
                      (setq download-index (1+ download-index))
                      (if (= download-index 1)
                          (progn
                            (funcall
                             deliver
                             (chidu-result-ok-create
                              :value
                              (chidu-store-compose-resource-observation-with
                               resource :digest (make-string 64 ?a))))
                            (lambda ()
                              (setq stale-first
                                    (1+ (or stale-first 0)))))
                        (setq second-deliver deliver)
                        (lambda ()
                          (setq current-second
                                (1+ (or current-second 0))))))))
                (setq
                 operation
                 (chidu-checkout-draft
                  runtime endpoint account mailbox row
                  (lambda (_value) (setq succeeded t))
                  (lambda (_value) (setq failed t))))
                (should second-deliver)
                (should-not succeeded)
                (should-not failed)
                (chidu-runtime-cancel-operation runtime operation)
                (should (= 1 current-second))
                (should-not stale-first)
                (should-not stale-remote)))))
      (when runtime (chidu-runtime-close runtime))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(provide 'chidu-drafts-test)

;;; chidu-drafts-test.el ends here
