;;; chidu-test-support.el --- Shared Chidu test helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'appkit-surface)
(require 'json)
(require 'chidu-jmap-types)
(require 'chidu-store)
(require 'chidu-store-sqlite)

(defun chidu-test-app-create (runtime &optional state)
  "Start one canonical test App owning RUNTIME and optional directory STATE."
  (require 'chidu)
  (let ((app (appkit-app-start
              chidu--app-type :identity (make-symbol "chidu-test-")
              :input (or state (chidu--state-create)))))
    (appkit-app-send app (list :runtime runtime))
    app))

(defun chidu-test-drain (owner)
  "Drain already queued work for OWNER and its App's live Surfaces.
Do not wait for network responses or timers.  Fail if work does not settle."
  (let ((app (if (appkit-app-p owner) owner (appkit-surface-app owner)))
        (remaining 1000)
        pending)
    (while
        (progn
          (setq pending nil)
          (let ((loops (if app (list (appkit-app-loop app))
                         (list (appkit-surface-loop owner)))))
            (when app
              (maphash (lambda (_identity entry)
                         (push (appkit-surface-loop (cdr entry)) loops))
                       (appkit-app-surfaces app)))
            (dolist (loop loops)
              (when (eq (appkit-loop-status loop) 'faulted)
                (error "Chidu test owner faulted: %S" (appkit-loop-fault loop)))
              (when (> (appkit-loop-pending-count loop) 0)
                (setq pending t)
                (when (<= (cl-decf remaining) 0)
                  (error "Chidu test work did not settle"))
                (appkit-loop-run-pass loop))))
          pending))))

(defun chidu-store-test--payload (value)
  "Encode JSON VALUE as an unibyte payload without framing."
  (encode-coding-string
   (json-serialize value :null-object :json-null :false-object :json-false)
   'utf-8-unix t))

(defun chidu-store-test--method-response (method call-id arguments)
  "Encode one JMAP METHOD response with CALL-ID and ARGUMENTS."
  (chidu-store-test--payload
   `(:sessionState "session"
     :methodResponses [[,method ,arguments ,call-id]])))

(defun chidu-store-test--store-call (store operation)
  "Synchronously invoke STORE OPERATION in a unit test."
  (let (result)
    (chidu-store-call store operation (lambda (value) (setq result value)))
    result))

(defun chidu-store-test--session-observation-with
    (state accounts &optional max-objects-in-set max-size-request)
  "Return fake Session observation for STATE and ACCOUNTS vector."
  (chidu-store-session-observation-create
   :username "me@example.test"
   :state state
   :api-url "https://mail.example.test/jmap/api"
   :download-url
   "https://mail.example.test/jmap/download/{accountId}/{blobId}/{name}?type={type}"
   :upload-url "https://mail.example.test/jmap/upload/{accountId}"
   :event-source-url
   "https://mail.example.test/jmap/eventsource/?types={types}"
   :max-size-request (or max-size-request 1048576)
   :max-objects-in-get 256
   :max-objects-in-set (or max-objects-in-set 128)
   :capabilities
   (vector chidu-jmap-core-capability
           chidu-jmap-mail-capability
           chidu-jmap-submission-capability)
   :accounts accounts))

(defun chidu-store-test--account-observation (&optional identities state)
  "Return one fake Account observation using IDENTITIES and identity STATE."
  (chidu-store-account-observation-create
   :remote-account-id "remote-account"
   :name "Mail"
   :personal-p t
   :read-only-p nil
   :primary-mail-p t
   :primary-submission-p t
   :identity-state (or state "identity-1")
   :capabilities
   (vector chidu-jmap-mail-capability
           chidu-jmap-submission-capability)
   :identities (or identities (vector))))

(defun chidu-store-test--prepare-mailbox-account
    (store &optional max-objects-in-set)
  "Create a connected fake Account in STORE and return its local id."
  (let* ((endpoint
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-configure-endpoint-create
             :session-url "https://mail.example.test/.well-known/jmap"
             :login "me@example.test"
             :authentication 'basic))))
         (connected
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-observe-session-create
             :endpoint-id (chidu-store-endpoint-endpoint-id endpoint)
             :observation
             (chidu-store-test--session-observation-with
              "session-mailbox"
              (vector (chidu-store-test--account-observation))
              max-objects-in-set))))))
    (chidu-store-account-account-id
     (aref (chidu-store-endpoint-accounts connected) 0))))

(defun chidu-store-test--email-metadata (remote-id)
  "Return immutable metadata-v1 fixture for REMOTE-ID."
  (chidu-store-email-metadata-create
   :remote-blob-id (concat "blob-" remote-id)
   :remote-thread-id (concat "thread-" remote-id)
   :size 42
   :received-at "2026-08-26T12:00:00Z"
   :sender (vector)
   :from
   (vector
    (chidu-store-email-address-create
     :name "Alice" :email "alice@example.test"))
   :to (vector) :cc (vector) :bcc (vector) :reply-to (vector)
   :subject (concat "Subject " remote-id)
   :message-ids (vector (concat "mid-" remote-id))
   :in-reply-to (vector) :references (vector)
   :has-attachment-p nil))

(defun chidu-store-test--hydration-observation (plan &optional state)
  "Return a successful homogeneous hydration observation for PLAN at STATE."
  (let ((kind (chidu-store-email-hydration-plan-kind plan)))
    (chidu-store-email-hydration-observation-create
     :kind kind
     :state (or state "email-hydrated")
     :results
     (vconcat
      (cl-loop
       for target across (chidu-store-email-hydration-plan-targets plan)
       for remote-id =
       (chidu-store-email-hydration-target-remote-email-id target)
       collect
       (chidu-store-email-hydration-result-create
        :remote-email-id remote-id
        :found-p t
        :metadata
        (and (eq kind 'full)
             (chidu-store-test--email-metadata remote-id))
        :preview (and (eq kind 'full) (concat "Preview " remote-id))
        :remote-mailbox-ids (vector "inbox")
        :keywords (vector)))))))

(defun chidu-store-test--value (store operation)
  "Return successful STORE OPERATION value, or signal in a test."
  (let ((result (chidu-store-test--store-call store operation)))
    (unless (chidu-result-ok-p result)
      (error "Store test operation failed: %S" result))
    (chidu-result-ok-value result)))

(defun chidu-store-test--email-entry (remote-id received-at &rest properties)
  "Return canonical test Email entry for REMOTE-ID at RECEIVED-AT.

PROPERTIES may override :thread-id, :from-name, :from-email, :subject,
:to, :cc, :bcc, :preview, :mailbox-ids, :keywords, and
:has-attachment-p."
  (append (list :remote-id remote-id :received-at received-at) properties))

(defun chidu-store-test--entry-value (entry key default)
  "Return ENTRY KEY when present, otherwise DEFAULT."
  (if (plist-member entry key) (plist-get entry key) default))

(defun chidu-store-test--entry-metadata (entry)
  "Return immutable metadata fixture for canonical test ENTRY."
  (let* ((remote-id (plist-get entry :remote-id))
         (from-name
          (chidu-store-test--entry-value entry :from-name "Alice"))
         (from-email
          (chidu-store-test--entry-value
           entry :from-email "alice@example.test")))
    (chidu-store-email-metadata-with
     (chidu-store-test--email-metadata remote-id)
     :remote-thread-id
     (chidu-store-test--entry-value
      entry :thread-id (concat "thread-" remote-id))
     :received-at (plist-get entry :received-at)
     :from
     (if from-email
         (vector
          (chidu-store-email-address-create
           :name from-name :email from-email))
       (vector))
     :to (chidu-store-test--entry-value entry :to (vector))
     :cc (chidu-store-test--entry-value entry :cc (vector))
     :bcc (chidu-store-test--entry-value entry :bcc (vector))
     :subject
     (chidu-store-test--entry-value
      entry :subject (concat "Subject " remote-id))
     :has-attachment-p
     (and (chidu-store-test--entry-value
           entry :has-attachment-p nil)
          t))))

(defun chidu-store-test--activate-email-generation
    (store account-id entries &optional final-state)
  "Activate canonical ENTRIES for ACCOUNT-ID in STORE.

ENTRIES is a vector produced by `chidu-store-test--email-entry'.  Return the
final live Email synchronization context."
  (unless (vectorp entries)
    (signal 'wrong-type-argument (list 'vectorp entries)))
  (let* ((baseline-state "email/test-baseline")
         (hydrated-state "email/test-hydrated")
         (live-state (or final-state "email/test-live"))
         (by-id (make-hash-table :test #'equal))
         (remote-ids
          (vconcat
           (cl-loop
            for entry across entries
            for remote-id = (plist-get entry :remote-id)
            do (puthash remote-id entry by-id)
            collect remote-id)))
         (initial
          (chidu-store-test--value
           store
           (chidu-store-op-get-email-sync-context-create
            :account-id account-id)))
         (context
          (chidu-store-test--value
           store
           (chidu-store-op-begin-email-bootstrap-create
            :account-id account-id
            :expected-revision
            (chidu-store-email-sync-context-revision initial)
            :state baseline-state
            :profile-version "metadata-v1")))
         (generation-id
          (chidu-store-email-sync-context-generation-id context)))
    (dolist (ids (list remote-ids (vector)))
      (setq
       context
       (chidu-store-test--value
        store
        (chidu-store-op-append-email-query-chunk-create
         :account-id account-id
         :generation-id generation-id
         :expected-revision
         (chidu-store-email-sync-context-revision context)
         :observation
         (chidu-store-email-query-page-observation-create
          :query-state "query/test"
          :can-calculate-changes-p t
          :position
          (chidu-store-email-sync-context-committed-count context)
          :remote-email-ids ids)))))
    (setq
     context
     (chidu-store-test--value
      store
      (chidu-store-op-apply-email-membership-changes-create
       :account-id account-id
       :generation-id generation-id
       :expected-revision
       (chidu-store-email-sync-context-revision context)
       :expected-state baseline-state
       :observation
       (chidu-store-email-changes-observation-create
        :old-state baseline-state :new-state hydrated-state))))
    (let ((continue-p t))
      (while continue-p
        (let* ((plan
                (chidu-store-test--value
                 store
                 (chidu-store-op-get-email-hydration-plan-create
                  :account-id account-id :limit 10000)))
               (targets
                (chidu-store-email-hydration-plan-targets plan)))
          (if (zerop (length targets))
              (setq continue-p nil)
            (let ((kind (chidu-store-email-hydration-plan-kind plan)))
              (setq
               context
               (chidu-store-test--value
                store
                (chidu-store-op-apply-email-hydration-create
                 :account-id account-id
                 :generation-id generation-id
                 :expected-revision
                 (chidu-store-email-sync-context-revision context)
                 :observation
                 (chidu-store-email-hydration-observation-create
                  :kind kind
                  :state hydrated-state
                  :results
                  (vconcat
                   (cl-loop
                    for target across targets
                    for remote-id =
                    (chidu-store-email-hydration-target-remote-email-id
                     target)
                    for entry = (gethash remote-id by-id)
                    unless entry
                    do (error "Missing canonical test entry: %s" remote-id)
                    collect
                    (chidu-store-email-hydration-result-create
                     :remote-email-id remote-id
                     :found-p t
                     :metadata
                     (and (eq kind 'full)
                          (chidu-store-test--entry-metadata entry))
                     :preview
                     (and (eq kind 'full)
                          (chidu-store-test--entry-value
                           entry :preview (concat "Preview " remote-id)))
                     :remote-mailbox-ids
                     (chidu-store-test--entry-value
                      entry :mailbox-ids (vector "inbox"))
                     :keywords
                     (chidu-store-test--entry-value
                      entry :keywords (vector))))))))))))))
    (setq
     context
     (chidu-store-test--value
      store
      (chidu-store-op-finish-email-hydration-create
       :account-id account-id
       :generation-id generation-id
       :expected-revision
       (chidu-store-email-sync-context-revision context))))
    (setq
     context
     (chidu-store-email-round-result-context
      (chidu-store-test--value
       store
       (chidu-store-op-apply-email-catchup-round-create
        :account-id account-id
        :generation-id generation-id
        :expected-revision
        (chidu-store-email-sync-context-revision context)
        :expected-state hydrated-state
        :observation
        (chidu-store-email-catchup-observation-create
         :changes
         (chidu-store-email-changes-observation-create
          :old-state hydrated-state :new-state live-state))))))
    (chidu-store-test--value
     store
     (chidu-store-op-activate-email-generation-create
      :account-id account-id
      :generation-id generation-id
      :expected-revision
      (chidu-store-email-sync-context-revision context)
      :expected-state live-state))))

(defun chidu-store-test--canonical-summary
    (store account-id mailbox &optional limit)
  "Return ACCOUNT-ID MAILBOX canonical Summary from STORE."
  (chidu-store-test--value
   store
   (chidu-store-op-get-mailbox-summary-create
    :account-id account-id
    :mailbox-id (chidu-store-mailbox-mailbox-id mailbox)
    :limit (or limit 50))))

(defun chidu-test-store-create ()
  "Return a disposable SQLite Store that removes its private root on close."
  (unless (sqlite-available-p)
    (error "Chidu tests require Emacs built with SQLite support"))
  (let* ((root (make-temp-file "chidu-test-store-" t))
         (_mode (set-file-modes root #o700))
         (inner (chidu-store-sqlite-create root))
         closed-p)
    (chidu-store-capability-create
     :name 'sqlite-test
     :invoke-function
     (lambda (operation deliver)
       (chidu-store-call inner operation deliver))
     :inspect-function
     (lambda () (chidu-store-inspect inner))
     :close-function
     (lambda ()
       (unless closed-p
         (setq closed-p t)
         (unwind-protect
             (chidu-store-close inner)
           (when (file-directory-p root)
             (delete-directory root t))))))))

(provide 'chidu-test-support)

;;; chidu-test-support.el ends here
