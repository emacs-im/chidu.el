;;; chidu-parse-test.el --- Email/parse contracts for Chidu -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'cl-lib)
(require 'seq)
(require 'ert)
(require 'appkit-core)
(require 'appkit-media-card)
(require 'chidu)
(require 'chidu-attachment)
(require 'chidu-jmap-parse)
(require 'chidu-parse-sync)
(require 'chidu-parsed-message)
(require 'chidu-store-sqlite)
(require 'chidu-test-support)

(defun chidu-parse-test--address (name email)
  "Return one parsed address fixture using NAME and EMAIL."
  (chidu-store-email-address-create :name name :email email))

(defun chidu-parse-test--attachment
    (&optional part-id blob-id media-type name)
  "Return one attachment fixture."
  (chidu-store-email-attachment-create
   :part-id (or part-id "nested-part")
   :blob-id (or blob-id "nested-blob")
   :size 42
   :name (or name "nested.eml")
   :media-type (or media-type "message/rfc822")
   :charset nil
   :disposition "attachment"
   :cid nil
   :language (vector "en")
   :location nil))

(defun chidu-parse-test--message (&optional attachment)
  "Return one parsed message fixture containing optional ATTACHMENT."
  (chidu-store-parsed-message-create
   :message-ids (vector "attached@example.test")
   :in-reply-to (vector "parent@example.test")
   :references (vector "root@example.test" "parent@example.test")
   :sender (vector)
   :from (vector (chidu-parse-test--address "Alice" "alice@example.test"))
   :to (vector (chidu-parse-test--address "Bob" "bob@example.test"))
   :cc (vector (chidu-parse-test--address nil "cc@example.test"))
   :bcc (vector)
   :reply-to (vector)
   :subject "Attached subject"
   :sent-at "2026-08-26T01:02:03Z"
   :preview "Attached preview"
   :body
   (chidu-store-email-body-create
    :email-state nil
    :text-content "Attached body"
    :html-content ""
    :truncated-p nil
    :encoding-problem-p nil
    :attachments (if attachment (vector attachment) (vector)))))

(defun chidu-parse-test--observation (blob-id profile-version &optional attachment)
  "Return parsed Blob observation for BLOB-ID and PROFILE-VERSION."
  (chidu-store-parsed-blob-observation-create
   :blob-id blob-id
   :profile-version profile-version
   :message (chidu-parse-test--message attachment)))

(defun chidu-parse-test--endpoint-account (store account-id)
  "Return connected Endpoint and ACCOUNT-ID Account from STORE."
  (let* ((endpoints
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store (chidu-store-op-list-endpoints-create))))
         (endpoint (aref endpoints 0))
         (account
          (cl-find account-id (chidu-store-endpoint-accounts endpoint)
                   :key #'chidu-store-account-account-id :test #'equal)))
    (list endpoint account)))

(defun chidu-parse-test--source-view (app)
  "Return a disposable Generated source reader below APP."
  (appkit-open-generated-surface
   (appkit-surface-type-create
    :name 'chidu-parse-test :mode #'special-mode
    :init (lambda (_context _input)
            (appkit-next :model (chidu-parsed-message-state-create) :render appkit-render-none))
    :update #'chidu-surface-update
    :renderer-factory
    (lambda (_surface)
      (appkit-generated-renderer-create
       :mount (lambda (&rest _) nil)
       :merge (lambda (_old new) new)
       :render (lambda (&rest _) nil)
       :unmount (lambda (_surface) nil))))
   :app app :identity (make-symbol "parse-source")
   :buffer-name "*Chidu parse source*"))

(ert-deftest chidu-jmap-email-parse-codec-keeps-message-shape-without-email-identity ()
  (let* ((body-values (make-hash-table :test #'equal))
         (parsed (make-hash-table :test #'equal))
         (profile (chidu-jmap-parse-profile-version 4096))
         (request (chidu-jmap-parse--request "remote-account" "root-blob" 4096))
         (arguments (aref (aref (plist-get request :methodCalls) 0) 1)))
    (puthash
     "text"
     '(:value "Parsed plain body"
              :isEncodingProblem :json-false :isTruncated :json-false)
     body-values)
    (puthash
     "html"
     '(:value "<p>Parsed HTML body</p>"
              :isEncodingProblem :json-false :isTruncated t)
     body-values)
    (puthash
     "root-blob"
     `(:messageId ["attached@example.test"]
                  :inReplyTo ["parent@example.test"]
                  :references ["root@example.test" "parent@example.test"]
                  :sender :json-null
                  :from [(:name "Alice" :email "alice@example.test")]
                  :to [(:name :json-null :email "")]
                  :cc :json-null :bcc :json-null :replyTo :json-null
                  :subject "Attached subject"
                  :sentAt "2026-08-26T01:02:03Z"
                  :preview "Attached preview"
                  :bodyValues ,body-values
                  :textBody [(:partId "text" :type "text/plain")]
                  :htmlBody [(:partId "html" :type "text/html")]
                  :attachments
                  [(:partId "nested" :blobId "nested-blob" :size 42
                            :name "nested.eml" :type "message/rfc822"
                            :charset :json-null :disposition "attachment"
                            :cid :json-null :language ["en"] :location :json-null)])
     parsed)
    (let* ((observation
            (chidu-jmap-parse--decode
             (chidu-store-test--method-response
              "Email/parse" "email-parse"
              `(:accountId "remote-account"
                           :parsed ,parsed))
             "remote-account" "root-blob" profile))
           (message (chidu-store-parsed-blob-observation-message observation))
           (body (chidu-store-parsed-message-body message))
           (nested (aref (chidu-store-email-body-attachments body) 0)))
      (should (equal (vector "root-blob") (plist-get arguments :blobIds)))
      (should (= 4096 (plist-get arguments :maxBodyValueBytes)))
      (dolist (forbidden '("id" "mailboxIds" "keywords" "receivedAt"))
        (should-not (seq-contains-p (plist-get arguments :properties)
                                    forbidden #'equal)))
      (should (equal "root-blob"
                     (chidu-store-parsed-blob-observation-blob-id observation)))
      (should (equal profile
                     (chidu-store-parsed-blob-observation-profile-version
                      observation)))
      (should (equal "Alice"
                     (chidu-store-email-address-name
                      (aref (chidu-store-parsed-message-from message) 0))))
      ;; Email/parse is best effort for malformed messages; preserve an empty
      ;; addr-spec instead of rejecting the complete attached message.
      (should (equal ""
                     (chidu-store-email-address-email
                      (aref (chidu-store-parsed-message-to message) 0))))
      (should (equal "Parsed plain body"
                     (chidu-store-email-body-text-content body)))
      (should (chidu-store-email-body-truncated-p body))
      (should-not (chidu-store-email-body-email-state body))
      (should (equal "message/rfc822"
                     (chidu-store-email-attachment-media-type nested))))
    ;; Every requested Blob must be settled exactly once.
    (should-error
     (chidu-jmap-parse--decode
      (chidu-store-test--method-response
       "Email/parse" "email-parse"
       `(:accountId "remote-account"
                    :parsed ,parsed
                    :notParsable ["root-blob"]
                    :notFound :json-null))
      "remote-account" "root-blob" profile)
     :type 'chidu-jmap-error)))

(ert-deftest chidu-parsed-blob-store-cas-replaces-attachments ()
  "Parsed Blob attachment rows replace atomically under one profile revision."
  (when (sqlite-available-p)
    (let* ((root (make-temp-file "chidu-parse-cas-" t))
           (profile (chidu-jmap-parse-profile-version 4096))
           store account-id)
      (set-file-modes root #o700)
      (cl-labels
          ((observation
             (attachments)
             (let* ((message (chidu-parse-test--message))
                    (body (chidu-store-parsed-message-body message)))
               (chidu-store-parsed-blob-observation-create
                :blob-id "root-blob"
                :profile-version profile
                :message
                (chidu-store-parsed-message-with
                 message
                 :body
                 (chidu-store-email-body-with
                  body :attachments attachments)))))
           (commit
             (revision attachments)
             (chidu-store-test--store-call
              store
              (chidu-store-op-replace-parsed-blob-create
               :account-id account-id
               :blob-id "root-blob"
               :profile-version profile
               :expected-revision revision
               :observation (observation attachments))))
           (read-context
             ()
             (chidu-store-test--store-call
              store
              (chidu-store-op-get-parsed-blob-create
               :account-id account-id
               :blob-id "root-blob"
               :profile-version profile))))
        (unwind-protect
            (progn
              (setq store (chidu-store-sqlite-create root)
                    account-id
                    (chidu-store-test--prepare-mailbox-account store))
              (let ((first
                     (chidu-parse-test--attachment
                      "part-a" "blob-a" "text/plain" "first.txt"))
                    (second
                     (chidu-parse-test--attachment
                      "part-b" "blob-b" "message/rfc822" "second.eml")))
                (should
                 (chidu-result-ok-p
                  (commit 0 (vector first second))))
                (let ((stale (commit 0 (vector first))))
                  (should (chidu-result-failure-p stale))
                  (should (eq 'revision-conflict
                              (chidu-result-failure-kind stale))))
                (should
                 (chidu-result-ok-p
                  (commit
                   1
                   (vector
                    (chidu-parse-test--attachment
                     "part-c" "blob-c" "application/pdf" "only.pdf"))))))
              (chidu-store-close store)
              (setq store (chidu-store-sqlite-create root))
              (let* ((result (read-context))
                     (context (chidu-result-ok-value result))
                     (message
                      (chidu-store-parsed-blob-context-message context))
                     (attachments
                      (chidu-store-email-body-attachments
                       (chidu-store-parsed-message-body message))))
                (should (chidu-result-ok-p result))
                (should (= 2
                           (chidu-store-parsed-blob-context-revision context)))
                (should (= 1 (length attachments)))
                (should
                 (equal "part-c"
                        (chidu-store-email-attachment-part-id
                         (aref attachments 0))))))
          (when store (ignore-errors (chidu-store-close store)))
          (when (file-directory-p root) (delete-directory root t)))))))

(ert-deftest chidu-parsed-blob-sync-persists-and-drives-nested-reader
    ()
  (when (sqlite-available-p)
    (let*
        ((root (make-temp-file "chidu-parse-store-" t))
         (profile
          (chidu-jmap-parse-profile-version
           chidu-email-body-value-byte-limit))
         (root-attachment
          (chidu-parse-test--attachment "root-part" "root-blob"
                                        "message/rfc822"
                                        "attached.eml"))
         (nested-attachment (chidu-parse-test--attachment)) store
         runtime account-id endpoint account committed failure app
         source-view)
      (set-file-modes root 448)
      (unwind-protect
          (progn
            (setq store (chidu-store-sqlite-create root) account-id
                  (chidu-store-test--prepare-mailbox-account store))
            (pcase-let
                ((`(,stored-endpoint ,stored-account)
                  (chidu-parse-test--endpoint-account store
                                                      account-id)))
              (setq endpoint stored-endpoint account stored-account))
            (setq runtime
                  (chidu-runtime-open :data-root root :store store))
            (cl-letf
                (((symbol-function 'chidu-runtime--endpoint-secret)
                  (lambda (_endpoint) (copy-sequence "secret")))
                 ((symbol-function 'chidu-jmap-fetch-parsed-blob)
                  (lambda (context secret limit deliver)
                    (should
                     (equal "root-blob"
                            (chidu-store-parsed-blob-context-blob-id
                             context)))
                    (should
                     (= chidu-email-body-value-byte-limit limit))
                    (clear-string secret)
                    (funcall deliver
                             (chidu-result-ok-create :value
                                                     (chidu-parse-test--observation
                                                      "root-blob"
                                                      profile
                                                      nested-attachment)))
                    nil)))
              (chidu-refresh-parsed-blob runtime account "root-blob"
                                         (lambda (context)
                                           (setq committed context))
                                         (lambda (value)
                                           (setq failure value))))
            (should-not failure)
            (should (chidu-store-parsed-blob-context-p committed))
            (should
             (= 1
                (chidu-store-parsed-blob-context-revision committed)))
            (chidu-runtime-close runtime)
            (setq runtime nil store nil)
            (setq store (chidu-store-sqlite-create root) runtime
                  (chidu-runtime-open :data-root root :store store))
            (pcase-let
                ((`(,stored-endpoint ,stored-account)
                  (chidu-parse-test--endpoint-account store
                                                      account-id)))
              (setq endpoint stored-endpoint account stored-account))
            (let (loaded other-profile)
              (chidu-runtime-parsed-blob runtime account "root-blob"
                                         profile
                                         (lambda (context)
                                           (setq loaded context))
                                         (lambda (failure)
                                           (ert-fail
                                            (format "%S" failure))))
              (chidu-runtime-parsed-blob runtime account "root-blob"
                                         "parsed-message-v2:1"
                                         (lambda (context)
                                           (setq other-profile
                                                 context))
                                         (lambda (failure)
                                           (ert-fail
                                            (format "%S" failure))))
              (should
               (= 1 (chidu-store-parsed-blob-context-revision loaded)))
              (should
               (equal "Attached body"
                      (chidu-store-email-body-text-content
                       (chidu-store-parsed-message-body
                        (chidu-store-parsed-blob-context-message
                         loaded)))))
              (should
               (= 0
                  (chidu-store-parsed-blob-context-revision
                   other-profile)))
              (should-not
               (chidu-store-parsed-blob-context-message other-profile)))
            (setq app
                  (appkit-app-start chidu--app-type :identity
                                    (make-symbol "parsed-reader-app")))
            (appkit-app-send app (list :runtime runtime))
            (setq source-view (chidu-parse-test--source-view app))
            (let*
                ((source-context
                  (chidu-store-email-body-context-create :endpoint
                                                         endpoint
                                                         :account
                                                         account
                                                         :local-email-id
                                                         "parent-local"
                                                         :remote-email-id
                                                         "parent-remote"
                                                         :body
                                                         (chidu-store-email-body-create
                                                          :email-state
                                                          "parent-state"
                                                          :text-content
                                                          "Parent"
                                                          :html-content
                                                          ""
                                                          :truncated-p
                                                          nil
                                                          :encoding-problem-p
                                                          nil
                                                          :attachments
                                                          (vector
                                                           root-attachment))))
                 buffer)
              (cl-letf
                  (((symbol-function 'chidu-attachment-download)
                    (lambda (&rest _)
                      (ert-fail
                       "Email/parse open must not download raw bytes")))
                   ((symbol-function 'chidu-refresh-parsed-blob)
                    (lambda (&rest _)
                      (ert-fail
                       "persisted parse must render Store-first"))))
                (progn
                  (chidu-attachment-open source-view source-context
                                         root-attachment)
                  (with-timeout
                      (2
                       (ert-fail
                        "Attached-message reader did not open"))
                    (while
                        (not
                         (appkit-app-surface app
                                             (list 'parsed-message
                                                   (chidu-store-account-account-id
                                                    account)
                                                   (chidu-store-email-attachment-blob-id
                                                    root-attachment)
                                                   (chidu-jmap-parse-profile-version
                                                    chidu-email-body-value-byte-limit))))
                      (accept-process-output nil 0.01))
                    (chidu-test-drain source-view))
                  (setq buffer
                        (appkit-surface-buffer
                         (appkit-app-surface app
                                             (list 'parsed-message
                                                   (chidu-store-account-account-id
                                                    account)
                                                   (chidu-store-email-attachment-blob-id
                                                    root-attachment)
                                                   (chidu-jmap-parse-profile-version
                                                    chidu-email-body-value-byte-limit)))))))
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (should (eq major-mode 'chidu-parsed-message-mode))
                (let ((view (appkit-current-surface)))
                  (chidu-test-drain view))
                (let
                    ((text
                      (buffer-substring-no-properties (point-min)
                                                      (point-max))))
                  (should (string-match-p "Attached subject" text))
                  (should
                   (string-match-p "Alice <alice@example\\.test>" text))
                  (should (string-match-p "Attached body" text))
                  (should (string-match-p "nested\\.eml" text)))
                (goto-char (point-min))
                (should (search-forward "nested.eml" nil t))
                (let*
                    ((card (appkit-media-card-context-at-point))
                     (payload (plist-get card :payload))
                     (nested-context (plist-get payload :context)))
                  (should
                   (chidu-store-parsed-blob-context-p nested-context))
                  (should
                   (equal "root-blob"
                          (chidu-store-parsed-blob-context-blob-id
                           nested-context)))))))
        (when (appkit-app-live-p app)
          (appkit-app-send app (list :runtime nil))
          (dolist (entry (hash-table-values (appkit-app-surfaces app)))
            (when (buffer-live-p (appkit-surface-buffer (cdr entry)))
              (kill-buffer (appkit-surface-buffer (cdr entry)))))
          (appkit-app-close app))
        (when runtime (chidu-runtime-close runtime))
        (when (and store (not runtime))
          (ignore-errors (chidu-store-close store)))
        (when (file-directory-p root) (delete-directory root t))))))

(provide 'chidu-parse-test)

;;; chidu-parse-test.el ends here
