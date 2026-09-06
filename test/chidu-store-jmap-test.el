;;; chidu-store-jmap-test.el --- Store and JMAP tests for Chidu -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'ert)

(require 'chidu-jmap-http)
(require 'chidu-jmap-discovery)
(require 'chidu-jmap-event-source)
(require 'chidu-jmap-body)
(require 'chidu-jmap-conversation)
(require 'chidu-jmap-email)
(require 'chidu-jmap-email-changes)
(require 'chidu-jmap-mailbox)
(require 'chidu-mailbox-move)
(require 'chidu-jmap-mailbox-move)
(require 'chidu-jmap-set)
(require 'chidu-jmap-search)
(require 'chidu-jmap-seen)
(require 'chidu-result)
(require 'chidu-search-query)
(require 'chidu-test-support)
(require 'chidu-store-sqlite)

(defun chidu-store-test--summary-observation-row
    (remote-email-id remote-thread-id &optional subject)
  "Return one fake remote Summary row for the supplied ids."
  (chidu-store-email-summary-observation-row-create
   :remote-email-id remote-email-id
   :remote-thread-id remote-thread-id
   :received-at "2026-08-25T01:02:03Z"
   :from-name remote-email-id
   :from-email (concat remote-email-id "@example.test")
   :subject (or subject remote-email-id)
   :preview (concat "preview-" remote-email-id)
   :unread-p nil :flagged-p nil :has-attachment-p nil))

(defun chidu-store-test--conversation-observation-row
    (remote-email-id message-ids in-reply-to references)
  "Return one fake Conversation row with the supplied header relations."
  (chidu-store-conversation-observation-row-create
   :summary-row
   (chidu-store-test--summary-observation-row
    remote-email-id "thread-1")
   :sent-at nil
   :message-ids (vconcat message-ids)
   :in-reply-to (vconcat in-reply-to)
   :references (vconcat references)))

(ert-deftest chidu-store-local-id-has-rfc4122-shape ()
  (let ((left (chidu-store-new-local-id))
        (right (chidu-store-new-local-id)))
    (should (chidu-store-local-id-p left))
    (should (chidu-store-local-id-p right))
    (should-not (equal left right))))

(ert-deftest chidu-store-configure-is-stable-and-closed ()
  (let* ((store (chidu-test-store-create))
         (first
          (chidu-store-test--store-call
           store
           (chidu-store-op-configure-endpoint-create
            :session-url "https://mail.example.test/.well-known/jmap"
            :login "me@example.test"
            :authentication 'basic)))
         (first-endpoint (chidu-result-ok-value first))
         (second
          (chidu-store-test--store-call
           store
           (chidu-store-op-configure-endpoint-create
            :session-url "https://mail.example.test/.well-known/jmap"
            :login "me@example.test"
            :authentication 'bearer)))
         (second-endpoint (chidu-result-ok-value second))
         (listed
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store (chidu-store-op-list-endpoints-create))))
         (runtime
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store (chidu-store-op-runtime-create)))))
    (should (equal (chidu-store-endpoint-endpoint-id first-endpoint)
                   (chidu-store-endpoint-endpoint-id second-endpoint)))
    (should (eq 'bearer
                (chidu-store-endpoint-authentication second-endpoint)))
    (should (= 1 (length listed)))
    (should (equal "2" (chidu-store-runtime-change-seq runtime)))
    (should-error
     (chidu-store-call store 'arbitrary-sql #'ignore))))

(ert-deftest chidu-jmap-http-uses-plz-with-chidu-policy-and-scrubs-args ()
  (let (captured-args captured-curl-args process)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq captured-args arguments
                           captured-curl-args (copy-sequence plz-curl-default-args)
                           process (make-pipe-process
                                    :name "chidu-plz-test" :noquery t))
                     ;; Mirror plz 0.9.1's diagnostic copy so the wrapper has
                     ;; something sensitive to scrub.
                     (process-put process :plz-args arguments)
                     process)))
          (let ((secret (copy-sequence "abc.DEF_123"))
                returned)
            (setq returned
                  (chidu-jmap-http-request
                   "https://mail.example.test/jmap/api"
                   "me@example.test" 'bearer secret #'ignore
                   :body `(:using [,chidu-jmap-core-capability])))
            (should (eq process returned))
            (should (member "--disable" captured-curl-args))
            (should (member "--proto" captured-curl-args))
            (should (member "=https" captured-curl-args))
            (should (member "--max-filesize" captured-curl-args))
            (should (member "--location" captured-curl-args))
            (should (member "--post301" captured-curl-args))
            (should (member "--post302" captured-curl-args))
            (should (member "--post303" captured-curl-args))
            (should (member "--proto-redir" captured-curl-args))
            (should (member "--max-redirs" captured-curl-args))
            (should-not (member "--location-trusted" captured-curl-args))
            (should-not (process-get process :plz-args))
            (should-not
             (string-match-p "abc\.DEF_123" (prin1-to-string captured-args)))
            (let* ((properties (nthcdr 2 captured-args))
                   (headers (plist-get properties :headers))
                   (authorization (cdr (assoc "Authorization" headers)))
                   (body (plist-get properties :body)))
              (should (cl-every #'zerop authorization))
              (should (cl-every #'zerop body)))
            ;; The workflow still owns the source secret for subsequent JMAP
            ;; calls; only per-request copies are scrubbed here.
            (should (equal "abc.DEF_123" secret))))
      (when (and process (process-live-p process))
        (delete-process process)))))

(ert-deftest chidu-jmap-http-rejects-oversized-request-before-dispatch ()
  (let (result dispatched)
    (cl-letf (((symbol-function 'plz)
               (lambda (&rest _arguments)
                 (setq dispatched t)
                 (error "must not dispatch"))))
      (should-not
       (chidu-jmap-http-request
        "https://mail.example.test/jmap/api"
        "me@example.test" 'bearer (copy-sequence "secret")
        (lambda (value) (setq result value))
        :body `(:using [,chidu-jmap-core-capability])
        :max-request-bytes 1))
      (should-not dispatched)
      (should (chidu-result-failure-p result))
      (should (eq 'request-too-large
                  (chidu-result-failure-kind result))))))

(ert-deftest chidu-jmap-http-types-status-and-size-failures ()
  (let ((authentication
         (chidu-jmap-http--http-result
          (make-plz-response :version 1.1 :status 401 :headers nil :body "")
          1024))
        (server
         (chidu-jmap-http--http-result
          (make-plz-response :version 1.1 :status 503 :headers nil :body "")
          1024))
        (oversized
         (chidu-jmap-http--http-result
          (make-plz-response :version 1.1 :status 200 :headers nil
                             :body (make-string 16 ?x))
          8)))
    (should (eq 'authentication-rejected
                (chidu-result-failure-kind authentication)))
    (should (eq 'http-error (chidu-result-failure-kind server)))
    (should (chidu-result-failure-retryable-p server))
    (should (eq 'response-too-large
                (chidu-result-failure-kind oversized)))))

(ert-deftest chidu-jmap-email-baseline-codec ()
  (let* ((state-request (chidu-jmap-email--state-request "remote-account"))
         (state-call (aref (plist-get state-request :methodCalls) 0))
         (state-arguments (aref state-call 1))
         (first-query
          (chidu-jmap-email--query-request "remote-account" nil 2))
         (first-arguments
          (aref (aref (plist-get first-query :methodCalls) 0) 1))
         (next-query
          (chidu-jmap-email--query-request "remote-account" "email-2" 2))
         (next-arguments
          (aref (aref (plist-get next-query :methodCalls) 0) 1))
         (state-bytes
          (chidu-store-test--payload
           '(:sessionState "session"
             :methodResponses
             [["Email/get"
               (:accountId "remote-account" :state "email-0"
                :list [] :notFound [])
               "email-state"]])))
         (query-bytes
          (chidu-store-test--payload
           '(:sessionState "session"
             :methodResponses
             [["Email/query"
               (:accountId "remote-account"
                :queryState "query-1"
                :canCalculateChanges t
                :position 0
                :ids ["email-1" "email-2"])
               "email-query"]])))
         (page
          (chidu-jmap-email--validate-query-page
           query-bytes "remote-account" 2)))
    (should (equal (vector) (plist-get state-arguments :ids)))
    (should (= 0 (plist-get first-arguments :position)))
    (should-not (plist-member first-arguments :anchor))
    (should (equal "email-2" (plist-get next-arguments :anchor)))
    (should (= 1 (plist-get next-arguments :anchorOffset)))
    (should-not (plist-member next-arguments :position))
    (should
     (equal "email-0"
            (chidu-jmap-email--validate-state
             state-bytes "remote-account")))
    (should (equal "query-1"
                   (chidu-store-email-query-page-observation-query-state page)))
    (should
     (equal (vector "email-1" "email-2")
            (chidu-store-email-query-page-observation-remote-email-ids page)))))

(ert-deftest chidu-jmap-email-changes-normalizes-overlap-and-errors ()
  (let* ((request
           (chidu-jmap-email-changes--request
            "remote-account" "email/state:0" 10))
         (arguments (aref (aref (plist-get request :methodCalls) 0) 1))
         (bytes
          (chidu-store-test--method-response
           "Email/changes" "email-changes"
           '(:accountId "remote-account"
             :oldState "email/state:0" :newState "email/state:1"
             :hasMoreChanges t
             :created ["email-1" "email-2"]
             :updated ["email-2" "email-3" "email-4"]
             :destroyed ["email-2" "email-4"])))
         (page
          (chidu-jmap-email-changes--decode
           bytes "remote-account" "email/state:0" 10))
         (cannot
          (chidu-jmap-email-changes--decode
           (chidu-store-test--payload
            '(:sessionState "session"
              :methodResponses
              [["error" (:type "cannotCalculateChanges") "email-changes"]]))
           "remote-account" "email/state:old" 10)))
    (should (equal "email/state:0" (plist-get arguments :sinceState)))
    (should (= 10 (plist-get arguments :maxChanges)))
    (should (equal (vector "email-1")
                   (chidu-jmap-email-changes-page-created page)))
    (should (equal (vector "email-3")
                   (chidu-jmap-email-changes-page-updated page)))
    (should (equal (vector "email-2" "email-4")
                   (chidu-jmap-email-changes-page-destroyed page)))
    (should (chidu-jmap-email-changes-page-has-more-changes-p page))
    (should (chidu-result-failure-p cannot))
    (should (eq 'cannot-calculate-changes
                (chidu-result-failure-kind cannot)))
    ;; The wire limit applies before overlap normalization.
    (should-error
     (chidu-jmap-email-changes--decode
      bytes "remote-account" "email/state:0" 6)
     :type 'chidu-jmap-error)))

(ert-deftest chidu-jmap-query-page-slice-detects-more-without-total ()
  (let* ((probed
          (chidu-jmap-email-query-page-slice
           (chidu-store-email-query-page-observation-create
            :query-state "q" :position 0
            :remote-email-ids (vector "email-1" "email-2"))
           1 2))
         (exhausted
          (chidu-jmap-email-query-page-slice
           (chidu-store-email-query-page-observation-create
            :query-state "q" :position 0
            :remote-email-ids (vector "email-1"))
           1 2))
         (server-clamped
          (chidu-jmap-email-query-page-slice
           (chidu-store-email-query-page-observation-create
            :query-state "q" :position 0 :server-limit 1
            :remote-email-ids (vector "email-1"))
           1 2))
         (unprobed
          (chidu-jmap-email-query-page-slice
           (chidu-store-email-query-page-observation-create
            :query-state "q" :position 0 :server-limit 1
            :remote-email-ids (vector "email-1"))
           1 1)))
    (should (equal (vector "email-1") (plist-get probed :ids)))
    (should (equal "email-1" (plist-get probed :cursor)))
    (should (plist-get probed :maybe-more-p))
    (should-not (plist-get exhausted :maybe-more-p))
    (should (plist-get server-clamped :maybe-more-p))
    (should (plist-get unprobed :maybe-more-p))))

(ert-deftest chidu-jmap-body-and-conversation-codec ()
  (let ((body-values (make-hash-table :test #'equal))
        (mailbox-ids (make-hash-table :test #'equal))
        (keywords (make-hash-table :test #'equal)))
    (puthash "1"
             '(:value "Full plain body"
               :isEncodingProblem :json-false :isTruncated :json-false)
             body-values)
    (puthash "2"
             '(:value "<p>Full HTML body</p>"
               :isEncodingProblem :json-false :isTruncated t)
             body-values)
    (puthash "inbox" t mailbox-ids)
    (puthash "$flagged" t keywords)
    (cl-labels
        ((email-wire
           (id message-id reply references)
           `(:id ,id :threadId "thread-1"
             :mailboxIds ,mailbox-ids :keywords ,keywords
             :receivedAt "2026-08-25T01:02:03Z" :sentAt :json-null
             :from [(:name :json-null
                     :email ,(concat id "@example.test"))]
             :subject ,id :preview ,(concat "preview-" id)
             :hasAttachment :json-false
             :messageId [,message-id]
             :inReplyTo ,(if reply (vector reply) :json-null)
             :references ,(vconcat references))))
      (let* ((body-request
              (chidu-jmap-body--request "remote-account" "email-2" 4096))
             (body-arguments
              (aref (aref (plist-get body-request :methodCalls) 0) 1))
             (body
              (chidu-jmap-body--validate
               (chidu-store-test--method-response
                "Email/get" "email-body"
                `(:accountId "remote-account" :state "body/state:1"
                  :list
                  [(:id "email-2" :bodyValues ,body-values
                    :textBody [(:partId "1" :type "text/plain")]
                    :htmlBody [(:partId "2" :type "text/html")]
                    :attachments
                    [(:partId "3" :blobId "blob-image" :size 128
                      :name "diagram.png" :type "image/png"
                      :charset :json-null :disposition "inline"
                      :cid "diagram@example.test" :language ["en"]
                      :location "images/diagram.png")
                     (:partId "4" :blobId "blob-pdf" :size 4096
                      :name "report.pdf" :type "application/pdf"
                      :charset :json-null :disposition "attachment"
                      :cid :json-null :language :json-null
                      :location :json-null)])]
                  :notFound []))
               "remote-account" "email-2"))
             (thread
              (chidu-jmap-conversation--validate-thread
               (chidu-store-test--method-response
                "Thread/get" "conversation-thread"
                '(:accountId "remote-account" :state "thread/state:1"
                  :list
                  [(:id "thread-1"
                    :emailIds ["email-1" "email-2" "email-3"])]
                  :notFound []))
               "remote-account" "thread-1" 8))
             (conversation
              (chidu-jmap-conversation--validate-emails
               (chidu-store-test--method-response
                "Email/get" "conversation-email"
                `(:accountId "remote-account"
                  :state "email/state:conversation"
                  :list
                  [,(email-wire "email-3" "m3" "missing" '("m1" "m2"))
                   ,(email-wire "email-1" "m1" nil nil)
                   ,(email-wire "email-2" "m2" "m1" '("m1"))]
                  :notFound []))
               "remote-account" "thread-1" (cdr thread) (car thread)))
             (tree
              (chidu-store-materialize-conversation-rows
               conversation (lambda (id) (concat "local-" id))))
             (malformed
              (chidu-store-materialize-conversation-rows
               (chidu-store-conversation-observation-create
                :remote-thread-id "thread-1"
                :thread-state "thread/state:malformed"
                :email-state "email/state:malformed"
                :complete-p t
                :rows
                (vector
                 (chidu-store-test--conversation-observation-row
                  "cycle-1" '("cycle-message-1")
                  '("cycle-message-2") nil)
                 (chidu-store-test--conversation-observation-row
                  "cycle-2" '("cycle-message-2")
                  '("cycle-message-1") nil)
                 (chidu-store-test--conversation-observation-row
                  "duplicate-1" '("duplicate-message") nil nil)
                 (chidu-store-test--conversation-observation-row
                  "duplicate-2" '("duplicate-message") nil nil)
                 (chidu-store-test--conversation-observation-row
                  "duplicate-reply" '("unique-message")
                  '("duplicate-message") nil)))
               (lambda (id) (concat "local-" id)))))
        (should (= 4096 (plist-get body-arguments :maxBodyValueBytes)))
        (should (equal "Full plain body"
                       (chidu-store-email-body-observation-text-content body)))
        (should (equal "<p>Full HTML body</p>"
                       (chidu-store-email-body-observation-html-content body)))
        (should (chidu-store-email-body-observation-truncated-p body))
        (let* ((attachments
                (chidu-store-email-body-observation-attachments body))
               (image (aref attachments 0))
               (document (aref attachments 1)))
          (should (= 2 (length attachments)))
          (should (equal "diagram.png"
                         (chidu-store-email-attachment-name image)))
          (should (equal "image/png"
                         (chidu-store-email-attachment-media-type image)))
          (should (equal "inline"
                         (chidu-store-email-attachment-disposition image)))
          (should (equal (vector "en")
                         (chidu-store-email-attachment-language image)))
          (should (equal "blob-pdf"
                         (chidu-store-email-attachment-blob-id document)))
          (should (= 4096
                     (chidu-store-email-attachment-size document))))
        (should (equal (vector "email-1" "email-2" "email-3")
                       (cdr thread)))
        (should
         (equal '("email-1" "email-2" "email-3")
                (cl-loop
                 for item across tree
                 collect
                 (chidu-store-email-summary-row-remote-email-id
                  (chidu-store-conversation-row-summary-row item)))))
        (should-not
         (chidu-store-conversation-row-parent-local-email-id (aref tree 0)))
        (should (equal "local-email-1"
                       (chidu-store-conversation-row-parent-local-email-id
                        (aref tree 1))))
        (should (equal "local-email-2"
                       (chidu-store-conversation-row-parent-local-email-id
                        (aref tree 2))))
        (should (equal '(0 1 2)
                       (cl-loop for item across tree
                                collect
                                (chidu-store-conversation-row-depth item))))
        ;; Break the oldest cycle edge; ambiguous Message-ID has no parent.
        (should-not
         (chidu-store-conversation-row-parent-local-email-id
          (aref malformed 0)))
        (should (equal "local-cycle-1"
                       (chidu-store-conversation-row-parent-local-email-id
                        (aref malformed 1))))
        (should-not
         (chidu-store-conversation-row-parent-local-email-id
          (aref malformed 4)))))))

(ert-deftest chidu-store-rejects-unsafe-endpoint ()
  (let ((store (chidu-test-store-create)))
    (unwind-protect
        (should-error
         (chidu-store-test--store-call
          store
          (chidu-store-op-configure-endpoint-create
           :session-url "http://user:secret@example.test/jmap"
           :login "me@example.test"
           :authentication 'basic))
         :type 'chidu-invariant-error)
      (chidu-store-close store))))

(defun chidu-store-test--session-response-bytes
    (&optional account-id primary-account-id)
  "Return a complete fake Session response using remote Account ids."
  (let ((core (make-hash-table :test #'equal))
        (capabilities (make-hash-table :test #'equal))
        (account-capabilities (make-hash-table :test #'equal))
        (accounts (make-hash-table :test #'equal))
        (primary (make-hash-table :test #'equal))
        (account (make-hash-table :test #'equal))
        (remote-id (or account-id "remote-account")))
    (dolist (entry
             '(("maxSizeUpload" . 1048576)
               ("maxConcurrentUpload" . 1)
               ("maxSizeRequest" . 1048576)
               ("maxConcurrentRequests" . 1)
               ("maxCallsInRequest" . 16)
               ("maxObjectsInGet" . 1024)
               ("maxObjectsInSet" . 1024)))
      (puthash (car entry) (cdr entry) core))
    (puthash "collationAlgorithms" (vector) core)
    (puthash chidu-jmap-core-capability core capabilities)
    (puthash chidu-jmap-mail-capability
             (make-hash-table :test #'equal) capabilities)
    (let ((mail (make-hash-table :test #'equal)))
      (puthash "maxSizeAttachmentsPerEmail" 2097152 mail)
      (puthash chidu-jmap-mail-capability mail account-capabilities))
    (puthash "name" "Mail" account)
    (puthash "isPersonal" t account)
    (puthash "isReadOnly" :json-false account)
    (puthash "accountCapabilities" account-capabilities account)
    (puthash remote-id account accounts)
    (puthash chidu-jmap-mail-capability
             (or primary-account-id remote-id) primary)
    (chidu-store-test--payload
     `(:capabilities ,capabilities
       :accounts ,accounts
       :primaryAccounts ,primary
       :username "me@example.test"
       :apiUrl "https://mail.example.test/jmap/api"
       :downloadUrl
       "https://mail.example.test/jmap/download/{accountId}/{blobId}/{name}?type={type}"
       :uploadUrl "https://mail.example.test/jmap/upload/{accountId}"
       :eventSourceUrl
       "https://mail.example.test/jmap/eventsource/?types={types}"
       :state "session-state"))))

(defun chidu-store-test--identity-response-bytes (identity-id)
  "Return one fake Identity/get response using IDENTITY-ID."
  (chidu-store-test--payload
   `(:sessionState "session-state"
     :methodResponses
     [["Identity/get"
       (:accountId "remote-account"
        :state "identity-state"
        :list
        [(:id ,identity-id
          :name "Me"
          :email "me@example.test"
          :mayDelete :json-false)]
        :notFound [])
       "identity-get"]])))

(ert-deftest chidu-jmap-single-response-enforces-envelope-once ()
  (let ((wrong-call
         (chidu-store-test--payload
          '(:sessionState "session-state"
            :methodResponses
            [["Identity/get"
              (:accountId "remote-account"
               :state "identity-state"
               :list []
               :notFound [])
              "wrong-call"]]))))
    (should-error
     (chidu-jmap-parse-single-method-response
      wrong-call "Identity/get" "identity-get" "remote-account")
     :type 'chidu-jmap-error))
  (let ((method-error
         (chidu-store-test--payload
          '(:sessionState "session-state"
            :methodResponses
            [["error" (:type "accountNotFound") "identity-get"]]))))
    (should-error
     (chidu-jmap-parse-single-method-response
      method-error "Identity/get" "identity-get" "remote-account")
     :type 'chidu-jmap-error)))

(ert-deftest chidu-jmap-discovery-clears-secret-on-startup-failure ()
  (let ((endpoint
         (chidu-store-endpoint-create
          :endpoint-id "endpoint"
          :session-url "not-an-https-url"
          :login "me@example.test"
          :authentication 'basic))
        (secret (copy-sequence "startup-secret"))
        result)
    (should-not
     (chidu-jmap-discover
      endpoint secret (lambda (value) (setq result value))))
    (should (chidu-result-failure-p result))
    (should (eq 'jmap-discovery-failed
                (chidu-result-failure-kind result)))
    (should-not (equal secret "startup-secret"))))

(ert-deftest chidu-jmap-id-enforces-rfc8620-shape ()
  (should (equal "AZaz09_-" (chidu-jmap--id "AZaz09_-" "test Id")))
  (should (= 255 (length (chidu-jmap--id (make-string 255 ?a) "test Id"))))
  (dolist (value
           (list "" "contains=" "contains/" "contains+" "汉"
                 (make-string 256 ?a)))
    (should-error (chidu-jmap--id value "test Id")
                  :type 'chidu-jmap-error)))

(ert-deftest chidu-jmap-json-parses-unibyte-utf8-strictly ()
  (let* ((bytes
          (encode-coding-string "{\"value\":\"é\"}" 'utf-8-unix t))
         (object (chidu-jmap--parse-json-object bytes "test JSON")))
    (should (equal "é" (gethash "value" object))))
  (should-error
   (chidu-jmap--parse-json-object
    (unibyte-string ?{ ?\" ?x ?\" ?: ?\" 255 ?\" ?})
    "test JSON")
   :type 'chidu-jmap-error))

(ert-deftest chidu-jmap-decoders-enforce-id-at-object-boundaries ()
  (let ((session
         (chidu-jmap--validate-session
          (chidu-store-test--session-response-bytes)
          "https://mail.example.test/.well-known/jmap")))
    (should (= 1048576
               (chidu-store-session-observation-max-size-upload
                session)))
    (should (= 1048576
               (chidu-store-session-observation-max-size-request
                session)))
    (should (= 1024
               (chidu-store-session-observation-max-objects-in-get
                session)))
    (should (= 1024
               (chidu-store-session-observation-max-objects-in-set
                session)))
    (should
     (= 2097152
        (chidu-store-account-observation-max-size-attachments-per-email
         (aref (chidu-store-session-observation-accounts session) 0)))))
  (should-error
   (chidu-jmap--validate-session
    (chidu-store-test--session-response-bytes "bad/account" "remote-account")
    "https://mail.example.test/.well-known/jmap")
   :type 'chidu-jmap-error)
  (should-error
   (chidu-jmap--validate-session
    (chidu-store-test--session-response-bytes "remote-account" "bad/account")
    "https://mail.example.test/.well-known/jmap")
   :type 'chidu-jmap-error)
  (should-error
   (chidu-jmap--validate-identities
    (chidu-store-test--identity-response-bytes "bad/identity")
    "remote-account")
   :type 'chidu-jmap-error)
  (let ((mailbox (chidu-store-test--mailbox-wire "bad/mailbox")))
    (should-error (chidu-jmap--mailbox-observation mailbox)
                  :type 'chidu-jmap-error))
  (let ((mailbox (chidu-store-test--mailbox-wire "child" "bad/parent")))
    (should-error (chidu-jmap--mailbox-observation mailbox)
                  :type 'chidu-jmap-error)))

(defun chidu-store-test--session-observation ()
  "Return a validated fake Session observation."
  (chidu-store-session-observation-create
   :username "me@example.test"
   :state "session-1"
   :api-url "https://mail.example.test/jmap/api"
   :download-url "https://mail.example.test/jmap/download/{accountId}/{blobId}/{name}?type={type}"
   :upload-url "https://mail.example.test/jmap/upload/{accountId}"
   :event-source-url "https://mail.example.test/jmap/eventsource/?types={types}"
   :max-size-request 1048576
   :max-objects-in-get 256
   :max-objects-in-set 128
   :capabilities
   (vector chidu-jmap-core-capability
           chidu-jmap-mail-capability
           chidu-jmap-submission-capability)
   :accounts
   (vector
    (chidu-store-account-observation-create
     :remote-account-id "remote-account"
     :name "Mail"
     :personal-p t
     :read-only-p nil
     :primary-mail-p t
     :primary-submission-p t
     :identity-state "identity-1"
     :capabilities
     (vector chidu-jmap-mail-capability
             chidu-jmap-submission-capability)
     :identities
     (vector
      (chidu-store-identity-observation-create
       :remote-identity-id "remote-identity"
       :name "Me"
       :email "me@example.test"))))))

(defun chidu-store-test--identity-observation ()
  "Return one fake Identity observation."
  (chidu-store-identity-observation-create
   :remote-identity-id "remote-identity"
   :name "Me"
   :email "me@example.test"))

(defun chidu-store-test--exercise-store-visibility (store)
  "Exercise identity stability and visibility transitions in STORE."
  (let* ((configured
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-configure-endpoint-create
             :session-url "https://mail.example.test/.well-known/jmap"
             :login "me@example.test"
             :authentication 'basic))))
         (endpoint-id (chidu-store-endpoint-endpoint-id configured))
         (first
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-observe-session-create
             :endpoint-id endpoint-id
             :observation
             (chidu-store-test--session-observation-with
              "session-1"
              (vector
               (chidu-store-test--account-observation
                (vector (chidu-store-test--identity-observation)))))))))
         (first-account (aref (chidu-store-endpoint-accounts first) 0))
         (first-identity (aref (chidu-store-account-identities first-account) 0))
         (account-id (chidu-store-account-account-id first-account))
         (identity-id (chidu-store-identity-identity-id first-identity))
         (second
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-observe-session-create
             :endpoint-id endpoint-id
             :observation
             (chidu-store-test--session-observation-with
              "session-2"
              (vector
               (chidu-store-test--account-observation
                (vector) "identity-2")))))))
         (second-account (aref (chidu-store-endpoint-accounts second) 0))
         (second-identity
          (aref (chidu-store-account-identities second-account) 0))
         (third
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-observe-session-create
             :endpoint-id endpoint-id
             :observation
             (chidu-store-test--session-observation-with
              "session-3" (vector))))))
         (third-account (aref (chidu-store-endpoint-accounts third) 0))
         (third-identity
          (aref (chidu-store-account-identities third-account) 0))
         (runtime
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store (chidu-store-op-runtime-create)))))
    (should (= 1048576
               (chidu-store-endpoint-max-size-request first)))
    (should (chidu-store-account-available-p first-account))
    (should (chidu-store-identity-available-p first-identity))
    (should (equal account-id
                   (chidu-store-account-account-id second-account)))
    (should (equal identity-id
                   (chidu-store-identity-identity-id second-identity)))
    (should (chidu-store-account-available-p second-account))
    (should-not (chidu-store-identity-available-p second-identity))
    (should (equal account-id
                   (chidu-store-account-account-id third-account)))
    (should (equal identity-id
                   (chidu-store-identity-identity-id third-identity)))
    (should-not (chidu-store-account-available-p third-account))
    (should-not (chidu-store-identity-available-p third-identity))
    (should (equal "4" (chidu-store-runtime-change-seq runtime)))
    `(:endpoint-id ,endpoint-id
      :account-id ,account-id
      :identity-id ,identity-id
      :store-id ,(chidu-store-runtime-store-id runtime))))

(ert-deftest chidu-store-sqlite-matches-visibility-and-survives-reopen ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-sqlite-store-" t))
        first-state
        store)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root)
                first-state
                (chidu-store-test--exercise-store-visibility store))
          (let ((snapshot (chidu-store-inspect store)))
            (should (eq 'sqlite (plist-get snapshot :backend)))
            (should-not (plist-get snapshot :strict-native-open-p)))
          (chidu-store-close store)
          (setq store nil)
          (setq store (chidu-store-sqlite-create root))
          (let* ((runtime
                  (chidu-result-ok-value
                   (chidu-store-test--store-call
                    store (chidu-store-op-runtime-create))))
                 (endpoints
                  (chidu-result-ok-value
                   (chidu-store-test--store-call
                    store (chidu-store-op-list-endpoints-create))))
                 (endpoint (aref endpoints 0))
                 (account (aref (chidu-store-endpoint-accounts endpoint) 0))
                 (identity (aref (chidu-store-account-identities account) 0)))
            (should (equal (plist-get first-state :store-id)
                           (chidu-store-runtime-store-id runtime)))
            (should (equal "4" (chidu-store-runtime-change-seq runtime)))
            (should (equal (plist-get first-state :endpoint-id)
                           (chidu-store-endpoint-endpoint-id endpoint)))
            (should (= 1048576
                       (chidu-store-endpoint-max-size-request endpoint)))
            (should (equal (plist-get first-state :account-id)
                           (chidu-store-account-account-id account)))
            (should (equal (plist-get first-state :identity-id)
                           (chidu-store-identity-identity-id identity)))
            (should-not (chidu-store-account-available-p account))
            (should-not (chidu-store-identity-available-p identity)))
          (dolist (suffix '("" "-wal" "-shm"))
            (let ((path (expand-file-name
                         (concat "store.sqlite3" suffix) root)))
              (when (file-exists-p path)
                (should (= #o600 (logand (file-modes path) #o777)))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-store-sqlite-rejects-a-second-live-owner ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-sqlite-owner-" t))
        first
        second)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq first (chidu-store-sqlite-create root))
          (should-error
           (setq second (chidu-store-sqlite-create root))
           :type 'chidu-invariant-error)
          (chidu-store-close first)
          (setq first nil
                second (chidu-store-sqlite-create root))
          (should
           (eq 'sqlite-exclusive-transaction
               (plist-get (chidu-store-inspect second) :owner-lock))))
      (when second (chidu-store-close second))
      (when first (chidu-store-close first))
      (when (file-directory-p root) (delete-directory root t)))))

(defun chidu-store-test--mailbox-rights (&optional writable)
  "Return fake Mailbox rights, enabling write rights when WRITABLE."
  (chidu-store-mailbox-rights-create
   :may-read-items-p t
   :may-add-items-p (and writable t)
   :may-remove-items-p (and writable t)
   :may-set-seen-p (and writable t)
   :may-set-keywords-p (and writable t)
   :may-create-child-p (and writable t)
   :may-rename-p (and writable t)
   :may-delete-p (and writable t)
   :may-submit-p (and writable t)))

(defun chidu-store-test--mailbox-observation
    (remote-id name &rest properties)
  "Return fake Mailbox observation for REMOTE-ID and NAME.

PROPERTIES may override parent, role, counts, rights, and subscription."
  (chidu-store-mailbox-observation-create
   :remote-mailbox-id remote-id
   :name name
   :parent-remote-mailbox-id (plist-get properties :parent)
   :role (plist-get properties :role)
   :sort-order (or (plist-get properties :sort-order) 0)
   :total-emails (or (plist-get properties :total-emails) 0)
   :unread-emails (or (plist-get properties :unread-emails) 0)
   :total-threads (or (plist-get properties :total-threads) 0)
   :unread-threads (or (plist-get properties :unread-threads) 0)
   :rights (or (plist-get properties :rights)
               (chidu-store-test--mailbox-rights))
   :subscribed-p (plist-get properties :subscribed-p)))

(defun chidu-store-test--exercise-mailbox-store (store)
  "Exercise Mailbox snapshot identity, CAS, hierarchy, and retention in STORE."
  (let* ((account-id (chidu-store-test--prepare-mailbox-account store))
         (initial
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-get-mailbox-sync-context-create
             :account-id account-id))))
         first
         second)
    (should-not (chidu-store-mailbox-sync-context-state initial))
    (should (= 0 (chidu-store-mailbox-sync-context-revision initial)))
    (should
     (= 0 (length (chidu-store-mailbox-sync-context-mailboxes initial))))
    (setq
     first
     (chidu-result-ok-value
      (chidu-store-test--store-call
       store
       (chidu-store-op-observe-mailbox-snapshot-create
        :account-id account-id
        :expected-revision 0
        :observation
        (chidu-store-mailbox-snapshot-observation-create
         :state "mailbox-1"
         :mailboxes
         (vector
          (chidu-store-test--mailbox-observation
           "inbox" "Inbox" :role "inbox" :sort-order 10
           :total-emails 8 :unread-emails 0
           :total-threads 7 :unread-threads 2
           :rights (chidu-store-test--mailbox-rights t)
           :subscribed-p t)
          (chidu-store-test--mailbox-observation
           "child" "Child" :parent "inbox" :sort-order 20)))))))
    (should
     (equal "mailbox-1" (chidu-store-mailbox-sync-context-state first)))
    (should (= 1 (chidu-store-mailbox-sync-context-revision first)))
    (let* ((mailboxes (chidu-store-mailbox-sync-context-mailboxes first))
           (child
            (cl-find "child" mailboxes
                     :key #'chidu-store-mailbox-remote-mailbox-id
                     :test #'equal))
           (inbox
            (cl-find "inbox" mailboxes
                     :key #'chidu-store-mailbox-remote-mailbox-id
                     :test #'equal))
           (inbox-id (chidu-store-mailbox-mailbox-id inbox))
           (child-id (chidu-store-mailbox-mailbox-id child))
           (conflict
            (chidu-store-test--store-call
             store
             (chidu-store-op-observe-mailbox-snapshot-create
              :account-id account-id
              :expected-revision 0
              :observation
              (chidu-store-mailbox-snapshot-observation-create
               :state "stale" :mailboxes (vector))))))
      (should (chidu-store-local-id-p inbox-id))
      (should (chidu-store-local-id-p child-id))
      (should
       (equal inbox-id (chidu-store-mailbox-parent-mailbox-id child)))
      (should (= 0 (chidu-store-mailbox-unread-emails inbox)))
      (should (= 2 (chidu-store-mailbox-unread-threads inbox)))
      (should (equal "inbox" (chidu-store-mailbox-role inbox)))
      (should (= 10 (chidu-store-mailbox-sort-order inbox)))
      (should (chidu-store-mailbox-subscribed-p inbox))
      (let ((rights (chidu-store-mailbox-rights inbox)))
        (dolist (accessor
                 '(chidu-store-mailbox-rights-may-read-items-p
                   chidu-store-mailbox-rights-may-add-items-p
                   chidu-store-mailbox-rights-may-remove-items-p
                   chidu-store-mailbox-rights-may-set-seen-p
                   chidu-store-mailbox-rights-may-set-keywords-p
                   chidu-store-mailbox-rights-may-create-child-p
                   chidu-store-mailbox-rights-may-rename-p
                   chidu-store-mailbox-rights-may-delete-p
                   chidu-store-mailbox-rights-may-submit-p))
          (should (funcall accessor rights))))
      (should (chidu-result-failure-p conflict))
      (should
       (eq 'revision-conflict (chidu-result-failure-kind conflict)))
      (setq
       second
       (chidu-result-ok-value
        (chidu-store-test--store-call
         store
         (chidu-store-op-observe-mailbox-snapshot-create
          :account-id account-id
          :expected-revision 1
          :observation
          (chidu-store-mailbox-snapshot-observation-create
           :state "mailbox-2"
           :mailboxes
           (vector
            (chidu-store-test--mailbox-observation
             "inbox" "Inbox" :role "inbox" :sort-order 10
             :total-emails 9 :unread-emails 1
             :total-threads 8 :unread-threads 1
             :rights (chidu-store-test--mailbox-rights t)
             :subscribed-p t)))))))
      (let* ((mailboxes
              (chidu-store-mailbox-sync-context-mailboxes second))
             (new-inbox
              (cl-find "inbox" mailboxes
                       :key #'chidu-store-mailbox-remote-mailbox-id
                       :test #'equal))
             (retained-child
              (cl-find "child" mailboxes
                       :key #'chidu-store-mailbox-remote-mailbox-id
                       :test #'equal)))
        (should (= 2 (chidu-store-mailbox-sync-context-revision second)))
        (should
         (equal "mailbox-2"
                (chidu-store-mailbox-sync-context-state second)))
        (should
         (equal inbox-id (chidu-store-mailbox-mailbox-id new-inbox)))
        (should
         (equal child-id
                (chidu-store-mailbox-mailbox-id retained-child)))
        (should (chidu-store-mailbox-available-p new-inbox))
        (should-not (chidu-store-mailbox-available-p retained-child))
        (should (= 9 (chidu-store-mailbox-total-emails new-inbox)))
        `(:account-id ,account-id
          :inbox-id ,inbox-id
          :child-id ,child-id
          :state ,(chidu-store-mailbox-sync-context-state second)
          :revision
          ,(chidu-store-mailbox-sync-context-revision second))))))

(ert-deftest chidu-store-sqlite-mailbox-snapshot-survives-reopen ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-sqlite-mailbox-" t))
        first
        store)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root)
                first (chidu-store-test--exercise-mailbox-store store))
          (chidu-store-close store)
          (setq store nil
                store (chidu-store-sqlite-create root))
          (let* ((context
                  (chidu-result-ok-value
                   (chidu-store-test--store-call
                    store
                    (chidu-store-op-list-mailboxes-create
                     :account-id (plist-get first :account-id)))))
                 (mailboxes
                  (chidu-store-mailbox-sync-context-mailboxes context))
                 (inbox
                  (cl-find "inbox" mailboxes
                           :key #'chidu-store-mailbox-remote-mailbox-id
                           :test #'equal))
                 (child
                  (cl-find "child" mailboxes
                           :key #'chidu-store-mailbox-remote-mailbox-id
                           :test #'equal)))
            (should (equal "mailbox-2"
                           (chidu-store-mailbox-sync-context-state context)))
            (should (= 2 (chidu-store-mailbox-sync-context-revision context)))
            (should (equal (plist-get first :inbox-id)
                           (chidu-store-mailbox-mailbox-id inbox)))
            (should (equal (plist-get first :child-id)
                           (chidu-store-mailbox-mailbox-id child)))
            (should (chidu-store-mailbox-available-p inbox))
            (should-not (chidu-store-mailbox-available-p child))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(defun chidu-store-test--mailbox-wire (&optional id parent)
  "Return one complete fake Mailbox wire object for ID and PARENT."
  (let ((rights (make-hash-table :test #'equal))
        (mailbox (make-hash-table :test #'equal)))
    (dolist (name '("mayReadItems" "mayAddItems" "mayRemoveItems"
                    "maySetSeen" "maySetKeywords" "mayCreateChild"
                    "mayRename" "mayDelete" "maySubmit"))
      (puthash name t rights))
    (puthash "id" (or id "inbox") mailbox)
    (puthash "name" "Inbox" mailbox)
    (puthash "parentId" (or parent :json-null) mailbox)
    (puthash "role" "inbox" mailbox)
    (puthash "sortOrder" 10 mailbox)
    (puthash "totalEmails" 4 mailbox)
    (puthash "unreadEmails" 1 mailbox)
    (puthash "totalThreads" 3 mailbox)
    (puthash "unreadThreads" 1 mailbox)
    (puthash "myRights" rights mailbox)
    (puthash "isSubscribed" :json-false mailbox)
    mailbox))

(defun chidu-store-test--mailbox-response-bytes (mailboxes)
  "Return encoded Mailbox/get response containing MAILBOXES vector."
  (chidu-store-test--payload
   `(:sessionState "session-2"
     :methodResponses
     [["Mailbox/get"
       (:accountId "remote-account"
        :state "mailbox-state"
        :list ,mailboxes
        :notFound [])
       "mailbox-get"]])))

(ert-deftest chidu-jmap-mailbox-validates-complete-snapshot ()
  (let* ((observation
          (chidu-jmap--validate-mailboxes
           (chidu-store-test--mailbox-response-bytes
            (vector (chidu-store-test--mailbox-wire)))
           "remote-account"))
         (mailbox
          (aref (chidu-store-mailbox-snapshot-observation-mailboxes
                 observation)
                0)))
    (should (equal "mailbox-state"
                   (chidu-store-mailbox-snapshot-observation-state
                    observation)))
    (should (equal "inbox"
                   (chidu-store-mailbox-observation-remote-mailbox-id
                    mailbox)))
    (should (= 4 (chidu-store-mailbox-observation-total-emails mailbox)))
    (should-not
     (chidu-store-mailbox-observation-subscribed-p mailbox))
    (should
     (chidu-store-mailbox-rights-may-submit-p
      (chidu-store-mailbox-observation-rights mailbox)))))

(ert-deftest chidu-jmap-mailbox-allows-more-unread-threads-than-unread-emails ()
  (let ((mailbox (chidu-store-test--mailbox-wire)))
    (puthash "unreadEmails" 0 mailbox)
    (puthash "unreadThreads" 1 mailbox)
    (let* ((snapshot
            (chidu-jmap--validate-mailboxes
             (chidu-store-test--mailbox-response-bytes (vector mailbox))
             "remote-account"))
           (decoded
            (aref
             (chidu-store-mailbox-snapshot-observation-mailboxes snapshot)
             0)))
      (should (= 0 (chidu-store-mailbox-observation-unread-emails decoded)))
      (should (= 1 (chidu-store-mailbox-observation-unread-threads decoded))))))

(ert-deftest chidu-jmap-mailbox-rejects-duplicates-and-invalid-counts ()
  (should-error
   (chidu-jmap--validate-mailboxes
    (chidu-store-test--mailbox-response-bytes
     (vector (chidu-store-test--mailbox-wire "same")
             (chidu-store-test--mailbox-wire "same")))
    "remote-account")
   :type 'chidu-jmap-error)
  (let ((mailbox (chidu-store-test--mailbox-wire)))
    (puthash "totalEmails" -1 mailbox)
    (should-error
     (chidu-jmap--validate-mailboxes
      (chidu-store-test--mailbox-response-bytes (vector mailbox))
      "remote-account")
     :type 'chidu-jmap-error)))

(ert-deftest chidu-jmap-mailbox-enforces-rfc-snapshot-invariants ()
  (cl-labels
      ((wire (id name parent role)
         (let ((mailbox (chidu-store-test--mailbox-wire id parent)))
           (puthash "name" name mailbox)
           (puthash "role" (or role :json-null) mailbox)
           mailbox))
       (reject (mailboxes)
         (should-error
          (chidu-jmap--validate-mailboxes
           (chidu-store-test--mailbox-response-bytes mailboxes)
           "remote-account")
          :type 'chidu-jmap-error)))
    (let ((empty-name (wire "empty" "" nil nil)))
      (reject (vector empty-name)))
    (let ((too-large (wire "large" "Large" nil nil)))
      (puthash "sortOrder" (expt 2 31) too-large)
      (reject (vector too-large)))
    (let ((bad-count (wire "count" "Count" nil nil)))
      (puthash "totalEmails" 1 bad-count)
      (puthash "unreadEmails" 2 bad-count)
      (reject (vector bad-count)))
    (reject
     (vector (wire "one" "One" nil "inbox")
             (wire "two" "Two" nil "inbox")))
    (reject
     (vector (wire "one" "Same" nil nil)
             (wire "two" "Same" nil nil)))
    (reject (vector (wire "child" "Child" "missing" nil)))
    (reject
     (vector (wire "one" "One" "two" nil)
             (wire "two" "Two" "one" nil)))
    (let ((snapshot
           (chidu-jmap--validate-mailboxes
            (chidu-store-test--mailbox-response-bytes
             (vector (wire "root" "Root" nil "inbox")
                     (wire "child" "Child" "root" nil)))
            "remote-account")))
      (should
       (= 2
          (length
           (chidu-store-mailbox-snapshot-observation-mailboxes snapshot)))))))

(defun chidu-store-test--email-value (store operation)
  "Return successful Email STORE OPERATION value."
  (let ((result (chidu-store-test--store-call store operation)))
    (should (chidu-result-ok-p result))
    (chidu-result-ok-value result)))

(defun chidu-store-test--email-context (store account-id)
  "Return ACCOUNT-ID Email context from STORE."
  (chidu-store-test--email-value
   store
   (chidu-store-op-get-email-sync-context-create :account-id account-id)))

(defun chidu-store-test--email-chunk (context query-state ids)
  "Build the next query-prefix operation from CONTEXT, QUERY-STATE, and IDS."
  (chidu-store-op-append-email-query-chunk-create
   :account-id
   (chidu-store-account-account-id
    (chidu-store-email-sync-context-account context))
   :generation-id (chidu-store-email-sync-context-generation-id context)
   :expected-revision (chidu-store-email-sync-context-revision context)
   :observation
   (chidu-store-email-query-page-observation-create
    :query-state query-state
    :can-calculate-changes-p t
    :position (chidu-store-email-sync-context-committed-count context)
    :remote-email-ids ids)))

(defun chidu-store-test--exercise-email-query-baseline (store)
  "Exercise durable Email query baseline transitions in STORE."
  (let* ((account-id (chidu-store-test--prepare-mailbox-account store))
         (context (chidu-store-test--email-context store account-id)))
    (should (eq 'uninitialized
                (chidu-store-email-sync-context-phase context)))
    (setq context
          (chidu-store-test--email-value
           store
           (chidu-store-op-begin-email-bootstrap-create
            :account-id account-id :expected-revision 0
            :state "email-0" :profile-version "metadata-v1")))
    (let ((first-generation
           (chidu-store-email-sync-context-generation-id context)))
      (setq context
            (chidu-store-test--email-value
             store
             (chidu-store-test--email-chunk
              context "query-1" (vector "email-1" "email-2"))))
      (should (= 2
                 (chidu-store-email-sync-context-committed-count context)))
      (let ((drift
             (chidu-store-test--store-call
              store
              (chidu-store-test--email-chunk
               context "query-2" (vector "email-3")))))
        (should (eq 'query-state-changed
                    (chidu-result-failure-kind drift)))
        (should (= 2 (chidu-store-email-sync-context-revision context))))
      (setq context
            (chidu-store-test--email-value
             store
             (chidu-store-op-restart-email-bootstrap-create
              :account-id account-id
              :generation-id first-generation
              :expected-revision
              (chidu-store-email-sync-context-revision context)
              :state "email-1" :profile-version "metadata-v1")))
      (should-not
       (equal first-generation
              (chidu-store-email-sync-context-generation-id context))))
    (setq context
          (chidu-store-test--email-value
           store
           (chidu-store-test--email-chunk
            context "query-2" (vector "email-1" "email-3")))
          context
          (chidu-store-test--email-value
           store
           (chidu-store-test--email-chunk context "query-2" (vector))))
    (should (eq 'membership-catchup
                (chidu-store-email-sync-context-phase context)))
    (should (= 2 (chidu-store-email-sync-context-committed-count context)))
    (should (equal "email-3"
                   (chidu-store-email-sync-context-anchor-remote-email-id
                    context)))
    (should (= 5 (chidu-store-email-sync-context-revision context)))
    `(:account-id ,account-id
      :generation-id
      ,(chidu-store-email-sync-context-generation-id context))))

(ert-deftest chidu-store-email-query-baseline-survives-reopen ()
  (when (sqlite-available-p)
    (let ((root (make-temp-file "chidu-sqlite-email-" t)) store baseline)
      (set-file-modes root #o700)
      (unwind-protect
          (progn
            (setq store (chidu-store-sqlite-create root)
                  baseline
                  (chidu-store-test--exercise-email-query-baseline store))
            (chidu-store-close store)
            (setq store (chidu-store-sqlite-create root))
            (let ((context
                   (chidu-store-test--email-context
                    store (plist-get baseline :account-id))))
              (should
               (equal (plist-get baseline :generation-id)
                      (chidu-store-email-sync-context-generation-id context)))
              (should (eq 'membership-catchup
                          (chidu-store-email-sync-context-phase context)))
              (should (= 5 (chidu-store-email-sync-context-revision context)))))
        (when store (chidu-store-close store))
        (when (file-directory-p root) (delete-directory root t))))))

(defun chidu-store-test--email-generation-remote-ids
    (store account-id generation-id)
  "Return GENERATION-ID remote Email ids from SQLite test STORE."
  (let* ((root (plist-get (chidu-store-inspect store) :root))
         (database (sqlite-open (expand-file-name "store.sqlite3" root))))
    (unwind-protect
        (vconcat
         (mapcar
          #'car
          (sqlite-select
           database
           "SELECT email.remote_email_id
              FROM jmap_email_generation_member AS member
              JOIN jmap_email_record AS email
                ON email.account_id = member.account_id
               AND email.local_email_id = member.local_email_id
             WHERE member.account_id = ? AND member.generation_id = ?
             ORDER BY member.ordinal"
           (list account-id generation-id))))
      (sqlite-close database))))

(ert-deftest chidu-store-email-membership-changes-are-cas-and-durable ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-sqlite-email-changes-" t)) store setup)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root)
                setup (chidu-store-test--exercise-email-query-baseline store))
          (let* ((account-id (plist-get setup :account-id))
                 (generation-id (plist-get setup :generation-id))
                 (first-observation
                  (chidu-store-email-changes-observation-create
                   :old-state "email-1" :new-state "email-2"
                   :has-more-changes-p t
                   :created (vector "email-4")
                   :updated (vector "email-3")
                   :destroyed (vector "email-1")))
                 (first
                  (chidu-store-test--email-value
                   store
                   (chidu-store-op-apply-email-membership-changes-create
                    :account-id account-id :generation-id generation-id
                    :expected-revision 5 :expected-state "email-1"
                    :observation first-observation))))
            (should (eq 'membership-catchup
                        (chidu-store-email-sync-context-phase first)))
            (should (equal "email-2"
                           (chidu-store-email-sync-context-state first)))
            (should (= 6 (chidu-store-email-sync-context-revision first)))
            (should
             (equal (vector "email-3" "email-4")
                    (chidu-store-test--email-generation-remote-ids
                     store account-id generation-id)))
            (let ((stale
                   (chidu-store-test--store-call
                    store
                    (chidu-store-op-apply-email-membership-changes-create
                     :account-id account-id :generation-id generation-id
                     :expected-revision 5 :expected-state "email-1"
                     :observation first-observation))))
              (should (chidu-result-failure-p stale))
              (should (eq 'revision-conflict
                          (chidu-result-failure-kind stale))))
            (let ((second
                   (chidu-store-test--email-value
                    store
                    (chidu-store-op-apply-email-membership-changes-create
                     :account-id account-id :generation-id generation-id
                     :expected-revision 6 :expected-state "email-2"
                     :observation
                     (chidu-store-email-changes-observation-create
                      :old-state "email-2" :new-state "email-3"
                      :has-more-changes-p nil
                      :created (vector "email-5")
                      :destroyed (vector "email-4"))))))
              (should (eq 'hydrating
                          (chidu-store-email-sync-context-phase second)))
              (should (equal "email-3"
                             (chidu-store-email-sync-context-state second)))
              (should (= 7
                         (chidu-store-email-sync-context-revision second))))
            (should
             (equal (vector "email-3" "email-5")
                    (chidu-store-test--email-generation-remote-ids
                     store account-id generation-id)))
            (chidu-store-close store)
            (setq store (chidu-store-sqlite-create root))
            (let ((reopened
                   (chidu-store-test--email-context store account-id)))
              (should (eq 'hydrating
                          (chidu-store-email-sync-context-phase reopened)))
              (should (equal "email-3"
                             (chidu-store-email-sync-context-state reopened)))
              (should (= 7
                         (chidu-store-email-sync-context-revision reopened)))
              (should
               (equal (vector "email-3" "email-5")
                      (chidu-store-test--email-generation-remote-ids
                       store account-id generation-id))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(defun chidu-store-test--exercise-canonical-mailbox (store)
  "Exercise canonical Summary, body, and Conversation state in STORE."
  (let* ((account-id (chidu-store-test--prepare-mailbox-account store))
         (mailbox-context
          (chidu-store-test--value
           store
           (chidu-store-op-observe-mailbox-snapshot-create
            :account-id account-id :expected-revision 0
            :observation
            (chidu-store-mailbox-snapshot-observation-create
             :state "mailbox-summary"
             :mailboxes
             (vector
              (chidu-store-test--mailbox-observation
               "inbox" "Inbox" :role "inbox"))))))
         (mailbox
          (aref (chidu-store-mailbox-sync-context-mailboxes mailbox-context) 0))
         (mailbox-id (chidu-store-mailbox-mailbox-id mailbox))
         (_generation
          (chidu-store-test--activate-email-generation
           store account-id
           (vector
            (chidu-store-test--email-entry
             "email-1" "2026-08-25T03:00:00Z"
             :thread-id "thread-1"
             :subject "Subject" :preview "Preview"
             :has-attachment-p t)
            (chidu-store-test--email-entry
             "email-2" "2026-08-24T03:00:00Z"
             :thread-id "thread-1"
             :from-name "Bob" :from-email "bob@example.test"
             :subject "Older" :preview "Older preview"
             :keywords (vector "$seen" "$flagged"))
            (chidu-store-test--email-entry
             "email-3" "2026-08-23T03:00:00Z"
             :thread-id "thread-1"
             :subject "Oldest" :preview "Oldest preview"))))
         (summary
          (chidu-store-test--canonical-summary
           store account-id mailbox 2))
         (local-id
          (chidu-store-email-summary-row-local-email-id
           (aref (chidu-store-mailbox-summary-context-rows summary) 0)))
         (body-initial
          (chidu-store-test--value
           store
           (chidu-store-op-get-email-body-create
            :account-id account-id :local-email-id local-id
            :remote-email-id "email-1")))
         (body-context
          (chidu-store-test--value
           store
           (chidu-store-op-replace-email-body-create
            :account-id account-id :local-email-id local-id
            :remote-email-id "email-1" :expected-revision 0
            :observation
            (chidu-store-email-body-observation-create
             :remote-email-id "email-1"
             :email-state "body/state:stored"
             :text-content "Full body"
             :html-content "<p>Full body</p>"
             :truncated-p nil :encoding-problem-p nil
             :attachments
             (vector
              (chidu-store-email-attachment-create
               :part-id "part-1" :blob-id "blob-1" :size 12
               :name "notes.txt" :media-type "text/plain"
               :charset "utf-8" :disposition "attachment"
               :language (vector "en")
               :location "notes.txt"))))))
         (conversation-initial
          (chidu-store-test--value
           store
           (chidu-store-op-get-conversation-create
            :account-id account-id :remote-thread-id "thread-1")))
         (conversation
          (chidu-store-test--value
           store
           (chidu-store-op-replace-conversation-create
            :account-id account-id :remote-thread-id "thread-1"
            :expected-revision 0
            :observation
            (chidu-store-conversation-observation-create
             :remote-thread-id "thread-1"
             :thread-state "thread/state:stored"
             :email-state "email/state:stored"
             :complete-p t
             :rows
             (vector
              (chidu-store-test--conversation-observation-row
               "email-1" '("m1") nil nil)
              (chidu-store-test--conversation-observation-row
               "email-2" '("m2") '("m1") '("m1"))
              (chidu-store-test--conversation-observation-row
               "email-3" '("m3") '("missing") '("m1" "m2"))))))))
    (should
     (equal '("email-1" "email-2")
            (cl-loop
             for row across
             (chidu-store-mailbox-summary-context-rows summary)
             collect (chidu-store-email-summary-row-remote-email-id row))))
    (should (chidu-store-mailbox-summary-context-maybe-more-p summary))
    (should-not (chidu-store-email-body-context-body body-initial))
    (should (= 1 (chidu-store-email-body-context-revision body-context)))
    (should
     (equal "Full body"
            (chidu-store-email-body-text-content
             (chidu-store-email-body-context-body body-context))))
    (should (= 0
               (chidu-store-conversation-context-revision
                conversation-initial)))
    (should (= 1 (chidu-store-conversation-context-revision conversation)))
    (let ((rows (chidu-store-conversation-context-rows conversation)))
      (should (= 3 (length rows)))
      (should
       (equal local-id
              (chidu-store-email-summary-row-local-email-id
               (chidu-store-conversation-row-summary-row (aref rows 0)))))
      (should
       (equal '(0 1 2)
              (cl-loop for item across rows
                       collect (chidu-store-conversation-row-depth item)))))
    (list :account-id account-id :mailbox-id mailbox-id :mailbox mailbox
          :local-id local-id :remote-thread-id "thread-1")))

(ert-deftest chidu-store-canonical-summary-has-no-projection-fallback ()
  (skip-unless (sqlite-available-p))
  (let ((store (chidu-test-store-create)))
    (unwind-protect
        (let* ((account-id
                (chidu-store-test--prepare-mailbox-account store))
               (mailbox-context
                (chidu-store-test--value
                 store
                 (chidu-store-op-observe-mailbox-snapshot-create
                  :account-id account-id :expected-revision 0
                  :observation
                  (chidu-store-mailbox-snapshot-observation-create
                   :state "mailbox/no-index"
                   :mailboxes
                   (vector
                    (chidu-store-test--mailbox-observation
                     "inbox" "Inbox" :role "inbox"))))))
               (mailbox
                (aref
                 (chidu-store-mailbox-sync-context-mailboxes
                  mailbox-context)
                 0))
               (result
                (chidu-store-test--store-call
                 store
                 (chidu-store-op-get-mailbox-summary-create
                  :account-id account-id
                  :mailbox-id (chidu-store-mailbox-mailbox-id mailbox)
                  :limit 50))))
          (should (chidu-result-failure-p result))
          (should
           (eq 'email-index-unavailable
               (chidu-result-failure-kind result))))
      (chidu-store-close store))))

(ert-deftest chidu-store-canonical-summary-is-bounded-and-survives-reopen ()
  (skip-unless (sqlite-available-p))
  (let ((root (make-temp-file "chidu-sqlite-summary-" t)) store baseline)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root)
                baseline
                (chidu-store-test--exercise-canonical-mailbox store))
          (chidu-store-close store)
          (setq store (chidu-store-sqlite-create root))
          (let* ((account-id (plist-get baseline :account-id))
                 (mailbox (plist-get baseline :mailbox))
                 (context
                  (chidu-store-test--canonical-summary
                   store account-id mailbox 2))
                 (extended
                  (chidu-store-test--canonical-summary
                   store account-id mailbox 3))
                 (body-context
                  (chidu-store-test--value
                   store
                   (chidu-store-op-get-email-body-create
                    :account-id account-id
                    :local-email-id (plist-get baseline :local-id)
                    :remote-email-id "email-1")))
                 (conversation
                  (chidu-store-test--value
                   store
                   (chidu-store-op-get-conversation-create
                    :account-id account-id
                    :remote-thread-id
                    (plist-get baseline :remote-thread-id)))))
            (should (= 2 (length
                          (chidu-store-mailbox-summary-context-rows context))))
            (should (chidu-store-mailbox-summary-context-maybe-more-p context))
            (should (= 3 (length
                          (chidu-store-mailbox-summary-context-rows extended))))
            (should-not
             (chidu-store-mailbox-summary-context-maybe-more-p extended))
            (should
             (equal (plist-get baseline :local-id)
                    (chidu-store-email-summary-row-local-email-id
                     (aref
                      (chidu-store-mailbox-summary-context-rows context)
                      0))))
            (should (= 1 (chidu-store-email-body-context-revision
                          body-context)))
            (should
             (equal "Full body"
                    (chidu-store-email-body-text-content
                     (chidu-store-email-body-context-body body-context))))
            (should (= 1 (chidu-store-conversation-context-revision
                          conversation)))
            (should (= 3 (length
                          (chidu-store-conversation-context-rows
                           conversation))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-store-email-attachments-replace-atomically-and-survive-reopen ()
  "Attachment rows keep server order and nullable fields across a reopen.

A second CAS replace must fully supersede the first list rather than merge
with it, so a shrinking attachment list leaves no orphan rows behind."
  (when (sqlite-available-p)
    (let ((root (make-temp-file "chidu-sqlite-attachment-" t))
          store account-id local-id)
      (set-file-modes root #o700)
      (cl-flet
          ((commit
             (revision attachments)
             (chidu-result-ok-value
              (chidu-store-test--store-call
               store
               (chidu-store-op-replace-email-body-create
                :account-id account-id :local-email-id local-id
                :remote-email-id "email-1" :expected-revision revision
                :observation
                (chidu-store-email-body-observation-create
                 :remote-email-id "email-1"
                 :email-state (format "body/state:%d" revision)
                 :text-content "Full body" :html-content ""
                 :truncated-p nil :encoding-problem-p nil
                 :attachments attachments)))))
           (stored
             ()
             (chidu-store-email-body-attachments
              (chidu-store-email-body-context-body
               (chidu-result-ok-value
                (chidu-store-test--store-call
                 store
                 (chidu-store-op-get-email-body-create
                  :account-id account-id :local-email-id local-id
                  :remote-email-id "email-1")))))))
        (unwind-protect
            (progn
              (setq store (chidu-store-sqlite-create root))
              (let ((baseline
                     (chidu-store-test--exercise-canonical-mailbox store)))
                (setq account-id (plist-get baseline :account-id)
                      local-id (plist-get baseline :local-id)))
              ;; `chidu-store-test--exercise-canonical-mailbox' already left one
              ;; attachment at revision 1; replace it with a longer list whose
              ;; second part carries only the RFC 8621 required fields.
              (commit 1
                      (vector
                       (chidu-store-email-attachment-create
                        :part-id "part-a" :blob-id "blob-a" :size 3
                        :name "first.txt" :media-type "text/plain"
                        :charset "utf-8" :disposition "inline"
                        :cid "first@example.test" :language (vector "en")
                        :location "first.txt")
                       (chidu-store-email-attachment-create
                        :part-id "part-b" :blob-id "blob-b" :size 7
                        :media-type "application/octet-stream"
                        :language (vector))))
              (chidu-store-close store)
              (setq store (chidu-store-sqlite-create root))
              (let ((attachments (stored)))
                (should (= 2 (length attachments)))
                ;; Server order is the stored order, not blobId or name order.
                (should
                 (equal '("part-a" "part-b")
                        (cl-loop for item across attachments
                                 collect
                                 (chidu-store-email-attachment-part-id item))))
                (let ((second (aref attachments 1)))
                  ;; Nullable columns round-trip as nil, not as "".
                  (should-not (chidu-store-email-attachment-name second))
                  (should-not (chidu-store-email-attachment-charset second))
                  (should-not
                   (chidu-store-email-attachment-disposition second))
                  (should-not (chidu-store-email-attachment-cid second))
                  (should-not (chidu-store-email-attachment-location second))
                  (should (= 7 (chidu-store-email-attachment-size second)))
                  (should
                   (equal (vector)
                          (chidu-store-email-attachment-language second)))))
              ;; Shrinking the list must delete the superseded row outright.
              (commit 2
                      (vector
                       (chidu-store-email-attachment-create
                        :part-id "part-c" :blob-id "blob-c" :size 1
                        :name "only.txt" :media-type "text/plain"
                        :language (vector))))
              (chidu-store-close store)
              (setq store (chidu-store-sqlite-create root))
              (let ((attachments (stored)))
                (should (= 1 (length attachments)))
                (should
                 (equal "part-c"
                        (chidu-store-email-attachment-part-id
                         (aref attachments 0))))))
          (when store (chidu-store-close store))
          (when (file-directory-p root) (delete-directory root t)))))))

(defun chidu-store-test--exercise-search-projection
    (store observation query-key)
  "Commit OBSERVATION under QUERY-KEY in STORE and return stable ids."
  (let* ((account-id (chidu-store-test--prepare-mailbox-account store))
         (initial
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-get-search-create
             :account-id account-id :query-key query-key))))
         (committed
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-replace-search-create
             :account-id account-id :query-key query-key
             :expected-revision 0 :observation observation))))
         (row (aref (chidu-store-search-context-rows committed) 0))
         (summary (chidu-store-search-row-summary-row row))
         (snippet (chidu-store-search-row-snippet row))
         (conflict
          (chidu-store-test--store-call
           store
           (chidu-store-op-replace-search-create
            :account-id account-id :query-key query-key
            :expected-revision 0 :observation observation)))
         (append-observation
          (chidu-store-search-observation-create
           :query-key query-key
           :query-text (chidu-store-search-observation-query-text observation)
           :filter-json (chidu-store-search-observation-filter-json observation)
           :query-state (chidu-store-search-observation-query-state observation)
           :email-state "email/state:search-2"
           :cursor-remote-email-id "email-2"
           :maybe-more-p nil
           :rows
           (vector
            (chidu-store-search-observation-row-create
             :summary-row
             (chidu-store-email-summary-observation-row-create
              :remote-email-id "email-2"
              :remote-thread-id "thread-2"
              :received-at "2026-08-24T01:02:03Z"
              :from-name "Bob" :from-email "bob@example.test"
              :subject "Older result" :preview "Older preview"
              :unread-p nil :flagged-p t :has-attachment-p nil)
             :remote-mailbox-ids (vector "inbox")
             :snippet
             (chidu-store-search-snippet-create
              :subject "<mark>Older</mark> result"
              :preview "older matching text")))))
         (drift
          (chidu-store-test--store-call
           store
           (chidu-store-op-append-search-create
            :account-id account-id :query-key query-key
            :expected-revision 1
            :expected-query-state "query/state:other"
            :expected-cursor-remote-email-id "email-1"
            :observation append-observation)))
         (overlap-observation
          (chidu-store-search-observation-with
           append-observation
           :cursor-remote-email-id "email-1"
           :rows
           (vector
            (chidu-store-search-observation-row-create
             :summary-row
             (chidu-store-email-summary-observation-row-create
              :remote-email-id "email-1"
              :remote-thread-id "thread-1"
              :received-at "2026-08-25T01:02:03Z"
              :subject "Duplicate" :preview ""
              :unread-p t :flagged-p nil :has-attachment-p nil)
             :remote-mailbox-ids (vector "inbox")
             :snippet nil))))
         (overlap
          (chidu-store-test--store-call
           store
           (chidu-store-op-append-search-create
            :account-id account-id :query-key query-key
            :expected-revision 1
            :expected-query-state
            (chidu-store-search-observation-query-state observation)
            :expected-cursor-remote-email-id "email-1"
            :observation overlap-observation)))
         (appended
          (chidu-result-ok-value
           (chidu-store-test--store-call
            store
            (chidu-store-op-append-search-create
             :account-id account-id :query-key query-key
             :expected-revision 1
             :expected-query-state
             (chidu-store-search-observation-query-state observation)
             :expected-cursor-remote-email-id "email-1"
             :observation append-observation)))))
    (should (= 0 (chidu-store-search-context-revision initial)))
    (should (= 1 (chidu-store-search-context-revision committed)))
    (should (chidu-store-search-context-maybe-more-p committed))
    (should (equal "email-1"
                   (chidu-store-search-context-cursor-remote-email-id
                    committed)))
    (should (chidu-result-failure-p drift))
    (should (eq 'query-state-changed (chidu-result-failure-kind drift)))
    (should (chidu-result-failure-p overlap))
    (should (eq 'query-page-overlap (chidu-result-failure-kind overlap)))
    (should (= 2 (chidu-store-search-context-revision appended)))
    (should-not (chidu-store-search-context-maybe-more-p appended))
    (should (equal "email-2"
                   (chidu-store-search-context-cursor-remote-email-id
                    appended)))
    (should
     (equal '("email-1" "email-2")
            (cl-loop
             for item across (chidu-store-search-context-rows appended)
             collect
             (chidu-store-email-summary-row-remote-email-id
              (chidu-store-search-row-summary-row item)))))
    (should (equal "from:alice is:unread emoji width"
                   (chidu-store-search-context-query-text committed)))
    (should (equal (vector "inbox")
                   (chidu-store-search-row-remote-mailbox-ids row)))
    (should (equal "<mark>Emoji</mark> width"
                   (chidu-store-search-snippet-subject snippet)))
    (should (equal "email-1"
                   (chidu-store-email-summary-row-remote-email-id summary)))
    (should (chidu-store-email-summary-row-unread-p summary))
    (should (chidu-result-failure-p conflict))
    (should (eq 'revision-conflict
                (chidu-result-failure-kind conflict)))
    `(:account-id ,account-id
      :query-key ,query-key
      :local-email-id
      ,(chidu-store-email-summary-row-local-email-id summary))))

(ert-deftest chidu-search-query-jmap-and-store-form-one-closed-contract ()
  (let* ((mailbox
          (chidu-store-mailbox-create
           :mailbox-id "mailbox-local" :remote-mailbox-id "inbox"
           :name "Inbox" :role "inbox" :sort-order 10
           :total-emails 1 :unread-emails 1
           :total-threads 1 :unread-threads 1
           :rights (chidu-store-test--mailbox-rights)
           :subscribed-p t :available-p t))
         (spec
          (chidu-search-query-compile
           "from:alice is:unread emoji width" (vector mailbox) mailbox))
         (request
           (chidu-jmap-search--request "remote-account" spec 1))
         (next-request
          (chidu-jmap-search--request
           "remote-account" spec 2 "email-1"))
         (calls (plist-get request :methodCalls))
         (next-query-args
          (aref (aref (plist-get next-request :methodCalls) 0) 1))
         (query-args (aref (aref calls 0) 1))
         (get-args (aref (aref calls 1) 1))
         (snippet-args (aref (aref calls 2) 1))
         (mailboxes (make-hash-table :test #'equal))
         (keywords (make-hash-table :test #'equal)))
    (puthash "inbox" t mailboxes)
    (let* ((filter (chidu-search-spec-filter spec))
           (conditions (plist-get filter :conditions))
           (reference (plist-get get-args (intern ":#ids")))
           (bytes
            (chidu-store-test--payload
             `(:sessionState "session"
               :methodResponses
               [["Email/query"
                 (:accountId "remote-account"
                  :queryState "search/query:1"
                  :canCalculateChanges t
                  :position 0 :ids ["email-1"])
                 "search-query"]
                ["Email/get"
                 (:accountId "remote-account" :state "email/state:search"
                  :list
                  [(:id "email-1" :threadId "thread-1"
                    :mailboxIds ,mailboxes :keywords ,keywords
                    :receivedAt "2026-08-25T01:02:03Z"
                    :from [(:name "Alice" :email "alice@example.test")]
                    :subject "Emoji width" :preview "ordinary preview"
                    :hasAttachment :json-false)]
                  :notFound [])
                 "search-email"]
                ["SearchSnippet/get"
                 (:accountId "remote-account"
                  :list
                  [(:emailId "email-1"
                    :subject "<mark>Emoji</mark> width"
                    :preview "... <mark>emoji</mark> width ...")]
                  :notFound :json-null)
                 "search-snippet"]])))
           (observation
            (chidu-jmap-search--decode bytes "remote-account" spec 1)))
      (should (equal "AND" (plist-get filter :operator)))
      (should (= 4 (length conditions)))
      (should (= 1 (plist-get query-args :limit)))
      (should (= 0 (plist-get query-args :position)))
      (should (equal "email-1" (plist-get next-query-args :anchor)))
      (should (= 1 (plist-get next-query-args :anchorOffset)))
      (should-not (plist-member next-query-args :position))
      (should
       (equal
        '(:resultOf "search-query" :name "Email/query" :path "/ids")
        reference))
      (should (equal reference
                     (plist-get snippet-args (intern ":#emailIds"))))
      (should (equal (chidu-search-spec-filter spec)
                     (plist-get snippet-args :filter)))
      (should (= 1 (length
                    (chidu-store-search-observation-rows observation))))
      (should (equal "email-1"
                     (chidu-store-search-observation-cursor-remote-email-id
                      observation)))
      (should (chidu-store-search-observation-maybe-more-p observation))
      (when (sqlite-available-p)
        (let ((root (make-temp-file "chidu-sqlite-search-" t))
              store baseline)
          (set-file-modes root #o700)
          (unwind-protect
              (progn
                (setq store (chidu-store-sqlite-create root)
                      baseline
                      (chidu-store-test--exercise-search-projection
                       store observation (chidu-search-spec-query-key spec)))
                (chidu-store-close store)
                (setq store (chidu-store-sqlite-create root))
                (let* ((context
                        (chidu-result-ok-value
                         (chidu-store-test--store-call
                          store
                          (chidu-store-op-get-search-create
                           :account-id (plist-get baseline :account-id)
                           :query-key (plist-get baseline :query-key)))))
                       (row (aref (chidu-store-search-context-rows context) 0))
                       (summary (chidu-store-search-row-summary-row row)))
                  (should (= 2 (chidu-store-search-context-revision context)))
                  (should
                   (equal (plist-get baseline :local-email-id)
                          (chidu-store-email-summary-row-local-email-id
                           summary)))
                  (should (= 2 (length (chidu-store-search-context-rows context))))
                  (should (equal "email-2"
                                 (chidu-store-search-context-cursor-remote-email-id
                                  context)))))
            (when store (chidu-store-close store))
            (when (file-directory-p root) (delete-directory root t))))))))

(ert-deftest chidu-explicit-seen-is-exact-optimistic-and-durable ()
  (let* ((root (make-temp-file "chidu-sqlite-seen-" t))
         store
         baseline)
    (set-file-modes root #o700)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root)
                baseline (chidu-store-test--exercise-canonical-mailbox store))
          (let* ((account-id (plist-get baseline :account-id))
                 (mailbox-id (plist-get baseline :mailbox-id))
                 (local-id (plist-get baseline :local-id))
                 (query-key "seen-search")
                 (search-observation
                  (chidu-store-search-observation-create
                   :query-key query-key
                   :query-text "seen"
                   :filter-json "{\"text\":\"seen\"}"
                   :query-state "query/seen"
                   :email-state "email/seen"
                   :maybe-more-p nil
                   :rows
                   (vector
                    (chidu-store-search-observation-row-create
                     :summary-row
                     (chidu-store-email-summary-observation-row-create
                      :remote-email-id "email-1"
                      :remote-thread-id "thread-1"
                      :received-at "2026-08-25T01:02:03Z"
                      :from-name "Alice" :from-email "alice@example.test"
                      :subject "Subject" :preview "Preview"
                      :unread-p t :flagged-p nil :has-attachment-p nil)
                     :remote-mailbox-ids (vector "inbox")
                     :snippet nil))))
                 (_search
                  (chidu-result-ok-value
                   (chidu-store-test--store-call
                    store
                    (chidu-store-op-replace-search-create
                     :account-id account-id :query-key query-key
                     :expected-revision 0 :observation search-observation))))
                 (read-operation (chidu-store-new-local-id))
                 (unread-operation (chidu-store-new-local-id)))
            (cl-labels
                ((summary-unread-p
                   ()
                   (chidu-store-email-summary-row-unread-p
                    (aref
                     (chidu-store-mailbox-summary-context-rows
                      (chidu-result-ok-value
                       (chidu-store-test--store-call
                        store
                        (chidu-store-op-get-mailbox-summary-create
                         :account-id account-id :mailbox-id mailbox-id
                         :limit 50))))
                     0)))
                 (search-unread-p
                   ()
                   (chidu-store-email-summary-row-unread-p
                    (chidu-store-search-row-summary-row
                     (aref
                      (chidu-store-search-context-rows
                       (chidu-result-ok-value
                        (chidu-store-test--store-call
                         store
                         (chidu-store-op-get-search-create
                          :account-id account-id :query-key query-key))))
                      0))))
                 (conversation-unread-p
                   ()
                   (let* ((context
                           (chidu-result-ok-value
                            (chidu-store-test--store-call
                             store
                             (chidu-store-op-get-conversation-create
                              :account-id account-id
                              :remote-thread-id "thread-1"))))
                          (row
                           (cl-find
                            local-id
                            (chidu-store-conversation-context-rows context)
                            :key
                            (lambda (item)
                              (chidu-store-email-summary-row-local-email-id
                               (chidu-store-conversation-row-summary-row item)))
                            :test #'equal)))
                     (chidu-store-email-summary-row-unread-p
                      (chidu-store-conversation-row-summary-row row))))
                 (intents
                   ()
                   (chidu-store-seen-context-intents
                    (chidu-result-ok-value
                     (chidu-store-test--store-call
                      store
                      (chidu-store-op-list-seen-intents-create
                       :account-id account-id)))))
                 (accept
                   (operation-id desired-seen-p current-unread-p)
                   (chidu-store-test--store-call
                    store
                    (chidu-store-op-accept-seen-intent-create
                     :account-id account-id
                     :local-email-id local-id
                     :remote-email-id "email-1"
                     :operation-id operation-id
                     :desired-seen-p desired-seen-p
                     :current-unread-p current-unread-p)))
                 (settle
                   (operation-id outcome &optional error-kind)
                   (chidu-store-test--store-call
                    store
                    (chidu-store-op-settle-seen-intent-create
                     :account-id account-id
                     :local-email-id local-id
                     :operation-id operation-id
                     :outcome outcome
                     :error-kind error-kind))))
              ;; The wire request patches only $seen, and response coverage is
              ;; exact for the requested Email.
              (let* ((request
                       (chidu-jmap-seen--request
                        "remote-account" "email-1" t))
                     (arguments (aref (aref (plist-get request :methodCalls) 0) 1))
                     (update (plist-get arguments :update))
                     (patch (gethash "email-1" update))
                     (updated (make-hash-table :test #'equal))
                     (not-updated (make-hash-table :test #'equal)))
                (should (eq t (gethash "keywords/$seen" patch)))
                (should
                 (eq :json-null
                     (gethash
                      "keywords/$seen"
                      (gethash
                       "email-1"
                       (plist-get
                        (aref
                         (aref
                          (plist-get
                           (chidu-jmap-seen--request
                            "remote-account" "email-1" nil)
                           :methodCalls)
                          0)
                         1)
                        :update)))))
                (puthash "email-1" :json-null updated)
                (puthash "email-1"
                         (let ((error (make-hash-table :test #'equal)))
                           (puthash "type" "forbidden" error)
                           error)
                         not-updated)
                (should
                 (eq 'succeeded
                     (chidu-jmap-seen-response-outcome
                      (chidu-jmap-seen--validate-response
                       (chidu-store-test--method-response
                        "Email/set" "seen-set"
                        ;; Deployed Stalwart omits an empty `notUpdated'.
                        `(:accountId "remote-account" :oldState "e0" :newState "e1"
                          :updated ,updated))
                       "remote-account" "email-1"))))
                (should
                 (eq 'rejected
                     (chidu-jmap-seen-response-outcome
                      (chidu-jmap-seen--validate-response
                       (chidu-store-test--method-response
                        "Email/set" "seen-set"
                        ;; It likewise omits an empty `updated' on error.
                        `(:accountId "remote-account" :oldState "e1" :newState "e1"
                          :notUpdated ,not-updated))
                       "remote-account" "email-1")))))

              ;; Accepting a manual mark-read is immediately visible through
              ;; every projection, but the durable base row is not yet changed.
              (let ((change (chidu-result-ok-value
                             (accept read-operation t t))))
                (should (eq 'pending (chidu-store-seen-change-phase change)))
                (should-not (chidu-store-seen-change-unread-p change)))
              (should-not (summary-unread-p))
              (should-not (search-unread-p))
              (should-not (conversation-unread-p))
              (should (= 1 (length (intents))))

              ;; A newer explicit command supersedes the old operation.  A late
              ;; response for the old command is fenced by operation identity.
              (let ((change (chidu-result-ok-value
                             (accept unread-operation nil nil))))
                (should (chidu-store-seen-change-unread-p change)))
              (let ((stale (settle read-operation 'succeeded)))
                (should (chidu-result-failure-p stale))
                (should (eq 'stale-operation
                            (chidu-result-failure-kind stale))))
              (let ((unknown
                     (chidu-result-ok-value
                      (settle unread-operation 'unknown "network-error"))))
                (should (eq 'unknown
                            (chidu-store-seen-change-phase unknown)))
                (should (chidu-store-seen-change-unread-p unknown)))

              ;; Unknown delivery survives restart and remains an optimistic
              ;; overlay until the same idempotent intent is retried.
              (chidu-store-close store)
              (setq store (chidu-store-sqlite-create root))
              (should (= 1 (length (intents))))
              (should (eq 'unknown
                          (chidu-store-seen-intent-phase (aref (intents) 0))))
              (should (summary-unread-p))
              (should
               (eq 'committed
                   (chidu-store-seen-change-phase
                    (chidu-result-ok-value
                     (settle unread-operation 'succeeded)))))
              (should (= 0 (length (intents))))

              ;; Rejection rolls the optimistic state back; success makes it
              ;; the durable projection state after the intent is removed.
              (let ((operation (chidu-store-new-local-id)))
                (accept operation t t)
                (should-not (summary-unread-p))
                (let ((reverted
                       (chidu-result-ok-value
                        (settle operation 'rejected "forbidden"))))
                  (should (eq 'reverted
                              (chidu-store-seen-change-phase reverted)))
                  (should (chidu-store-seen-change-unread-p reverted))))
              (let ((operation (chidu-store-new-local-id)))
                (accept operation t t)
                (settle operation 'succeeded))
              (should-not (summary-unread-p))
              (should-not (search-unread-p))
              (should-not (conversation-unread-p))
              (should (= 0 (length (intents)))))))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-event-source-template-and-state-parser-are-bounded ()
  (let ((store (chidu-test-store-create)))
    (unwind-protect
        (let* ((account-id (chidu-store-test--prepare-mailbox-account store))
               (context
                (chidu-result-ok-value
                 (chidu-store-test--store-call
                  store
                  (chidu-store-op-get-email-sync-context-create
                   :account-id account-id))))
               (endpoint (chidu-store-email-sync-context-endpoint context))
               (url (chidu-jmap-event-source-url endpoint))
               (bytes
                (encode-coding-string
                 (concat
                  ": keepalive\n"
                  "event: ping\n"
                  "data: {}\n\n"
                  "event: state\n"
                  "data: {\"@type\":\"StateChange\",\"changed\":{"
                  "\"remote-account\":{\"Email\":\"e2\","
                  "\"EmailDelivery\":\"d2\"}}}\n\n")
                 'utf-8-unix t))
               (wakes (chidu-jmap-event-source-parse bytes))
               (wake (aref wakes 0)))
          (should (string-match-p "types=Email%2CEmailDelivery" url))
          (should (= 1 (length wakes)))
          (should (equal "remote-account"
                         (chidu-jmap-event-wake-remote-account-id wake)))
          (should (equal (vector "Email" "EmailDelivery")
                         (chidu-jmap-event-wake-types wake))))
      (chidu-store-close store))))

(ert-deftest chidu-jmap-mailbox-move-uses-exact-per-key-set-deltas ()
  (let* ((endpoint
          (chidu-store-endpoint-create
           :endpoint-id "endpoint"
           :session-url "https://mail.example.test/.well-known/jmap"
           :login "me@example.test" :authentication 'basic
           :api-url "https://mail.example.test/jmap/api"
           :max-size-request 1048576
           :max-objects-in-get 8 :max-objects-in-set 2))
         (account
          (chidu-store-account-create
           :account-id (chidu-store-new-local-id)
           :remote-account-id "remote-account" :name "Mail"
           :available-p t :read-only-p nil))
         (source
          (chidu-store-mailbox-create
           :mailbox-id (chidu-store-new-local-id)
           :remote-mailbox-id "inbox" :name "Inbox" :available-p t))
         (destination
          (chidu-store-mailbox-create
           :mailbox-id (chidu-store-new-local-id)
           :remote-mailbox-id "archive" :name "Archive" :available-p t))
         (intents
          (vector
           (chidu-store-mailbox-move-intent-create
            :local-email-id (chidu-store-new-local-id)
            :remote-email-id "email-1" :phase 'pending)
           (chidu-store-mailbox-move-intent-create
            :local-email-id (chidu-store-new-local-id)
            :remote-email-id "email-2" :phase 'unknown)))
         (context
          (chidu-store-mailbox-move-context-create
           :endpoint endpoint :account account
           :operation-id (chidu-store-new-local-id)
           :source-mailbox source :destination-mailbox destination
           :intents intents))
         (request
           (chidu-jmap-mailbox-move--request
            "remote-account" "inbox" "archive" intents))
         (arguments (aref (aref (plist-get request :methodCalls) 0) 1))
         (updates (plist-get arguments :update))
         (updated (make-hash-table :test #'equal))
         (not-updated (make-hash-table :test #'equal)))
    (should (= 2 (hash-table-count updates)))
    (dolist (remote-id '("email-1" "email-2"))
      (let ((patch (gethash remote-id updates)))
        (should (= 2 (hash-table-count patch)))
        (should (eq t (gethash "mailboxIds/archive" patch)))
        (should (eq :json-null (gethash "mailboxIds/inbox" patch)))))
    (should
     (equal "a~0~1b"
            (chidu-jmap-patch-path-component "a~/b" "test component")))
    (puthash "email-1" :json-null updated)
    (puthash
     "email-2"
     (let ((error (make-hash-table :test #'equal)))
       (puthash "type" "forbidden" error)
       error)
     not-updated)
    (let* ((response
            (chidu-jmap-set-validate-update-response
             (chidu-store-test--method-response
              "Email/set" "mailbox-move"
              `(:accountId "remote-account" :oldState "e0" :newState "e1"
                :updated ,updated :notUpdated ,not-updated))
             "Email/set" "mailbox-move" "remote-account"
             (vector "email-1" "email-2")))
           (targets (chidu-jmap-set-update-response-results response)))
      (should
       (equal "e0" (chidu-jmap-set-update-response-old-state response)))
      (should
       (equal "e1" (chidu-jmap-set-update-response-new-state response)))
      (should (eq 'succeeded
                  (chidu-jmap-set-target-result-outcome (aref targets 0))))
      (should (eq 'rejected
                  (chidu-jmap-set-target-result-outcome (aref targets 1))))
      (should
       (equal "forbidden"
              (chidu-jmap-set-target-result-error-kind (aref targets 1)))))
    (let* ((response
            (chidu-jmap-set-validate-update-response
             (chidu-store-test--method-response
              "error" "mailbox-move"
              '(:type "serverPartialFail" :description "uncertain"))
             "Email/set" "mailbox-move" "remote-account"
             (vector "email-1" "email-2")))
           (targets (chidu-jmap-set-update-response-results response)))
      (should (cl-every
               (lambda (target)
                 (eq 'unknown
                     (chidu-jmap-set-target-result-outcome target)))
               (append targets nil))))
    (should-error
     (chidu-jmap-set-validate-update-response
      (chidu-store-test--method-response
       "Email/set" "mailbox-move"
       `(:accountId "remote-account" :oldState "e0" :newState "e1"
         :updated ,updated))
      "Email/set" "mailbox-move" "remote-account"
      (vector "email-1" "email-2"))
     :type 'chidu-jmap-error)))

(provide 'chidu-store-jmap-test)

;;; chidu-store-jmap-test.el ends here
