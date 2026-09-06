;;; chidu-store-sqlite-materialization.el --- SQLite message materialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Selected Email body, attachment, parsed Blob, and Conversation
;; materialization read/write operations.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'sqlite)
(require 'subr-x)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)

(defun chidu-store-sqlite--attachment-from-row (row context)
  "Decode attachment ROW from SQLite for CONTEXT."
  (pcase-let
      ((`(,part-id ,blob-id ,size ,name ,media-type ,charset
          ,disposition ,cid ,language-json ,location)
        row))
    (chidu-store-email-attachment-create
     :part-id part-id
     :blob-id blob-id
     :size size
     :name name
     :media-type media-type
     :charset charset
     :disposition disposition
     :cid cid
     :language
     (chidu-store-sqlite--string-vector-from-json
      language-json (format "%s language" context))
     :location location)))

(defun chidu-store-sqlite--email-attachments
    (database account-id local-email-id)
  "Return ACCOUNT-ID LOCAL-EMAIL-ID attachment descriptors from DATABASE."
  (vconcat
   (mapcar
    (lambda (row)
      (chidu-store-sqlite--attachment-from-row row "Email attachment"))
    (chidu-sql-select database
      [:select
       [part-id blob-id size name media-type charset
                disposition cid language-json location]
       :from jmap-email-attachment
       :where [:and
               [:= account-id [:bind account-id]]
               [:= local-email-id [:bind local-email-id]]]
       :order-by [[ordinal :asc]]]))))

(defun chidu-store-sqlite--email-body-context
    (state account-id local-email-id remote-email-id)
  "Return ACCOUNT-ID body context from SQLite STATE.

LOCAL-EMAIL-ID and REMOTE-EMAIL-ID must name the same Email."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id)))
         (actual-remote-id
          (and location (stringp local-email-id)
               (caar
                (chidu-sql-select database
                  [:select [remote-email-id]
                   :from jmap-email-record
                   :where [:and
                           [:= account-id [:bind account-id]]
                           [:= local-email-id [:bind local-email-id]]]])))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((null actual-remote-id)
      (chidu-result-failure-create
       :kind 'unknown-email
       :data (list :account-id account-id :local-email-id local-email-id)
       :retryable-p nil))
     ((not (equal actual-remote-id remote-email-id))
      (chidu-result-failure-create
       :kind 'email-identity-mismatch
       :data (list :account-id account-id
                   :local-email-id local-email-id
                   :remote-email-id remote-email-id)
       :retryable-p nil))
     (t
      (chidu-result-ok-create
       :value
       (or
        (chidu-sql-one database
            [:select
             [[email-state email-state]
              [text-content text-content]
              [html-content html-content]
              [revision revision]
              [is-truncated is-truncated]
              [encoding-problem encoding-problem]]
             :from jmap-email-body
             :where [:and
                     [:= account-id [:bind account-id]]
                     [:= local-email-id [:bind local-email-id]]]]
          (chidu-store-email-body-context-create
           :endpoint (car location)
           :account (cdr location)
           :local-email-id local-email-id
           :remote-email-id actual-remote-id
           :revision revision
           :body
           (chidu-store-email-body-create
            :email-state email-state
            :text-content text-content
            :html-content html-content
            :truncated-p (chidu-store-sqlite--bool is-truncated)
            :encoding-problem-p
            (chidu-store-sqlite--bool encoding-problem)
            :attachments
            (chidu-store-sqlite--email-attachments
             database account-id local-email-id))))
        (chidu-store-email-body-context-create
         :endpoint (car location)
         :account (cdr location)
         :local-email-id local-email-id
         :remote-email-id actual-remote-id)))))))

(defun chidu-store-sqlite--parsed-attachments
    (database account-id blob-id profile-version)
  "Return parsed BLOB-ID attachments from DATABASE.

ACCOUNT-ID and PROFILE-VERSION select the exact materialization."
  (vconcat
   (mapcar
    (lambda (row)
      (chidu-store-sqlite--attachment-from-row row "Parsed attachment"))
    (chidu-sql-select database
      [:select
       [part-id blob-id size name media-type charset
                disposition cid language-json location]
       :from jmap-parsed-attachment
       :where [:and
               [:= account-id [:bind account-id]]
               [:= source-blob-id [:bind blob-id]]
               [:= profile-version [:bind profile-version]]]
       :order-by [[ordinal :asc]]]))))

(defun chidu-store-sqlite--parsed-blob-context
    (state account-id blob-id profile-version)
  "Return ACCOUNT-ID parsed BLOB-ID context from SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((not (and (stringp blob-id) (not (string-empty-p blob-id))
                (stringp profile-version)
                (not (string-empty-p profile-version))))
      (chidu-result-failure-create
       :kind 'invalid-blob-locator
       :data (list :account-id account-id)
       :retryable-p nil))
     (t
      (chidu-result-ok-create
       :value
       (or
        (chidu-sql-one database
            [:select
             [[revision revision]
              [message-ids-json message-ids-json]
              [in-reply-to-json in-reply-to-json]
              [references-json references-json]
              [sender-json sender-json]
              [from-json from-json]
              [to-json to-json]
              [cc-json cc-json]
              [bcc-json bcc-json]
              [reply-to-json reply-to-json]
              [subject subject]
              [sent-at sent-at]
              [preview preview]
              [text-content text-content]
              [html-content html-content]
              [is-truncated is-truncated]
              [encoding-problem encoding-problem]]
             :from jmap-parsed-blob
             :where [:and
                     [:= account-id [:bind account-id]]
                     [:= blob-id [:bind blob-id]]
                     [:= profile-version [:bind profile-version]]]]
          (chidu-store-parsed-blob-context-create
           :endpoint (car location)
           :account (cdr location)
           :blob-id blob-id
           :profile-version profile-version
           :revision revision
           :message
           (chidu-store-parsed-message-create
            :message-ids
            (chidu-store-sqlite--string-vector-from-json
             message-ids-json "Parsed messageId")
            :in-reply-to
            (chidu-store-sqlite--string-vector-from-json
             in-reply-to-json "Parsed inReplyTo")
            :references
            (chidu-store-sqlite--string-vector-from-json
             references-json "Parsed references")
            :sender
            (chidu-store-sqlite--email-address-vector-from-json
             sender-json "Parsed sender")
            :from
            (chidu-store-sqlite--email-address-vector-from-json
             from-json "Parsed from")
            :to
            (chidu-store-sqlite--email-address-vector-from-json
             to-json "Parsed to")
            :cc
            (chidu-store-sqlite--email-address-vector-from-json
             cc-json "Parsed cc")
            :bcc
            (chidu-store-sqlite--email-address-vector-from-json
             bcc-json "Parsed bcc")
            :reply-to
            (chidu-store-sqlite--email-address-vector-from-json
             reply-to-json "Parsed replyTo")
            :subject subject
            :sent-at sent-at
            :preview preview
            :body
            (chidu-store-email-body-create
             :email-state nil
             :text-content text-content
             :html-content html-content
             :truncated-p (chidu-store-sqlite--bool is-truncated)
             :encoding-problem-p
             (chidu-store-sqlite--bool encoding-problem)
             :attachments
             (chidu-store-sqlite--parsed-attachments
              database account-id blob-id profile-version)))))
        (chidu-store-parsed-blob-context-create
         :endpoint (car location)
         :account (cdr location)
         :blob-id blob-id
         :profile-version profile-version)))))))

(defun chidu-store-sqlite--conversation-rows
    (database account-id remote-thread-id)
  "Return Conversation rows from DATABASE for ACCOUNT-ID and REMOTE-THREAD-ID."
  (vconcat
   (chidu-sql-map database
       [:select
        [[local-id row:local-email-id]
         [remote-id email:remote-email-id]
         [parent-id row:parent-local-email-id]
         [depth row:depth]
         [received-at row:received-at]
         [sent-at row:sent-at]
         [from-name row:from-name]
         [from-email row:from-email]
         [subject row:subject]
         [preview row:preview]
         [unread
          [:call coalesce
                 [:case intent:desired-seen [1 0] [0 1]]
                 row:is-unread]]
         [flagged row:is-flagged]
         [attachment row:has-attachment]
         [message-ids-json row:message-ids-json]
         [in-reply-to-json row:in-reply-to-json]
         [references-json row:references-json]]
        :from [:as jmap-conversation-row row]
        :joins
        [[:inner [:as jmap-email-record email]
          :on [:and
               [:= email:account-id row:account-id]
               [:= email:local-email-id row:local-email-id]]]
         [:left [:as jmap-seen-intent intent]
          :on [:and
               [:= intent:account-id row:account-id]
               [:= intent:local-email-id row:local-email-id]]]]
        :where [:and
                [:= row:account-id [:bind account-id]]
                [:= row:remote-thread-id [:bind remote-thread-id]]]
        :order-by [[row:ordinal :asc]]]
     (chidu-store-conversation-row-create
      :summary-row
      (chidu-store-email-summary-row-create
       :local-email-id local-id
       :remote-email-id remote-id
       :remote-thread-id remote-thread-id
       :received-at received-at
       :from-name from-name
       :from-email from-email
       :subject subject
       :preview preview
       :unread-p (chidu-store-sqlite--bool unread)
       :flagged-p (chidu-store-sqlite--bool flagged)
       :has-attachment-p (chidu-store-sqlite--bool attachment))
      :sent-at sent-at
      :message-ids
      (chidu-store-sqlite--string-vector-from-json
       message-ids-json "Conversation messageId")
      :in-reply-to
      (chidu-store-sqlite--string-vector-from-json
       in-reply-to-json "Conversation inReplyTo")
      :references
      (chidu-store-sqlite--string-vector-from-json
       references-json "Conversation references")
      :parent-local-email-id parent-id
      :depth depth))))

(defun chidu-store-sqlite--conversation-context
    (state account-id remote-thread-id)
  "Return ACCOUNT-ID Conversation context from SQLite STATE.

REMOTE-THREAD-ID selects the projection."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((not (and (stringp remote-thread-id)
                (not (string-empty-p remote-thread-id))))
      (chidu-result-failure-create
       :kind 'unknown-thread
       :data (list :account-id account-id
                   :remote-thread-id remote-thread-id)
       :retryable-p nil))
     (t
      (chidu-result-ok-create
       :value
       (or
        (chidu-sql-one database
            [:select
             [[thread-state thread-state]
              [email-state email-state]
              [revision revision]
              [is-complete is-complete]]
             :from jmap-conversation
             :where [:and
                     [:= account-id [:bind account-id]]
                     [:= remote-thread-id [:bind remote-thread-id]]]]
          (chidu-store-conversation-context-create
           :endpoint (car location)
           :account (cdr location)
           :remote-thread-id remote-thread-id
           :thread-state thread-state
           :email-state email-state
           :revision revision
           :complete-p (chidu-store-sqlite--bool is-complete)
           :rows
           (chidu-store-sqlite--conversation-rows
            database account-id remote-thread-id)))
        (chidu-store-conversation-context-create
         :endpoint (car location)
         :account (cdr location)
         :remote-thread-id remote-thread-id)))))))

(defun chidu-store-sqlite--replace-email-body (state operation)
  "CAS-replace Email body OPERATION in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-replace-email-body-account-id operation))
         (local-email-id
          (chidu-store-op-replace-email-body-local-email-id operation))
         (remote-email-id
          (chidu-store-op-replace-email-body-remote-email-id operation))
         (expected-revision
          (chidu-store-op-replace-email-body-expected-revision operation))
         (observation
          (chidu-store-op-replace-email-body-observation operation))
         (context-result
          (chidu-store-sqlite--email-body-context
           state account-id local-email-id remote-email-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-revision
              (chidu-store-email-body-context-revision context)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id
                       :local-email-id local-email-id
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((not
           (equal remote-email-id
                  (chidu-store-email-body-observation-remote-email-id
                   observation)))
          (chidu-result-failure-create
           :kind 'email-identity-mismatch
           :data (list :account-id account-id
                       :local-email-id local-email-id)
           :retryable-p nil))
         (t
          (with-sqlite-transaction database
            (let ((change-seq
                   (chidu-store-sqlite--increment-change-seq database)))
              (chidu-sql-execute database
                [:insert :into jmap-email-body
                 :row
                 [[account-id [:bind account-id]]
                  [local-email-id [:bind local-email-id]]
                  [email-state
                   [:bind
                    (chidu-store-email-body-observation-email-state observation)]
                   :update]
                  [text-content
                   [:bind
                    (chidu-store-email-body-observation-text-content observation)]
                   :update]
                  [html-content
                   [:bind
                    (chidu-store-email-body-observation-html-content observation)]
                   :update]
                  [revision [:bind (1+ actual-revision)] :update]
                  [is-truncated
                   [:bind
                    (chidu-store-sqlite--integer-bool
                     (chidu-store-email-body-observation-truncated-p observation))]
                   :update]
                  [encoding-problem
                   [:bind
                    (chidu-store-sqlite--integer-bool
                     (chidu-store-email-body-observation-encoding-problem-p
                      observation))]
                   :update]
                  [observed-change-seq [:bind change-seq] :update]]
                 :on-conflict [account-id local-email-id]])
              (chidu-sql-execute database
                [:delete :from jmap-email-attachment
                 :where [:and
                         [:= account-id [:bind account-id]]
                         [:= local-email-id [:bind local-email-id]]]])
              (cl-loop
               for attachment across
               (chidu-store-email-body-observation-attachments observation)
               for ordinal from 0
               do
               (chidu-sql-execute database
                 [:insert :into jmap-email-attachment
                  :row
                  [[account-id [:bind account-id]]
                   [local-email-id [:bind local-email-id]]
                   [ordinal [:bind ordinal]]
                   [part-id
                    [:bind
                     (chidu-store-email-attachment-part-id attachment)]]
                   [blob-id
                    [:bind
                     (chidu-store-email-attachment-blob-id attachment)]]
                   [size [:bind (chidu-store-email-attachment-size attachment)]]
                   [name [:bind (chidu-store-email-attachment-name attachment)]]
                   [media-type
                    [:bind
                     (chidu-store-email-attachment-media-type attachment)]]
                   [charset
                    [:bind
                     (chidu-store-email-attachment-charset attachment)]]
                   [disposition
                    [:bind
                     (chidu-store-email-attachment-disposition attachment)]]
                   [cid [:bind (chidu-store-email-attachment-cid attachment)]]
                   [language-json
                    [:bind
                     (chidu-store-sqlite--string-vector-json
                      (chidu-store-email-attachment-language attachment)
                      "Email attachment language")]]
                   [location
                    [:bind
                     (chidu-store-email-attachment-location attachment)]]]]))))
          (chidu-store-sqlite--email-body-context
           state account-id local-email-id remote-email-id)))))))

(defun chidu-store-sqlite--replace-parsed-blob (state operation)
  "CAS-replace parsed Blob OPERATION in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-replace-parsed-blob-account-id operation))
         (blob-id
          (chidu-store-op-replace-parsed-blob-blob-id operation))
         (profile-version
          (chidu-store-op-replace-parsed-blob-profile-version operation))
         (expected-revision
          (chidu-store-op-replace-parsed-blob-expected-revision operation))
         (observation
          (chidu-store-op-replace-parsed-blob-observation operation))
         (context-result
          (chidu-store-sqlite--parsed-blob-context
           state account-id blob-id profile-version)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-revision
              (chidu-store-parsed-blob-context-revision context)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id :blob-id blob-id
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((or
           (not
            (equal blob-id
                   (chidu-store-parsed-blob-observation-blob-id
                    observation)))
           (not
            (equal profile-version
                   (chidu-store-parsed-blob-observation-profile-version
                    observation))))
          (chidu-result-failure-create
           :kind 'blob-identity-mismatch
           :data (list :account-id account-id :blob-id blob-id)
           :retryable-p nil))
         (t
          (let* ((message
                  (chidu-store-parsed-blob-observation-message observation))
                 (body (chidu-store-parsed-message-body message)))
            (with-sqlite-transaction database
              (let ((change-seq
                     (chidu-store-sqlite--increment-change-seq database)))
                (chidu-sql-execute database
                  [:insert :into jmap-parsed-blob
                   :row
                   [[account-id [:bind account-id]]
                    [blob-id [:bind blob-id]]
                    [profile-version [:bind profile-version]]
                    [revision [:bind (1+ actual-revision)] :update]
                    [message-ids-json
                     [:bind
                      (chidu-store-sqlite--string-vector-json
                       (chidu-store-parsed-message-message-ids message)
                       "Parsed messageId")]
                     :update]
                    [in-reply-to-json
                     [:bind
                      (chidu-store-sqlite--string-vector-json
                       (chidu-store-parsed-message-in-reply-to message)
                       "Parsed inReplyTo")]
                     :update]
                    [references-json
                     [:bind
                      (chidu-store-sqlite--string-vector-json
                       (chidu-store-parsed-message-references message)
                       "Parsed references")]
                     :update]
                    [sender-json
                     [:bind
                      (chidu-store-sqlite--email-address-vector-json
                       (chidu-store-parsed-message-sender message))]
                     :update]
                    [from-json
                     [:bind
                      (chidu-store-sqlite--email-address-vector-json
                       (chidu-store-parsed-message-from message))]
                     :update]
                    [to-json
                     [:bind
                      (chidu-store-sqlite--email-address-vector-json
                       (chidu-store-parsed-message-to message))]
                     :update]
                    [cc-json
                     [:bind
                      (chidu-store-sqlite--email-address-vector-json
                       (chidu-store-parsed-message-cc message))]
                     :update]
                    [bcc-json
                     [:bind
                      (chidu-store-sqlite--email-address-vector-json
                       (chidu-store-parsed-message-bcc message))]
                     :update]
                    [reply-to-json
                     [:bind
                      (chidu-store-sqlite--email-address-vector-json
                       (chidu-store-parsed-message-reply-to message))]
                     :update]
                    [subject
                     [:bind (chidu-store-parsed-message-subject message)]
                     :update]
                    [sent-at
                     [:bind (chidu-store-parsed-message-sent-at message)]
                     :update]
                    [preview
                     [:bind (chidu-store-parsed-message-preview message)]
                     :update]
                    [text-content
                     [:bind (chidu-store-email-body-text-content body)]
                     :update]
                    [html-content
                     [:bind (chidu-store-email-body-html-content body)]
                     :update]
                    [is-truncated
                     [:bind
                      (chidu-store-sqlite--integer-bool
                       (chidu-store-email-body-truncated-p body))]
                     :update]
                    [encoding-problem
                     [:bind
                      (chidu-store-sqlite--integer-bool
                       (chidu-store-email-body-encoding-problem-p body))]
                     :update]
                    [observed-change-seq [:bind change-seq] :update]]
                   :on-conflict [account-id blob-id profile-version]])
                (chidu-sql-execute database
                  [:delete :from jmap-parsed-attachment
                   :where [:and
                           [:= account-id [:bind account-id]]
                           [:= source-blob-id [:bind blob-id]]
                           [:= profile-version [:bind profile-version]]]])
                (cl-loop
                 for attachment across
                 (chidu-store-email-body-attachments body)
                 for ordinal from 0
                 do
                 (chidu-sql-execute database
                   [:insert :into jmap-parsed-attachment
                    :row
                    [[account-id [:bind account-id]]
                     [source-blob-id [:bind blob-id]]
                     [profile-version [:bind profile-version]]
                     [ordinal [:bind ordinal]]
                     [part-id
                      [:bind
                       (chidu-store-email-attachment-part-id attachment)]]
                     [blob-id
                      [:bind
                       (chidu-store-email-attachment-blob-id attachment)]]
                     [size
                      [:bind (chidu-store-email-attachment-size attachment)]]
                     [name
                      [:bind (chidu-store-email-attachment-name attachment)]]
                     [media-type
                      [:bind
                       (chidu-store-email-attachment-media-type attachment)]]
                     [charset
                      [:bind
                       (chidu-store-email-attachment-charset attachment)]]
                     [disposition
                      [:bind
                       (chidu-store-email-attachment-disposition attachment)]]
                     [cid
                      [:bind (chidu-store-email-attachment-cid attachment)]]
                     [language-json
                      [:bind
                       (chidu-store-sqlite--string-vector-json
                        (chidu-store-email-attachment-language attachment)
                        "Parsed attachment language")]]
                     [location
                      [:bind
                       (chidu-store-email-attachment-location attachment)]]]]))))
            (chidu-store-sqlite--parsed-blob-context
             state account-id blob-id profile-version))))))))

(defun chidu-store-sqlite--replace-conversation (state operation)
  "CAS-replace Conversation OPERATION in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-replace-conversation-account-id operation))
         (remote-thread-id
          (chidu-store-op-replace-conversation-remote-thread-id operation))
         (expected-revision
          (chidu-store-op-replace-conversation-expected-revision operation))
         (observation
          (chidu-store-op-replace-conversation-observation operation))
         (context-result
          (chidu-store-sqlite--conversation-context
           state account-id remote-thread-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let ((actual-revision
             (chidu-store-conversation-context-revision
              (chidu-result-ok-value context-result))))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id
                       :remote-thread-id remote-thread-id
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((not
           (equal remote-thread-id
                  (chidu-store-conversation-observation-remote-thread-id
                   observation)))
          (chidu-result-failure-create
           :kind 'thread-mismatch
           :data (list :account-id account-id
                       :remote-thread-id remote-thread-id)
           :retryable-p nil))
         (t
          (with-sqlite-transaction database
            (let* ((change-seq
                    (chidu-store-sqlite--increment-change-seq database))
                   (rows
                    (chidu-store-materialize-conversation-rows
                     observation
                     (lambda (remote-email-id)
                       (chidu-store-sqlite--email-record-id
                        database account-id remote-email-id change-seq))))
                   (next-revision (1+ actual-revision)))
              (chidu-sql-execute database
                [:insert :into jmap-conversation
                 :row
                 [[account-id [:bind account-id]]
                  [remote-thread-id [:bind remote-thread-id]]
                  [thread-state
                   [:bind
                    (chidu-store-conversation-observation-thread-state observation)]
                   :update]
                  [email-state
                   [:bind
                    (chidu-store-conversation-observation-email-state observation)]
                   :update]
                  [revision [:bind next-revision] :update]
                  [is-complete
                   [:bind
                    (chidu-store-sqlite--integer-bool
                     (chidu-store-conversation-observation-complete-p
                      observation))]
                   :update]
                  [observed-change-seq [:bind change-seq] :update]]
                 :on-conflict [account-id remote-thread-id]])
              (chidu-sql-execute database
                [:delete
                 :from jmap-conversation-row
                 :where [:and
                         [:= account-id [:bind account-id]]
                         [:= remote-thread-id [:bind remote-thread-id]]]])
              (cl-loop
               for item across rows
               for ordinal from 0
               for summary = (chidu-store-conversation-row-summary-row item)
               do
               (chidu-sql-execute database
                 [:insert :into jmap-conversation-row
                  :row
                  [[account-id [:bind account-id]]
                   [remote-thread-id [:bind remote-thread-id]]
                   [ordinal [:bind ordinal]]
                   [local-email-id
                    [:bind
                     (chidu-store-email-summary-row-local-email-id summary)]]
                   [parent-local-email-id
                    [:bind
                     (chidu-store-conversation-row-parent-local-email-id item)]]
                   [depth
                    [:bind (chidu-store-conversation-row-depth item)]]
                   [received-at
                    [:bind (chidu-store-email-summary-row-received-at summary)]]
                   [sent-at
                    [:bind (chidu-store-conversation-row-sent-at item)]]
                   [from-name
                    [:bind (chidu-store-email-summary-row-from-name summary)]]
                   [from-email
                    [:bind (chidu-store-email-summary-row-from-email summary)]]
                   [subject
                    [:bind (chidu-store-email-summary-row-subject summary)]]
                   [preview
                    [:bind (chidu-store-email-summary-row-preview summary)]]
                   [is-unread
                    [:bind
                     (chidu-store-sqlite--integer-bool
                      (chidu-store-email-summary-row-unread-p summary))]]
                   [is-flagged
                    [:bind
                     (chidu-store-sqlite--integer-bool
                      (chidu-store-email-summary-row-flagged-p summary))]]
                   [has-attachment
                    [:bind
                     (chidu-store-sqlite--integer-bool
                      (chidu-store-email-summary-row-has-attachment-p summary))]]
                   [message-ids-json
                    [:bind
                     (chidu-store-sqlite--string-vector-json
                      (chidu-store-conversation-row-message-ids item)
                      "Conversation messageId")]]
                   [in-reply-to-json
                    [:bind
                     (chidu-store-sqlite--string-vector-json
                      (chidu-store-conversation-row-in-reply-to item)
                      "Conversation inReplyTo")]]
                   [references-json
                    [:bind
                     (chidu-store-sqlite--string-vector-json
                      (chidu-store-conversation-row-references item)
                      "Conversation references")]]]]))))
          (chidu-store-sqlite--conversation-context
           state account-id remote-thread-id)))))))

(provide 'chidu-store-sqlite-materialization)

;;; chidu-store-sqlite-materialization.el ends here
