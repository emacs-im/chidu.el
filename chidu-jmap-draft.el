;;; chidu-jmap-draft.el --- JMAP server Draft wire boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Strict requests and responses for creating one JMAP Draft Email, locating a
;; create whose response was lost, and deleting a confirmed predecessor.  The
;; durable publication reducer lives outside this adapter.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-compose)
(require 'chidu-jmap-api)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-store)

(chidu-define-record chidu-jmap-draft-create-result
    "One exact Email/set create outcome for a server Draft."
  outcome
  remote-email-id
  remote-blob-id
  error-kind)

(chidu-define-record chidu-jmap-draft-cleanup-result
    "One exact Email/set destroy outcome for a predecessor Draft."
  outcome
  error-kind)

(chidu-define-record chidu-jmap-draft-reconcile-match
    "One exact server Draft matching a durable create identity."
  remote-email-id
  remote-blob-id)

(chidu-define-record chidu-jmap-draft-cleanup-evidence
    "One authoritative predecessor observation before conditional cleanup."
  state
  found-p
  remote-blob-id
  (remote-mailbox-ids (vector))
  (keywords (vector)))

(defun chidu-jmap-draft--nullable-object (value context)
  "Return nullable object VALUE for CONTEXT."
  (if (eq value :json-null)
      nil
    (chidu-jmap--hash value context)))

(defun chidu-jmap-draft--validate-description (object context)
  "Validate optional error description in OBJECT for CONTEXT."
  (let ((value (gethash "description" object :json-null)))
    (unless (eq value :json-null)
      (chidu-jmap--string value context t))))

(defun chidu-jmap-draft--only-map-value (object key context)
  "Return OBJECT value for exact KEY, rejecting extra keys for CONTEXT."
  (when object
    (unless (= 1 (hash-table-count object))
      (signal 'chidu-jmap-error
              (list (format "%s contains unexpected objects" context))))
    (let ((missing (make-symbol "missing")))
      (let ((value (gethash key object missing)))
        (when (eq value missing)
          (signal 'chidu-jmap-error
                  (list (format "%s did not settle the expected id" context))))
        value))))

(defun chidu-jmap-draft--destroyed-target-p
    (wire-destroyed remote-email-id)
  "Return non-nil when WIRE-DESTROYED exactly settles REMOTE-EMAIL-ID."
  (let ((destroyed
         (if (eq wire-destroyed :json-null)
             (vector)
           (chidu-jmap--vector
            wire-destroyed "Draft SetResponse destroyed"))))
    (pcase (length destroyed)
      (0 nil)
      (1
       (let ((actual
              (chidu-jmap--id (aref destroyed 0) "Destroyed Draft id")))
         (unless (equal actual remote-email-id)
           (signal 'chidu-jmap-error
                   '("Draft cleanup destroyed an unrequested id")))
         t))
      (_
       (signal 'chidu-jmap-error
               '("Draft cleanup destroyed unexpected objects"))))))

(defun chidu-jmap-draft-create-request
    (remote-account-id creation-id email-object)
  "Return one Email/set create request for REMOTE-ACCOUNT-ID.

CREATION-ID names EMAIL-OBJECT in the Set request."
  (let ((create (make-hash-table :test #'equal)))
    (puthash creation-id email-object create)
    `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
             :methodCalls
             [["Email/set"
               (:accountId ,remote-account-id :create ,create)
               "draft-create"]])))

(defun chidu-jmap-draft-cleanup-get-request
    (remote-account-id remote-email-id)
  "Return REMOTE-ACCOUNT-ID predecessor read for REMOTE-EMAIL-ID."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Email/get"
             (:accountId ,remote-account-id
                         :ids [,remote-email-id]
                         :properties ["id" "blobId" "mailboxIds" "keywords"])
             "draft-cleanup-get"]]))

(defun chidu-jmap-draft-cleanup-request
    (remote-account-id remote-email-id state)
  "Return conditional REMOTE-ACCOUNT-ID destroy for REMOTE-EMAIL-ID at STATE."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Email/set"
             (:accountId ,remote-account-id
                         :ifInState ,state
                         :destroy [,remote-email-id])
             "draft-cleanup"]]))

(defun chidu-jmap-draft-reconcile-request
    (remote-account-id remote-drafts-mailbox-id message-id)
  "Return REMOTE-ACCOUNT-ID Draft lookup for MESSAGE-ID.

REMOTE-DRAFTS-MAILBOX-ID scopes the bounded query."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Email/query"
             (:accountId ,remote-account-id
                         :filter
                         (:operator "AND"
                                    :conditions
                                    [(:inMailbox ,remote-drafts-mailbox-id)
                                     (:hasKeyword "$draft")
                                     (:header ["Message-ID" ,message-id])])
                         :sort [(:property "receivedAt" :isAscending :json-false)]
                         :collapseThreads :json-false
                         :calculateTotal t
                         :position 0
                         :limit 2)
             "draft-query"]
            ["Email/get"
             (:accountId ,remote-account-id
                         ,(intern ":#ids")
                         (:resultOf "draft-query" :name "Email/query" :path "/ids")
                         :properties ["id" "blobId" "messageId" "mailboxIds" "keywords"])
             "draft-get"]]))

(defun chidu-jmap-draft--single-invocation
    (bytes expected-call-id remote-account-id)
  "Return (NAME . ARGUMENTS) from single-call response BYTES.

EXPECTED-CALL-ID and REMOTE-ACCOUNT-ID are checked for normal responses."
  (let* ((wire (chidu-jmap--parse-json-object bytes "Draft Set response"))
         (_session-state
          (chidu-jmap--string
           (chidu-jmap--required wire "sessionState" "Draft Set response")
           "Draft Set sessionState"))
         (responses
          (chidu-jmap--vector
           (chidu-jmap--required
            wire "methodResponses" "Draft Set response")
           "Draft Set methodResponses")))
    (unless (= 1 (length responses))
      (signal 'chidu-jmap-error
              '("Draft Set response has unexpected invocation count")))
    (let ((invocation (aref responses 0)))
      (unless (and (vectorp invocation) (= 3 (length invocation)))
        (signal 'chidu-jmap-error '("Draft Set invocation is malformed")))
      (let* ((name
              (chidu-jmap--string (aref invocation 0) "Draft Set method"))
             (arguments
              (chidu-jmap--hash
               (aref invocation 1) "Draft Set arguments"))
             (call-id
              (chidu-jmap--string
               (aref invocation 2) "Draft Set call id")))
        (unless (equal call-id expected-call-id)
          (signal 'chidu-jmap-error '("Draft Set call id mismatch")))
        (unless (equal name "error")
          (unless (equal name "Email/set")
            (signal 'chidu-jmap-error
                    '("Draft Set returned an unexpected method")))
          (let ((actual-account-id
                 (chidu-jmap--id
                  (chidu-jmap--required
                   arguments "accountId" "Draft SetResponse")
                  "Draft SetResponse accountId")))
            (unless (equal actual-account-id remote-account-id)
              (signal 'chidu-jmap-error
                      '("Draft SetResponse accountId mismatch")))))
        (cons name arguments)))))

(defun chidu-jmap-draft--method-error (arguments constructor)
  "Decode method error ARGUMENTS using result CONSTRUCTOR."
  (let* ((type
          (chidu-jmap--string
           (chidu-jmap--required arguments "type" "Draft method error")
           "Draft method error type"))
         (outcome (if (equal type "serverPartialFail") 'unknown 'rejected)))
    (chidu-jmap-draft--validate-description
     arguments "Draft method error description")
    (funcall constructor outcome type)))

(defun chidu-jmap-draft-validate-create-response
    (bytes remote-account-id creation-id)
  "Validate Draft create BYTES for REMOTE-ACCOUNT-ID and CREATION-ID."
  (pcase-let* ((`(,name . ,arguments)
                (chidu-jmap-draft--single-invocation
                 bytes "draft-create" remote-account-id)))
    (if (equal name "error")
        (chidu-jmap-draft--method-error
         arguments
         (lambda (outcome type)
           (chidu-jmap-draft-create-result-create
            :outcome outcome :error-kind type)))
      (let* ((created
              (chidu-jmap-draft--nullable-object
               (gethash "created" arguments :json-null)
               "Draft SetResponse created"))
             (not-created
              (chidu-jmap-draft--nullable-object
               (gethash "notCreated" arguments :json-null)
               "Draft SetResponse notCreated"))
             (created-value
              (chidu-jmap-draft--only-map-value
               created creation-id "Draft SetResponse created"))
             (error-value
              (chidu-jmap-draft--only-map-value
               not-created creation-id "Draft SetResponse notCreated")))
        (unless (xor created-value error-value)
          (signal 'chidu-jmap-error
                  '("Draft SetResponse did not settle the creation exactly once")))
        (if created-value
            (let* ((object
                    (chidu-jmap--hash created-value "Created Draft Email"))
                   (remote-id
                    (chidu-jmap--id
                     (chidu-jmap--required
                      object "id" "Created Draft Email")
                     "Created Draft id"))
                   (remote-blob-id
                    (chidu-jmap--id
                     (chidu-jmap--required
                      object "blobId" "Created Draft Email")
                     "Created Draft blobId")))
              (chidu-jmap--id
               (chidu-jmap--required object "threadId" "Created Draft Email")
               "Created Draft threadId")
              (chidu-jmap--safe-nonnegative-integer
               (chidu-jmap--required object "size" "Created Draft Email")
               "Created Draft size")
              (chidu-jmap-draft-create-result-create
               :outcome 'succeeded
               :remote-email-id remote-id
               :remote-blob-id remote-blob-id))
          (let* ((object
                  (chidu-jmap--hash error-value "Draft SetError"))
                 (type
                  (chidu-jmap--string
                   (chidu-jmap--required object "type" "Draft SetError")
                   "Draft SetError type")))
            (chidu-jmap-draft--validate-description
             object "Draft SetError description")
            (chidu-jmap-draft-create-result-create
             :outcome 'rejected :error-kind type)))))))

(defun chidu-jmap-draft-validate-cleanup-get-response
    (bytes remote-account-id remote-email-id)
  "Validate cleanup read BYTES for REMOTE-ACCOUNT-ID and REMOTE-EMAIL-ID."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Email/get" "draft-cleanup-get" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (state
          (chidu-jmap--string
           (chidu-jmap--required arguments "state" "Draft cleanup Email/get")
           "Draft cleanup state"))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Draft cleanup Email/get")
           "Draft cleanup list"))
         (not-found
          (chidu-jmap--nullable-vector
           (chidu-jmap--required
            arguments "notFound" "Draft cleanup Email/get")
           "Draft cleanup notFound")))
    (cond
     ((and (zerop (length wire-list))
           (= 1 (length not-found))
           (equal remote-email-id
                  (chidu-jmap--id
                   (aref not-found 0) "Draft cleanup notFound id")))
      (chidu-jmap-draft-cleanup-evidence-create
       :state state :found-p nil))
     ((not (and (= 1 (length wire-list))
                (zerop (length not-found))))
      (signal 'chidu-jmap-error
              '("Draft cleanup Email/get returned incomplete coverage")))
     (t
      (let* ((email
              (chidu-jmap--hash
               (aref wire-list 0) "Draft cleanup Email"))
             (actual-id
              (chidu-jmap--id
               (chidu-jmap--required email "id" "Draft cleanup Email")
               "Draft cleanup Email id")))
        (unless (equal actual-id remote-email-id)
          (signal 'chidu-jmap-error
                  '("Draft cleanup Email/get returned a different id")))
        (chidu-jmap-draft-cleanup-evidence-create
         :state state
         :found-p t
         :remote-blob-id
         (chidu-jmap--id
          (chidu-jmap--required email "blobId" "Draft cleanup Email")
          "Draft cleanup blobId")
         :remote-mailbox-ids
         (chidu-jmap--true-map-keys
          (chidu-jmap--required email "mailboxIds" "Draft cleanup Email")
          "Draft cleanup mailboxIds" t)
         :keywords
         (chidu-jmap--true-map-keys
          (chidu-jmap--required email "keywords" "Draft cleanup Email")
          "Draft cleanup keywords")))))))

(defun chidu-jmap-draft-validate-cleanup-response
    (bytes remote-account-id remote-email-id)
  "Validate cleanup BYTES for REMOTE-ACCOUNT-ID and REMOTE-EMAIL-ID."
  (pcase-let* ((`(,name . ,arguments)
                (chidu-jmap-draft--single-invocation
                 bytes "draft-cleanup" remote-account-id)))
    (if (equal name "error")
        (chidu-jmap-draft--method-error
         arguments
         (lambda (outcome type)
           (chidu-jmap-draft-cleanup-result-create
            :outcome outcome :error-kind type)))
      (let* ((destroyed-p
              (chidu-jmap-draft--destroyed-target-p
               (gethash "destroyed" arguments :json-null)
               remote-email-id))
             (not-destroyed
              (chidu-jmap-draft--nullable-object
               (gethash "notDestroyed" arguments :json-null)
               "Draft SetResponse notDestroyed"))
             (error-value
              (chidu-jmap-draft--only-map-value
               not-destroyed remote-email-id
               "Draft SetResponse notDestroyed")))
        (unless (xor destroyed-p error-value)
          (signal 'chidu-jmap-error
                  '("Draft cleanup did not settle the target exactly once")))
        (if destroyed-p
            (chidu-jmap-draft-cleanup-result-create :outcome 'succeeded)
          (let* ((object
                  (chidu-jmap--hash error-value "Draft cleanup SetError"))
                 (type
                  (chidu-jmap--string
                   (chidu-jmap--required object "type" "Draft cleanup SetError")
                   "Draft cleanup SetError type")))
            (chidu-jmap-draft--validate-description
             object "Draft cleanup SetError description")
            (chidu-jmap-draft-cleanup-result-create
             :outcome (if (equal type "notFound") 'succeeded 'rejected)
             :error-kind type)))))))

(defun chidu-jmap-draft--true-member-p (object key context)
  "Return non-nil when OBJECT contains KEY with JSON true for CONTEXT."
  (let ((value (gethash key object :json-null)))
    (cond
     ((eq value t) t)
     ((eq value :json-null) nil)
     ((eq value :json-false) nil)
     (t
      (signal 'chidu-jmap-error
              (list (format "%s has a non-Boolean set value" context)))))))

(defun chidu-jmap-draft-validate-reconcile-response
    (bytes remote-account-id remote-drafts-mailbox-id message-id)
  "Decode BYTES for REMOTE-ACCOUNT-ID, REMOTE-DRAFTS-MAILBOX-ID, and MESSAGE-ID."
  (let* ((responses
          (chidu-jmap-parse-method-responses
           bytes
           `(("Email/query" "draft-query" ,remote-account-id)
             ("Email/get" "draft-get" ,remote-account-id))))
         (query-arguments
          (chidu-jmap-method-response-arguments (aref responses 0)))
         (get-arguments
          (chidu-jmap-method-response-arguments (aref responses 1)))
         (ids
          (chidu-jmap--vector
           (chidu-jmap--required query-arguments "ids" "Draft Email/query")
           "Draft Email/query ids"))
         (total
          (chidu-jmap--safe-nonnegative-integer
           (chidu-jmap--required query-arguments "total" "Draft Email/query")
           "Draft Email/query total"))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required get-arguments "list" "Draft Email/get")
           "Draft Email/get list"))
         (not-found
          (let ((value
                 (chidu-jmap--required
                  get-arguments "notFound" "Draft Email/get")))
            (if (eq value :json-null)
                (vector)
              (chidu-jmap--vector value "Draft Email/get notFound"))))
         (expected (make-hash-table :test #'equal))
         (settled (make-hash-table :test #'equal))
         exact)
    (when (> (length ids) 2)
      (signal 'chidu-jmap-error
              '("Draft Email/query exceeded the requested limit")))
    (unless (<= (length ids) total)
      (signal 'chidu-jmap-error '("Draft Email/query total is inconsistent")))
    (cl-loop for value across ids
             for id = (chidu-jmap--id value "Draft Email/query id")
             do (when (gethash id expected)
                  (signal 'chidu-jmap-error
                          '("Draft Email/query returned a duplicate id")))
             do (puthash id t expected))
    (cl-loop for value across not-found
             for id = (chidu-jmap--id value "Draft Email/get notFound id")
             do
             (unless (gethash id expected)
               (signal 'chidu-jmap-error
                       '("Draft Email/get returned unrequested notFound id")))
             (when (gethash id settled)
               (signal 'chidu-jmap-error
                       '("Draft Email/get settled one id more than once")))
             (puthash id t settled))
    (cl-loop
     for wire across wire-list
     for object = (chidu-jmap--hash wire "Draft Email/get item")
     for id =
     (chidu-jmap--id
      (chidu-jmap--required object "id" "Draft Email/get item")
      "Draft Email/get id")
     do
     (unless (gethash id expected)
       (signal 'chidu-jmap-error
               '("Draft Email/get returned an unrequested id")))
     (when (gethash id settled)
       (signal 'chidu-jmap-error
               '("Draft Email/get settled one id more than once")))
     (puthash id t settled)
     (let* ((blob-id
             (chidu-jmap--id
              (chidu-jmap--required object "blobId" "Draft Email/get item")
              "Draft Email/get blobId"))
            (message-ids
             (chidu-jmap--vector
              (chidu-jmap--required
               object "messageId" "Draft Email/get item")
              "Draft Email messageId"))
            (mailbox-ids
             (chidu-jmap--hash
              (chidu-jmap--required
               object "mailboxIds" "Draft Email/get item")
              "Draft Email mailboxIds"))
            (keywords
             (chidu-jmap--hash
              (chidu-jmap--required
               object "keywords" "Draft Email/get item")
              "Draft Email keywords")))
       (when (and
              (cl-loop for value across message-ids
                       thereis
                       (equal message-id
                              (chidu-jmap--string
                               value "Draft Email Message-ID")))
              (chidu-jmap-draft--true-member-p
               mailbox-ids remote-drafts-mailbox-id "Draft mailboxIds")
              (chidu-jmap-draft--true-member-p
               keywords "$draft" "Draft keywords"))
         (push
          (chidu-jmap-draft-reconcile-match-create
           :remote-email-id id :remote-blob-id blob-id)
          exact))))
    (unless (= (hash-table-count expected) (hash-table-count settled))
      (signal 'chidu-jmap-error
              '("Draft Email/get did not settle every queried id")))
    (vconcat (nreverse exact))))

(defun chidu-jmap-draft-create
    (context document creation-id message-id secret deliver)
  "Create one server Draft for CONTEXT DOCUMENT using owned SECRET.

CREATION-ID and MESSAGE-ID identify the immutable attempt.  DELIVER receives a
typed result and the caller remains responsible for clearing SECRET."
  (let* ((endpoint (chidu-store-compose-context-endpoint context))
         (account (chidu-store-compose-context-account context))
         (identity (chidu-store-compose-context-identity context))
         (drafts-mailbox
          (or (chidu-store-compose-context-drafts-mailbox context)
              (signal 'chidu-jmap-error '("Drafts Mailbox is unavailable"))))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (email
          (chidu-jmap-compose-draft-email
           document identity
           (chidu-store-mailbox-remote-mailbox-id drafts-mailbox)
           message-id
           (chidu-store-compose-context-resources context))))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-draft-create-request
      remote-account-id creation-id email)
     (lambda (bytes)
       (chidu-jmap-draft-validate-create-response
        bytes remote-account-id creation-id))
     deliver)))

(defun chidu-jmap-draft-reconcile (context attempt secret deliver)
  "Locate CONTEXT ATTEMPT after an uncertain create using SECRET.

Call DELIVER with one typed result."
  (let* ((endpoint (chidu-store-compose-context-endpoint context))
         (account (chidu-store-compose-context-account context))
         (drafts-mailbox
          (or (chidu-store-compose-context-drafts-mailbox context)
              (signal 'chidu-jmap-error '("Drafts Mailbox is unavailable"))))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (remote-mailbox-id
          (chidu-store-mailbox-remote-mailbox-id drafts-mailbox))
         (message-id
          (chidu-store-draft-publish-attempt-message-id attempt)))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-draft-reconcile-request
      remote-account-id remote-mailbox-id message-id)
     (lambda (bytes)
       (chidu-jmap-draft-validate-reconcile-response
        bytes remote-account-id remote-mailbox-id message-id))
     deliver)))

(defun chidu-jmap-draft-cleanup-read
    (context attempt secret deliver)
  "Read CONTEXT ATTEMPT predecessor using caller-owned SECRET.

Call DELIVER with one typed result."
  (let* ((endpoint (chidu-store-compose-context-endpoint context))
         (account (chidu-store-compose-context-account context))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (remote-email-id
          (or
           (chidu-store-draft-publish-attempt-predecessor-remote-email-id
            attempt)
           (signal 'chidu-jmap-error
                   '("Draft cleanup has no predecessor Email")))))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-draft-cleanup-get-request
      remote-account-id remote-email-id)
     (lambda (bytes)
       (chidu-jmap-draft-validate-cleanup-get-response
        bytes remote-account-id remote-email-id))
     deliver)))

(defun chidu-jmap-draft-cleanup-destroy
    (context attempt state secret deliver)
  "Conditionally destroy CONTEXT ATTEMPT predecessor at STATE.

SECRET remains caller-owned.  Call DELIVER with one typed result."
  (let* ((endpoint (chidu-store-compose-context-endpoint context))
         (account (chidu-store-compose-context-account context))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (remote-email-id
          (or
           (chidu-store-draft-publish-attempt-predecessor-remote-email-id
            attempt)
           (signal 'chidu-jmap-error
                   '("Draft cleanup has no predecessor Email")))))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-draft-cleanup-request
      remote-account-id remote-email-id state)
     (lambda (bytes)
       (chidu-jmap-draft-validate-cleanup-response
        bytes remote-account-id remote-email-id))
     deliver)))

(provide 'chidu-jmap-draft)

;;; chidu-jmap-draft.el ends here
