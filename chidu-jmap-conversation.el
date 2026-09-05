;;; chidu-jmap-conversation.el --- Bounded JMAP Conversation fetch -*- lexical-binding: t; -*-

;;; Commentary:

;; Fetch one JMAP Thread membership, then the bounded Email metadata required
;; to build a reply tree.  Thread/get is the membership source.  Parsed RFC
;; Message-ID relations are merely display edges and never admit extra Emails.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-email)
(require 'chidu-jmap-http)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-result)
(require 'chidu-store)

(defcustom chidu-conversation-email-limit 256
  "Maximum number of Emails fetched for one on-demand Conversation."
  :type 'positive-integer
  :group 'chidu)

(defconst chidu-jmap-conversation-properties
  ["id" "threadId" "mailboxIds" "keywords" "receivedAt" "sentAt"
   "from" "subject" "preview" "hasAttachment"
   "messageId" "inReplyTo" "references"]
  "Email properties fetched for a reply-tree Conversation projection.")

(cl-defstruct (chidu-jmap-conversation-fetch
               (:constructor chidu-jmap-conversation-fetch-create))
  "Mechanical state for one cancelable Conversation fetch."
  context
  secret
  limit
  deliver
  thread-state
  remote-email-ids
  request
  completed-p
  canceled-p)

(defun chidu-jmap-conversation--thread-request
    (remote-account-id remote-thread-id)
  "Return Thread/get request for REMOTE-THREAD-ID in REMOTE-ACCOUNT-ID."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Thread/get"
             (:accountId ,remote-account-id :ids [,remote-thread-id])
             "conversation-thread"]]))

(defun chidu-jmap-conversation--email-request
    (remote-account-id remote-email-ids)
  "Return Email/get request for REMOTE-EMAIL-IDS in REMOTE-ACCOUNT-ID."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Email/get"
             (:accountId ,remote-account-id
                         :ids ,remote-email-ids
                         :properties ,chidu-jmap-conversation-properties)
             "conversation-email"]]))

(defun chidu-jmap-conversation--id-vector (wire context)
  "Validate unique JMAP Id vector WIRE for CONTEXT."
  (let ((items (chidu-jmap--vector wire context))
        (seen (make-hash-table :test #'equal))
        result)
    (cl-loop
     for wire-id across items
     for remote-id = (chidu-jmap--id wire-id context)
     do
     (when (gethash remote-id seen)
       (signal 'chidu-jmap-error
               (list (format "%s contains a duplicate Id" context))))
     (puthash remote-id t seen)
     (push remote-id result))
    (vconcat (nreverse result))))

(defun chidu-jmap-conversation--validate-thread
    (bytes remote-account-id remote-thread-id limit)
  "Validate Thread/get BYTES for REMOTE-ACCOUNT-ID and REMOTE-THREAD-ID.

LIMIT bounds membership.  Return (THREAD-STATE . EMAIL-IDS), or nil when the
Thread is not found."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Thread/get" "conversation-thread" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (thread-state
          (chidu-jmap--string
           (chidu-jmap--required arguments "state" "Thread/get")
           "Thread state"))
         (list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Thread/get")
           "Thread/get list"))
         (not-found
          (chidu-jmap-conversation--id-vector
           (chidu-jmap--required arguments "notFound" "Thread/get")
           "Thread/get notFound")))
    (cond
     ((and (zerop (length list))
           (= 1 (length not-found))
           (equal remote-thread-id (aref not-found 0)))
      nil)
     ((not (and (= 1 (length list)) (zerop (length not-found))))
      (signal 'chidu-jmap-error
              '("Thread/get returned unexpected coverage")))
     (t
      (let* ((thread (chidu-jmap--hash (aref list 0) "Thread object"))
             (actual-id
              (chidu-jmap--id
               (chidu-jmap--required thread "id" "Thread object")
               "Thread id"))
             (email-ids
              (chidu-jmap-conversation--id-vector
               (chidu-jmap--required thread "emailIds" "Thread object")
               "Thread emailIds")))
        (unless (equal actual-id remote-thread-id)
          (signal 'chidu-jmap-error '("Thread/get returned a different Thread")))
        (when (zerop (length email-ids))
          (signal 'chidu-jmap-error '("Thread has no Email ids")))
        (when (> (length email-ids) limit)
          (signal 'chidu-jmap-error
                  (list
                   (format "Thread has %d Emails; effective Email/get limit is %d"
                           (length email-ids) limit))))
        (cons thread-state email-ids))))))

(defun chidu-jmap-conversation--nullable-date (value context)
  "Return nil for JSON null or copied date string VALUE for CONTEXT."
  (if (eq value :json-null)
      nil
    (chidu-jmap--string value context)))

(defun chidu-jmap-conversation--validate-emails
    (bytes remote-account-id remote-thread-id remote-email-ids thread-state)
  "Validate Email/get BYTES for REMOTE-ACCOUNT-ID and REMOTE-THREAD-ID.

Preserve REMOTE-EMAIL-IDS order and attach THREAD-STATE to the observation."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Email/get" "conversation-email" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (email-state
          (chidu-jmap--string
           (chidu-jmap--required arguments "state" "Email/get")
           "Email state"))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Email/get")
           "Email/get list"))
         (wire-not-found
          (chidu-jmap--vector
           (chidu-jmap--required arguments "notFound" "Email/get")
           "Email/get notFound"))
         (expected (make-hash-table :test #'equal))
         (settled (make-hash-table :test #'equal))
         (rows-by-id (make-hash-table :test #'equal))
         rows)
    (cl-loop for remote-id across remote-email-ids
             do (puthash remote-id t expected))
    (cl-loop
     for wire across wire-list
     for email = (chidu-jmap--hash wire "Conversation Email object")
     for remote-id =
     (chidu-jmap--id
      (chidu-jmap--required email "id" "Conversation Email object")
      "Email id")
     do
     (unless (gethash remote-id expected)
       (signal 'chidu-jmap-error
               '("Email/get returned an Email outside the Thread")))
     (when (gethash remote-id settled)
       (signal 'chidu-jmap-error
               '("Email/get settled one Conversation Email twice")))
     (let ((summary
            (chidu-jmap-email--decode-view-row
             wire expected nil "Conversation Email object")))
       (unless
           (equal remote-thread-id
                  (chidu-store-email-summary-observation-row-remote-thread-id
                   summary))
         (signal 'chidu-jmap-error
                 '("Conversation Email belongs to another Thread")))
       (puthash
        remote-id
        (chidu-store-conversation-observation-row-create
         :summary-row summary
         :sent-at
         (chidu-jmap-conversation--nullable-date
          (chidu-jmap--required email "sentAt" "Conversation Email object")
          "Email sentAt")
         :message-ids
         (chidu-jmap-email--header-id-vector
          (chidu-jmap--required email "messageId" "Conversation Email object")
          "Email messageId")
         :in-reply-to
         (chidu-jmap-email--header-id-vector
          (chidu-jmap--required email "inReplyTo" "Conversation Email object")
          "Email inReplyTo")
         :references
         (chidu-jmap-email--header-id-vector
          (chidu-jmap--required email "references" "Conversation Email object")
          "Email references"))
        rows-by-id))
     (puthash remote-id t settled))
    (cl-loop
     for wire-id across wire-not-found
     for remote-id = (chidu-jmap--id wire-id "Email/get notFound id")
     do
     (unless (gethash remote-id expected)
       (signal 'chidu-jmap-error
               '("Email/get returned an unrequested notFound id")))
     (when (gethash remote-id settled)
       (signal 'chidu-jmap-error
               '("Email/get settled one Conversation Email twice")))
     (puthash remote-id t settled))
    (unless (= (hash-table-count expected) (hash-table-count settled))
      (signal 'chidu-jmap-error
              '("Email/get did not settle every Thread Email id")))
    (cl-loop for remote-id across remote-email-ids
             when (gethash remote-id rows-by-id)
             do (push (gethash remote-id rows-by-id) rows))
    (when (null rows)
      (signal 'chidu-jmap-error
              '("Conversation has no readable Email objects")))
    (chidu-store-conversation-observation-create
     :remote-thread-id remote-thread-id
     :thread-state thread-state
     :email-state email-state
     :complete-p (zerop (length wire-not-found))
     :rows (vconcat (nreverse rows)))))

(defun chidu-jmap-conversation--finish (fetch result)
  "Complete FETCH exactly once with RESULT and clear its credential."
  (unless (chidu-jmap-conversation-fetch-completed-p fetch)
    (setf (chidu-jmap-conversation-fetch-completed-p fetch) t
          (chidu-jmap-conversation-fetch-request fetch) nil)
    (when-let* ((secret (chidu-jmap-conversation-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-conversation-fetch-secret fetch) nil))
    (unless (chidu-jmap-conversation-fetch-canceled-p fetch)
      (funcall (chidu-jmap-conversation-fetch-deliver fetch) result))))

(defun chidu-jmap-conversation--request
    (fetch body decoder continuation)
  "For FETCH, send BODY, decode with DECODER, then call CONTINUATION."
  (let* ((context (chidu-jmap-conversation-fetch-context fetch))
         (endpoint (chidu-store-conversation-context-endpoint context))
         request)
    (condition-case error-data
        (setq
         request
         (chidu-jmap-http-request
          (chidu-store-endpoint-api-url endpoint)
          (chidu-store-endpoint-login endpoint)
          (chidu-store-endpoint-authentication endpoint)
          (chidu-jmap-conversation-fetch-secret fetch)
          (lambda (result)
            (unless (chidu-jmap-conversation-fetch-completed-p fetch)
              (setf (chidu-jmap-conversation-fetch-request fetch) nil)
              (cond
               ((chidu-result-failure-p result)
                (chidu-jmap-conversation--finish fetch result))
               ((chidu-result-ok-p result)
                (condition-case validation-error
                    (let ((response (chidu-result-ok-value result)))
                      (if (= 200 (chidu-jmap-http-response-status response))
                          (funcall
                           continuation
                           (funcall
                            decoder
                            (chidu-jmap-http-response-body response)))
                        (chidu-jmap-conversation--finish
                         fetch
                         (chidu-result-failure-create
                          :kind 'unexpected-http-status
                          :data
                          (list :status
                                (chidu-jmap-http-response-status response))
                          :retryable-p nil))))
                  (error
                   (chidu-jmap-conversation--finish
                    fetch
                    (chidu-result-failure-create
                     :kind 'invalid-jmap-response
                     :data
                     (list :message
                           (error-message-string validation-error))
                     :retryable-p nil)))))
               (t (chidu-jmap-conversation--finish fetch result)))))
          :body body
          :max-request-bytes
          (chidu-store-endpoint-max-size-request endpoint)
          :byte-cap chidu-jmap-api-byte-cap))
      (error
       (chidu-jmap-conversation--finish
        fetch
        (chidu-result-failure-create
         :kind 'jmap-request-failed
         :data (list :message (error-message-string error-data))
         :retryable-p nil))))
    (when (and request
               (not (chidu-jmap-conversation-fetch-completed-p fetch)))
      (setf (chidu-jmap-conversation-fetch-request fetch) request))))

(defun chidu-jmap-conversation--after-thread (fetch result)
  "Continue FETCH after decoded Thread/get RESULT."
  (if (null result)
      (chidu-jmap-conversation--finish
       fetch
       (chidu-result-failure-create
        :kind 'thread-not-found
        :data
        (list :remote-thread-id
              (chidu-store-conversation-context-remote-thread-id
               (chidu-jmap-conversation-fetch-context fetch)))
        :retryable-p nil))
    (let* ((context (chidu-jmap-conversation-fetch-context fetch))
           (account (chidu-store-conversation-context-account context))
           (remote-account-id
            (chidu-store-account-remote-account-id account))
           (remote-thread-id
            (chidu-store-conversation-context-remote-thread-id context))
           (thread-state (car result))
           (email-ids (cdr result)))
      (setf (chidu-jmap-conversation-fetch-thread-state fetch) thread-state
            (chidu-jmap-conversation-fetch-remote-email-ids fetch) email-ids)
      (chidu-jmap-conversation--request
       fetch
       (chidu-jmap-conversation--email-request
        remote-account-id email-ids)
       (lambda (bytes)
         (chidu-jmap-conversation--validate-emails
          bytes remote-account-id remote-thread-id email-ids thread-state))
       (lambda (observation)
         (chidu-jmap-conversation--finish
          fetch (chidu-result-ok-create :value observation)))))))

(defun chidu-jmap-fetch-conversation (context secret limit deliver)
  "Fetch reply-tree metadata for Conversation CONTEXT using owned SECRET.

LIMIT bounds Thread membership before Email/get.  DELIVER receives one typed
result.  Return a zero-argument cancellation function, or nil after a
synchronous startup failure."
  (unless (chidu-store-conversation-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-conversation-context-p context)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'positive-integer-p limit)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let* ((endpoint (chidu-store-conversation-context-endpoint context))
         (account (chidu-store-conversation-context-account context))
         (server-limit (chidu-store-endpoint-max-objects-in-get endpoint))
         (effective-limit (min limit (or server-limit limit)))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (remote-thread-id
          (chidu-store-conversation-context-remote-thread-id context))
         (fetch
          (chidu-jmap-conversation-fetch-create
           :context context :secret secret :limit effective-limit
           :deliver deliver)))
    (chidu-jmap-conversation--request
     fetch
     (chidu-jmap-conversation--thread-request
      remote-account-id remote-thread-id)
     (lambda (bytes)
       (chidu-jmap-conversation--validate-thread
        bytes remote-account-id remote-thread-id effective-limit))
     (lambda (result)
       (chidu-jmap-conversation--after-thread fetch result)))
    (unless (chidu-jmap-conversation-fetch-completed-p fetch)
      (lambda ()
        (unless (chidu-jmap-conversation-fetch-completed-p fetch)
          (setf (chidu-jmap-conversation-fetch-canceled-p fetch) t)
          (when-let* ((request
                       (chidu-jmap-conversation-fetch-request fetch)))
            (chidu-jmap-http-cancel request))
          (chidu-jmap-conversation--finish
           fetch
           (chidu-result-failure-create
            :kind 'canceled :data nil :retryable-p nil)))))))

(provide 'chidu-jmap-conversation)

;;; chidu-jmap-conversation.el ends here
