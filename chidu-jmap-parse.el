;;; chidu-jmap-parse.el --- Bounded JMAP attached-message parsing -*- lexical-binding: t; -*-

;;; Commentary:

;; Parse one account-scoped Blob as an RFC message with JMAP `Email/parse'.
;; The result is a read-only message shape, not a top-level Email identity:
;; Chidu deliberately does not request or synthesize id, mailboxIds, keywords,
;; or receivedAt.  Display content shares the same strict body/attachment codec
;; as selected `Email/get'.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-body)
(require 'chidu-jmap-email)
(require 'chidu-jmap-http)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-result)
(require 'chidu-store)

(defconst chidu-jmap-parse-profile-base "parsed-message-v1"
  "Version of Chidu's structured attached-message parse profile.")

(defun chidu-jmap-parse-profile-version (body-value-byte-limit)
  "Return parse profile id for BODY-VALUE-BYTE-LIMIT."
  (unless (and (integerp body-value-byte-limit)
               (> body-value-byte-limit 0))
    (signal 'wrong-type-argument
            (list 'positive-integer-p body-value-byte-limit)))
  (format "%s:%d" chidu-jmap-parse-profile-base body-value-byte-limit))

(defconst chidu-jmap-parse-properties
  ["messageId" "inReplyTo" "references"
   "sender" "from" "to" "cc" "bcc" "replyTo"
   "subject" "sentAt" "preview"
   "bodyValues" "textBody" "htmlBody" "attachments"]
  "Email properties fetched for one parsed attached message.")

(cl-defstruct (chidu-jmap-parse-fetch
               (:constructor chidu-jmap-parse-fetch-create))
  "One cancelable Email/parse request."
  secret
  deliver
  request
  completed-p
  canceled-p)

(defun chidu-jmap-parse--request
    (remote-account-id blob-id body-value-byte-limit)
  "Return Email/parse request for BLOB-ID in REMOTE-ACCOUNT-ID.

BODY-VALUE-BYTE-LIMIT bounds each decoded text or HTML part."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Email/parse"
             (:accountId ,remote-account-id
                         :blobIds [,blob-id]
                         :properties ,chidu-jmap-parse-properties
                         :bodyProperties ,chidu-jmap-body-part-properties
                         :fetchTextBodyValues t
                         :fetchHTMLBodyValues t
                         :maxBodyValueBytes ,body-value-byte-limit)
             "email-parse"]]))

(defun chidu-jmap-parse--nullable-date (value context)
  "Return nil for JSON null or copied date string VALUE for CONTEXT."
  (if (eq value :json-null)
      nil
    (chidu-jmap--string value context)))

(defun chidu-jmap-parse--nullable-id-map (value context)
  "Return validated JMAP Id keyed object VALUE for CONTEXT.

JSON null is normalized to an empty hash table."
  (if (eq value :json-null)
      (make-hash-table :test #'equal)
    (let ((wire (chidu-jmap--hash value context))
          (result (make-hash-table :test #'equal)))
      (maphash
       (lambda (wire-id item)
         (let ((id (chidu-jmap--id wire-id context)))
           (when (gethash id result)
             (signal 'chidu-jmap-error
                     (list (format "%s contains a duplicate id" context))))
           (puthash id item result)))
       wire)
      result)))

(defun chidu-jmap-parse--decode-message (wire blob-id profile-version)
  "Decode parsed Email WIRE for BLOB-ID and PROFILE-VERSION."
  (let* ((email (chidu-jmap--hash wire "parsed Email object"))
         (subject-value
          (chidu-jmap--required email "subject" "parsed Email object"))
         (preview-value (gethash "preview" email ""))
         (body
          (chidu-jmap-body-decode-content
           email nil "parsed Email object")))
    (chidu-store-parsed-blob-observation-create
     :blob-id blob-id
     :profile-version profile-version
     :message
     (chidu-store-parsed-message-create
      :message-ids
      (chidu-jmap-email--header-id-vector
       (chidu-jmap--required email "messageId" "parsed Email object")
       "parsed Email messageId")
      :in-reply-to
      (chidu-jmap-email--header-id-vector
       (chidu-jmap--required email "inReplyTo" "parsed Email object")
       "parsed Email inReplyTo")
      :references
      (chidu-jmap-email--header-id-vector
       (chidu-jmap--required email "references" "parsed Email object")
       "parsed Email references")
      :sender
      (chidu-jmap-email--address-vector
       (chidu-jmap--required email "sender" "parsed Email object")
       "parsed Email sender")
      :from
      (chidu-jmap-email--address-vector
       (chidu-jmap--required email "from" "parsed Email object")
       "parsed Email from")
      :to
      (chidu-jmap-email--address-vector
       (chidu-jmap--required email "to" "parsed Email object")
       "parsed Email to")
      :cc
      (chidu-jmap-email--address-vector
       (chidu-jmap--required email "cc" "parsed Email object")
       "parsed Email cc")
      :bcc
      (chidu-jmap-email--address-vector
       (chidu-jmap--required email "bcc" "parsed Email object")
       "parsed Email bcc")
      :reply-to
      (chidu-jmap-email--address-vector
       (chidu-jmap--required email "replyTo" "parsed Email object")
       "parsed Email replyTo")
      :subject
      (if (eq subject-value :json-null)
          ""
        (chidu-jmap--string subject-value "parsed Email subject" t))
      :sent-at
      (chidu-jmap-parse--nullable-date
       (chidu-jmap--required email "sentAt" "parsed Email object")
       "parsed Email sentAt")
      :preview
      (if (eq preview-value :json-null)
          ""
        (chidu-jmap--string preview-value "parsed Email preview" t))
      :body body))))

(defun chidu-jmap-parse--decode
    (bytes remote-account-id blob-id profile-version)
  "Decode Email/parse BYTES for REMOTE-ACCOUNT-ID and BLOB-ID.

PROFILE-VERSION identifies the exact local materialization profile.  Return a
parsed observation, `not-parsable', or `not-found'."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Email/parse" "email-parse" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (parsed
          (chidu-jmap-parse--nullable-id-map
           (chidu-jmap--required arguments "parsed" "Email/parse")
           "Email/parse parsed"))
         ;; Stalwart omits these empty nullable settlement collections on a
         ;; successful parse.  Normalize omission exactly like JSON null, but
         ;; keep `parsed' required and retain the exact-once settlement proof
         ;; below so no requested Blob can disappear silently.
         (not-parsable
          (chidu-jmap-email--nullable-id-vector
           (gethash "notParsable" arguments :json-null)
           "Email/parse notParsable"))
         (not-found
          (chidu-jmap-email--nullable-id-vector
           (gethash "notFound" arguments :json-null)
           "Email/parse notFound"))
         (settlements 0)
         parsed-value)
    (maphash
     (lambda (id value)
       (unless (equal id blob-id)
         (signal 'chidu-jmap-error
                 '("Email/parse returned an unrequested parsed Blob")))
       (setq settlements (1+ settlements)
             parsed-value value))
     parsed)
    (cl-loop
     for id across not-parsable
     do
     (unless (equal id blob-id)
       (signal 'chidu-jmap-error
               '("Email/parse returned an unrequested notParsable Blob")))
     (setq settlements (1+ settlements)))
    (cl-loop
     for id across not-found
     do
     (unless (equal id blob-id)
       (signal 'chidu-jmap-error
               '("Email/parse returned an unrequested notFound Blob")))
     (setq settlements (1+ settlements)))
    (unless (= settlements 1)
      (signal 'chidu-jmap-error
              '("Email/parse did not settle the requested Blob exactly once")))
    (cond
     (parsed-value
      (chidu-jmap-parse--decode-message
       parsed-value blob-id profile-version))
     ((= (length not-parsable) 1) 'not-parsable)
     ((= (length not-found) 1) 'not-found)
     (t
      (signal 'chidu-jmap-error
              '("Email/parse returned an impossible settlement"))))))

(defun chidu-jmap-parse--finish (fetch result)
  "Complete FETCH exactly once with RESULT and clear its credential."
  (unless (chidu-jmap-parse-fetch-completed-p fetch)
    (setf (chidu-jmap-parse-fetch-completed-p fetch) t
          (chidu-jmap-parse-fetch-request fetch) nil)
    (when-let* ((secret (chidu-jmap-parse-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-parse-fetch-secret fetch) nil))
    (unless (chidu-jmap-parse-fetch-canceled-p fetch)
      (funcall (chidu-jmap-parse-fetch-deliver fetch) result))))

(defun chidu-jmap-fetch-parsed-blob
    (context secret body-value-byte-limit deliver)
  "Parse Blob from CONTEXT using owned SECRET.

BODY-VALUE-BYTE-LIMIT bounds each decoded display part.  DELIVER receives one
typed result.  Return a zero-argument cancellation function, or nil after a
synchronous startup failure."
  (unless (chidu-store-parsed-blob-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-parsed-blob-context-p context)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (and (integerp body-value-byte-limit)
               (> body-value-byte-limit 0))
    (signal 'wrong-type-argument
            (list 'positive-integer-p body-value-byte-limit)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let* ((endpoint (chidu-store-parsed-blob-context-endpoint context))
         (account (chidu-store-parsed-blob-context-account context))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (blob-id (chidu-store-parsed-blob-context-blob-id context))
         (profile-version
          (chidu-store-parsed-blob-context-profile-version context))
         (expected-profile
          (chidu-jmap-parse-profile-version body-value-byte-limit))
         (fetch
          (chidu-jmap-parse-fetch-create :secret secret :deliver deliver))
         request)
    (unless (equal profile-version expected-profile)
      (signal 'chidu-invariant-error
              (list "Parsed Blob context profile does not match request"
                    profile-version expected-profile)))
    (condition-case error-data
        (setq
         request
         (chidu-jmap-http-request
          (chidu-store-endpoint-api-url endpoint)
          (chidu-store-endpoint-login endpoint)
          (chidu-store-endpoint-authentication endpoint)
          secret
          (lambda (result)
            (unless (chidu-jmap-parse-fetch-completed-p fetch)
              (setf (chidu-jmap-parse-fetch-request fetch) nil)
              (cond
               ((chidu-result-failure-p result)
                (chidu-jmap-parse--finish fetch result))
               ((chidu-result-ok-p result)
                (condition-case validation-error
                    (let ((response (chidu-result-ok-value result)))
                      (if (/= 200 (chidu-jmap-http-response-status response))
                          (chidu-jmap-parse--finish
                           fetch
                           (chidu-result-failure-create
                            :kind 'unexpected-http-status
                            :data
                            (list :status
                                  (chidu-jmap-http-response-status response))
                            :retryable-p nil))
                        (pcase
                            (chidu-jmap-parse--decode
                             (chidu-jmap-http-response-body response)
                             remote-account-id blob-id profile-version)
                          ('not-parsable
                           (chidu-jmap-parse--finish
                            fetch
                            (chidu-result-failure-create
                             :kind 'blob-not-parsable
                             :data (list :blob-id blob-id)
                             :retryable-p nil)))
                          ('not-found
                           (chidu-jmap-parse--finish
                            fetch
                            (chidu-result-failure-create
                             :kind 'blob-not-found
                             :data (list :blob-id blob-id)
                             :retryable-p nil)))
                          (observation
                           (chidu-jmap-parse--finish
                            fetch
                            (chidu-result-ok-create
                             :value observation))))))
                  (error
                   (chidu-jmap-parse--finish
                    fetch
                    (chidu-result-failure-create
                     :kind 'invalid-jmap-response
                     :data
                     (list :message
                           (error-message-string validation-error))
                     :retryable-p nil)))))
               (t (chidu-jmap-parse--finish fetch result)))))
          :body
          (chidu-jmap-parse--request
           remote-account-id blob-id body-value-byte-limit)
          :max-request-bytes
          (chidu-store-endpoint-max-size-request endpoint)
          :byte-cap chidu-jmap-api-byte-cap))
      (error
       (chidu-jmap-parse--finish
        fetch
        (chidu-result-failure-create
         :kind 'jmap-request-failed
         :data (list :message (error-message-string error-data))
         :retryable-p nil))))
    (when (and request (not (chidu-jmap-parse-fetch-completed-p fetch)))
      (setf (chidu-jmap-parse-fetch-request fetch) request))
    (unless (chidu-jmap-parse-fetch-completed-p fetch)
      (lambda ()
        (unless (chidu-jmap-parse-fetch-completed-p fetch)
          (setf (chidu-jmap-parse-fetch-canceled-p fetch) t)
          (when-let* ((active (chidu-jmap-parse-fetch-request fetch)))
            (chidu-jmap-http-cancel active))
          (chidu-jmap-parse--finish
           fetch
           (chidu-result-failure-create
            :kind 'canceled :data nil :retryable-p nil)))))))

(provide 'chidu-jmap-parse)

;;; chidu-jmap-parse.el ends here
