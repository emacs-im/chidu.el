;;; chidu-jmap-body.el --- Bounded JMAP Email display body fetch -*- lexical-binding: t; -*-

;;; Commentary:

;; Fetch only the text/html display body for one selected Email.  Each body
;; value and the complete HTTP response are bounded.  Attachment metadata is fetched through JMAP's standard `attachments'
;; convenience property.  Raw source and full MIME structure materialization
;; remain separate slices; the reader may present attachment-backed cid and
;; Content-Location images without granting SHR arbitrary network access.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-jmap-http)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-result)
(require 'chidu-store)

(defcustom chidu-email-body-value-byte-limit (* 1024 1024)
  "Maximum decoded bytes requested for each selected Email text part."
  :type 'positive-integer
  :group 'chidu)

(defconst chidu-jmap-body-properties
  ["id" "bodyValues" "textBody" "htmlBody" "attachments"]
  "Email properties fetched for selected display content.")

(defconst chidu-jmap-body-part-properties
  ["partId" "blobId" "size" "name" "type" "charset"
   "disposition" "cid" "language" "location"]
  "EmailBodyPart properties fetched for display content and attachments.")

(cl-defstruct (chidu-jmap-body-fetch
               (:constructor chidu-jmap-body-fetch-create))
  "One cancelable selected Email body request."
  secret
  deliver
  request
  completed-p
  canceled-p)

(defun chidu-jmap-body--request
    (remote-account-id remote-email-id body-value-byte-limit)
  "Request REMOTE-EMAIL-ID body in REMOTE-ACCOUNT-ID.

BODY-VALUE-BYTE-LIMIT bounds each decoded part."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
    :methodCalls
    [["Email/get"
      (:accountId ,remote-account-id
       :ids [,remote-email-id]
       :properties ,chidu-jmap-body-properties
       :bodyProperties ,chidu-jmap-body-part-properties
       :fetchTextBodyValues t
       :fetchHTMLBodyValues t
       :maxBodyValueBytes ,body-value-byte-limit)
      "email-body"]]))

(defun chidu-jmap-body--value (wire part-id)
  "Validate body value WIRE for PART-ID and return a small plist."
  (let ((value (chidu-jmap--hash wire "EmailBodyValue")))
    (list
     :value
     (chidu-jmap--string
      (chidu-jmap--required value "value" "EmailBodyValue")
      (format "body value %s" part-id) t)
     :truncated-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required value "isTruncated" "EmailBodyValue")
      "EmailBodyValue isTruncated")
     :encoding-problem-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required value "isEncodingProblem" "EmailBodyValue")
      "EmailBodyValue isEncodingProblem"))))

(defun chidu-jmap-body--values (wire)
  "Validate bodyValues object WIRE and return part-id keyed values."
  (let ((object (chidu-jmap--hash wire "Email bodyValues"))
        (values (make-hash-table :test #'equal)))
    (maphash
     (lambda (part-id value)
       (let ((id (chidu-jmap--string part-id "Email bodyValues partId")))
         (when (gethash id values)
           (signal 'chidu-jmap-error
                   '("Email bodyValues contains a duplicate partId")))
         (puthash id (chidu-jmap-body--value value id) values)))
     object)
    values))

(defun chidu-jmap-body--part (wire context)
  "Validate EmailBodyPart WIRE for CONTEXT and return (PART-ID . TYPE)."
  (let* ((part (chidu-jmap--hash wire context))
         (wire-id
          (chidu-jmap--required part "partId" context))
         (part-id
          (unless (eq wire-id :json-null)
            (chidu-jmap--string wire-id (format "%s partId" context))))
         (type
          (downcase
           (chidu-jmap--string
            (chidu-jmap--required part "type" context)
            (format "%s type" context)))))
    (cons part-id type)))

(defun chidu-jmap-body--part-ids (wire context)
  "Validate body-part list WIRE for CONTEXT and return ordered pairs."
  (let ((parts (chidu-jmap--vector wire context))
        result)
    (cl-loop for item across parts
             do (push (chidu-jmap-body--part item context) result))
    (nreverse result)))

(defun chidu-jmap-body--nullable-string-property (part key context)
  "Return nullable string KEY from body PART for CONTEXT."
  (chidu-jmap--nullable-string
   (chidu-jmap--required part key context)
   (format "%s %s" context key)))

(defun chidu-jmap-body--language (wire context)
  "Return nullable language vector WIRE for CONTEXT."
  (if (eq wire :json-null)
      (vector)
    (let ((values (chidu-jmap--vector wire context))
          result)
      (cl-loop for value across values
               do (push (chidu-jmap--string value context) result))
      (vconcat (nreverse result)))))

(defun chidu-jmap-body--media-type (value context)
  "Return normalized MIME media type VALUE for CONTEXT."
  (chidu-jmap--media-type value context))

(defun chidu-jmap-body--disposition (part context)
  "Return normalized nullable disposition from body PART for CONTEXT."
  (when-let* ((value
               (chidu-jmap-body--nullable-string-property
                part "disposition" context)))
    (let ((disposition (downcase value)))
      (unless (string-match-p "\\`[^[:space:];]+\\'" disposition)
        (signal 'chidu-jmap-error
                (list (format "%s disposition is not a token" context))))
      disposition)))

(defun chidu-jmap-body--attachment (wire ordinal seen)
  "Decode attachment WIRE at ORDINAL, rejecting duplicates in SEEN."
  (let* ((context (format "Email attachment %d" ordinal))
         (part (chidu-jmap--hash wire context))
         (part-id
          (chidu-jmap--string
           (chidu-jmap--required part "partId" context)
           (format "%s partId" context)))
         (blob-id
          (chidu-jmap--id
           (chidu-jmap--required part "blobId" context)
           (format "%s blobId" context)))
         (media-type
          (chidu-jmap-body--media-type
           (chidu-jmap--required part "type" context)
           (format "%s type" context))))
    (when (gethash part-id seen)
      (signal 'chidu-jmap-error
              (list "Email attachments contain a duplicate partId" part-id)))
    (when (string-prefix-p "multipart/" media-type)
      (signal 'chidu-jmap-error
              (list "Email attachments must not contain multipart parts"
                    part-id)))
    (puthash part-id t seen)
    (chidu-store-email-attachment-create
     :part-id part-id
     :blob-id blob-id
     :size
     (chidu-jmap--safe-nonnegative-integer
      (chidu-jmap--required part "size" context)
      (format "%s size" context))
     :name (chidu-jmap-body--nullable-string-property part "name" context)
     :media-type media-type
     :charset
     (chidu-jmap-body--nullable-string-property part "charset" context)
     :disposition (chidu-jmap-body--disposition part context)
     :cid (chidu-jmap-body--nullable-string-property part "cid" context)
     :language
     (chidu-jmap-body--language
      (chidu-jmap--required part "language" context)
      (format "%s language" context))
     :location
     (chidu-jmap-body--nullable-string-property part "location" context))))

(defun chidu-jmap-body--attachments (wire)
  "Decode ordered Email attachment list WIRE."
  (let ((items (chidu-jmap--vector wire "Email attachments"))
        (seen (make-hash-table :test #'equal))
        result)
    (cl-loop for wire-part across items
             for ordinal from 0
             do (push
                 (chidu-jmap-body--attachment wire-part ordinal seen)
                 result))
    (vconcat (nreverse result))))

(defun chidu-jmap-body-decode-content (email email-state context)
  "Decode display content from validated EMAIL for CONTEXT.

EMAIL-STATE is the top-level Email object state, or nil for an `Email/parse'
result that deliberately has no collection identity or state."
  (let* ((values
          (chidu-jmap-body--values
           (chidu-jmap--required email "bodyValues" context)))
         (text-parts
          (chidu-jmap-body--part-ids
           (chidu-jmap--required email "textBody" context)
           (format "%s textBody" context)))
         (html-parts
          (chidu-jmap-body--part-ids
           (chidu-jmap--required email "htmlBody" context)
           (format "%s htmlBody" context)))
         (used (make-hash-table :test #'equal))
         (text
          (chidu-jmap-body--collect
           text-parts values "text/plain" used))
         (html
          (chidu-jmap-body--collect
           html-parts values "text/html" used)))
    (chidu-store-email-body-create
     :email-state email-state
     :text-content (nth 0 text)
     :html-content (nth 0 html)
     :truncated-p (or (nth 1 text) (nth 1 html))
     :encoding-problem-p (or (nth 2 text) (nth 2 html))
     :attachments
     (chidu-jmap-body--attachments
      (chidu-jmap--required email "attachments" context)))))

(defun chidu-jmap-body--collect
    (parts values wanted-type used-parts)
  "Collect WANTED-TYPE from PARTS using VALUES and mark USED-PARTS.

Return (CONTENT TRUNCATED-P ENCODING-PROBLEM-P)."
  (let (strings truncated-p encoding-problem-p)
    (dolist (part parts)
      (pcase-let ((`(,part-id . ,type) part))
        (when (and part-id (equal type wanted-type))
          (let ((value (gethash part-id values)))
            (unless value
              (signal 'chidu-jmap-error
                      (list "Email bodyValues is missing requested part"
                            part-id)))
            (puthash part-id t used-parts)
            (push (plist-get value :value) strings)
            (setq truncated-p
                  (or truncated-p (plist-get value :truncated-p))
                  encoding-problem-p
                  (or encoding-problem-p
                      (plist-get value :encoding-problem-p)))))))
    (list (string-join (nreverse strings) "\n\n")
          (and truncated-p t)
          (and encoding-problem-p t))))

(defun chidu-jmap-body--validate
    (bytes remote-account-id remote-email-id)
  "Validate selected Email body BYTES for REMOTE-ACCOUNT-ID and REMOTE-EMAIL-ID."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Email/get" "email-body" remote-account-id))
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
           "Email/get notFound")))
    (cond
     ((and (zerop (length wire-list))
           (= 1 (length wire-not-found))
           (equal remote-email-id
                  (chidu-jmap--id
                   (aref wire-not-found 0) "Email/get notFound id")))
      nil)
     ((not (and (= 1 (length wire-list))
                (zerop (length wire-not-found))))
      (signal 'chidu-jmap-error
              '("Selected Email/get returned unexpected list coverage")))
     (t
      (let* ((email
              (chidu-jmap--hash
               (aref wire-list 0) "selected Email object"))
             (actual-id
              (chidu-jmap--id
               (chidu-jmap--required email "id" "selected Email object")
               "selected Email id")))
        (unless (equal actual-id remote-email-id)
          (signal 'chidu-jmap-error
                  '("Selected Email/get returned a different id")))
        (let ((body
               (chidu-jmap-body-decode-content
                email email-state "selected Email object")))
          (chidu-store-email-body-observation-create
           :remote-email-id actual-id
           :email-state (chidu-store-email-body-email-state body)
           :text-content (chidu-store-email-body-text-content body)
           :html-content (chidu-store-email-body-html-content body)
           :truncated-p (chidu-store-email-body-truncated-p body)
           :encoding-problem-p
           (chidu-store-email-body-encoding-problem-p body)
           :attachments (chidu-store-email-body-attachments body))))))))

(defun chidu-jmap-body--finish (fetch result)
  "Complete FETCH exactly once with RESULT and clear its credential."
  (unless (chidu-jmap-body-fetch-completed-p fetch)
    (setf (chidu-jmap-body-fetch-completed-p fetch) t
          (chidu-jmap-body-fetch-request fetch) nil)
    (when-let* ((secret (chidu-jmap-body-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-body-fetch-secret fetch) nil))
    (unless (chidu-jmap-body-fetch-canceled-p fetch)
      (funcall (chidu-jmap-body-fetch-deliver fetch) result))))

(defun chidu-jmap-fetch-email-body
    (context secret body-value-byte-limit deliver)
  "Fetch selected Email body for CONTEXT using owned SECRET.

BODY-VALUE-BYTE-LIMIT bounds each decoded text part.  DELIVER receives one
typed result.  Return a zero-argument cancellation function, or nil after a
synchronous startup failure."
  (unless (chidu-store-email-body-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-body-context-p context)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (and (integerp body-value-byte-limit)
               (> body-value-byte-limit 0))
    (signal 'wrong-type-argument
            (list 'positive-integer-p body-value-byte-limit)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let* ((endpoint (chidu-store-email-body-context-endpoint context))
         (account (chidu-store-email-body-context-account context))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (remote-email-id
          (chidu-store-email-body-context-remote-email-id context))
         (fetch
          (chidu-jmap-body-fetch-create
           :secret secret :deliver deliver))
         request)
    (condition-case error-data
        (setq
         request
         (chidu-jmap-http-request
          (chidu-store-endpoint-api-url endpoint)
          (chidu-store-endpoint-login endpoint)
          (chidu-store-endpoint-authentication endpoint)
          secret
          (lambda (result)
            (unless (chidu-jmap-body-fetch-completed-p fetch)
              (setf (chidu-jmap-body-fetch-request fetch) nil)
              (cond
               ((chidu-result-failure-p result)
                (chidu-jmap-body--finish fetch result))
               ((chidu-result-ok-p result)
                (condition-case validation-error
                    (let* ((response (chidu-result-ok-value result))
                           (observation
                            (and (= 200
                                    (chidu-jmap-http-response-status response))
                                 (chidu-jmap-body--validate
                                  (chidu-jmap-http-response-body response)
                                  remote-account-id remote-email-id))))
                      (cond
                       ((/= 200 (chidu-jmap-http-response-status response))
                        (chidu-jmap-body--finish
                         fetch
                         (chidu-result-failure-create
                          :kind 'unexpected-http-status
                          :data
                          (list :status
                                (chidu-jmap-http-response-status response))
                          :retryable-p nil)))
                       ((null observation)
                        (chidu-jmap-body--finish
                         fetch
                         (chidu-result-failure-create
                          :kind 'email-not-found
                          :data (list :remote-email-id remote-email-id)
                          :retryable-p nil)))
                       (t
                        (chidu-jmap-body--finish
                         fetch
                         (chidu-result-ok-create :value observation)))))
                  (error
                   (chidu-jmap-body--finish
                    fetch
                    (chidu-result-failure-create
                     :kind 'invalid-jmap-response
                     :data
                     (list :message
                           (error-message-string validation-error))
                     :retryable-p nil)))))
               (t (chidu-jmap-body--finish fetch result)))))
          :body
          (chidu-jmap-body--request
           remote-account-id remote-email-id body-value-byte-limit)
          :max-request-bytes
          (chidu-store-endpoint-max-size-request endpoint)
          :byte-cap chidu-jmap-api-byte-cap))
      (error
       (chidu-jmap-body--finish
        fetch
        (chidu-result-failure-create
         :kind 'jmap-request-failed
         :data (list :message (error-message-string error-data))
         :retryable-p nil))))
    (when (and request (not (chidu-jmap-body-fetch-completed-p fetch)))
      (setf (chidu-jmap-body-fetch-request fetch) request))
    (unless (chidu-jmap-body-fetch-completed-p fetch)
      (lambda ()
        (unless (chidu-jmap-body-fetch-completed-p fetch)
          (setf (chidu-jmap-body-fetch-canceled-p fetch) t)
          (when-let* ((active (chidu-jmap-body-fetch-request fetch)))
            (chidu-jmap-http-cancel active))
          (chidu-jmap-body--finish
           fetch
           (chidu-result-failure-create
            :kind 'canceled :data nil :retryable-p nil)))))))

(provide 'chidu-jmap-body)

;;; chidu-jmap-body.el ends here
