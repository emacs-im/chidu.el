;;; chidu-jmap-upload.el --- JMAP Blob upload for Compose resources -*- lexical-binding: t; -*-

;;; Commentary:

;; Upload one exact managed Compose resource through the Session uploadUrl.
;; Blob data is immutable, so a lost response is safely handled by uploading
;; the same verified local bytes again.  Durable publication state belongs to
;; Email/set; this adapter only validates and returns confirmed Blob evidence.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-jmap-http)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-store)

(chidu-define-record chidu-jmap-upload-result
    "One validated JMAP Blob upload response."
  blob-id
  media-type
  size)

(defun chidu-jmap-upload-url (endpoint remote-account-id)
  "Expand ENDPOINT's uploadUrl for REMOTE-ACCOUNT-ID."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "upload Account id"))
  ;; RFC 8620 permits a dedicated upload origin.  The authenticated Session
  ;; template is the trust boundary; the HTTP layer still enforces HTTPS.
  (chidu-jmap--expand-url-template
   (chidu-store-endpoint-upload-url endpoint)
   "uploadUrl"
   `(("accountId" . ,remote-account-id))
   '("accountId")))

(defun chidu-jmap-upload-validate-response
    (bytes remote-account-id expected-media-type expected-size)
  "Validate JMAP upload BYTES for REMOTE-ACCOUNT-ID.

EXPECTED-MEDIA-TYPE and EXPECTED-SIZE must match the response."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "upload Account id"))
  (unless (and (stringp expected-media-type)
               (string-match-p
                "\\`[^[:space:]/;]+/[^[:space:]/;]+\\'"
                expected-media-type))
    (signal 'wrong-type-argument
            (list 'media-type-p expected-media-type)))
  (unless (and (integerp expected-size) (>= expected-size 0))
    (signal 'wrong-type-argument
            (list 'nonnegative-integer-p expected-size)))
  (let* ((wire (chidu-jmap--parse-json-object bytes "JMAP upload response"))
         (account-id
          (chidu-jmap--id
           (chidu-jmap--required wire "accountId" "JMAP upload response")
           "upload accountId"))
         (blob-id
          (chidu-jmap--id
           (chidu-jmap--required wire "blobId" "JMAP upload response")
           "upload blobId"))
         (media-type
          (downcase
           (chidu-jmap--string
            (chidu-jmap--required wire "type" "JMAP upload response")
            "upload type")))
         (size
          (chidu-jmap--safe-nonnegative-integer
           (chidu-jmap--required wire "size" "JMAP upload response")
           "upload size")))
    (unless (equal account-id remote-account-id)
      (signal 'chidu-jmap-error '("JMAP upload accountId mismatch")))
    (unless (equal media-type (downcase expected-media-type))
      (signal 'chidu-jmap-error '("JMAP upload media type mismatch")))
    (unless (= size expected-size)
      (signal 'chidu-jmap-error '("JMAP upload size mismatch")))
    (chidu-jmap-upload-result-create
     :blob-id blob-id :media-type media-type :size size)))

(defun chidu-jmap-upload-compose-resource
    (endpoint account resource file secret deliver)
  "Upload exact local RESOURCE FILE for ACCOUNT through ENDPOINT.

SECRET is owned by the caller.  DELIVER receives one typed result.  Return a
zero-argument cancellation function while the HTTP request remains live."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-compose-resource-p resource)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-resource-p resource)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let* ((remote-account-id
          (chidu-store-account-remote-account-id account))
         (media-type
          (chidu-jmap--media-type
           (chidu-store-compose-resource-media-type resource)
           "Compose resource media type"))
         (expected-size (chidu-store-compose-resource-size resource))
         (maximum (chidu-store-endpoint-max-size-upload endpoint))
         process)
    (setq
     process
     (chidu-jmap-http-upload-file
      (chidu-jmap-upload-url endpoint remote-account-id)
      (chidu-store-endpoint-login endpoint)
      (chidu-store-endpoint-authentication endpoint)
      secret file media-type
      (lambda (result)
        (cond
         ((chidu-result-failure-p result) (funcall deliver result))
         ((chidu-result-ok-p result)
          (condition-case error-data
              (let ((response (chidu-result-ok-value result)))
                (if (/= 201 (chidu-jmap-http-response-status response))
                    (funcall
                     deliver
                     (chidu-result-failure-create
                      :kind 'unexpected-http-status
                      :data
                      (list :status
                            (chidu-jmap-http-response-status response))
                      :retryable-p
                      (>= (chidu-jmap-http-response-status response) 500)))
                  (funcall
                   deliver
                   (chidu-result-ok-create
                    :value
                    (chidu-jmap-upload-validate-response
                     (chidu-jmap-http-response-body response)
                     remote-account-id media-type expected-size)))))
            (error
             (funcall
              deliver
              (chidu-result-failure-create
               :kind 'invalid-jmap-response
               :data (list :message (error-message-string error-data))
               :retryable-p nil)))))
         (t
          (funcall
           deliver
           (chidu-result-failure-create
            :kind 'invalid-result :data (list :value result)
            :retryable-p nil)))))
      :max-upload-bytes maximum
      :byte-cap chidu-jmap-upload-response-byte-cap))
    (when process
      (lambda () (chidu-jmap-http-cancel process)))))

(provide 'chidu-jmap-upload)

;;; chidu-jmap-upload.el ends here
