;;; chidu-jmap-download.el --- JMAP Blob materialization for Compose -*- lexical-binding: t; -*-

;;; Commentary:

;; Download one immutable JMAP Blob into Chidu's private content-addressed
;; Compose resource tree.  A remote Draft checkout is not durable until every
;; attachment has both its original Blob id and exact local SHA-256 evidence.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-compose-resource)
(require 'chidu-jmap-http)
(require 'chidu-jmap-types)
(require 'chidu-result)
(require 'chidu-store)

(defun chidu-jmap-download--name (name)
  "Return a bounded safe presentation NAME for downloadUrl expansion."
  (let ((text
         (and (stringp name)
              (string-trim
               (replace-regexp-in-string
                "[[:cntrl:]\r\n\t]+" " " name)))))
    (if (and text (not (string-empty-p text)))
        (truncate-string-to-width text 240 nil nil "")
      "attachment")))

(defun chidu-jmap-download-url
    (endpoint remote-account-id blob-id media-type name)
  "Expand ENDPOINT's downloadUrl for one immutable Blob.

REMOTE-ACCOUNT-ID owns BLOB-ID.  MEDIA-TYPE and NAME are presentation metadata
used only for the HTTP response; they are not part of the Blob identity."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "download Account id")
        blob-id (chidu-jmap--id blob-id "download Blob id"))
  (setq media-type
        (chidu-jmap--media-type media-type "download media type"))
  ;; RFC 8620 permits a dedicated download origin.  The authenticated Session
  ;; template is the trust boundary; the HTTP layer still enforces HTTPS and
  ;; does not forward credentials across redirect origins.
  (chidu-jmap--expand-url-template
   (chidu-store-endpoint-download-url endpoint)
   "downloadUrl"
   `(("accountId" . ,remote-account-id)
     ("blobId" . ,blob-id)
     ("type" . ,media-type)
     ("name" . ,(chidu-jmap-download--name name)))
   '("accountId" "blobId" "type" "name")))

(defun chidu-jmap-download--failure (kind data &optional retryable-p)
  "Return Blob materialization failure KIND with DATA and RETRYABLE-P."
  (chidu-result-failure-create
   :kind kind :data data :retryable-p retryable-p))

(defun chidu-jmap-download-compose-resource
    (endpoint account resource data-root secret deliver)
  "Materialize remote Compose RESOURCE for ACCOUNT through ENDPOINT.

DATA-ROOT owns Chidu's private content-addressed byte store.  SECRET is owned
by the caller.  DELIVER receives one typed result whose successful value is a
copy of RESOURCE carrying both its original remote Blob id and a local SHA-256
digest.  Return a zero-argument cancellation function while HTTP is live."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-compose-resource-observation-p resource)
    (signal
     'wrong-type-argument
     (list 'chidu-store-compose-resource-observation-p resource)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let* ((expected-size
          (chidu-store-compose-resource-observation-size resource))
         (remote-blob-id
          (chidu-store-compose-resource-observation-remote-blob-id resource))
         (limit (chidu-compose-resource-byte-limit nil))
         target
         process)
    (unless (and (integerp expected-size) (>= expected-size 0))
      (signal 'wrong-type-argument
              (list 'nonnegative-integer-p expected-size)))
    (cond
     ((not remote-blob-id)
      (funcall
       deliver
       (chidu-jmap-download--failure
        'compose-resource-blob-unavailable
        (list
         :resource-id
         (chidu-store-compose-resource-observation-resource-id resource))))
      nil)
     ((> expected-size limit)
      (funcall
       deliver
       (chidu-jmap-download--failure
        'compose-resource-too-large
        (list :actual-bytes expected-size :byte-cap limit)))
      nil)
     (t
      (cl-labels
          ((discard ()
             (when target
               (ignore-errors
                 (chidu-compose-resource-discard-download-target
                  data-root target))))
           (settle (result)
             (cond
              ((chidu-result-failure-p result)
               (discard)
               (funcall deliver result))
              ((chidu-result-ok-p result)
               (condition-case error-data
                   (let ((digest
                          (chidu-compose-resource-install-download
                           data-root
                           (chidu-result-ok-value result)
                           expected-size)))
                     (discard)
                     (funcall
                      deliver
                      (chidu-result-ok-create
                       :value
                       (chidu-store-compose-resource-observation-with
                        resource :digest digest))))
                 (error
                  (discard)
                  (funcall
                   deliver
                   (chidu-jmap-download--failure
                    'compose-resource-materialization-failed
                    (list :message (error-message-string error-data)))))))
              (t
               (discard)
               (funcall
                deliver
                (chidu-jmap-download--failure
                 'invalid-result (list :value result)))))))
        (condition-case error-data
            (progn
              (setq target
                    (chidu-compose-resource-download-target data-root)
                    process
                    (chidu-jmap-http-download-file
                     (chidu-jmap-download-url
                      endpoint
                      (chidu-store-account-remote-account-id account)
                      remote-blob-id
                      (chidu-store-compose-resource-observation-media-type
                       resource)
                      (chidu-store-compose-resource-observation-name resource))
                     (chidu-store-endpoint-login endpoint)
                     (chidu-store-endpoint-authentication endpoint)
                     secret target #'settle
                     :max-download-bytes (max 1 expected-size)
                     :accept
                     (chidu-store-compose-resource-observation-media-type
                      resource)))
              (when process
                (lambda ()
                  (chidu-jmap-http-cancel process)
                  (discard))))
          (error
           (discard)
           (funcall
            deliver
            (chidu-jmap-download--failure
             'compose-resource-materialization-failed
             (list :message (error-message-string error-data))))
           nil)))))))

(provide 'chidu-jmap-download)

;;; chidu-jmap-download.el ends here
