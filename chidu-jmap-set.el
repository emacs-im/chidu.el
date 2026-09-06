;;; chidu-jmap-set.el --- Strict JMAP Set update boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; JMAP Set methods may partially succeed per object.  This module owns the
;; shared wire boundary for update-only calls: exact target coverage, method
;; error semantics, SetError decoding, and no accidental acceptance of extra
;; object ids.  Domain workflows remain responsible for durable intent and
;; retry policy.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-http)
(require 'chidu-jmap-types)
(require 'chidu-result)
(require 'chidu-store)
(require 'chidu-record)

(chidu-define-record chidu-jmap-set-target-result
    "One target outcome decoded from a JMAP SetResponse."
  remote-id
  outcome
  error-kind
  error-description)

(chidu-define-record chidu-jmap-set-update-response
    "Exact update settlement returned by one JMAP Set method."
  old-state
  new-state
  (results (vector)))

(defun chidu-jmap-set--nullable-object (value context)
  "Return nullable object VALUE for CONTEXT."
  (if (eq value :json-null)
      nil
    (chidu-jmap--hash value context)))

(defun chidu-jmap-set--nullable-description (object context)
  "Return nullable SetError description from OBJECT for CONTEXT."
  (let ((value (gethash "description" object :json-null)))
    (if (eq value :json-null)
        nil
      (chidu-jmap--string value context t))))

(defun chidu-jmap-set--required-nullable-state
    (arguments key context)
  "Return required nullable state KEY from ARGUMENTS for CONTEXT."
  (let ((value (chidu-jmap--required arguments key context)))
    (if (eq value :json-null)
        nil
      (chidu-jmap--string value context t))))

(defun chidu-jmap-set--method-error-outcome (type)
  "Return safe local settlement outcome for method error TYPE."
  ;; RFC 8620 says a method-level error has made no externally visible change,
  ;; except serverPartialFail, where the server cannot make that guarantee.
  (if (equal type "serverPartialFail") 'unknown 'rejected))

(defun chidu-jmap-set--method-error-results (arguments expected-ids)
  "Return target results for method error ARGUMENTS and EXPECTED-IDS."
  (let* ((type
          (chidu-jmap--string
           (chidu-jmap--required arguments "type" "Set method error")
           "Set method error type"))
         (description
          (chidu-jmap-set--nullable-description
           arguments "Set method error description"))
         (outcome (chidu-jmap-set--method-error-outcome type)))
    (cl-loop
     for remote-id across expected-ids
     collect
     (chidu-jmap-set-target-result-create
      :remote-id remote-id
      :outcome outcome
      :error-kind type
      :error-description description))))

(defun chidu-jmap-set--expected-table (expected-ids)
  "Return exact target table for nonempty EXPECTED-IDS."
  (unless (and (vectorp expected-ids) (> (length expected-ids) 0))
    (signal 'chidu-jmap-error '("Set update requires target ids")))
  (let ((expected (make-hash-table :test #'equal)))
    (cl-loop
     for value across expected-ids
     for remote-id = (chidu-jmap--id value "Set target id")
     do
     (when (gethash remote-id expected)
       (signal 'chidu-jmap-error '("Set update contains a duplicate target id")))
     do (puthash remote-id t expected))
    expected))

(defun chidu-jmap-set--validate-map-keys (object expected settled context)
  "Validate OBJECT keys against EXPECTED and SETTLED for CONTEXT."
  (when object
    (maphash
     (lambda (wire-id _value)
       (let ((remote-id (chidu-jmap--id wire-id context)))
         (unless (gethash remote-id expected)
           (signal 'chidu-jmap-error
                   (list (format "%s returned an unrequested id" context))))
         (when (gethash remote-id settled)
           (signal 'chidu-jmap-error
                   (list (format "%s settled one id more than once" context))))
         (puthash remote-id t settled)))
     object)))

(defun chidu-jmap-set--validate-updated-values (updated)
  "Validate each UPDATED map value as an object or JSON null."
  (when updated
    (maphash
     (lambda (_remote-id value)
       (unless (or (eq value :json-null) (hash-table-p value))
         (signal 'chidu-jmap-error
                 '("SetResponse updated value must be an object or null"))))
     updated)))

(defun chidu-jmap-set--decode-update-results
    (arguments expected-ids expected)
  "Decode exact update results from ARGUMENTS for EXPECTED-IDS and EXPECTED."
  (let* ((updated
          (chidu-jmap-set--nullable-object
           (gethash "updated" arguments :json-null) "SetResponse updated"))
         (not-updated
          (chidu-jmap-set--nullable-object
           (gethash "notUpdated" arguments :json-null)
           "SetResponse notUpdated"))
         (settled (make-hash-table :test #'equal)))
    (chidu-jmap-set--validate-updated-values updated)
    (chidu-jmap-set--validate-map-keys
     updated expected settled "SetResponse updated")
    (chidu-jmap-set--validate-map-keys
     not-updated expected settled "SetResponse notUpdated")
    (unless (= (hash-table-count expected) (hash-table-count settled))
      (signal 'chidu-jmap-error
              '("SetResponse did not settle every requested update")))
    (cl-loop
     for remote-id across expected-ids
     collect
     (if (and updated (gethash remote-id updated))
         (chidu-jmap-set-target-result-create
          :remote-id remote-id :outcome 'succeeded)
       (let* ((wire-error (gethash remote-id not-updated))
              (error-object
               (chidu-jmap--hash wire-error "SetResponse SetError"))
              (type
               (chidu-jmap--string
                (chidu-jmap--required
                 error-object "type" "SetResponse SetError")
                "SetResponse SetError type")))
         (chidu-jmap-set-target-result-create
          :remote-id remote-id
          :outcome 'rejected
          :error-kind type
          :error-description
          (chidu-jmap-set--nullable-description
           error-object "SetResponse SetError description")))))))

(defun chidu-jmap-set-validate-update-response
    (bytes method-name call-id remote-account-id expected-ids)
  "Validate update-only Set response BYTES.

METHOD-NAME and CALL-ID identify the invocation.  REMOTE-ACCOUNT-ID must match
normal responses.  EXPECTED-IDS is the complete target vector; every id must be
settled exactly once and no extra id is accepted."
  (let* ((wire (chidu-jmap--parse-json-object bytes "Set response"))
         (_session-state
          (chidu-jmap--string
           (chidu-jmap--required wire "sessionState" "Set response")
           "Set response sessionState"))
         (responses
          (chidu-jmap--vector
           (chidu-jmap--required wire "methodResponses" "Set response")
           "Set response methodResponses"))
         (expected (chidu-jmap-set--expected-table expected-ids)))
    (unless (= 1 (length responses))
      (signal 'chidu-jmap-error '("Set response has unexpected invocation count")))
    (let ((invocation (aref responses 0)))
      (unless (and (vectorp invocation) (= 3 (length invocation)))
        (signal 'chidu-jmap-error '("Set response invocation is malformed")))
      (let* ((name (chidu-jmap--string (aref invocation 0) "Set method name"))
             (arguments
              (chidu-jmap--hash (aref invocation 1) "Set response arguments"))
             (actual-call-id
              (chidu-jmap--string (aref invocation 2) "Set response call id")))
        (unless (equal actual-call-id call-id)
          (signal 'chidu-jmap-error '("Set response call id mismatch")))
        (if (equal name "error")
            (chidu-jmap-set-update-response-create
             :results
             (vconcat
              (chidu-jmap-set--method-error-results arguments expected-ids)))
          (unless (equal name method-name)
            (signal 'chidu-jmap-error
                    '("Set response returned an unexpected method")))
          (let ((actual-account-id
                 (chidu-jmap--id
                  (chidu-jmap--required arguments "accountId" "SetResponse")
                  "SetResponse accountId")))
            (unless (equal actual-account-id remote-account-id)
              (signal 'chidu-jmap-error '("SetResponse accountId mismatch")))
            (chidu-jmap-set-update-response-create
             :old-state
             (chidu-jmap-set--required-nullable-state
              arguments "oldState" "SetResponse")
             :new-state
             (chidu-jmap--string
              (chidu-jmap--required arguments "newState" "SetResponse")
              "SetResponse newState" t)
             :results
             (vconcat
              (chidu-jmap-set--decode-update-results
               arguments expected-ids expected)))))))))

(defun chidu-jmap-set-result-for-id (response remote-id)
  "Return RESPONSE result for REMOTE-ID, or nil."
  (cl-find remote-id
           (chidu-jmap-set-update-response-results response)
           :key #'chidu-jmap-set-target-result-remote-id
           :test #'equal))

(cl-defstruct (chidu-jmap-set-fetch
               (:constructor chidu-jmap-set-fetch-create))
  "Mechanical state for one cancelable update-only Set request."
  secret
  deliver
  request
  completed-p
  canceled-p)

(defun chidu-jmap-set-update-request
    (remote-account-id call-id updates)
  "Return update-only Email/set request for REMOTE-ACCOUNT-ID.

CALL-ID identifies the invocation and UPDATES is the exact Id-to-PatchObject
map."
  (unless (and (hash-table-p updates) (> (hash-table-count updates) 0))
    (signal 'chidu-jmap-error '("Set update requires a nonempty update map")))
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
    :methodCalls
    [["Email/set"
      (:accountId ,remote-account-id :update ,updates)
      ,call-id]]))

(defun chidu-jmap-set-update-request-size
    (remote-account-id call-id updates)
  "Return encoded UPDATES request size for REMOTE-ACCOUNT-ID and CALL-ID."
  (string-bytes
   (chidu-jmap-http-encode-body
    (chidu-jmap-set-update-request remote-account-id call-id updates))))

(defun chidu-jmap-set--expected-ids (updates)
  "Return deterministic exact target ids from update map UPDATES."
  (vconcat (sort (hash-table-keys updates) #'string<)))

(defun chidu-jmap-set--finish (fetch result)
  "Complete FETCH exactly once with RESULT and clear its credential."
  (unless (chidu-jmap-set-fetch-completed-p fetch)
    (setf (chidu-jmap-set-fetch-completed-p fetch) t
          (chidu-jmap-set-fetch-request fetch) nil)
    (when-let* ((secret (chidu-jmap-set-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-set-fetch-secret fetch) nil))
    (unless (chidu-jmap-set-fetch-canceled-p fetch)
      (funcall (chidu-jmap-set-fetch-deliver fetch) result))))

(defun chidu-jmap-set--cancel (fetch)
  "Cancel FETCH without settling any durable domain target."
  (unless (chidu-jmap-set-fetch-completed-p fetch)
    (setf (chidu-jmap-set-fetch-canceled-p fetch) t
          (chidu-jmap-set-fetch-completed-p fetch) t)
    (when-let* ((request (chidu-jmap-set-fetch-request fetch)))
      (chidu-jmap-http-cancel request)
      (setf (chidu-jmap-set-fetch-request fetch) nil))
    (when-let* ((secret (chidu-jmap-set-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-set-fetch-secret fetch) nil))))

(defun chidu-jmap-set-update
    (endpoint account secret call-id updates deliver)
  "Send one update-only Email/set through ENDPOINT for ACCOUNT.

The adapter owns SECRET.  CALL-ID identifies the method invocation, UPDATES is
the exact target map, and DELIVER receives a typed result containing a strict
`chidu-jmap-set-update-response'.  Return a cancellation function."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (and (stringp call-id) (not (string-empty-p call-id)))
    (signal 'wrong-type-argument (list 'nonempty-string-p call-id)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let* ((remote-account-id
          (chidu-store-account-remote-account-id account))
         (expected-ids (chidu-jmap-set--expected-ids updates))
         (fetch
          (chidu-jmap-set-fetch-create
           :secret secret :deliver deliver)))
    (condition-case error-data
        (setf
         (chidu-jmap-set-fetch-request fetch)
         (chidu-jmap-http-request
          (chidu-store-endpoint-api-url endpoint)
          (chidu-store-endpoint-login endpoint)
          (chidu-store-endpoint-authentication endpoint)
          secret
          (lambda (result)
            (cond
             ((chidu-result-failure-p result)
              (chidu-jmap-set--finish fetch result))
             ((chidu-result-ok-p result)
              (condition-case validation-error
                  (let ((response (chidu-result-ok-value result)))
                    (if (= 200 (chidu-jmap-http-response-status response))
                        (chidu-jmap-set--finish
                         fetch
                         (chidu-result-ok-create
                          :value
                          (chidu-jmap-set-validate-update-response
                           (chidu-jmap-http-response-body response)
                           "Email/set" call-id remote-account-id expected-ids)))
                      (chidu-jmap-set--finish
                       fetch
                       (chidu-result-failure-create
                        :kind 'unexpected-http-status
                        :data
                        (list :status
                              (chidu-jmap-http-response-status response))
                        :retryable-p nil))))
                (error
                 (chidu-jmap-set--finish
                  fetch
                  (chidu-result-failure-create
                   :kind 'invalid-jmap-response
                   :data (list :message
                               (error-message-string validation-error))
                   :retryable-p nil)))))
             (t (chidu-jmap-set--finish fetch result))))
          :body
          (chidu-jmap-set-update-request
           remote-account-id call-id updates)
          :max-request-bytes
          (chidu-store-endpoint-max-size-request endpoint)
          :byte-cap chidu-jmap-api-byte-cap))
      (error
       (chidu-jmap-set--finish
        fetch
        (chidu-result-failure-create
         :kind 'jmap-request-failed
         :data (list :message (error-message-string error-data))
         :retryable-p nil))))
    (apply-partially #'chidu-jmap-set--cancel fetch)))

(provide 'chidu-jmap-set)

;;; chidu-jmap-set.el ends here
