;;; chidu-jmap-discovery.el --- JMAP Session and Identity discovery -*- lexical-binding: t; -*-

;;; Commentary:

;; Bounded Session discovery and Identity/get workflow built on `plz'.  It
;; validates wire data into Store observations but never mutates Store or
;; application state.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'chidu-jmap-http)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-store)

(cl-defstruct (chidu-jmap-discovery
               (:constructor chidu-jmap-discovery-create))
  "Mutable mechanical state for one discovery effect."
  endpoint
  secret
  deliver
  observation
  pending-accounts
  active-request
  completed-p
  canceled-p)

(defun chidu-jmap--validate-core-capability (capabilities)
  "Validate required core CAPABILITIES and return JMAP request limits.

Return `[MAX-SIZE-REQUEST MAX-SIZE-UPLOAD MAX-OBJECTS-IN-GET
MAX-OBJECTS-IN-SET]'."
  (let* ((core
          (chidu-jmap--hash
           (chidu-jmap--required
            capabilities chidu-jmap-core-capability "Session capabilities")
           "core capability"))
         (max-size-request
          (chidu-jmap--safe-positive-integer
           (chidu-jmap--required
            core "maxSizeRequest" "core capability")
           "maxSizeRequest"))
         (max-size-upload
          (chidu-jmap--safe-positive-integer
           (chidu-jmap--required
            core "maxSizeUpload" "core capability")
           "maxSizeUpload"))
         (max-objects-in-get
          (chidu-jmap--safe-positive-integer
           (chidu-jmap--required
            core "maxObjectsInGet" "core capability")
           "maxObjectsInGet"))
         (max-objects-in-set
          (chidu-jmap--safe-positive-integer
           (chidu-jmap--required
            core "maxObjectsInSet" "core capability")
           "maxObjectsInSet")))
    (dolist (key '("maxConcurrentUpload"
                   "maxConcurrentRequests" "maxCallsInRequest"))
      (chidu-jmap--safe-positive-integer
       (chidu-jmap--required core key "core capability") key))
    (let ((collations
           (chidu-jmap--vector
            (chidu-jmap--required
             core "collationAlgorithms" "core capability")
            "collationAlgorithms")))
      (cl-loop for collation across collations
               do (chidu-jmap--string collation "collation algorithm")))
    (vector max-size-request max-size-upload
            max-objects-in-get max-objects-in-set)))

(defun chidu-jmap--validate-session (bytes final-session-url)
  "Validate Session JSON BYTES fetched from FINAL-SESSION-URL."
  (let* ((wire (chidu-jmap--parse-json-object bytes "JMAP Session"))
         (capabilities
          (chidu-jmap--hash
           (chidu-jmap--required wire "capabilities" "JMAP Session")
           "Session capabilities"))
         (object-limits
          (chidu-jmap--validate-core-capability capabilities))
         (max-size-request (aref object-limits 0))
         (max-size-upload (aref object-limits 1))
         (max-objects-in-get (aref object-limits 2))
         (max-objects-in-set (aref object-limits 3))
         (accounts-wire
          (chidu-jmap--hash
           (chidu-jmap--required wire "accounts" "JMAP Session")
           "Session accounts"))
         (primary
          (chidu-jmap--hash
           (chidu-jmap--required wire "primaryAccounts" "JMAP Session")
           "primaryAccounts"))
         (session-url (chidu-jmap--parse-url final-session-url "Session URL"))
         (api-url-text
          (chidu-jmap--string
           (chidu-jmap--required wire "apiUrl" "JMAP Session")
           "apiUrl"))
         (api-url (chidu-jmap--parse-url api-url-text "apiUrl"))
         (primary-mail
          (when-let* ((value (gethash chidu-jmap-mail-capability primary)))
            (chidu-jmap--id value "primary mail Account id")))
         (primary-submission
          (when-let* ((value
                       (gethash chidu-jmap-submission-capability primary)))
            (chidu-jmap--id value "primary submission Account id")))
         (primary-contacts
          (when-let* ((value
                       (gethash chidu-jmap-contacts-capability primary)))
            (chidu-jmap--id value "primary contacts Account id")))
         accounts)
    (unless (gethash chidu-jmap-mail-capability capabilities)
      (signal 'chidu-jmap-error
              '("Session does not advertise JMAP Mail")))
    (unless (chidu-jmap--same-origin-p session-url api-url)
      (signal 'chidu-jmap-error
              '("apiUrl changes authentication origin")))
    (when (and primary-submission
               (not (gethash chidu-jmap-submission-capability capabilities)))
      (signal 'chidu-jmap-error
              '("primary submission Account exists without capability")))
    (when primary-contacts
      (unless (gethash chidu-jmap-contacts-capability capabilities)
        (signal 'chidu-jmap-error
                '("primary contacts Account exists without capability")))
      (let* ((wire-account
              (gethash primary-contacts accounts-wire))
             (account
              (and wire-account
                   (chidu-jmap--hash
                    wire-account "primary contacts Account")))
             (account-capabilities
              (and account
                   (chidu-jmap--hash
                    (chidu-jmap--required
                     account "accountCapabilities" "primary contacts Account")
                    "primary contacts Account capabilities"))))
        (unless (and account-capabilities
                     (gethash chidu-jmap-contacts-capability
                              account-capabilities))
          (signal 'chidu-jmap-error
                  '("primary contacts Account is invalid")))))
    (maphash
     (lambda (remote-id account-wire)
       (let* ((account
               (chidu-jmap--hash account-wire "Session Account"))
              (account-capabilities
               (chidu-jmap--hash
                (chidu-jmap--required
                 account "accountCapabilities" "Session Account")
                "Account capabilities")))
         (when-let* ((mail-capability
                      (gethash chidu-jmap-mail-capability account-capabilities)))
           (setq mail-capability
                 (chidu-jmap--hash mail-capability "Account mail capability"))
           (push
            (chidu-store-account-observation-create
             :remote-account-id
             (chidu-jmap--id remote-id "remote Account id")
             :name
             (chidu-jmap--string
              (chidu-jmap--required account "name" "Session Account")
              "Account name" t)
             :personal-p
             (chidu-jmap--json-boolean
              (chidu-jmap--required
               account "isPersonal" "Session Account")
              "isPersonal")
             :read-only-p
             (chidu-jmap--json-boolean
              (chidu-jmap--required
               account "isReadOnly" "Session Account")
              "isReadOnly")
             :primary-mail-p (equal primary-mail remote-id)
             :primary-submission-p
             (equal primary-submission remote-id)
             :identity-state nil
             :max-size-attachments-per-email
             (chidu-jmap--safe-nonnegative-integer
              (chidu-jmap--required
               mail-capability "maxSizeAttachmentsPerEmail"
               "Account mail capability")
              "maxSizeAttachmentsPerEmail")
             :capabilities
             (chidu-jmap--capability-names
              account-capabilities "Account capabilities")
             :identities (vector))
            accounts))))
     accounts-wire)
    (setq accounts
          (sort accounts
                (lambda (left right)
                  (string<
                   (chidu-store-account-observation-remote-account-id left)
                   (chidu-store-account-observation-remote-account-id right)))))
    (unless accounts
      (signal 'chidu-jmap-error
              '("Session exposes no mail-capable Account")))
    (when (and primary-mail
               (not
                (seq-find
                 (lambda (account)
                   (equal
                    primary-mail
                    (chidu-store-account-observation-remote-account-id
                     account)))
                 accounts)))
      (signal 'chidu-jmap-error
              '("primary mail Account is not mail-capable")))
    (when (and primary-submission
               (not
                (seq-find
                 (lambda (account)
                   (and
                    (equal
                     primary-submission
                     (chidu-store-account-observation-remote-account-id
                      account))
                    (seq-contains-p
                     (chidu-store-account-observation-capabilities account)
                     chidu-jmap-submission-capability
                     #'equal)))
                 accounts)))
      (signal 'chidu-jmap-error
              '("primary submission Account is invalid")))
    (chidu-store-session-observation-create
     :username
     (chidu-jmap--string
      (chidu-jmap--required wire "username" "JMAP Session")
      "Session username")
     :state
     (chidu-jmap--string
      (chidu-jmap--required wire "state" "JMAP Session")
      "Session state")
     :api-url api-url-text
     :download-url
     (chidu-jmap--https-template
      (chidu-jmap--required wire "downloadUrl" "JMAP Session")
      "downloadUrl")
     :upload-url
     (chidu-jmap--https-template
      (chidu-jmap--required wire "uploadUrl" "JMAP Session")
      "uploadUrl")
     :event-source-url
     (chidu-jmap--https-template
      (chidu-jmap--required wire "eventSourceUrl" "JMAP Session")
      "eventSourceUrl")
     :max-size-request max-size-request
     :max-size-upload max-size-upload
     :max-objects-in-get max-objects-in-get
     :max-objects-in-set max-objects-in-set
     :primary-contacts-remote-account-id primary-contacts
     :capabilities
     (chidu-jmap--capability-names capabilities "Session capabilities")
     :accounts (vconcat accounts))))

(defun chidu-jmap--identity-request (account-id)
  "Return bounded Identity/get request for ACCOUNT-ID."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-submission-capability]
    :methodCalls
    [["Identity/get"
      (:accountId ,account-id
       :properties ["id" "name" "email" "mayDelete"])
      "identity-get"]]))

(defun chidu-jmap--validate-identities (bytes account-id)
  "Validate Identity/get JSON BYTES for ACCOUNT-ID.

Return (STATE . IDENTITIES-VECTOR)."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Identity/get" "identity-get" account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (state
          (chidu-jmap--string
           (chidu-jmap--required arguments "state" "Identity/get")
           "Identity state"))
         (not-found
          (chidu-jmap--vector
           (chidu-jmap--required arguments "notFound" "Identity/get")
           "Identity notFound"))
         (list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Identity/get")
           "Identity list"))
         (seen (make-hash-table :test #'equal))
         identities)
    (unless (zerop (length not-found))
      (signal 'chidu-jmap-error
              '("Identity/get unexpectedly returned notFound")))
    (cl-loop
     for identity-wire across list
     do
     (let* ((identity (chidu-jmap--hash identity-wire "Identity"))
            (remote-id
             (chidu-jmap--id
              (chidu-jmap--required identity "id" "Identity")
              "Identity id")))
       (when (gethash remote-id seen)
         (signal 'chidu-jmap-error
                 '("Identity/get returned duplicate id")))
       (puthash remote-id t seen)
       (push
        (chidu-store-identity-observation-create
         :remote-identity-id remote-id
         :name
         (chidu-jmap--string
          (chidu-jmap--required identity "name" "Identity")
          "Identity name" t)
         :email
         (chidu-jmap--string
          (chidu-jmap--required identity "email" "Identity")
          "Identity email"))
        identities)))
    (cons
     state
     (vconcat
      (sort identities
            (lambda (left right)
              (string<
               (chidu-store-identity-observation-remote-identity-id left)
               (chidu-store-identity-observation-remote-identity-id right))))))))

(defun chidu-jmap--replace-account-identities
    (observation account-id identity-state identities)
  "Return OBSERVATION with ACCOUNT-ID data set to IDENTITY-STATE and IDENTITIES."
  (chidu-store-session-observation-with
   observation
   :accounts
   (cl-map
    'vector
    (lambda (account)
      (if (equal account-id
                 (chidu-store-account-observation-remote-account-id account))
          (chidu-store-account-observation-with
           account :identity-state identity-state :identities identities)
        account))
    (chidu-store-session-observation-accounts observation))))

(defun chidu-jmap--finish-discovery (discovery result)
  "Complete DISCOVERY exactly once with RESULT."
  (unless (chidu-jmap-discovery-completed-p discovery)
    (setf (chidu-jmap-discovery-completed-p discovery) t)
    (when-let* ((secret (chidu-jmap-discovery-secret discovery)))
      (clear-string secret)
      (setf (chidu-jmap-discovery-secret discovery) nil))
    (unless (chidu-jmap-discovery-canceled-p discovery)
      (funcall (chidu-jmap-discovery-deliver discovery) result))))

(defun chidu-jmap--fail-discovery (discovery result)
  "Fail DISCOVERY using typed RESULT."
  (chidu-jmap--finish-discovery
   discovery
   (if (chidu-result-failure-p result)
       result
     (chidu-result-failure-create
      :kind 'jmap-discovery-failed
      :data nil
      :retryable-p nil))))

(defun chidu-jmap--discover-next-identity (discovery)
  "Fetch the next submission Account identity set for DISCOVERY."
  (let ((pending (chidu-jmap-discovery-pending-accounts discovery)))
    (if (null pending)
        (chidu-jmap--finish-discovery
         discovery
         (chidu-result-ok-create
          :value (chidu-jmap-discovery-observation discovery)))
      (let* ((account (car pending))
             (account-id
              (chidu-store-account-observation-remote-account-id account))
             (observation (chidu-jmap-discovery-observation discovery)))
        (setf (chidu-jmap-discovery-pending-accounts discovery) (cdr pending))
        (setf
         (chidu-jmap-discovery-active-request discovery)
         (chidu-jmap-http-request
          (chidu-store-session-observation-api-url observation)
          (chidu-store-endpoint-login
           (chidu-jmap-discovery-endpoint discovery))
          (chidu-store-endpoint-authentication
           (chidu-jmap-discovery-endpoint discovery))
          (chidu-jmap-discovery-secret discovery)
          (lambda (result)
            (cond
             ((chidu-result-failure-p result)
              (chidu-jmap--fail-discovery discovery result))
             ((chidu-result-ok-p result)
              (condition-case error-data
                  (let* ((response (chidu-result-ok-value result))
                         (parsed
                          (chidu-jmap--validate-identities
                           (chidu-jmap-http-response-body response)
                           account-id)))
                    (setf
                     (chidu-jmap-discovery-observation discovery)
                     (chidu-jmap--replace-account-identities
                      (chidu-jmap-discovery-observation discovery)
                      account-id (car parsed) (cdr parsed)))
                    (chidu-jmap--discover-next-identity discovery))
                (error
                 (chidu-jmap--fail-discovery
                  discovery
                  (chidu-result-failure-create
                   :kind 'invalid-jmap-response
                   :data (list :message (error-message-string error-data))
                   :retryable-p nil)))))
             (t
              (chidu-jmap--fail-discovery discovery result))))
          :body (chidu-jmap--identity-request account-id)
          :max-request-bytes
          (chidu-store-session-observation-max-size-request observation)
          :byte-cap
          chidu-jmap-api-byte-cap))))))

(defun chidu-jmap--after-session (discovery session-url response)
  "Validate Session RESPONSE for DISCOVERY using configured SESSION-URL."
  (condition-case error-data
      (let* ((observation
              (chidu-jmap--validate-session
               (chidu-jmap-http-response-body response) session-url))
             (submission-accounts
              (cl-loop
               for account across
               (chidu-store-session-observation-accounts observation)
               when
               (seq-contains-p
                (chidu-store-account-observation-capabilities account)
                chidu-jmap-submission-capability
                #'equal)
               collect account)))
        (setf (chidu-jmap-discovery-observation discovery) observation
              (chidu-jmap-discovery-pending-accounts discovery)
              submission-accounts)
        (chidu-jmap--discover-next-identity discovery))
    (error
     (chidu-jmap--fail-discovery
      discovery
      (chidu-result-failure-create
       :kind 'invalid-jmap-response
       :data (list :message (error-message-string error-data))
       :retryable-p nil)))))

(defun chidu-jmap--discover-session-request (discovery session-url)
  "Fetch configured SESSION-URL for DISCOVERY."
  (setf
   (chidu-jmap-discovery-active-request discovery)
   (chidu-jmap-http-request
    session-url
    (chidu-store-endpoint-login
     (chidu-jmap-discovery-endpoint discovery))
    (chidu-store-endpoint-authentication
     (chidu-jmap-discovery-endpoint discovery))
    (chidu-jmap-discovery-secret discovery)
    (lambda (result)
      (cond
       ((chidu-result-failure-p result)
        (chidu-jmap--fail-discovery discovery result))
       ((chidu-result-ok-p result)
        (let* ((response (chidu-result-ok-value result))
               (status (chidu-jmap-http-response-status response)))
          (if (<= 200 status 299)
              (chidu-jmap--after-session discovery session-url response)
            (chidu-jmap--fail-discovery
             discovery
             (chidu-result-failure-create
              :kind 'unexpected-http-status
              :data (list :status status)
              :retryable-p nil)))))
       (t
        (chidu-jmap--fail-discovery discovery result))))
    :byte-cap chidu-jmap-session-byte-cap)))

(defun chidu-jmap-discover (endpoint secret deliver)
  "Start bounded Session/Identity discovery for ENDPOINT.

After argument validation, this function owns mutable string SECRET and clears
it after completion, cancellation, or startup failure.  Return a zero-argument
cancellation function, or nil if DELIVER was called synchronously."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let (discovery)
    (condition-case error-data
        (let ((session-url (chidu-store-endpoint-session-url endpoint)))
          (chidu-jmap--parse-url session-url "Session URL")
          (setq discovery
                (chidu-jmap-discovery-create
                 :endpoint endpoint
                 :secret secret
                 :deliver deliver))
          (chidu-jmap--discover-session-request discovery session-url)
          (lambda ()
            (unless (chidu-jmap-discovery-completed-p discovery)
              (setf (chidu-jmap-discovery-canceled-p discovery) t)
              (when-let* ((request
                            (chidu-jmap-discovery-active-request discovery)))
                (chidu-jmap-http-cancel request))
              (chidu-jmap--finish-discovery
               discovery
               (chidu-result-failure-create
                :kind 'canceled :data nil :retryable-p nil)))))
      (error
       (let ((failure
              (chidu-result-failure-create
               :kind 'jmap-discovery-failed
               :data (list :message (error-message-string error-data))
               :retryable-p nil)))
         (if discovery
             (chidu-jmap--finish-discovery discovery failure)
           (clear-string secret)
           (funcall deliver failure)))
       nil))))

(provide 'chidu-jmap-discovery)

;;; chidu-jmap-discovery.el ends here
