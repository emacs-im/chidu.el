;;; chidu-store-sqlite-directory.el --- SQLite JMAP directory persistence -*- lexical-binding: t; -*-

;;; Commentary:

;; Endpoint, Account, Identity, and Mailbox reads plus Session/Mailbox
;; observation commits for Chidu's sole SQLite Store backend.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'subr-x)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)

(defun chidu-store-sqlite--identity-from-row (row)
  "Decode Identity ROW."
  (pcase-let ((`(,identity-id ,remote-id ,name ,email ,available) row))
    (chidu-store-identity-create
     :identity-id identity-id
     :remote-identity-id remote-id
     :name name
     :email email
     :available-p (chidu-store-sqlite--bool available))))

(defun chidu-store-sqlite--identities (database account-id)
  "Return Identity vector for ACCOUNT-ID from DATABASE."
  (vconcat
   (mapcar
    #'chidu-store-sqlite--identity-from-row
    (chidu-sql-select database
      [:select [identity-id remote-identity-id name email is-available]
               :from jmap-identity
               :where [:= account-id [:bind account-id]]
               :order-by [[remote-identity-id :asc] [identity-id :asc]]]))))

(defun chidu-store-sqlite--account-from-row (database row)
  "Decode Account ROW from DATABASE."
  (pcase-let
      ((`(,account-id ,remote-id ,name ,personal ,read-only
                      ,primary-mail ,primary-submission ,available
                      ,identity-state ,max-size-attachments ,capabilities-json)
        row))
    (chidu-store-account-create
     :account-id account-id
     :remote-account-id remote-id
     :name name
     :personal-p (chidu-store-sqlite--bool personal)
     :read-only-p (chidu-store-sqlite--bool read-only)
     :primary-mail-p (chidu-store-sqlite--bool primary-mail)
     :primary-submission-p
     (chidu-store-sqlite--bool primary-submission)
     :available-p (chidu-store-sqlite--bool available)
     :identity-state identity-state
     :max-size-attachments-per-email max-size-attachments
     :capabilities
     (chidu-store-sqlite--capability-names
      capabilities-json "Account capabilities")
     :identities (chidu-store-sqlite--identities database account-id))))

(defun chidu-store-sqlite--accounts (database endpoint-id)
  "Return Account vector for ENDPOINT-ID from DATABASE."
  (vconcat
   (mapcar
    (lambda (row)
      (chidu-store-sqlite--account-from-row database row))
    (chidu-sql-select database
      [:select
       [account-id remote-account-id name
                   is-personal is-read-only is-primary-mail
                   is-primary-submission is-available
                   identity-state max-size-attachments-per-email capabilities-json]
       :from jmap-account
       :where [:= endpoint-id [:bind endpoint-id]]
       :order-by [[remote-account-id :asc] [account-id :asc]]]))))

(defun chidu-store-sqlite--endpoint-from-row (database row)
  "Decode Endpoint ROW from DATABASE."
  (pcase-let
      ((`(,endpoint-id ,session-url ,login ,authentication
                       ,session-username ,session-state ,api-url
                       ,download-url ,upload-url ,event-source-url
                       ,max-size-request ,max-size-upload ,max-objects-in-get
                       ,max-objects-in-set
                       ,primary-contacts-remote-account-id
                       ,capabilities-json)
        row))
    (chidu-store-endpoint-create
     :endpoint-id endpoint-id
     :session-url session-url
     :login login
     :authentication (intern authentication)
     :session-username session-username
     :session-state session-state
     :api-url api-url
     :download-url download-url
     :upload-url upload-url
     :event-source-url event-source-url
     :max-size-request max-size-request
     :max-size-upload max-size-upload
     :max-objects-in-get max-objects-in-get
     :max-objects-in-set max-objects-in-set
     :primary-contacts-remote-account-id
     primary-contacts-remote-account-id
     :capabilities
     (chidu-store-sqlite--capability-names
      capabilities-json "Endpoint capabilities")
     :accounts (chidu-store-sqlite--accounts database endpoint-id))))

(defun chidu-store-sqlite--endpoint (database endpoint-id)
  "Return Endpoint ENDPOINT-ID from DATABASE, or nil."
  (when-let* ((row
               (car
                (chidu-sql-select database
                  [:select
                   [endpoint-id session-url login authentication
                                session-username session-state api-url
                                download-url upload-url event-source-url
                                max-size-request max-size-upload max-objects-in-get
                                max-objects-in-set primary-contacts-remote-account-id
                                capabilities-json]
                   :from jmap-endpoint
                   :where [:= endpoint-id [:bind endpoint-id]]]))))
    (chidu-store-sqlite--endpoint-from-row database row)))

(defun chidu-store-sqlite--list-endpoints (database)
  "Return sorted Endpoint vector from DATABASE."
  (vconcat
   (mapcar
    (lambda (row)
      (chidu-store-sqlite--endpoint-from-row database row))
    (chidu-sql-select database
      [:select
       [endpoint-id session-url login authentication
                    session-username session-state api-url
                    download-url upload-url event-source-url
                    max-size-request max-size-upload max-objects-in-get
                    max-objects-in-set primary-contacts-remote-account-id
                    capabilities-json]
       :from jmap-endpoint
       :order-by [[session-url :asc] [login :asc] [endpoint-id :asc]]]))))

(defun chidu-store-sqlite--mailbox-from-row (row)
  "Decode Mailbox ROW."
  (pcase-let
      ((`(,mailbox-id ,remote-id ,name ,parent-id ,parent-remote-id ,role
                      ,sort-order ,total-emails ,unread-emails
                      ,total-threads ,unread-threads
                      ,may-read-items ,may-add-items ,may-remove-items
                      ,may-set-seen ,may-set-keywords ,may-create-child
                      ,may-rename ,may-delete ,may-submit
                      ,subscribed ,available)
        row))
    (chidu-store-mailbox-create
     :mailbox-id mailbox-id
     :remote-mailbox-id remote-id
     :name name
     :parent-mailbox-id parent-id
     :parent-remote-mailbox-id parent-remote-id
     :role role
     :sort-order sort-order
     :total-emails total-emails
     :unread-emails unread-emails
     :total-threads total-threads
     :unread-threads unread-threads
     :rights
     (chidu-store-mailbox-rights-create
      :may-read-items-p (chidu-store-sqlite--bool may-read-items)
      :may-add-items-p (chidu-store-sqlite--bool may-add-items)
      :may-remove-items-p (chidu-store-sqlite--bool may-remove-items)
      :may-set-seen-p (chidu-store-sqlite--bool may-set-seen)
      :may-set-keywords-p (chidu-store-sqlite--bool may-set-keywords)
      :may-create-child-p (chidu-store-sqlite--bool may-create-child)
      :may-rename-p (chidu-store-sqlite--bool may-rename)
      :may-delete-p (chidu-store-sqlite--bool may-delete)
      :may-submit-p (chidu-store-sqlite--bool may-submit))
     :subscribed-p (chidu-store-sqlite--bool subscribed)
     :available-p (chidu-store-sqlite--bool available))))

(defun chidu-store-sqlite--mailboxes (database account-id)
  "Return Mailbox vector for ACCOUNT-ID from DATABASE."
  (vconcat
   (mapcar
    #'chidu-store-sqlite--mailbox-from-row
    (chidu-sql-select database
      [:select
       [mailbox-id remote-mailbox-id name
                   parent-mailbox-id parent-remote-mailbox-id role
                   sort-order total-emails unread-emails
                   total-threads unread-threads
                   may-read-items may-add-items may-remove-items
                   may-set-seen may-set-keywords may-create-child
                   may-rename may-delete may-submit
                   is-subscribed is-available]
       :from jmap-mailbox
       :where [:= account-id [:bind account-id]]
       :order-by [[remote-mailbox-id :asc] [mailbox-id :asc]]]))))

(defun chidu-store-sqlite--account-location (database account-id)
  "Return (ENDPOINT . ACCOUNT) for ACCOUNT-ID in DATABASE, or nil."
  (when-let* ((endpoint-id
               (caar
                (chidu-sql-select database
                  [:select [endpoint-id]
                           :from jmap-account
                           :where [:= account-id [:bind account-id]]])))
              (endpoint (chidu-store-sqlite--endpoint database endpoint-id))
              (account
               (cl-find
                account-id (chidu-store-endpoint-accounts endpoint)
                :key #'chidu-store-account-account-id :test #'equal)))
    (cons endpoint account)))

(defun chidu-store-sqlite--mailbox-checkpoint (database account-id)
  "Read ACCOUNT-ID Mailbox checkpoint from DATABASE as (STATE . REVISION)."
  (if-let* ((row
             (car
              (chidu-sql-select database
                [:select [state revision]
                         :from jmap-type-checkpoint
                         :where [:and
                                 [:= account-id [:bind account-id]]
                                 [:= data-type [:literal "Mailbox"]]]]))))
      (cons (car row) (cadr row))
    (cons nil 0)))

(defun chidu-store-sqlite--mailbox-context (state account-id)
  "Return ACCOUNT-ID Mailbox sync context from SQLite STATE."
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
     ((null (chidu-store-endpoint-api-url (car location)))
      (chidu-result-failure-create
       :kind 'endpoint-not-connected :data (list :account-id account-id)
       :retryable-p nil))
     (t
      (let ((checkpoint
             (chidu-store-sqlite--mailbox-checkpoint database account-id)))
        (chidu-result-ok-create
         :value
         (chidu-store-mailbox-sync-context-create
          :endpoint (car location)
          :account (cdr location)
          :state (car checkpoint)
          :revision (cdr checkpoint)
          :mailboxes (chidu-store-sqlite--mailboxes database account-id))))))))

(defun chidu-store-sqlite--mailbox-by-id (database account-id mailbox-id)
  "Return MAILBOX-ID below ACCOUNT-ID in DATABASE, or nil."
  (cl-find mailbox-id
           (chidu-store-sqlite--mailboxes database account-id)
           :key #'chidu-store-mailbox-mailbox-id
           :test #'equal))

(defun chidu-store-sqlite--mailbox-by-remote-id
    (database account-id remote-mailbox-id)
  "Return DATABASE Mailbox below ACCOUNT-ID by REMOTE-MAILBOX-ID."
  (cl-find remote-mailbox-id
           (chidu-store-sqlite--mailboxes database account-id)
           :key #'chidu-store-mailbox-remote-mailbox-id
           :test #'equal))

(defun chidu-store-sqlite--configure (state operation)
  "Apply Endpoint configure OPERATION to SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (session-url
          (chidu-store-normalize-session-url
           (chidu-store-op-configure-endpoint-session-url operation)))
         (login
          (chidu-store-validate-login
           (chidu-store-op-configure-endpoint-login operation)))
         (authentication
          (chidu-store-validate-authentication
           (chidu-store-op-configure-endpoint-authentication operation)))
         endpoint-id)
    (with-sqlite-transaction database
      (setq endpoint-id
            (caar
             (chidu-sql-select database
               [:select [endpoint-id]
                        :from jmap-endpoint
                        :where [:and
                                [:= session-url [:bind session-url]]
                                [:= login [:bind login]]]])))
      (unless endpoint-id
        (setq endpoint-id (chidu-store-new-local-id)))
      (chidu-sql-execute database
        [:insert :into jmap-endpoint
                 :row
                 [[endpoint-id [:bind endpoint-id]]
                  [session-url [:bind session-url]]
                  [login [:bind login]]
                  [authentication [:bind (symbol-name authentication)] :update]]
                 :on-conflict [session-url login]])
      (chidu-store-sqlite--increment-change-seq database))
    (chidu-store-sqlite--endpoint database endpoint-id)))

(defun chidu-store-sqlite--account-id
    (database endpoint-id remote-account-id)
  "Return stable Account id in DATABASE for REMOTE-ACCOUNT-ID under ENDPOINT-ID."
  (or (caar
       (chidu-sql-select database
         [:select [account-id]
                  :from jmap-account
                  :where [:and
                          [:= endpoint-id [:bind endpoint-id]]
                          [:= remote-account-id [:bind remote-account-id]]]]))
      (chidu-store-new-local-id)))

(defun chidu-store-sqlite--identity-id
    (database account-id remote-identity-id)
  "Return stable Identity id in DATABASE for REMOTE-IDENTITY-ID under ACCOUNT-ID."
  (or (caar
       (chidu-sql-select database
         [:select [identity-id]
                  :from jmap-identity
                  :where [:and
                          [:= account-id [:bind account-id]]
                          [:= remote-identity-id [:bind remote-identity-id]]]]))
      (chidu-store-new-local-id)))

(defun chidu-store-sqlite--save-identity
    (database account-id observation change-seq)
  "Save Identity OBSERVATION below ACCOUNT-ID in DATABASE."
  (unless (chidu-store-identity-observation-p observation)
    (signal 'chidu-invariant-error
            (list "Invalid Identity observation" observation)))
  (let* ((remote-id
          (chidu-store-validate-id
           (chidu-store-identity-observation-remote-identity-id observation)
           "remote Identity id"))
         (identity-id
          (chidu-store-sqlite--identity-id
           database account-id remote-id)))
    (chidu-sql-execute database
      [:insert :into jmap-identity
               :row
               [[identity-id [:bind identity-id]]
                [account-id [:bind account-id]]
                [remote-identity-id [:bind remote-id]]
                [name [:bind (or (chidu-store-identity-observation-name observation)
                                 "")] :update]
                [email
                 [:bind
                  (chidu-store-validate-id
                   (chidu-store-identity-observation-email observation)
                   "Identity email")]
                 :update]
                [reply-to-json nil :update]
                [bcc-json nil :update]
                [text-signature [:literal ""] :update]
                [html-signature [:literal ""] :update]
                [may-delete 0 :update]
                [is-available 1 :update]
                [observed-change-seq [:bind change-seq] :update]]
               :on-conflict [account-id remote-identity-id]])))

(defun chidu-store-sqlite--save-account
    (database endpoint-id observation change-seq)
  "Save Account OBSERVATION below ENDPOINT-ID in DATABASE."
  (unless (chidu-store-account-observation-p observation)
    (signal 'chidu-invariant-error
            (list "Invalid Account observation" observation)))
  (let* ((remote-id
          (chidu-store-validate-id
           (chidu-store-account-observation-remote-account-id observation)
           "remote Account id"))
         (account-id
          (chidu-store-sqlite--account-id
           database endpoint-id remote-id)))
    (chidu-sql-execute database
      [:insert :into jmap-account
               :row
               [[account-id [:bind account-id]]
                [endpoint-id [:bind endpoint-id]]
                [remote-account-id [:bind remote-id]]
                [name [:bind (or (chidu-store-account-observation-name observation)
                                 "")] :update]
                [is-personal
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-account-observation-personal-p observation))]
                 :update]
                [is-read-only
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-account-observation-read-only-p observation))]
                 :update]
                [is-primary-mail
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-account-observation-primary-mail-p observation))]
                 :update]
                [is-primary-submission
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-account-observation-primary-submission-p observation))]
                 :update]
                [is-available 1 :update]
                [capabilities-json
                 [:bind
                  (chidu-store-sqlite--capability-json
                   (chidu-store-account-observation-capabilities observation)
                   "Account capabilities")]
                 :update]
                [extra-properties-json [:literal "{}"] :update]
                [identity-state
                 [:bind (chidu-store-account-observation-identity-state observation)]
                 :update]
                [max-size-attachments-per-email
                 [:bind
                  (let ((value
                         (chidu-store-account-observation-max-size-attachments-per-email
                          observation)))
                    (when (and value
                               (not (and (integerp value) (>= value 0))))
                      (signal 'chidu-invariant-error
                              '("Account maxSizeAttachmentsPerEmail is invalid")))
                    value)]
                 :update]
                [observed-change-seq [:bind change-seq] :update]]
               :on-conflict [endpoint-id remote-account-id]])
    (chidu-sql-execute database
      [:update jmap-identity
               :set [[is-available 0]]
               :where [:= account-id [:bind account-id]]])
    (cl-loop
     for identity across
     (chidu-store-account-observation-identities observation)
     do
     (chidu-store-sqlite--save-identity
      database account-id identity change-seq))))

(defun chidu-store-sqlite--mailbox-id
    (database account-id remote-mailbox-id)
  "Return stable Mailbox id in DATABASE for REMOTE-MAILBOX-ID under ACCOUNT-ID."
  (or (caar
       (chidu-sql-select database
         [:select [mailbox-id]
                  :from jmap-mailbox
                  :where [:and
                          [:= account-id [:bind account-id]]
                          [:= remote-mailbox-id [:bind remote-mailbox-id]]]]))
      (chidu-store-new-local-id)))

(defun chidu-store-sqlite--save-mailbox
    (database account-id item remote-to-local change-seq)
  "Save Mailbox observation ITEM below ACCOUNT-ID in DATABASE."
  (let* ((remote-id
          (chidu-store-mailbox-observation-remote-mailbox-id item))
         (parent-remote-id
          (chidu-store-mailbox-observation-parent-remote-mailbox-id item))
         (rights (chidu-store-mailbox-observation-rights item)))
    (chidu-sql-execute database
      [:insert :into jmap-mailbox
               :row
               [[mailbox-id [:bind (gethash remote-id remote-to-local)]]
                [account-id [:bind account-id]]
                [remote-mailbox-id [:bind remote-id]]
                [name [:bind (chidu-store-mailbox-observation-name item)] :update]
                [parent-mailbox-id
                 [:bind
                  (and parent-remote-id
                       (gethash parent-remote-id remote-to-local))]
                 :update]
                [parent-remote-mailbox-id [:bind parent-remote-id] :update]
                [role [:bind (chidu-store-mailbox-observation-role item)] :update]
                [sort-order
                 [:bind (chidu-store-mailbox-observation-sort-order item)] :update]
                [total-emails
                 [:bind (chidu-store-mailbox-observation-total-emails item)] :update]
                [unread-emails
                 [:bind (chidu-store-mailbox-observation-unread-emails item)] :update]
                [total-threads
                 [:bind (chidu-store-mailbox-observation-total-threads item)] :update]
                [unread-threads
                 [:bind (chidu-store-mailbox-observation-unread-threads item)] :update]
                [may-read-items
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-read-items-p rights))]
                 :update]
                [may-add-items
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-add-items-p rights))]
                 :update]
                [may-remove-items
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-remove-items-p rights))]
                 :update]
                [may-set-seen
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-set-seen-p rights))]
                 :update]
                [may-set-keywords
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-set-keywords-p rights))]
                 :update]
                [may-create-child
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-create-child-p rights))]
                 :update]
                [may-rename
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-rename-p rights))]
                 :update]
                [may-delete
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-delete-p rights))]
                 :update]
                [may-submit
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-rights-may-submit-p rights))]
                 :update]
                [is-subscribed
                 [:bind
                  (chidu-store-sqlite--integer-bool
                   (chidu-store-mailbox-observation-subscribed-p item))]
                 :update]
                [is-available 1 :update]
                [observed-change-seq [:bind change-seq] :update]]
               :on-conflict [account-id remote-mailbox-id]])))

(defun chidu-store-sqlite--observe-mailbox-snapshot (state operation)
  "CAS-commit complete Mailbox snapshot OPERATION into SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-observe-mailbox-snapshot-account-id operation))
         (expected-revision
          (chidu-store-op-observe-mailbox-snapshot-expected-revision operation))
         (observation
          (chidu-store-op-observe-mailbox-snapshot-observation operation))
         (context-result
          (chidu-store-sqlite--mailbox-context state account-id)))
    (cond
     ((chidu-result-failure-p context-result) context-result)
     ((not (chidu-store-mailbox-snapshot-observation-p observation))
      (signal 'chidu-invariant-error
              (list "Invalid Mailbox snapshot observation" observation)))
     (t
      (let* ((context (chidu-result-ok-value context-result))
             (actual-revision
              (chidu-store-mailbox-sync-context-revision context)))
        (if (not (and (integerp expected-revision)
                      (= expected-revision actual-revision)))
            (chidu-result-failure-create
             :kind 'revision-conflict
             :data (list :account-id account-id
                         :expected expected-revision
                         :actual actual-revision)
             :retryable-p t)
          (let* ((state-token
                  (chidu-store-mailbox-snapshot-observation-state observation))
                 (validated
                  (chidu-store-mailbox-snapshot-observation-mailboxes
                   observation))
                 (remote-to-local (make-hash-table :test #'equal)))
            (dolist
                (row
                 (chidu-sql-select database
                   [:select [remote-mailbox-id mailbox-id]
                            :from jmap-mailbox
                            :where [:= account-id [:bind account-id]]]))
              (puthash (car row) (cadr row) remote-to-local))
            (cl-loop
             for item across validated
             for remote-id =
             (chidu-store-mailbox-observation-remote-mailbox-id item)
             do
             (puthash
              remote-id
              (chidu-store-sqlite--mailbox-id
               database account-id remote-id)
              remote-to-local))
            (with-sqlite-transaction database
              (let ((change-seq
                     (chidu-store-sqlite--increment-change-seq database)))
                (chidu-sql-execute database
                  [:update jmap-mailbox
                           :set [[is-available 0]
                                 [observed-change-seq [:bind change-seq]]]
                           :where [:= account-id [:bind account-id]]])
                (cl-loop
                 for item across validated
                 do
                 (chidu-store-sqlite--save-mailbox
                  database account-id item remote-to-local change-seq))
                (chidu-sql-execute database
                  [:insert :into jmap-type-checkpoint
                           :row
                           [[account-id [:bind account-id]]
                            [data-type [:literal "Mailbox"]]
                            [state [:bind state-token] :update]
                            [revision
                             1
                             [:update [:+ jmap-type-checkpoint:revision 1]]]
                            [observed-change-seq [:bind change-seq] :update]]
                           :on-conflict [account-id data-type]])))
            (chidu-store-sqlite--mailbox-context state account-id))))))))

(defun chidu-store-sqlite--save-session
    (database endpoint-id observation change-seq)
  "Save Session OBSERVATION for ENDPOINT-ID in DATABASE."
  (chidu-sql-execute database
    [:update jmap-endpoint
             :set
             [[session-username
               [:bind
                (chidu-store-validate-nonempty-string
                 (chidu-store-session-observation-username observation)
                 "Session username")]]
              [session-state
               [:bind
                (chidu-store-validate-nonempty-string
                 (chidu-store-session-observation-state observation)
                 "Session state")]]
              [api-url
               [:bind
                (chidu-store-normalize-session-url
                 (chidu-store-session-observation-api-url observation))]]
              [download-url
               [:bind (chidu-store-session-observation-download-url observation)]]
              [upload-url
               [:bind (chidu-store-session-observation-upload-url observation)]]
              [event-source-url
               [:bind (chidu-store-session-observation-event-source-url observation)]]
              [max-size-request
               [:bind
                (chidu-store-validate-positive-integer
                 (chidu-store-session-observation-max-size-request observation)
                 "Session maxSizeRequest")]]
              [max-size-upload
               [:bind
                (let ((value
                       (chidu-store-session-observation-max-size-upload observation)))
                  (when value
                    (chidu-store-validate-positive-integer
                     value "Session maxSizeUpload"))
                  value)]]
              [max-objects-in-get
               [:bind
                (chidu-store-validate-positive-integer
                 (chidu-store-session-observation-max-objects-in-get observation)
                 "Session maxObjectsInGet")]]
              [max-objects-in-set
               [:bind
                (chidu-store-validate-positive-integer
                 (chidu-store-session-observation-max-objects-in-set observation)
                 "Session maxObjectsInSet")]]
              [primary-contacts-remote-account-id
               [:bind
                (chidu-store-session-observation-primary-contacts-remote-account-id
                 observation)]]
              [capabilities-json
               [:bind
                (chidu-store-sqlite--capability-json
                 (chidu-store-session-observation-capabilities observation)
                 "Session capabilities")]]
              [extra-properties-json [:literal "{}"]]
              [observed-change-seq [:bind change-seq]]]
             :where [:= endpoint-id [:bind endpoint-id]]]))

(defun chidu-store-sqlite--observe-session (state operation)
  "Apply Session observation OPERATION to SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (endpoint-id
          (chidu-store-op-observe-session-endpoint-id operation))
         (observation
          (chidu-store-op-observe-session-observation operation)))
    (unless (chidu-store-session-observation-p observation)
      (signal 'chidu-invariant-error
              (list "Invalid Session observation" observation)))
    (if (null (chidu-store-sqlite--endpoint database endpoint-id))
        (chidu-result-failure-create
         :kind 'unknown-endpoint
         :data (list :endpoint-id endpoint-id)
         :retryable-p nil)
      (with-sqlite-transaction database
        (let ((change-seq
               (chidu-store-sqlite--increment-change-seq database)))
          (chidu-store-sqlite--save-session
           database endpoint-id observation change-seq)
          (chidu-sql-execute database
            [:update jmap-account
                     :set [[is-available 0]]
                     :where [:= endpoint-id [:bind endpoint-id]]])
          (chidu-sql-execute database
            [:update jmap-identity
                     :set [[is-available 0]]
                     :where
                     [:in account-id
                          [:select [account-id]
                                   :from jmap-account
                                   :where [:= endpoint-id [:bind endpoint-id]]]]])
          (cl-loop
           for account across
           (chidu-store-session-observation-accounts observation)
           do
           (chidu-store-sqlite--save-account
            database endpoint-id account change-seq))))
      (chidu-result-ok-create
       :value (chidu-store-sqlite--endpoint database endpoint-id)))))

(provide 'chidu-store-sqlite-directory)

;;; chidu-store-sqlite-directory.el ends here
