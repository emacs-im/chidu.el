;;; chidu-jmap-mailbox.el --- Bounded JMAP Mailbox adapter -*- lexical-binding: t; -*-

;;; Commentary:

;; Mechanical asynchronous Mailbox/get adapter.  It owns one secret copy,
;; returns validated Store observation data, and never mutates runtime or Store
;; state directly.

;;; Code:

(require 'cl-lib)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-jmap-http)
(require 'chidu-jmap-types)
(require 'chidu-jmap-response)
(require 'chidu-store)

(defconst chidu-jmap-mailbox-max-items 65536
  "Maximum Mailbox objects accepted from one complete Mailbox/get.")

(cl-defstruct (chidu-jmap-mailbox-fetch
               (:constructor chidu-jmap-mailbox-fetch-create))
  "Mutable mechanical state for one Mailbox/get effect."
  secret
  deliver
  request
  completed-p
  canceled-p)

(defun chidu-jmap--mailbox-sort-order (value)
  "Return RFC 8621 Mailbox sortOrder VALUE or signal."
  (unless (and (integerp value) (<= 0 value) (< value (expt 2 31)))
    (signal 'chidu-jmap-error
            '("Mailbox sortOrder must satisfy 0 <= value < 2^31")))
  value)

(defun chidu-jmap--mailbox-request (remote-account-id)
  "Return bounded Mailbox/get request for REMOTE-ACCOUNT-ID."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
    :methodCalls
    [["Mailbox/get"
      (:accountId ,remote-account-id
       :ids :json-null
       :properties
       ["id" "name" "parentId" "role" "sortOrder"
        "totalEmails" "unreadEmails" "totalThreads" "unreadThreads"
        "myRights" "isSubscribed"])
      "mailbox-get"]]))

(defun chidu-jmap--mailbox-rights (wire)
  "Validate Mailbox rights object WIRE."
  (let ((rights (chidu-jmap--hash wire "Mailbox myRights")))
    (chidu-store-mailbox-rights-create
     :may-read-items-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayReadItems" "Mailbox myRights")
      "Mailbox mayReadItems")
     :may-add-items-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayAddItems" "Mailbox myRights")
      "Mailbox mayAddItems")
     :may-remove-items-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayRemoveItems" "Mailbox myRights")
      "Mailbox mayRemoveItems")
     :may-set-seen-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "maySetSeen" "Mailbox myRights")
      "Mailbox maySetSeen")
     :may-set-keywords-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "maySetKeywords" "Mailbox myRights")
      "Mailbox maySetKeywords")
     :may-create-child-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayCreateChild" "Mailbox myRights")
      "Mailbox mayCreateChild")
     :may-rename-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayRename" "Mailbox myRights")
      "Mailbox mayRename")
     :may-delete-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayDelete" "Mailbox myRights")
      "Mailbox mayDelete")
     :may-submit-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "maySubmit" "Mailbox myRights")
      "Mailbox maySubmit"))))

(defun chidu-jmap--mailbox-observation (wire)
  "Validate one Mailbox object WIRE and return a Store observation."
  (let ((mailbox (chidu-jmap--hash wire "Mailbox")))
    (chidu-store-mailbox-observation-create
     :remote-mailbox-id
     (chidu-jmap--id
      (chidu-jmap--required mailbox "id" "Mailbox") "Mailbox id")
     :name
     (chidu-jmap--string
      (chidu-jmap--required mailbox "name" "Mailbox")
      "Mailbox name")
     :parent-remote-mailbox-id
     (chidu-jmap--nullable-id
      (chidu-jmap--required mailbox "parentId" "Mailbox")
      "Mailbox parentId")
     :role
     (chidu-jmap--nullable-string
      (chidu-jmap--required mailbox "role" "Mailbox") "Mailbox role")
     :sort-order
     (chidu-jmap--mailbox-sort-order
      (chidu-jmap--required mailbox "sortOrder" "Mailbox"))
     :total-emails
     (chidu-jmap--safe-nonnegative-integer
      (chidu-jmap--required mailbox "totalEmails" "Mailbox")
      "Mailbox totalEmails")
     :unread-emails
     (chidu-jmap--safe-nonnegative-integer
      (chidu-jmap--required mailbox "unreadEmails" "Mailbox")
      "Mailbox unreadEmails")
     :total-threads
     (chidu-jmap--safe-nonnegative-integer
      (chidu-jmap--required mailbox "totalThreads" "Mailbox")
      "Mailbox totalThreads")
     :unread-threads
     (chidu-jmap--safe-nonnegative-integer
      (chidu-jmap--required mailbox "unreadThreads" "Mailbox")
      "Mailbox unreadThreads")
     :rights
     (chidu-jmap--mailbox-rights
      (chidu-jmap--required mailbox "myRights" "Mailbox"))
     :subscribed-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required mailbox "isSubscribed" "Mailbox")
      "Mailbox isSubscribed"))))

(defun chidu-jmap-mailbox--snapshot (state mailboxes)
  "Validate complete MAILBOXES for STATE and return a Store observation."
  (let ((by-id (make-hash-table :test #'equal))
        (roles (make-hash-table :test #'equal))
        (siblings (make-hash-table :test #'equal))
        (marks (make-hash-table :test #'equal)))
    (cl-loop
     for mailbox across mailboxes
     for remote-id =
     (chidu-store-mailbox-observation-remote-mailbox-id mailbox)
     for role = (chidu-store-mailbox-observation-role mailbox)
     for total-emails =
     (chidu-store-mailbox-observation-total-emails mailbox)
     for unread-emails =
     (chidu-store-mailbox-observation-unread-emails mailbox)
     for total-threads =
     (chidu-store-mailbox-observation-total-threads mailbox)
     for unread-threads =
     (chidu-store-mailbox-observation-unread-threads mailbox)
     do
     (when (gethash remote-id by-id)
       (signal 'chidu-jmap-error
               (list "Mailbox/get returned duplicate id" remote-id)))
     (when (and role (not (equal role (downcase role))))
       (signal 'chidu-jmap-error '("Mailbox role must be lowercase")))
     (unless (and (<= unread-emails total-emails)
                  (<= total-threads total-emails)
                  (<= unread-threads total-threads))
       (signal 'chidu-jmap-error
               '("Mailbox count relationships are inconsistent")))
     (puthash remote-id mailbox by-id))
    (cl-loop
     for mailbox across mailboxes
     for remote-id =
     (chidu-store-mailbox-observation-remote-mailbox-id mailbox)
     for parent =
     (chidu-store-mailbox-observation-parent-remote-mailbox-id mailbox)
     for role = (chidu-store-mailbox-observation-role mailbox)
     for sibling-key =
     (list parent (chidu-store-mailbox-observation-name mailbox))
     do
     (when (and parent (not (gethash parent by-id)))
       (signal 'chidu-jmap-error
               (list "Mailbox parent is missing from complete snapshot"
                     remote-id parent)))
     (when role
       (when (gethash role roles)
         (signal 'chidu-jmap-error (list "Duplicate Mailbox role" role)))
       (puthash role remote-id roles))
     (when (gethash sibling-key siblings)
       (signal 'chidu-jmap-error
               (list "Duplicate sibling Mailbox name"
                     (chidu-store-mailbox-observation-name mailbox))))
     (puthash sibling-key remote-id siblings))
    (cl-labels
        ((visit
           (remote-id)
           (pcase (gethash remote-id marks)
             ('visiting
              (signal 'chidu-jmap-error
                      (list "Mailbox parent graph contains a cycle" remote-id)))
             ('done nil)
             (_
              (puthash remote-id 'visiting marks)
              (when-let* ((parent
                           (chidu-store-mailbox-observation-parent-remote-mailbox-id
                            (gethash remote-id by-id))))
                (visit parent))
              (puthash remote-id 'done marks)))))
      (maphash (lambda (remote-id _mailbox) (visit remote-id)) by-id))
    (chidu-store-mailbox-snapshot-observation-create
     :state state :mailboxes mailboxes)))

(defun chidu-jmap--validate-mailboxes (bytes remote-account-id)
  "Validate Mailbox/get JSON BYTES for REMOTE-ACCOUNT-ID."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Mailbox/get" "mailbox-get" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (state
          (chidu-jmap--string
           (chidu-jmap--required arguments "state" "Mailbox/get")
           "Mailbox state"))
         (not-found
          (chidu-jmap--vector
           (chidu-jmap--required arguments "notFound" "Mailbox/get")
           "Mailbox notFound"))
         (list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Mailbox/get")
           "Mailbox list")))
    (unless (zerop (length not-found))
      (signal 'chidu-jmap-error
              '("Mailbox/get unexpectedly returned notFound")))
    (when (> (length list) chidu-jmap-mailbox-max-items)
      (signal 'chidu-jmap-error
              '("Mailbox/get returned too many Mailbox objects")))
    (chidu-jmap-mailbox--snapshot
     state
     (cl-map 'vector #'chidu-jmap--mailbox-observation list))))

(defun chidu-jmap--finish-mailbox-fetch (fetch result)
  "Complete FETCH exactly once with RESULT."
  (unless (chidu-jmap-mailbox-fetch-completed-p fetch)
    (setf (chidu-jmap-mailbox-fetch-completed-p fetch) t)
    (when-let* ((secret (chidu-jmap-mailbox-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-mailbox-fetch-secret fetch) nil))
    (unless (chidu-jmap-mailbox-fetch-canceled-p fetch)
      (funcall (chidu-jmap-mailbox-fetch-deliver fetch) result))))

(defun chidu-jmap-fetch-mailboxes (context secret deliver)
  "Start bounded Mailbox/get for sync CONTEXT.

SECRET is an owned mutable string: this function clears it after completion or
cancellation.  Return a zero-argument cancellation function, or nil if startup
failed and DELIVER was called synchronously."
  (unless (chidu-store-mailbox-sync-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-mailbox-sync-context-p context)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let* ((endpoint (chidu-store-mailbox-sync-context-endpoint context))
         (account (chidu-store-mailbox-sync-context-account context))
         (fetch
          (chidu-jmap-mailbox-fetch-create
           :secret secret :deliver deliver)))
    (condition-case error-data
        (progn
          (setf
           (chidu-jmap-mailbox-fetch-request fetch)
           (chidu-jmap-http-request
            (chidu-store-endpoint-api-url endpoint)
            (chidu-store-endpoint-login endpoint)
            (chidu-store-endpoint-authentication endpoint)
            secret
            (lambda (result)
              (cond
               ((chidu-result-failure-p result)
                (chidu-jmap--finish-mailbox-fetch fetch result))
               ((chidu-result-ok-p result)
                (condition-case validation-error
                    (let ((response (chidu-result-ok-value result)))
                      (if (= 200 (chidu-jmap-http-response-status response))
                          (chidu-jmap--finish-mailbox-fetch
                           fetch
                           (chidu-result-ok-create
                            :value
                            (chidu-jmap--validate-mailboxes
                             (chidu-jmap-http-response-body response)
                             (chidu-store-account-remote-account-id account))))
                        (chidu-jmap--finish-mailbox-fetch
                         fetch
                         (chidu-result-failure-create
                          :kind 'unexpected-http-status
                          :data
                          (list
                           :status
                           (chidu-jmap-http-response-status response))
                          :retryable-p nil))))
                  (error
                   (chidu-jmap--finish-mailbox-fetch
                    fetch
                    (chidu-result-failure-create
                     :kind 'invalid-jmap-response
                     :data
                     (list :message
                           (error-message-string validation-error))
                     :retryable-p nil)))))
               (t
                (chidu-jmap--finish-mailbox-fetch fetch result))))
            :body
            (chidu-jmap--mailbox-request
             (chidu-store-account-remote-account-id account))
            :max-request-bytes
            (chidu-store-endpoint-max-size-request endpoint)
            :byte-cap
            chidu-jmap-api-byte-cap))
          (lambda ()
            (unless (chidu-jmap-mailbox-fetch-completed-p fetch)
              (setf (chidu-jmap-mailbox-fetch-canceled-p fetch) t)
              (when-let* ((request
                            (chidu-jmap-mailbox-fetch-request fetch)))
                (chidu-jmap-http-cancel request))
              (chidu-jmap--finish-mailbox-fetch
               fetch
               (chidu-result-failure-create
                :kind 'canceled :data nil :retryable-p nil)))))
      (error
       (chidu-jmap--finish-mailbox-fetch
        fetch
        (chidu-result-failure-create
         :kind 'jmap-request-failed
         :data (list :message (error-message-string error-data))
         :retryable-p nil))
       nil))))

(provide 'chidu-jmap-mailbox)

;;; chidu-jmap-mailbox.el ends here
