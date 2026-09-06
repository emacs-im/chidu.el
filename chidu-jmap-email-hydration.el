;;; chidu-jmap-email-hydration.el --- JMAP Email metadata hydration -*- lexical-binding: t; -*-

;;; Commentary:

;; Fetch one homogeneous metadata hydration plan.  A `full' plan retrieves the
;; immutable metadata-v1 fragment plus current mailboxIds/keywords; a `mutable'
;; plan retrieves only current mailboxIds/keywords.  Results are returned in the
;; exact Store-plan order, including explicit notFound settlements.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-api)
(require 'chidu-jmap-email)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-store)

(defconst chidu-jmap-email-hydration-full-properties
  ["id" "blobId" "threadId" "mailboxIds" "keywords" "size"
   "receivedAt" "sentAt" "sender" "from" "to" "cc" "bcc" "replyTo"
   "subject" "messageId" "inReplyTo" "references" "preview"
   "hasAttachment"]
  "Properties fetched for a full metadata-v1 hydration plan.")

(defconst chidu-jmap-email-hydration-mutable-properties
  ["id" "mailboxIds" "keywords"]
  "Properties fetched for a mutable-only hydration plan.")

(defun chidu-jmap-email-hydration--properties (kind)
  "Return Email/get properties for hydration KIND."
  (pcase kind
    ('full chidu-jmap-email-hydration-full-properties)
    ('mutable chidu-jmap-email-hydration-mutable-properties)
    (_ (signal 'chidu-invariant-error
               (list "Unknown Email hydration kind" kind)))))

(defun chidu-jmap-email-hydration--request
    (remote-account-id kind remote-email-ids)
  "Return a hydration request for REMOTE-EMAIL-IDS in REMOTE-ACCOUNT-ID.

KIND is `full' or `mutable'."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
    :methodCalls
    [["Email/get"
      (:accountId ,remote-account-id
       :ids ,remote-email-ids
       :properties ,(chidu-jmap-email-hydration--properties kind))
      "email-hydration"]]))

(defun chidu-jmap-email-hydration--set-vector (object)
  "Return sorted keys from validated Boolean-set OBJECT."
  (vconcat (sort (hash-table-keys object) #'string<)))

(defun chidu-jmap-email-hydration--nullable-string (value context)
  "Decode nullable String VALUE for CONTEXT."
  (unless (eq value :json-null)
    (chidu-jmap--string value context t)))

(defun chidu-jmap-email-hydration--string-or-empty (value context)
  "Decode nullable String VALUE for CONTEXT, using an empty string for null."
  (or (chidu-jmap-email-hydration--nullable-string value context) ""))

(defun chidu-jmap-email-hydration--mutable-fields (email context)
  "Decode current mutable fields from EMAIL for CONTEXT.

Return (MAILBOX-IDS . KEYWORDS)."
  (let ((mailbox-ids
         (chidu-jmap-email-hydration--set-vector
          (chidu-jmap-email--true-map
           (chidu-jmap--required email "mailboxIds" context)
           (format "%s mailboxIds" context) t)))
        (keywords
         (chidu-jmap-email-hydration--set-vector
          (chidu-jmap-email--true-map
           (chidu-jmap--required email "keywords" context)
           (format "%s keywords" context)))))
    (when (zerop (length mailbox-ids))
      (signal 'chidu-jmap-error
              (list (format "%s has no visible Mailbox" context))))
    (cons mailbox-ids keywords)))

(defun chidu-jmap-email-hydration--metadata (email context)
  "Decode immutable metadata-v1 fragment from EMAIL for CONTEXT."
  (chidu-store-email-metadata-create
   :remote-blob-id
   (chidu-jmap--id
    (chidu-jmap--required email "blobId" context)
    (format "%s blobId" context))
   :remote-thread-id
   (chidu-jmap--id
    (chidu-jmap--required email "threadId" context)
    (format "%s threadId" context))
   :size
   (chidu-jmap--safe-nonnegative-integer
    (chidu-jmap--required email "size" context)
    (format "%s size" context))
   :received-at
   (chidu-jmap--string
    (chidu-jmap--required email "receivedAt" context)
    (format "%s receivedAt" context))
   :sent-at
   (chidu-jmap-email-hydration--nullable-string
    (gethash "sentAt" email :json-null)
    (format "%s sentAt" context))
   :sender
   (chidu-jmap-email--address-vector
    (gethash "sender" email :json-null)
    (format "%s sender" context))
   :from
   (chidu-jmap-email--address-vector
    (gethash "from" email :json-null)
    (format "%s from" context))
   :to
   (chidu-jmap-email--address-vector
    (gethash "to" email :json-null)
    (format "%s to" context))
   :cc
   (chidu-jmap-email--address-vector
    (gethash "cc" email :json-null)
    (format "%s cc" context))
   :bcc
   (chidu-jmap-email--address-vector
    (gethash "bcc" email :json-null)
    (format "%s bcc" context))
   :reply-to
   (chidu-jmap-email--address-vector
    (gethash "replyTo" email :json-null)
    (format "%s replyTo" context))
   :subject
   (chidu-jmap-email-hydration--string-or-empty
    (gethash "subject" email :json-null)
    (format "%s subject" context))
   :message-ids
   (chidu-jmap-email--header-id-vector
    (gethash "messageId" email :json-null)
    (format "%s messageId" context))
   :in-reply-to
   (chidu-jmap-email--header-id-vector
    (gethash "inReplyTo" email :json-null)
    (format "%s inReplyTo" context))
   :references
   (chidu-jmap-email--header-id-vector
    (gethash "references" email :json-null)
    (format "%s references" context))
   :has-attachment-p
   (chidu-jmap--json-boolean
    (chidu-jmap--required email "hasAttachment" context)
    (format "%s hasAttachment" context))))

(defun chidu-jmap-email-hydration--found-result (wire kind expected)
  "Decode one found hydration WIRE object of KIND requested in EXPECTED."
  (let* ((email (chidu-jmap--hash wire "Email hydration object"))
         (remote-id
          (chidu-jmap--id
           (chidu-jmap--required email "id" "Email hydration object")
           "Email hydration id")))
    (unless (gethash remote-id expected)
      (signal 'chidu-jmap-error
              '("Email/get returned an unrequested hydration id")))
    (pcase-let ((`(,mailbox-ids . ,keywords)
                 (chidu-jmap-email-hydration--mutable-fields
                  email "Email hydration object")))
      (chidu-store-email-hydration-result-create
       :remote-email-id remote-id
       :found-p t
       :metadata
       (and (eq kind 'full)
            (chidu-jmap-email-hydration--metadata
             email "Email hydration object"))
       :preview
       (and (eq kind 'full)
            (chidu-jmap-email-hydration--string-or-empty
             (gethash "preview" email :json-null)
             "Email hydration preview"))
       :remote-mailbox-ids mailbox-ids
       :keywords keywords))))

(defun chidu-jmap-email-hydration--decode-arguments
    (arguments kind remote-email-ids)
  "Decode Email/get ARGUMENTS of KIND for exact REMOTE-EMAIL-IDS."
  (let* ((state
          (chidu-jmap--string
           (chidu-jmap--required arguments "state" "Email/get")
           "Email/get state" t))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Email/get")
           "Email/get list"))
         (not-found
          (chidu-jmap-email--nullable-id-vector
           (chidu-jmap--required arguments "notFound" "Email/get")
           "Email/get notFound"))
         (expected (make-hash-table :test #'equal))
         (settled (make-hash-table :test #'equal)))
    (chidu-jmap-email-hydration--properties kind)
    (cl-loop
     for remote-id across remote-email-ids
     do
     (setq remote-id
           (chidu-jmap--id remote-id "Email hydration target id"))
     (when (gethash remote-id expected)
       (signal 'chidu-jmap-error
               '("Email hydration targets contain a duplicate id")))
     (puthash remote-id t expected))
    (cl-loop
     for wire across wire-list
     for result =
     (chidu-jmap-email-hydration--found-result wire kind expected)
     for remote-id =
     (chidu-store-email-hydration-result-remote-email-id result)
     do
     (when (gethash remote-id settled)
       (signal 'chidu-jmap-error
               '("Email/get settled one hydration id twice")))
     (puthash remote-id result settled))
    (cl-loop
     for remote-id across not-found
     do
     (unless (gethash remote-id expected)
       (signal 'chidu-jmap-error
               '("Email/get returned an unrequested hydration notFound id")))
     (when (gethash remote-id settled)
       (signal 'chidu-jmap-error
               '("Email/get settled one hydration id twice")))
     (puthash
      remote-id
      (chidu-store-email-hydration-result-create
       :remote-email-id remote-id :found-p nil)
      settled))
    (unless (= (hash-table-count expected) (hash-table-count settled))
      (signal 'chidu-jmap-error
              '("Email/get did not settle every hydration target")))
    (chidu-store-email-hydration-observation-create
     :kind kind
     :state state
     :results
     (vconcat
      (cl-loop
       for remote-id across remote-email-ids
       collect (gethash remote-id settled))))))

(defun chidu-jmap-email-hydration--decode
    (bytes remote-account-id kind remote-email-ids call-id)
  "Decode Email/get BYTES for REMOTE-ACCOUNT-ID and REMOTE-EMAIL-IDS.

KIND selects the property profile and CALL-ID identifies the invocation."
  (let ((response
         (chidu-jmap-parse-single-method-response
          bytes "Email/get" call-id remote-account-id)))
    (chidu-jmap-email-hydration--decode-arguments
     (chidu-jmap-method-response-arguments response)
     kind remote-email-ids)))

(defun chidu-jmap-email-fetch-hydration (context secret plan deliver)
  "Fetch homogeneous metadata PLAN from sync CONTEXT using SECRET.

DELIVER receives a typed result."
  (unless (chidu-store-email-sync-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-sync-context-p context)))
  (unless (and (chidu-store-email-hydration-plan-p plan)
               (> (length (chidu-store-email-hydration-plan-targets plan)) 0))
    (signal 'wrong-type-argument
            (list 'nonempty-email-hydration-plan-p plan)))
  (let* ((targets (chidu-store-email-hydration-plan-targets plan))
         (remote-email-ids
          (vconcat
           (cl-loop
            for target across targets
            collect
            (chidu-store-email-hydration-target-remote-email-id target))))
         (endpoint (chidu-store-email-sync-context-endpoint context))
         (account (chidu-store-email-sync-context-account context))
         (remote-account-id
          (chidu-store-account-remote-account-id account)))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-email-hydration--request
      remote-account-id
      (chidu-store-email-hydration-plan-kind plan)
      remote-email-ids)
     (lambda (bytes)
       (chidu-jmap-email-hydration--decode
        bytes remote-account-id
        (chidu-store-email-hydration-plan-kind plan)
        remote-email-ids "email-hydration"))
     deliver)))

(provide 'chidu-jmap-email-hydration)

;;; chidu-jmap-email-hydration.el ends here
