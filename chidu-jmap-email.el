;;; chidu-jmap-email.el --- JMAP Email baseline requests -*- lexical-binding: t; -*-

;;; Commentary:

;; The first Email bootstrap effects: read the Email object state, then read
;; one stable Email/query membership page.  Durable progress belongs to the
;; Store; this module only constructs requests and validates bounded results.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-api)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-result)
(require 'subr-x)
(require 'chidu-store)

(chidu-define-record chidu-jmap-email-mutable-target
    "Current mutable state for one reconciled Email target."
  remote-id
  found-p
  (remote-mailbox-ids (vector))
  seen-p)

(chidu-define-record chidu-jmap-email-mutable-state
    "Authoritative targeted Email/get reconciliation result."
  state
  (targets (vector)))

(defconst chidu-jmap-email-header-id-limit 256
  "Maximum parsed message ids retained from one Email header property.")

(defun chidu-jmap-email--true-map (value context &optional id-keys-p)
  "Validate Boolean-set object VALUE for CONTEXT.

When ID-KEYS-P is non-nil, require every key to be a JMAP Id."
  (let ((object (chidu-jmap--hash value context))
        (copy (make-hash-table :test #'equal)))
    (maphash
     (lambda (key flag)
       (let ((validated-key
              (if id-keys-p
                  (chidu-jmap--id key context)
                (chidu-jmap--string key context))))
         (unless (eq flag t)
           (signal 'chidu-jmap-error
                   (list (format "%s values must be true" context))))
         (puthash validated-key t copy)))
     object)
    copy))

(defun chidu-jmap-email--address-vector (value context)
  "Decode nullable EmailAddress array VALUE for CONTEXT.

Return immutable Store address records in server order.  JMAP address parsing
is best effort, so preserve malformed or empty addr-spec Strings."
  (if (eq value :json-null)
      (vector)
    (let ((addresses (chidu-jmap--vector value context))
          result)
      (cl-loop
       for wire across addresses
       for ordinal from 0
       for label = (format "%s address %d" context ordinal)
       for address = (chidu-jmap--hash wire label)
       for wire-name = (chidu-jmap--required address "name" label)
       for name =
       (unless (eq wire-name :json-null)
         (chidu-jmap--string wire-name (format "%s name" label) t))
       for email =
       (chidu-jmap--string
        (chidu-jmap--required address "email" label)
        (format "%s email" label) t)
       do
       (push
        (chidu-store-email-address-create :name name :email email)
        result))
      (vconcat (nreverse result)))))

(defun chidu-jmap-email--first-address (value context)
  "Validate nullable address array VALUE for CONTEXT and return its first pair."
  (let ((addresses (chidu-jmap-email--address-vector value context)))
    (when (> (length addresses) 0)
      (let ((address (aref addresses 0)))
        (cons (chidu-store-email-address-name address)
              (chidu-store-email-address-email address))))))

(defun chidu-jmap-email--header-id-vector (value context)
  "Normalize nullable message-id array VALUE for CONTEXT.

Empty and duplicate strings are ignored.  Retained values are bounded by
`chidu-jmap-email-header-id-limit'."
  (if (eq value :json-null)
      (vector)
    (let ((wire (chidu-jmap--vector value context))
          (seen (make-hash-table :test #'equal))
          result)
      (when (> (length wire) chidu-jmap-email-header-id-limit)
        (signal 'chidu-jmap-error
                (list (format "%s contains too many message ids" context))))
      (cl-loop
       for item across wire
       for text = (chidu-jmap--string item context t)
       unless (or (string-empty-p text) (gethash text seen))
       do (puthash text t seen)
       and do (push text result))
      (vconcat (nreverse result)))))

(defun chidu-jmap-email--decode-view-observation
    (wire expected-ids &optional context)
  "Decode one Email WIRE object expected by EXPECTED-IDS.

Return (SUMMARY-ROW . REMOTE-MAILBOX-IDS).  CONTEXT customizes validation
diagnostics."
  (let* ((label (or context "Email view object"))
         (email (chidu-jmap--hash wire label))
         (remote-id
          (chidu-jmap--id
           (chidu-jmap--required email "id" label)
           "Email id")))
    (unless (gethash remote-id expected-ids)
      (signal 'chidu-jmap-error
              (list (format "%s returned an unrequested Email" label))))
    (let* ((mailboxes
            (chidu-jmap-email--true-map
             (chidu-jmap--required email "mailboxIds" label)
             "Email mailboxIds" t))
           (keywords
            (chidu-jmap-email--true-map
             (chidu-jmap--required email "keywords" label)
             "Email keywords"))
           (from
            (chidu-jmap-email--first-address
             (chidu-jmap--required email "from" label)
             "Email from"))
           (mailbox-ids
            (vconcat (sort (hash-table-keys mailboxes) #'string<))))
      (cons
       (chidu-store-email-summary-observation-row-create
        :remote-email-id remote-id
        :remote-thread-id
        (chidu-jmap--id
         (chidu-jmap--required email "threadId" label)
         "Email threadId")
        :received-at
        (chidu-jmap--string
         (chidu-jmap--required email "receivedAt" label)
         "Email receivedAt")
        :from-name (and from (car from))
        :from-email (and from (cdr from))
        :subject
        (let ((value (chidu-jmap--required email "subject" label)))
          (if (eq value :json-null)
              ""
            (chidu-jmap--string value "Email subject" t)))
        :preview
        (let ((value (gethash "preview" email "")))
          ;; Deployed servers may omit an empty derived preview hint.
          (if (eq value :json-null)
              ""
            (chidu-jmap--string value "Email preview" t)))
        :unread-p (not (gethash "$seen" keywords))
        :flagged-p (and (gethash "$flagged" keywords) t)
        :has-attachment-p
        (chidu-jmap--json-boolean
         (chidu-jmap--required email "hasAttachment" label)
         "Email hasAttachment"))
       mailbox-ids))))

(defun chidu-jmap-email--decode-view-row
    (wire expected-ids &optional required-mailbox-id context)
  "Decode one Email WIRE object expected by EXPECTED-IDS.

When REQUIRED-MAILBOX-ID is non-nil, return nil if the Email moved out of that
Mailbox.  CONTEXT customizes validation diagnostics."
  (pcase-let* ((`(,row . ,mailbox-ids)
                (chidu-jmap-email--decode-view-observation
                 wire expected-ids context)))
    (when (or (null required-mailbox-id)
              (seq-contains-p mailbox-ids required-mailbox-id #'equal))
      row)))

(defun chidu-jmap-email--nullable-id-vector (value context)
  "Decode nullable Id vector VALUE for CONTEXT."
  (if (eq value :json-null)
      (vector)
    (let ((wire (chidu-jmap--vector value context))
          (seen (make-hash-table :test #'equal))
          ids)
      (cl-loop for item across wire
               for id = (chidu-jmap--id item context)
               do
               (when (gethash id seen)
                 (signal 'chidu-jmap-error
                         (list (format "%s contains a duplicate id" context))))
               (puthash id t seen)
               (push id ids))
      (vconcat (nreverse ids)))))

(defun chidu-jmap-email--decode-view-get
    (arguments remote-email-ids &optional context)
  "Decode Email/get ARGUMENTS for exact REMOTE-EMAIL-IDS.

Return `(EMAIL-STATE . ROWS-BY-ID)'.  ROWS-BY-ID maps each returned id to
`(SUMMARY-ROW . REMOTE-MAILBOX-IDS)'.  CONTEXT customizes diagnostics."
  (let* ((label (or context "Email view object"))
         (arguments (chidu-jmap--hash arguments "Email/get"))
         (email-state
          (chidu-jmap--string
           (chidu-jmap--required arguments "state" "Email/get")
           "Email state"))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Email/get")
           "Email/get list"))
         (wire-not-found
          (chidu-jmap-email--nullable-id-vector
           (chidu-jmap--required arguments "notFound" "Email/get")
           "Email/get notFound"))
         (expected (make-hash-table :test #'equal))
         (settled (make-hash-table :test #'equal))
         (rows (make-hash-table :test #'equal)))
    (cl-loop for id across remote-email-ids do (puthash id t expected))
    (cl-loop
     for wire across wire-list
     for object = (chidu-jmap--hash wire label)
     for id =
     (chidu-jmap--id
      (chidu-jmap--required object "id" label) "Email id")
     do
     (unless (gethash id expected)
       (signal 'chidu-jmap-error
               (list (format "%s returned an unrequested Email" label))))
     (when (gethash id settled)
       (signal 'chidu-jmap-error
               '("Email/get settled one Email more than once")))
     (puthash id t settled)
     (puthash id
              (chidu-jmap-email--decode-view-observation
               wire expected label)
              rows))
    (cl-loop
     for id across wire-not-found
     do
     (unless (gethash id expected)
       (signal 'chidu-jmap-error
               '("Email/get returned an unrequested notFound id")))
     (when (gethash id settled)
       (signal 'chidu-jmap-error
               '("Email/get settled one Email more than once")))
     (puthash id t settled))
    (unless (= (hash-table-count expected) (hash-table-count settled))
      (signal 'chidu-jmap-error
              '("Email/get did not settle every requested Email")))
    (cons email-state rows)))

(defun chidu-jmap-email--state-request (remote-account-id)
  "Return an Email/get request that observes REMOTE-ACCOUNT-ID state only."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Email/get"
             (:accountId ,remote-account-id :ids [] :properties ["id"])
             "email-state"]]))

(defun chidu-jmap-email--validate-state (bytes remote-account-id)
  "Validate Email/get state-only BYTES for REMOTE-ACCOUNT-ID."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Email/get" "email-state" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Email/get")
           "Email/get list"))
         (not-found
          (chidu-jmap--vector
           (chidu-jmap--required arguments "notFound" "Email/get")
           "Email/get notFound")))
    (unless (and (zerop (length list)) (zerop (length not-found)))
      (signal 'chidu-jmap-error
              '("Email/get ids=[] returned objects or notFound ids")))
    (chidu-jmap--string
     (chidu-jmap--required arguments "state" "Email/get")
     "Email state")))

(defun chidu-jmap-email--mutable-request
    (remote-account-id remote-email-ids)
  "Return REMOTE-ACCOUNT-ID Email/get request for REMOTE-EMAIL-IDS."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Email/get"
             (:accountId ,remote-account-id
                         :ids ,remote-email-ids
                         :properties ["id" "mailboxIds" "keywords"])
             "email-mutable"]]))

(defun chidu-jmap-email-mutable-request-size (account remote-email-ids)
  "Return encoded mutable Email/get size for ACCOUNT and REMOTE-EMAIL-IDS."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (string-bytes
   (chidu-jmap-http-encode-body
    (chidu-jmap-email--mutable-request
     (chidu-store-account-remote-account-id account)
     remote-email-ids))))

(defun chidu-jmap-email--validate-mutable-state
    (bytes remote-account-id remote-email-ids)
  "Validate mutable Email/get BYTES for REMOTE-ACCOUNT-ID and REMOTE-EMAIL-IDS."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Email/get" "email-mutable" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (state
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
         (settled (make-hash-table :test #'equal))
         (targets (make-hash-table :test #'equal)))
    (cl-loop
     for remote-id across remote-email-ids
     do
     (let ((id (chidu-jmap--id remote-id "Email/get requested id")))
       (when (gethash id expected)
         (signal 'chidu-jmap-error
                 '("Email/get contains a duplicate requested id")))
       (puthash id t expected)))
    (cl-loop
     for wire across wire-list
     do
     (let* ((email (chidu-jmap--hash wire "Email/get mutable object"))
            (remote-id
             (chidu-jmap--id
              (chidu-jmap--required email "id" "Email/get mutable object")
              "Email/get id")))
       (unless (gethash remote-id expected)
         (signal 'chidu-jmap-error
                 '("Email/get returned an unrequested id")))
       (when (gethash remote-id settled)
         (signal 'chidu-jmap-error
                 '("Email/get settled one id more than once")))
       (let ((mailboxes
              (chidu-jmap-email--true-map
               (chidu-jmap--required
                email "mailboxIds" "Email/get mutable object")
               "Email/get mailboxIds" t))
             (keywords
              (chidu-jmap-email--true-map
               (chidu-jmap--required
                email "keywords" "Email/get mutable object")
               "Email/get keywords")))
         (puthash
          remote-id
          (chidu-jmap-email-mutable-target-create
           :remote-id remote-id
           :found-p t
           :remote-mailbox-ids
           (vconcat (sort (hash-table-keys mailboxes) #'string<))
           :seen-p (and (gethash "$seen" keywords) t))
          targets)
         (puthash remote-id t settled))))
    (cl-loop
     for remote-id across not-found
     do
     (unless (gethash remote-id expected)
       (signal 'chidu-jmap-error
               '("Email/get returned an unrequested notFound id")))
     (when (gethash remote-id settled)
       (signal 'chidu-jmap-error
               '("Email/get settled one id more than once")))
     (puthash
      remote-id
      (chidu-jmap-email-mutable-target-create
       :remote-id remote-id :found-p nil)
      targets)
     (puthash remote-id t settled))
    (unless (= (hash-table-count expected) (hash-table-count settled))
      (signal 'chidu-jmap-error
              '("Email/get did not settle every requested id")))
    (chidu-jmap-email-mutable-state-create
     :state state
     :targets
     (vconcat
      (cl-loop
       for remote-id across remote-email-ids
       collect (gethash remote-id targets))))))

(defun chidu-jmap-email--query-request
    (remote-account-id anchor-remote-email-id limit)
  "Return a baseline Email/query request for REMOTE-ACCOUNT-ID.

Use ANCHOR-REMOTE-EMAIL-ID with anchorOffset 1 when non-nil; otherwise start
at position zero.  LIMIT bounds the returned id page."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
           :methodCalls
           [["Email/query"
             (:accountId ,remote-account-id
                         :filter :json-null
                         :sort :json-null
                         :collapseThreads :json-false
                         :calculateTotal :json-false
                         :limit ,limit
                         ,@(if anchor-remote-email-id
                               `(:anchor ,anchor-remote-email-id :anchorOffset 1)
                             '(:position 0)))
             "email-query"]]))

(defun chidu-jmap-email--decode-query-page-arguments
    (arguments requested-limit &optional calculate-total-p)
  "Decode Email/query ARGUMENTS bounded by REQUESTED-LIMIT.

When CALCULATE-TOTAL-P is nil, reject a returned total."
  (let* ((arguments (chidu-jmap--hash arguments "Email/query"))
         (query-state
          (chidu-jmap--string
           (chidu-jmap--required arguments "queryState" "Email/query")
           "Email queryState"))
         (can-calculate-changes-p
          (chidu-jmap--json-boolean
           (chidu-jmap--required
            arguments "canCalculateChanges" "Email/query")
           "Email/query canCalculateChanges"))
         (position
          (chidu-jmap--safe-nonnegative-integer
           (chidu-jmap--required arguments "position" "Email/query")
           "Email/query position"))
         (wire-ids
          (chidu-jmap--vector
           (chidu-jmap--required arguments "ids" "Email/query")
           "Email/query ids"))
         (missing (make-symbol "missing"))
         (reported-total (gethash "total" arguments missing))
         (reported-limit (gethash "limit" arguments missing))
         (server-limit
          (unless (eq reported-limit missing)
            (chidu-jmap--safe-nonnegative-integer
             reported-limit "Email/query enforced limit")))
         (seen (make-hash-table :test #'equal))
         ids)
    (if calculate-total-p
        (unless (eq reported-total missing)
          (chidu-jmap--safe-nonnegative-integer
           reported-total "Email/query total"))
      (unless (eq reported-total missing)
        (signal 'chidu-jmap-error
                '("Email/query returned total although calculateTotal was false"))))
    (when (> (length wire-ids) requested-limit)
      (signal 'chidu-jmap-error
              '("Email/query returned more ids than requested")))
    (cl-loop for wire-id across wire-ids
             for remote-id = (chidu-jmap--id wire-id "Email id")
             do
             (when (gethash remote-id seen)
               (signal 'chidu-jmap-error
                       '("Email/query returned a duplicate id")))
             (puthash remote-id t seen)
             (push remote-id ids))
    (chidu-store-email-query-page-observation-create
     :query-state query-state
     :can-calculate-changes-p can-calculate-changes-p
     :position position
     :server-limit server-limit
     :remote-email-ids (vconcat (nreverse ids)))))

(defun chidu-jmap-email-query-page-slice (page page-size request-limit)
  "Return a bounded display slice from query PAGE.

PAGE-SIZE is the maximum number of ids retained in this page.  REQUEST-LIMIT is
the larger probe limit sent to Email/query.  The result is a plist containing
`:ids', `:cursor', and `:maybe-more-p'."
  (unless (chidu-store-email-query-page-observation-p page)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-query-page-observation-p page)))
  (unless (and (integerp page-size) (> page-size 0)
               (integerp request-limit) (>= request-limit page-size))
    (signal 'wrong-type-argument
            (list 'valid-query-page-bounds-p page-size request-limit)))
  (let* ((ids
          (chidu-store-email-query-page-observation-remote-email-ids page))
         (count (length ids))
         (visible-count (min page-size count))
         (visible-ids
          (if (= visible-count count)
              (copy-sequence ids)
            (cl-subseq ids 0 visible-count)))
         (server-limit
          (chidu-store-email-query-page-observation-server-limit page))
         (server-clamped-full-p
          (and (integerp server-limit)
               (< server-limit request-limit)
               (= count server-limit)))
         (probe-full-p
          (and (= request-limit page-size)
               (= count request-limit)))
         (maybe-more-p
          (or (> count page-size)
              server-clamped-full-p
              probe-full-p))
         (cursor
          (and (> visible-count 0)
               (aref visible-ids (1- visible-count)))))
    (list :ids visible-ids
          :cursor cursor
          :maybe-more-p maybe-more-p)))

(defun chidu-jmap-email-page-bounds (endpoint requested-page-size)
  "Return `(PAGE-SIZE . REQUEST-LIMIT)' for ENDPOINT and REQUESTED-PAGE-SIZE.

REQUEST-LIMIT reserves one extra Email/query id as a continuation probe when
Session.maxObjectsInGet permits it."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (and (integerp requested-page-size) (> requested-page-size 0))
    (signal 'wrong-type-argument
            (list 'positive-integer-p requested-page-size)))
  (let ((server-limit
         (chidu-store-endpoint-max-objects-in-get endpoint)))
    (cond
     ((null server-limit)
      (cons requested-page-size (1+ requested-page-size)))
     ((= server-limit 1) (cons 1 1))
     (t
      (let ((page-size (min requested-page-size (1- server-limit))))
        (cons page-size (1+ page-size)))))))

(defun chidu-jmap-email--validate-query-page
    (bytes remote-account-id requested-limit)
  "Validate Email/query BYTES for REMOTE-ACCOUNT-ID and REQUESTED-LIMIT."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Email/query" "email-query" remote-account-id)))
    (chidu-jmap-email--decode-query-page-arguments
     (chidu-jmap-method-response-arguments response) requested-limit)))

(defun chidu-jmap-email-fetch-state (context secret deliver)
  "Fetch Email state for CONTEXT using SECRET and call DELIVER."
  (let ((remote-account-id
         (chidu-store-account-remote-account-id
          (chidu-store-email-sync-context-account context))))
    (chidu-jmap-api-start
     (chidu-store-email-sync-context-endpoint context) secret
     (chidu-jmap-email--state-request remote-account-id)
     (lambda (bytes)
       (chidu-jmap-email--validate-state bytes remote-account-id))
     deliver)))

(defun chidu-jmap-email-fetch-query-page (context secret limit deliver)
  "Fetch a query page for CONTEXT using SECRET and LIMIT; call DELIVER."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'positive-integer-p limit)))
  (let ((remote-account-id
         (chidu-store-account-remote-account-id
          (chidu-store-email-sync-context-account context))))
    (chidu-jmap-api-start
     (chidu-store-email-sync-context-endpoint context) secret
     (chidu-jmap-email--query-request
      remote-account-id
      (chidu-store-email-sync-context-anchor-remote-email-id context)
      limit)
     (lambda (bytes)
       (chidu-jmap-email--validate-query-page
        bytes remote-account-id limit))
     deliver)))

(defun chidu-jmap-email-fetch-mutable-state
    (endpoint account remote-email-ids secret deliver)
  "From ENDPOINT, fetch ACCOUNT mutable state for REMOTE-EMAIL-IDS.

Use owned SECRET and call DELIVER with the typed result."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (and (vectorp remote-email-ids) (> (length remote-email-ids) 0))
    (signal 'wrong-type-argument
            (list 'nonempty-email-id-vector-p remote-email-ids)))
  (let ((remote-account-id
         (chidu-store-account-remote-account-id account)))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-email--mutable-request
      remote-account-id remote-email-ids)
     (lambda (bytes)
       (chidu-jmap-email--validate-mutable-state
        bytes remote-account-id remote-email-ids))
     deliver)))

(provide 'chidu-jmap-email)

;;; chidu-jmap-email.el ends here
