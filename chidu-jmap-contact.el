;;; chidu-jmap-contact.el --- Strict JMAP Contacts read adapter -*- lexical-binding: t; -*-

;;; Commentary:

;; RFC 9610 AddressBook and ContactCard reads.  The adapter preserves server
;; query order, validates exact /query -> /get coverage, and projects selected
;; JSContact fields into small read models.  It owns no Store or UI state.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'chidu-contact-model)
(require 'chidu-jmap-api)
(require 'chidu-jscontact)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-result)
(require 'chidu-store)

(defconst chidu-jmap-contact-address-book-limit 4096
  "Maximum AddressBooks accepted from one complete AddressBook/get.")

(defconst chidu-jmap-contact-summary-properties
  ["id" "uid" "kind" "name" "emails" "phones"
   "organizations" "titles" "updated" "addressBookIds"]
  "ContactCard properties used by bounded list projections.")

(defconst chidu-jmap-contact-detail-properties
  ["id" "uid" "kind" "name" "emails" "phones"
   "organizations" "titles" "addresses" "onlineServices" "notes"
   "members" "created" "updated" "addressBookIds"]
  "ContactCard properties used by the read-only detail projection.")

(defun chidu-jmap-contact--query-text (value context)
  "Return Contact query text VALUE for CONTEXT."
  (chidu-jmap--string value context t))

(defun chidu-jmap-contact--limit (value context)
  "Return positive bounded Contact query VALUE for CONTEXT."
  (unless (and (integerp value) (> value 0) (<= value 256))
    (signal 'chidu-jmap-error
            (list (format "%s must be in the range 1..256" context))))
  value)

(defun chidu-jmap-contact--rights (wire)
  "Decode AddressBook rights WIRE."
  (let ((rights (chidu-jmap--hash wire "AddressBook myRights")))
    (chidu-contact-rights-create
     :may-read-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayRead" "AddressBook myRights")
      "AddressBook mayRead")
     :may-write-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayWrite" "AddressBook myRights")
      "AddressBook mayWrite")
     :may-share-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayShare" "AddressBook myRights")
      "AddressBook mayShare")
     :may-delete-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required rights "mayDelete" "AddressBook myRights")
      "AddressBook mayDelete"))))

(defun chidu-jmap-contact--address-book (wire)
  "Decode one AddressBook WIRE object."
  (let* ((context "AddressBook/get item")
         (book (chidu-jmap--hash wire context))
         (name
          (chidu-jmap--string
           (chidu-jmap--required book "name" context) "AddressBook name"))
         (sort-order
          (chidu-jmap--safe-nonnegative-integer
           (chidu-jmap--required book "sortOrder" context)
           "AddressBook sortOrder")))
    (unless (< sort-order #x80000000)
      (signal 'chidu-jmap-error '("AddressBook sortOrder exceeds 2^31")))
    (when (> (string-bytes name) 255)
      (signal 'chidu-jmap-error '("AddressBook name exceeds 255 octets")))
    (chidu-address-book-create
     :remote-id
     (chidu-jmap--id
      (chidu-jmap--required book "id" context) "AddressBook id")
     :name name
     :description
     (let ((value (gethash "description" book :json-null)))
       (unless (eq value :json-null)
         (chidu-jmap--string value "AddressBook description" t)))
     :sort-order sort-order
     :default-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required book "isDefault" context)
      "AddressBook isDefault")
     :subscribed-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required book "isSubscribed" context)
      "AddressBook isSubscribed")
     :rights
     (chidu-jmap-contact--rights
      (chidu-jmap--required book "myRights" context)))))

(defun chidu-jmap-contact--address-book-less-p (left right)
  "Return non-nil when AddressBook LEFT sorts before RIGHT."
  (let ((left-order (chidu-address-book-sort-order left))
        (right-order (chidu-address-book-sort-order right)))
    (cond
     ((/= left-order right-order) (< left-order right-order))
     ((not (equal (chidu-address-book-name left)
                  (chidu-address-book-name right)))
      (string-collate-lessp
       (chidu-address-book-name left) (chidu-address-book-name right)))
     (t
      (string-lessp
       (chidu-address-book-remote-id left)
       (chidu-address-book-remote-id right))))))

(defun chidu-jmap-contact-address-books-request (remote-account-id)
  "Return AddressBook/get request for REMOTE-ACCOUNT-ID."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "contacts Account id"))
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-contacts-capability]
           :methodCalls
           [["AddressBook/get"
             (:accountId ,remote-account-id
                         :properties
                         ["id" "name" "description" "sortOrder" "isDefault"
                          "isSubscribed" "myRights"])
             "addressbook-get"]]))

(defun chidu-jmap-contact-validate-address-books-response
    (bytes remote-account-id)
  "Decode AddressBook/get BYTES for REMOTE-ACCOUNT-ID."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "AddressBook/get" "addressbook-get" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "AddressBook/get")
           "AddressBook/get list"))
         (not-found
          (chidu-jmap--nullable-vector
           (chidu-jmap--required arguments "notFound" "AddressBook/get")
           "AddressBook/get notFound"))
         (seen (make-hash-table :test #'equal))
         (default-count 0)
         books)
    (when (> (length not-found) 0)
      (signal 'chidu-jmap-error
              '("AddressBook/get all returned unexpected notFound ids")))
    (when (> (length wire-list) chidu-jmap-contact-address-book-limit)
      (signal 'chidu-jmap-error
              '("AddressBook/get returned too many AddressBooks")))
    (cl-loop
     for wire across wire-list
     for book = (chidu-jmap-contact--address-book wire)
     for id = (chidu-address-book-remote-id book)
     do
     (when (gethash id seen)
       (signal 'chidu-jmap-error
               '("AddressBook/get returned a duplicate id")))
     (puthash id t seen)
     (when (chidu-address-book-default-p book)
       (setq default-count (1+ default-count)))
     (push book books))
    (when (> default-count 1)
      (signal 'chidu-jmap-error
              '("AddressBook/get returned multiple default books")))
    (chidu-address-book-directory-create
     :state
     (chidu-jmap--string
      (chidu-jmap--required arguments "state" "AddressBook/get")
      "AddressBook state")
     :address-books
     (vconcat (sort books #'chidu-jmap-contact--address-book-less-p)))))

(defun chidu-jmap-contact--query-filter (address-book-id query)
  "Return ContactCard filter for ADDRESS-BOOK-ID and QUERY."
  (append
   (list :inAddressBook address-book-id)
   (unless (string-empty-p query) (list :text query))))

(defun chidu-jmap-contact-page-request
    (remote-account-id address-book-id query limit &optional anchor-id)
  "Return bounded ContactCard page request.

REMOTE-ACCOUNT-ID owns ADDRESS-BOOK-ID.  QUERY is a text filter, LIMIT bounds
cards, and ANCHOR-ID resumes after a prior page when non-nil."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "contacts Account id")
        address-book-id
        (chidu-jmap--id address-book-id "AddressBook id")
        query (chidu-jmap-contact--query-text query "ContactCard query")
        limit (chidu-jmap-contact--limit limit "ContactCard page limit")
        anchor-id
        (and anchor-id (chidu-jmap--id anchor-id "ContactCard anchor id")))
  (let ((arguments
         (append
          (list
           :accountId remote-account-id
           :filter (chidu-jmap-contact--query-filter address-book-id query)
           :sort [(:property "updated" :isAscending :json-false)])
          (if anchor-id
              (list :anchor anchor-id :anchorOffset 1)
            (list :position 0))
          (list :limit limit :calculateTotal t))))
    `(:using [,chidu-jmap-core-capability ,chidu-jmap-contacts-capability]
             :methodCalls
             [["ContactCard/query" ,arguments "contact-query"]
              ["ContactCard/get"
               (:accountId ,remote-account-id
                           ,(intern ":#ids")
                           (:resultOf "contact-query" :name "ContactCard/query" :path "/ids")
                           :properties ,chidu-jmap-contact-summary-properties)
               "contact-get"]])))

(defun chidu-jmap-contact--method-error-type (bytes call-id)
  "Return method error type for CALL-ID in BYTES, or nil."
  (let* ((wire (chidu-jmap--parse-json-object bytes "JMAP Contacts response"))
         (responses
          (chidu-jmap--vector
           (chidu-jmap--required
            wire "methodResponses" "JMAP Contacts response")
           "JMAP Contacts methodResponses")))
    (cl-loop
     for item across responses
     when
     (and (vectorp item) (= 3 (length item))
          (equal call-id (aref item 2))
          (equal "error" (aref item 0)))
     return
     (let ((arguments
            (chidu-jmap--hash
             (aref item 1) "JMAP Contacts method error")))
       (chidu-jmap--string
        (chidu-jmap--required
         arguments "type" "JMAP Contacts method error")
        "JMAP Contacts method error type")))))

(defun chidu-jmap-contact--ordered-cards
    (ids wire-list not-found complete-p)
  "Return cards in query IDS order from WIRE-LIST and NOT-FOUND.

COMPLETE-P marks decoded ContactCards as detail projections."
  (let ((expected (make-hash-table :test #'equal))
        (settled (make-hash-table :test #'equal))
        (cards (make-hash-table :test #'equal)))
    (cl-loop for wire-id across ids
             for id = (chidu-jmap--id wire-id "ContactCard/query id")
             do
             (when (gethash id expected)
               (signal 'chidu-jmap-error
                       '("ContactCard/query returned a duplicate id")))
             (puthash id t expected))
    (cl-loop
     for wire across wire-list
     for card = (chidu-jscontact-decode-card wire complete-p)
     for id = (chidu-contact-card-remote-id card)
     do
     (unless (gethash id expected)
       (signal 'chidu-jmap-error
               '("ContactCard/get returned an unrequested id")))
     (when (gethash id settled)
       (signal 'chidu-jmap-error
               '("ContactCard/get returned a duplicate id")))
     (puthash id t settled)
     (puthash id card cards))
    (cl-loop
     for wire-id across not-found
     for id = (chidu-jmap--id wire-id "ContactCard/get notFound id")
     do
     (unless (gethash id expected)
       (signal 'chidu-jmap-error
               '("ContactCard/get returned unrequested notFound")))
     (when (gethash id settled)
       (signal 'chidu-jmap-error
               '("ContactCard/get settled an id twice")))
     (puthash id t settled))
    (maphash
     (lambda (id _present)
       (unless (gethash id settled)
         (signal 'chidu-jmap-error
                 '("ContactCard/get did not settle every queried id"))))
     expected)
    (vconcat
     (cl-loop for wire-id across ids
              for id = (chidu-jmap--id wire-id "ContactCard/query id")
              for card = (gethash id cards)
              when card collect card))))

(defun chidu-jmap-contact-validate-page-response
    (bytes remote-account-id query limit)
  "Decode bounded ContactCard page BYTES.

REMOTE-ACCOUNT-ID, QUERY, and LIMIT are exact request evidence."
  (let ((error-type
         (chidu-jmap-contact--method-error-type bytes "contact-query")))
    (if (equal error-type "anchorNotFound")
        (chidu-result-failure-create
         :kind 'contact-anchor-not-found :data nil :retryable-p t)
      (let* ((responses
              (chidu-jmap-parse-method-responses
               bytes
               `(("ContactCard/query" "contact-query" ,remote-account-id)
                 ("ContactCard/get" "contact-get" ,remote-account-id))))
             (query-arguments
              (chidu-jmap-method-response-arguments (aref responses 0)))
             (get-arguments
              (chidu-jmap-method-response-arguments (aref responses 1)))
             (ids
              (chidu-jmap--vector
               (chidu-jmap--required
                query-arguments "ids" "ContactCard/query")
               "ContactCard/query ids"))
             (wire-list
              (chidu-jmap--vector
               (chidu-jmap--required
                get-arguments "list" "ContactCard/get")
               "ContactCard/get list"))
             (not-found-wire
              (chidu-jmap--required
               get-arguments "notFound" "ContactCard/get"))
             (not-found
              (chidu-jmap--nullable-vector
               not-found-wire "ContactCard/get notFound"))
             (position
              (chidu-jmap--safe-nonnegative-integer
               (chidu-jmap--required
                query-arguments "position" "ContactCard/query")
               "ContactCard/query position"))
             (total
              (chidu-jmap--safe-nonnegative-integer
               (chidu-jmap--required
                query-arguments "total" "ContactCard/query")
               "ContactCard/query total")))
        (when (> (length ids) limit)
          (signal 'chidu-jmap-error
                  '("ContactCard/query exceeded the requested limit")))
        (when (> (+ position (length ids)) total)
          (signal 'chidu-jmap-error
                  '("ContactCard/query position exceeds total results")))
        (let ((next-position (+ position (length ids))))
          (chidu-contact-page-create
           :query-state
           (chidu-jmap--string
            (chidu-jmap--required
             query-arguments "queryState" "ContactCard/query")
            "ContactCard/query queryState")
           :total total
           :position position
           :next-position next-position
           :query query
           :cards
           (chidu-jmap-contact--ordered-cards
            ids wire-list not-found nil)
           :maybe-more-p (< next-position total)
           :anchor-id
           (and (> (length ids) 0)
                (chidu-jmap--id
                 (aref ids (1- (length ids)))
                 "ContactCard/query anchor"))))))))

(defun chidu-jmap-contact-detail-request
    (remote-account-id remote-contact-id)
  "Return REMOTE-ACCOUNT-ID detail request for REMOTE-CONTACT-ID."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "contacts Account id")
        remote-contact-id
        (chidu-jmap--id remote-contact-id "ContactCard id"))
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-contacts-capability]
           :methodCalls
           [["ContactCard/get"
             (:accountId ,remote-account-id
                         :ids [,remote-contact-id]
                         :properties ,chidu-jmap-contact-detail-properties)
             "contact-detail"]]))

(defun chidu-jmap-contact-validate-detail-response
    (bytes remote-account-id remote-contact-id)
  "Decode BYTES for REMOTE-ACCOUNT-ID and REMOTE-CONTACT-ID."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "ContactCard/get" "contact-detail" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "ContactCard/get")
           "ContactCard/get detail list"))
         (not-found-wire
          (chidu-jmap--required arguments "notFound" "ContactCard/get"))
         (not-found
          (chidu-jmap--nullable-vector
           not-found-wire "ContactCard/get detail notFound")))
    (cond
     ((and (= 0 (length wire-list))
           (= 1 (length not-found))
           (equal remote-contact-id
                  (chidu-jmap--id
                   (aref not-found 0) "ContactCard/get detail notFound id")))
      (chidu-result-failure-create
       :kind 'contact-not-found
       :data (list :remote-contact-id remote-contact-id)
       :retryable-p nil))
     ((or (/= 1 (length wire-list)) (> (length not-found) 0))
      (signal 'chidu-jmap-error
              '("ContactCard/get detail has incomplete coverage")))
     (t
      (let ((card (chidu-jscontact-decode-card (aref wire-list 0) t)))
        (unless (equal remote-contact-id (chidu-contact-card-remote-id card))
          (signal 'chidu-jmap-error
                  '("ContactCard/get detail returned the wrong id")))
        card)))))

(defun chidu-jmap-contact-search-request
    (remote-account-id query query-limit)
  "Return native completion search for REMOTE-ACCOUNT-ID QUERY QUERY-LIMIT."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "contacts Account id")
        query (chidu-jmap-contact--query-text query "ContactCard search")
        query-limit
        (chidu-jmap-contact--limit query-limit "ContactCard search limit"))
  (let ((query-arguments
         (append
          (list :accountId remote-account-id)
          (unless (string-empty-p query)
            (list :filter (list :text query)))
          (list
           :sort [(:property "updated" :isAscending :json-false)]
           :position 0 :limit query-limit))))
    `(:using [,chidu-jmap-core-capability ,chidu-jmap-contacts-capability]
             :methodCalls
             [["AddressBook/get"
               (:accountId ,remote-account-id
                           :properties ["id" "isSubscribed" "myRights"])
               "addressbook-get"]
              ["ContactCard/query" ,query-arguments "contact-query"]
              ["ContactCard/get"
               (:accountId ,remote-account-id
                           ,(intern ":#ids")
                           (:resultOf "contact-query" :name "ContactCard/query" :path "/ids")
                           :properties ,chidu-jmap-contact-summary-properties)
               "contact-get"]])))

(defun chidu-jmap-contact--visible-address-book-ids (arguments)
  "Return subscribed readable AddressBook ids from get ARGUMENTS."
  (let* ((wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "AddressBook/get")
           "AddressBook/get list"))
         (not-found
          (chidu-jmap--nullable-vector
           (chidu-jmap--required arguments "notFound" "AddressBook/get")
           "AddressBook/get notFound"))
         (visible (make-hash-table :test #'equal)))
    (when (> (length not-found) 0)
      (signal 'chidu-jmap-error
              '("AddressBook/get all returned unexpected notFound ids")))
    (when (> (length wire-list) chidu-jmap-contact-address-book-limit)
      (signal 'chidu-jmap-error
              '("AddressBook/get returned too many AddressBooks")))
    (cl-loop
     for wire-book across wire-list
     for book = (chidu-jmap--hash wire-book "AddressBook/get item")
     for id =
     (chidu-jmap--id
      (chidu-jmap--required book "id" "AddressBook/get item")
      "AddressBook id")
     for subscribed =
     (chidu-jmap--json-boolean
      (chidu-jmap--required book "isSubscribed" "AddressBook/get item")
      "AddressBook isSubscribed")
     for rights =
     (chidu-jmap-contact--rights
      (chidu-jmap--required book "myRights" "AddressBook/get item"))
     when (and subscribed (chidu-contact-rights-may-read-p rights))
     do (puthash id t visible))
    visible))

(defun chidu-jmap-contact-validate-search-response
    (bytes remote-account-id output-limit query-limit)
  "Decode completion search BYTES for REMOTE-ACCOUNT-ID.

OUTPUT-LIMIT bounds returned addresses; QUERY-LIMIT bounds queried cards."
  (let* ((responses
          (chidu-jmap-parse-method-responses
           bytes
           `(("AddressBook/get" "addressbook-get" ,remote-account-id)
             ("ContactCard/query" "contact-query" ,remote-account-id)
             ("ContactCard/get" "contact-get" ,remote-account-id))))
         (visible
          (chidu-jmap-contact--visible-address-book-ids
           (chidu-jmap-method-response-arguments (aref responses 0))))
         (query (chidu-jmap-method-response-arguments (aref responses 1)))
         (get (chidu-jmap-method-response-arguments (aref responses 2)))
         (ids
          (chidu-jmap--vector
           (chidu-jmap--required query "ids" "ContactCard/query")
           "ContactCard/query ids"))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required get "list" "ContactCard/get")
           "ContactCard/get list"))
         (not-found-wire
          (chidu-jmap--required get "notFound" "ContactCard/get"))
         (not-found
          (chidu-jmap--nullable-vector
           not-found-wire "ContactCard/get notFound"))
         (cards
          (chidu-jmap-contact--ordered-cards
           ids wire-list not-found nil))
         (seen (make-hash-table :test #'equal))
         result)
    (when (> (length ids) query-limit)
      (signal 'chidu-jmap-error
              '("ContactCard/query exceeded the requested limit")))
    (catch 'done
      (cl-loop
       for card across cards
       when
       (and (not (equal "group" (chidu-contact-card-kind card)))
            (chidu-contact-card-in-address-books-p card visible))
       do
       (cl-loop
        for email across (chidu-contact-card-emails card)
        for address = (chidu-contact-value-value email)
        for key = (downcase address)
        unless (gethash key seen)
        do
        (puthash key t seen)
        (push
         (chidu-store-email-address-create
          :name (chidu-contact-card-name card) :email address)
         result)
        when (>= (length result) output-limit)
        do (throw 'done nil))))
    (vconcat (nreverse result))))

(defun chidu-jmap-contact-address-books
    (endpoint remote-account-id secret deliver)
  "Fetch ENDPOINT AddressBooks for REMOTE-ACCOUNT-ID using SECRET.

Deliver the typed result through DELIVER."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "contacts Account id"))
  (chidu-jmap-api-start
   endpoint secret
   (chidu-jmap-contact-address-books-request remote-account-id)
   (lambda (bytes)
     (chidu-jmap-contact-validate-address-books-response
      bytes remote-account-id))
   deliver))

(defun chidu-jmap-contact-page
    (endpoint remote-account-id address-book-id query limit anchor-id
              secret deliver)
  "Fetch ENDPOINT page for REMOTE-ACCOUNT-ID and ADDRESS-BOOK-ID.

QUERY, LIMIT, and ANCHOR-ID define the page; use SECRET and DELIVER."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "contacts Account id")
        address-book-id
        (chidu-jmap--id address-book-id "AddressBook id"))
  (chidu-jmap-api-start
   endpoint secret
   (chidu-jmap-contact-page-request
    remote-account-id address-book-id query limit anchor-id)
   (lambda (bytes)
     (chidu-jmap-contact-validate-page-response
      bytes remote-account-id query limit))
   deliver))

(defun chidu-jmap-contact-detail
    (endpoint remote-account-id remote-contact-id secret deliver)
  "Fetch ENDPOINT REMOTE-CONTACT-ID from REMOTE-ACCOUNT-ID.

Use SECRET and deliver the typed result through DELIVER."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "contacts Account id")
        remote-contact-id
        (chidu-jmap--id remote-contact-id "ContactCard id"))
  (chidu-jmap-api-start
   endpoint secret
   (chidu-jmap-contact-detail-request remote-account-id remote-contact-id)
   (lambda (bytes)
     (chidu-jmap-contact-validate-detail-response
      bytes remote-account-id remote-contact-id))
   deliver))

(defun chidu-jmap-contact-search
    (endpoint remote-account-id query limit secret deliver)
  "Search ENDPOINT REMOTE-ACCOUNT-ID ContactCards for QUERY.

LIMIT bounds results; use SECRET and deliver through DELIVER."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "contacts Account id"))
  (let* ((server-limit
          (or (chidu-store-endpoint-max-objects-in-get endpoint) 256))
         (query-limit (min server-limit 256 (* limit 4))))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-contact-search-request
      remote-account-id query query-limit)
     (lambda (bytes)
       (chidu-jmap-contact-validate-search-response
        bytes remote-account-id limit query-limit))
     deliver)))

(provide 'chidu-jmap-contact)

;;; chidu-jmap-contact.el ends here
