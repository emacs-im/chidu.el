;;; chidu-draft-contract-test.el --- Remote Draft contract tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'cl-lib)
(require 'ert)
(require 'chidu-draft-semantics)
(require 'chidu-jmap-compose)
(require 'chidu-jmap-draft-checkout)
(require 'chidu-result)
(require 'chidu-store)
(require 'chidu-test-support)

(defun chidu-draft-contract-test--map (&rest pairs)
  "Return equality hash map from alternating PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(cl-defun chidu-draft-contract-test--snapshot-create
    (&key remote-email-id remote-blob-id from document
          (resources (vector)))
  "Return REMOTE-EMAIL-ID REMOTE-BLOB-ID FROM DOCUMENT fixture.

RESOURCES defaults to an empty vector."
  (unless (and (vectorp from) (= 1 (length from)))
    (error "Snapshot fixture needs one originator"))
  (let ((shape
         (chidu-draft-editable-shape-from-observations
          (aref from 0) document resources)))
    (chidu-draft-editable-snapshot-create
     :remote-email-id remote-email-id
     :remote-blob-id remote-blob-id
     :shape shape)))

(defun chidu-draft-contract-test--json-object (&rest pairs)
  "Return equality JSON object from alternating PAIRS."
  (apply #'chidu-draft-contract-test--map pairs))

(defun chidu-draft-contract-test--header (name value)
  "Return one complete JMAP EmailHeader using NAME and VALUE."
  (chidu-draft-contract-test--json-object "name" name "value" value))

(cl-defun chidu-draft-contract-test--body-part
    (&key type headers part-id blob-id size
          (name :json-null) (charset :json-null)
          (disposition :json-null) (cid :json-null)
          (language :json-null) (location :json-null)
          (subparts :missing))
  "Return complete TYPE HEADERS JMAP body-part fixture.

PART-ID, BLOB-ID, SIZE, NAME, CHARSET, DISPOSITION, CID, LANGUAGE, LOCATION,
and SUBPARTS supply the remaining wire fields."
  (let ((part
         (chidu-draft-contract-test--json-object
          "partId" (or part-id :json-null)
          "blobId" (or blob-id :json-null)
          "size" (if (null size) :json-null size)
          "headers" headers
          "name" name
          "type" type
          "charset" charset
          "disposition" disposition
          "cid" cid
          "language" language
          "location" location)))
    (unless (eq subparts :missing)
      (puthash "subParts" subparts part))
    part))

(defun chidu-draft-contract-test--body-value (value)
  "Return one complete EmailBodyValue for VALUE."
  (chidu-draft-contract-test--json-object
   "value" value
   "isEncodingProblem" :json-false
   "isTruncated" :json-false))

(defun chidu-draft-contract-test--top-headers (content-type null-subject-p)
  "Return top-level headers for CONTENT-TYPE, omitting Subject by NULL-SUBJECT-P."
  (vconcat
   (delq
    nil
    (list
     (chidu-draft-contract-test--header "From" "Me <me@example.test>")
     (chidu-draft-contract-test--header
      "To" " Alice\r\n\t<alice@example.test>, unfinished ")
     (chidu-draft-contract-test--header "Bcc" " hidden@example.test ")
     (unless null-subject-p
       (chidu-draft-contract-test--header "Subject" "Draft subject"))
     (chidu-draft-contract-test--header
      "Message-ID" "<external-draft@example.test>")
     (chidu-draft-contract-test--header
      "Date" "Fri, 28 Aug 2026 12:00:00 +0800")
     (chidu-draft-contract-test--header "MIME-Version" "1.0")
     (chidu-draft-contract-test--header "Content-Type" content-type)))))

(defun chidu-draft-contract-test--draft-wire (&optional html-p attachment-p null-subject-p)
  "Return one complete external Draft Email fixture.

HTML-P adds an unsupported HTML alternative.  ATTACHMENT-P adds one ordered
attachment.  NULL-SUBJECT-P omits Subject and returns JSON null."
  (let* ((content-type
          (cond
           (html-p "multipart/alternative; boundary=alternative")
           (attachment-p "multipart/mixed; boundary=mixed")
           (t "text/plain; charset=utf-8")))
         (top-headers
          (chidu-draft-contract-test--top-headers content-type null-subject-p))
         (text-headers
          (if (or html-p attachment-p)
              (vector
               (chidu-draft-contract-test--header
                "Content-Type" "text/plain; charset=utf-8")
               (chidu-draft-contract-test--header
                "Content-Transfer-Encoding" "8bit"))
            top-headers))
         (text
          (chidu-draft-contract-test--body-part
           :type "text/plain" :headers text-headers
           :part-id "text" :blob-id "blob-text" :size 16
           :charset "utf-8"))
         (values
          (chidu-draft-contract-test--map
           "text" (chidu-draft-contract-test--body-value "Plain Draft body")))
         html attachment root)
    (when html-p
      (setq
       html
       (chidu-draft-contract-test--body-part
        :type "text/html"
        :headers
        (vector
         (chidu-draft-contract-test--header
          "Content-Type" "text/html; charset=utf-8")
         (chidu-draft-contract-test--header
          "Content-Transfer-Encoding" "8bit"))
        :part-id "html" :blob-id "blob-html" :size 22
        :charset "utf-8")
       root
       (chidu-draft-contract-test--body-part
        :type "multipart/alternative" :headers top-headers
        :subparts (vector text html))))
    (when attachment-p
      (setq
       attachment
       (chidu-draft-contract-test--body-part
        :type "text/plain"
        :headers
        (vector
         (chidu-draft-contract-test--header
          "Content-Type" "text/plain; charset=UTF-8")
         (chidu-draft-contract-test--header
          "Content-Disposition" "attachment; filename=note.txt")
         (chidu-draft-contract-test--header
          "Content-Language" "en"))
        :part-id "attachment" :blob-id "blob-attachment" :size 4
        :name "note.txt" :charset "UTF-8"
        :disposition "attachment" :language ["en"])
       root
       (chidu-draft-contract-test--body-part
        :type "multipart/mixed" :headers top-headers
        :subparts (vector text attachment))))
    (unless root (setq root text))
    (chidu-draft-contract-test--json-object
     "id" "draft-remote"
     "blobId" "blob-draft"
     "messageId" ["external-draft@example.test"]
     "inReplyTo" []
     "references" []
     "mailboxIds" (chidu-draft-contract-test--map "drafts" t)
     "keywords" (chidu-draft-contract-test--map "$draft" t "$seen" t)
     "headers" top-headers
     "from" (vector (chidu-draft-contract-test--json-object
                     "name" "Me" "email" "me@example.test"))
     "sender" :json-null
     "subject" (if null-subject-p :json-null "Draft subject")
     "bodyValues" values
     "textBody" (vector text)
     "htmlBody" (cond (html-p (vector html)) (t (vector text)))
     "bodyStructure" root
     "attachments" (if attachment-p (vector attachment) (vector)))))

(defun chidu-draft-contract-test--checkout-payload (email)
  "Return one exact Draft checkout response for EMAIL."
  (let ((arguments
         (list
          :accountId "remote-account"
          :state "email/state"
          :list (vector email)
          :notFound [])))
    (chidu-store-test--method-response
     "Email/get" "draft-checkout" arguments)))

(defun chidu-draft-contract-test--validate-wire (email)
  "Validate and project one Draft EMAIL fixture."
  (chidu-jmap-draft-checkout-validate-response
   (chidu-draft-contract-test--checkout-payload email)
   "remote-account" "draft-remote" "drafts"))

(defun chidu-draft-contract-test--root-part (email)
  "Return EMAIL's bodyStructure object."
  (gethash "bodyStructure" email))

(defun chidu-draft-contract-test--set-top-headers (email headers)
  "Set EMAIL and its root body part to exact HEADERS."
  (puthash "headers" headers email)
  (puthash "headers" headers (chidu-draft-contract-test--root-part email))
  email)

(defun chidu-draft-contract-test--append-top-header (email name value)
  "Append top-level NAME VALUE to EMAIL and its root body part."
  (chidu-draft-contract-test--set-top-headers
   email
   (vconcat
    (gethash "headers" email)
    (vector (chidu-draft-contract-test--header name value)))))

(defun chidu-draft-contract-test--wire-part-by-id (part wanted-id)
  "Return body PART with WANTED-ID below recursive PART, or nil."
  (if (equal wanted-id (gethash "partId" part))
      part
    (let ((subparts (gethash "subParts" part :json-null)))
      (when (vectorp subparts)
        (cl-loop
         for child across subparts
         for found = (chidu-draft-contract-test--wire-part-by-id child wanted-id)
         when found return found)))))

(defun chidu-draft-contract-test--registered-resource (workspace-id observation)
  "Return registered resource fixture for WORKSPACE-ID and OBSERVATION."
  (chidu-store-compose-resource-create
   :resource-id
   (chidu-store-compose-resource-observation-resource-id observation)
   :workspace-id workspace-id
   :name (chidu-store-compose-resource-observation-name observation)
   :media-type
   (chidu-store-compose-resource-observation-media-type observation)
   :size (chidu-store-compose-resource-observation-size observation)
   :digest (make-string 64 ?a)
   :remote-blob-id
   (chidu-store-compose-resource-observation-remote-blob-id observation)
   :charset (chidu-store-compose-resource-observation-charset observation)
   :disposition
   (chidu-store-compose-resource-observation-disposition observation)
   :cid (chidu-store-compose-resource-observation-cid observation)
   :language
   (copy-sequence
    (chidu-store-compose-resource-observation-language observation))
   :location (chidu-store-compose-resource-observation-location observation)))

(ert-deftest chidu-jmap-draft-checkout-maps-wire-and-support-failures ()
  "The API boundary must preserve unsupported values and type invalid wire."
  (let* ((endpoint
          (chidu-store-endpoint-create
           :endpoint-id "endpoint"
           :session-url "https://mail.example.test/.well-known/jmap"
           :login "me@example.test"
           :authentication 'basic
           :api-url "https://mail.example.test/jmap/api"
           :max-size-request 1048576))
         (account
          (chidu-store-account-create
           :account-id "account"
           :remote-account-id "remote-account"
           :name "Mail"
           :available-p t))
         (mailbox
          (chidu-store-mailbox-create
           :mailbox-id "mailbox"
           :remote-mailbox-id "drafts"
           :name "Drafts"
           :role "drafts"
           :available-p t))
         (invalid (chidu-draft-contract-test--draft-wire nil nil nil))
         (unsupported (chidu-draft-contract-test--draft-wire nil nil nil)))
    (puthash
     "subParts" [(:invalid t)]
     (chidu-draft-contract-test--root-part invalid))
    (chidu-draft-contract-test--append-top-header
     unsupported "X-Chidu-Unknown" "opaque")
    (dolist
        (case
         (list
          (list "invalid wire" invalid 'invalid-jmap-response)
          (list "unsupported shape" unsupported 'draft-metadata-unsupported)))
      (pcase-let ((`(,label ,email ,expected-kind) case))
        (ert-info ((format "API failure mapping: %s" label))
          (let ((payload
                 (chidu-draft-contract-test--checkout-payload email))
                result)
            (cl-letf
                (((symbol-function 'chidu-jmap-http-request)
                  (lambda (_url _login _authentication _secret callback
                                &rest _arguments)
                    (funcall
                     callback
                     (chidu-result-ok-create
                      :value
                      (chidu-jmap-http-response-create
                       :status 200
                       :body payload
                       :content-type "application/json")))
                    nil)))
              (should-not
               (chidu-jmap-draft-checkout
                endpoint account mailbox "draft-remote"
                (copy-sequence "secret")
                (lambda (value) (setq result value)))))
            (should (chidu-result-failure-p result))
            (should (eq expected-kind
                        (chidu-result-failure-kind result)))
            (should-not (chidu-result-failure-retryable-p result))))))))

(ert-deftest chidu-jmap-draft-checkout-builds-editable-snapshot ()
  "Strict wire observation should project to one closed editable snapshot."
  (let* ((request
           (chidu-jmap-draft-checkout-request
            "remote-account" "draft-remote"))
         (arguments (aref (aref (plist-get request :methodCalls) 0) 1))
         (properties (plist-get arguments :properties))
         (body-properties (plist-get arguments :bodyProperties))
         (snapshot
          (chidu-jmap-draft-checkout-validate-response
           (chidu-draft-contract-test--checkout-payload
            (chidu-draft-contract-test--draft-wire nil nil nil))
           "remote-account" "draft-remote" "drafts"))
         (shape (chidu-draft-editable-snapshot-shape snapshot)))
    (should (= 0 (plist-get arguments :maxBodyValueBytes)))
    (should (eq t (plist-get arguments :fetchTextBodyValues)))
    (should-not (plist-member arguments :fetchHTMLBodyValues))
    (dolist (property
             '("blobId" "headers" "sender" "messageId"
               "inReplyTo" "references"))
      (should (seq-contains-p properties property #'equal)))
    (should-not (seq-contains-p properties "header:To" #'equal))
    (should (seq-contains-p body-properties "headers" #'equal))
    (should (chidu-draft-editable-snapshot-p snapshot))
    (should
     (equal "Alice <alice@example.test>, unfinished"
            (chidu-draft-editable-shape-to shape)))
    (should (equal "" (chidu-draft-editable-shape-cc shape)))
    (should (equal "hidden@example.test"
                   (chidu-draft-editable-shape-bcc shape)))
    (should (equal "Draft subject"
                   (chidu-draft-editable-shape-subject shape)))
    (should (equal "Plain Draft body"
                   (chidu-draft-editable-shape-body shape)))
    (should
     (equal
      (chidu-store-email-address-create
       :name "Me" :email "me@example.test")
      (chidu-draft-editable-shape-originator shape)))
    (should
     (equal "blob-draft"
            (chidu-draft-editable-snapshot-remote-blob-id snapshot)))))

(ert-deftest chidu-jmap-draft-checkout-accepts-empty-opaque-part-id ()
  "JMAP String part ids may be empty and are not editable semantics."
  (let* ((email (chidu-draft-contract-test--draft-wire nil nil nil))
         (text (chidu-draft-contract-test--root-part email))
         (values (gethash "bodyValues" email))
         (body-value (gethash "text" values)))
    (puthash "partId" "" text)
    (remhash "text" values)
    (puthash "" body-value values)
    (let ((snapshot (chidu-draft-contract-test--validate-wire email)))
      (should (chidu-draft-editable-snapshot-p snapshot))
      (should
       (equal "Plain Draft body"
              (chidu-draft-editable-shape-body
               (chidu-draft-editable-snapshot-shape snapshot)))))))

(ert-deftest chidu-jmap-draft-checkout-projects-attachments-from-tree ()
  "Attachment metadata and order must come from canonical bodyStructure."
  (let* ((snapshot
          (chidu-jmap-draft-checkout-validate-response
           (chidu-draft-contract-test--checkout-payload
            (chidu-draft-contract-test--draft-wire nil t nil))
           "remote-account" "draft-remote" "drafts"))
         (shape (chidu-draft-editable-snapshot-shape snapshot))
         (resources (chidu-draft-editable-shape-resources shape))
         (resource (aref resources 0)))
    (should (= 1 (length resources)))
    (should (equal "note.txt"
                   (chidu-draft-resource-shape-name resource)))
    (should (equal ["en"]
                   (chidu-draft-resource-shape-language resource)))
    (should (equal "blob-attachment"
                   (chidu-draft-resource-shape-remote-blob-id resource)))))

(ert-deftest chidu-jmap-draft-checkout-returns-typed-absence ()
  (let ((missing
         (chidu-jmap-draft-checkout-validate-response
          (chidu-store-test--method-response
           "Email/get" "draft-checkout"
           (list :accountId "remote-account"
                 :state "email/state"
                 :list []
                 :notFound ["draft-remote"]))
          "remote-account" "draft-remote" "drafts")))
    (should (chidu-result-failure-p missing))
    (should (eq 'draft-not-found
                (chidu-result-failure-kind missing)))))

(ert-deftest chidu-draft-bind-checkout-requires-exact-originator ()
  "Selection may find case-insensitive candidates, but binding is exact."
  (let* ((main
          (chidu-store-identity-create
           :identity-id "main" :remote-identity-id "r-main"
           :name "Me" :email "me@example.test" :available-p t))
         (same
          (chidu-store-identity-create
           :identity-id "same" :remote-identity-id "r-same"
           :name "Me" :email "me@example.test" :available-p t))
         (alias
          (chidu-store-identity-create
           :identity-id "alias" :remote-identity-id "r-alias"
           :name "Another Name" :email "me@example.test" :available-p t))
         (document (chidu-store-compose-document-create))
         (exact
          (chidu-draft-contract-test--snapshot-create
           :remote-email-id "draft" :remote-blob-id "blob-draft"
           :from
           (vector
            (chidu-store-email-address-create
             :name "Me" :email "me@example.test"))
           :document document))
         (case-mismatch
          (chidu-draft-contract-test--snapshot-create
           :remote-email-id "draft" :remote-blob-id "blob-draft"
           :from
           (vector
            (chidu-store-email-address-create
             :name "Me" :email "ME@example.test"))
           :document document))
         (account
          (chidu-store-account-create
           :account-id "account" :remote-account-id "remote-account"
           :name "Mail" :available-p t :identities (vector main))))
    (let ((plan (chidu-draft-bind-checkout account exact)))
      (should (chidu-draft-checkout-plan-p plan))
      (should (eq main (chidu-draft-checkout-plan-identity plan))))
    (let ((missing
           (chidu-draft-bind-checkout
            (chidu-store-account-with account :identities (vector)) exact)))
      (should (chidu-result-failure-p missing))
      (should (eq 'draft-identity-unavailable
                  (chidu-result-failure-kind missing))))
    (dolist (snapshot (list case-mismatch
                            (chidu-draft-contract-test--snapshot-create
                             :remote-email-id "draft"
                             :remote-blob-id "blob-draft"
                             :from
                             (vector
                              (chidu-store-email-address-create
                               :name "External Alias"
                               :email "me@example.test"))
                             :document document)))
      (let ((unsupported (chidu-draft-bind-checkout account snapshot)))
        (should (chidu-result-failure-p unsupported))
        (should (eq 'draft-originator-unsupported
                    (chidu-result-failure-kind unsupported)))))
    (let ((ambiguous
           (chidu-draft-bind-checkout
            (chidu-store-account-with account :identities (vector main same))
            exact)))
      (should (chidu-result-failure-p ambiguous))
      (should (eq 'draft-identity-ambiguous
                  (chidu-result-failure-kind ambiguous))))
    (let ((unsupported
           (chidu-draft-bind-checkout
            (chidu-store-account-with account :identities (vector alias))
            exact)))
      (should (chidu-result-failure-p unsupported))
      (should (eq 'draft-originator-unsupported
                  (chidu-result-failure-kind unsupported))))
    (should-error
     (chidu-draft-bind-checkout
      account
      (chidu-draft-editable-snapshot-with
       exact
       :shape
       (chidu-draft-editable-shape-with
        (chidu-draft-editable-snapshot-shape exact)
        :originator
        (chidu-store-email-address-create
         :name "" :email "me@example.test"))))
     :type 'chidu-invariant-error)))

(ert-deftest chidu-jmap-draft-checkout-accepts-leaf-subparts-normalization ()
  "Missing, null, and empty leaf subParts are the same wire shape."
  (dolist (variant '(missing null empty))
    (ert-info ((format "leaf subParts: %s" variant))
      (let* ((email (chidu-draft-contract-test--draft-wire nil nil nil))
             (root (chidu-draft-contract-test--root-part email)))
        (pcase variant
          ('missing (remhash "subParts" root))
          ('null (puthash "subParts" :json-null root))
          ('empty (puthash "subParts" (vector) root)))
        (should
         (chidu-draft-editable-snapshot-p
          (chidu-draft-contract-test--validate-wire email)))))))

(ert-deftest chidu-jmap-draft-checkout-accepts-multipart-size-variants ()
  "RFC integer and deployed null multipart size representations are accepted."
  (dolist (size (list :json-null 128))
    (ert-info ((format "multipart size: %S" size))
      (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
             (root (chidu-draft-contract-test--root-part email)))
        (puthash "size" size root)
        (should
         (chidu-draft-editable-snapshot-p
          (chidu-draft-contract-test--validate-wire email)))))))

(ert-deftest chidu-jmap-draft-checkout-rejects-valid-unsupported-shapes ()
  "Every legal but unrepresentable Draft has one typed permanent failure."
  (dolist
      (case
       (list
        (list
         "multiple From" 'draft-originator-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "from"
              (vector
               (chidu-draft-contract-test--json-object
                "name" "Me" "email" "me@example.test")
               (chidu-draft-contract-test--json-object
                "name" "Other" "email" "other@example.test"))
              email)
             (chidu-draft-contract-test--append-top-header
              email "From" "Other <other@example.test>"))))
        (list
         "Sender" 'draft-originator-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "sender"
              (vector
               (chidu-draft-contract-test--json-object
                "name" "Sender" "email" "sender@example.test"))
              email)
             (chidu-draft-contract-test--append-top-header
              email "Sender" "Sender <sender@example.test>"))))
        (list
         "missing Subject" 'draft-metadata-unsupported
         (lambda ()
           (chidu-draft-contract-test--draft-wire nil nil t)))
        (list
         "empty To" 'draft-metadata-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash "value" "" (aref (gethash "headers" email) 1))
             email)))
        (list
         "multiple To" 'draft-metadata-unsupported
         (lambda ()
           (chidu-draft-contract-test--append-top-header
            (chidu-draft-contract-test--draft-wire nil nil nil)
            "To" "second@example.test")))
        (list
         "unknown header" 'draft-metadata-unsupported
         (lambda ()
           (chidu-draft-contract-test--append-top-header
            (chidu-draft-contract-test--draft-wire nil nil nil)
            "X-Chidu-Unknown" "opaque")))
        (list
         "unparseable Message-ID" 'draft-metadata-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash "messageId" (vector) email)
             email)))
        (list
         "duplicate Message-ID values" 'draft-metadata-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "messageId"
              ["external-draft@example.test"
               "external-draft@example.test"]
              email)
             email)))
        (list
         "empty Message-ID value" 'draft-metadata-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash "messageId" [""] email)
             email)))
        (list
         "reply metadata" 'draft-metadata-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash "inReplyTo" ["parent@example.test"] email)
             (chidu-draft-contract-test--append-top-header
              email "In-Reply-To" "<parent@example.test>"))))
        (list
         "extra keyword" 'draft-metadata-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "keywords"
              (chidu-draft-contract-test--map
               "$draft" t "$seen" t "$flagged" t)
              email)
             email)))
        (list
         "extra mailbox" 'draft-metadata-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "mailboxIds"
              (chidu-draft-contract-test--map "drafts" t "other" t)
              email)
             email)))
        (list
         "HTML alternative" 'draft-html-unsupported
         (lambda () (chidu-draft-contract-test--draft-wire t nil nil)))
        (list
         "nested multipart" 'draft-body-structure-unsupported
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (root (chidu-draft-contract-test--root-part email))
                  (children (gethash "subParts" root))
                  (text (aref children 0))
                  (attachment (aref children 1))
                  (nested
                   (chidu-draft-contract-test--body-part
                    :type "multipart/mixed"
                    :headers
                    (vector
                     (chidu-draft-contract-test--header
                      "Content-Type" "multipart/mixed; boundary=nested"))
                    :subparts (vector attachment))))
             (puthash "subParts" (vector text nested) root)
             email)))
        (list
         "unparseable text language" 'draft-body-metadata-unsupported
         (lambda ()
           (chidu-draft-contract-test--append-top-header
            (chidu-draft-contract-test--draft-wire nil nil nil)
            "Content-Language" "not a valid language list")))
        (list
         "unparseable attachment Content-ID"
         'draft-body-metadata-unsupported
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (attachment
                   (chidu-draft-contract-test--wire-part-by-id
                    (chidu-draft-contract-test--root-part email)
                    "attachment")))
             (puthash
              "headers"
              (vconcat
               (gethash "headers" attachment)
               (vector
                (chidu-draft-contract-test--header
                 "Content-ID" "not a parsed content id")))
              attachment)
             email)))
        (list
         "unsafe body text" 'draft-body-metadata-unsupported
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "text"
              (chidu-draft-contract-test--body-value "body\0tail")
              (gethash "bodyValues" email))
             email)))
        (list
         "empty attachment name" 'draft-body-metadata-unsupported
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (attachment
                   (chidu-draft-contract-test--wire-part-by-id
                    (chidu-draft-contract-test--root-part email)
                    "attachment")))
             (puthash "name" "" attachment)
             email)))
        (list
         "unsafe attachment name" 'draft-body-metadata-unsupported
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (attachment
                   (chidu-draft-contract-test--wire-part-by-id
                    (chidu-draft-contract-test--root-part email)
                    "attachment")))
             (puthash "name" "unsafe\nname.txt" attachment)
             email)))
        (list
         "text language" 'draft-body-metadata-unsupported
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil nil nil))
                  (text (chidu-draft-contract-test--root-part email)))
             (puthash "language" ["en"] text)
             (chidu-draft-contract-test--append-top-header
              email "Content-Language" "en"))))
        (list
         "text location" 'draft-body-metadata-unsupported
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil nil nil))
                  (text (chidu-draft-contract-test--root-part email)))
             (puthash "location" "https://example.test/body" text)
             (chidu-draft-contract-test--append-top-header
              email "Content-Location" "https://example.test/body"))))))
    (pcase-let ((`(,label ,kind ,builder) case))
      (ert-info ((format "unsupported Draft shape: %s" label))
        (let ((result (chidu-draft-contract-test--validate-wire (funcall builder))))
          (should (chidu-result-failure-p result))
          (should (eq kind (chidu-result-failure-kind result)))
          (should-not (chidu-result-failure-retryable-p result)))))))

(ert-deftest chidu-jmap-draft-checkout-rejects-invalid-wire-shapes ()
  "Protocol-invalid MIME and projection contradictions must signal wire errors."
  (dolist
      (case
       (list
        (list
         "bare header line break"
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "value" "Me <me@example.test>\nX-Injected: yes"
              (aref (gethash "headers" email) 0))
             email)))
        (list
         "multipart missing subParts"
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil t nil)))
             (remhash "subParts" (chidu-draft-contract-test--root-part email))
             email)))
        (list
         "multipart null subParts"
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil t nil)))
             (puthash
              "subParts" :json-null (chidu-draft-contract-test--root-part email))
             email)))
        (list
         "multipart partId"
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil t nil)))
             (puthash "partId" "root" (chidu-draft-contract-test--root-part email))
             email)))
        (list
         "multipart blobId"
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil t nil)))
             (puthash
              "blobId" "blob-root" (chidu-draft-contract-test--root-part email))
             email)))
        (list
         "leaf nonempty subParts"
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "subParts" [(:invalid t)]
              (chidu-draft-contract-test--root-part email))
             email)))
        (list
         "non-text charset"
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (attachment
                   (chidu-draft-contract-test--wire-part-by-id
                    (chidu-draft-contract-test--root-part email)
                    "attachment")))
             (puthash "type" "application/octet-stream" attachment)
             (puthash "charset" "utf-8" attachment)
             email)))
        (list
         "duplicate partId"
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (attachment
                   (chidu-draft-contract-test--wire-part-by-id
                    (chidu-draft-contract-test--root-part email) "attachment")))
             (puthash "partId" "text" attachment)
             email)))
        (list
         "attachment Blob mismatch"
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (copy (copy-hash-table (aref (gethash "attachments" email) 0))))
             (puthash "blobId" "blob-other" copy)
             (puthash "attachments" (vector copy) email)
             email)))
        (list
         "attachment type mismatch"
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (copy (copy-hash-table (aref (gethash "attachments" email) 0))))
             (puthash "type" "application/octet-stream" copy)
             (puthash "attachments" (vector copy) email)
             email)))
        (list
         "attachment metadata mismatch"
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (copy (copy-hash-table (aref (gethash "attachments" email) 0))))
             (puthash "name" "other.txt" copy)
             (puthash "attachments" (vector copy) email)
             email)))
        (list
         "attachment outside tree"
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil nil nil))
                  (extra
                   (chidu-draft-contract-test--body-part
                    :type "application/octet-stream"
                    :headers
                    (vector
                     (chidu-draft-contract-test--header
                      "Content-Type" "application/octet-stream"))
                    :part-id "outside" :blob-id "blob-outside" :size 3)))
             (puthash "attachments" (vector extra) email)
             email)))
        (list
         "attachment property without metadata header"
         (lambda ()
           (let* ((email (chidu-draft-contract-test--draft-wire nil t nil))
                  (attachment
                   (chidu-draft-contract-test--wire-part-by-id
                    (chidu-draft-contract-test--root-part email)
                    "attachment")))
             (puthash "cid" "missing-header@example.test" attachment)
             email)))
        (list
         "root headers mismatch"
         (lambda ()
           (let ((email (chidu-draft-contract-test--draft-wire nil nil nil)))
             (puthash
              "headers"
              (vconcat
               (gethash "headers" email)
               (vector
                (chidu-draft-contract-test--header "X-Mismatch" "value")))
              email)
             email)))))
    (pcase-let ((`(,label ,builder) case))
      (ert-info ((format "invalid Draft wire: %s" label))
        (should-error
         (chidu-draft-contract-test--validate-wire (funcall builder))
         :type 'chidu-jmap-error)))))

(ert-deftest chidu-draft-editable-normal-form-round-trips-to-compiler ()
  "Accepted remote semantics must equal the exact compiler input semantics."
  (let* ((snapshot
          (chidu-draft-contract-test--validate-wire
           (chidu-draft-contract-test--draft-wire nil t nil)))
         (identity
          (chidu-store-identity-create
           :identity-id "main" :remote-identity-id "identity-main"
           :name "Me" :email "me@example.test" :available-p t))
         (account
          (chidu-store-account-create
           :account-id "account" :remote-account-id "remote-account"
           :name "Mail" :available-p t :identities (vector identity)))
         (plan (chidu-draft-bind-checkout account snapshot))
         (observations (chidu-draft-checkout-plan-resources plan))
         (resources
          (vconcat
           (cl-loop
            for observation across observations
            collect
            (chidu-draft-contract-test--registered-resource
             "workspace" observation))))
         (compiler-shape
          (chidu-jmap-compose-editable-shape
           (chidu-draft-checkout-plan-document plan)
           (chidu-draft-checkout-plan-identity plan)
           resources))
         (email
          (chidu-jmap-compose-draft-email
           (chidu-draft-checkout-plan-document plan)
           (chidu-draft-checkout-plan-identity plan)
           "drafts" "new-message@example.test" resources))
         (body (gethash "bodyStructure" email))
         (parts (gethash "subParts" body))
         (attachment (aref parts 1)))
    (should (chidu-draft-checkout-plan-p plan))
    (should
     (equal
      (chidu-draft-editable-snapshot-shape snapshot)
      compiler-shape))
    (should (equal "multipart/mixed" (gethash "type" body)))
    (should (equal "blob-attachment" (gethash "blobId" attachment)))
    (should (equal "note.txt" (gethash "name" attachment)))
    (should (equal "attachment" (gethash "disposition" attachment)))
    (should (equal "utf-8" (gethash "charset" attachment)))
    (should (equal ["en"] (gethash "language" attachment)))
    (let ((from (aref (gethash "from" email) 0)))
      (should (equal "Me" (gethash "name" from)))
      (should (equal "me@example.test" (gethash "email" from))))))

(provide 'chidu-draft-contract-test)

;;; chidu-draft-contract-test.el ends here
