;;; chidu-jmap-draft-observation.el --- Strict remote Draft wire model -*- lexical-binding: t; -*-

;;; Commentary:

;; Decode complete JMAP Email and EmailBodyPart evidence into a strict immutable
;; remote observation.  This module answers only whether the wire response is
;; internally valid; it makes no claim that Chidu's editor can represent it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-jmap-email)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-store)

(defconst chidu-jmap-draft-observation-max-mime-depth 64
  "Maximum MIME nesting accepted at the Draft checkout wire boundary.")

(chidu-define-record chidu-jmap-draft-header
    "One validated immutable JMAP EmailHeader."
  name
  normalized-name
  value)

(chidu-define-record chidu-jmap-draft-body-value
    "One validated decoded JMAP EmailBodyValue."
  value
  truncated-p
  encoding-problem-p)

(chidu-define-record chidu-jmap-draft-part
    "One recursively validated immutable JMAP EmailBodyPart."
  part-id
  blob-id
  size
  (headers (vector))
  name
  media-type
  charset
  disposition
  cid
  (language (vector))
  location
  (subparts (vector)))

(chidu-define-record chidu-jmap-remote-draft-observation
    "All server facts required to decide Draft representability."
  remote-email-id
  remote-blob-id
  email-state
  (mailbox-ids (vector))
  (keywords (vector))
  (headers (vector))
  (message-ids (vector))
  (in-reply-to (vector))
  (references (vector))
  (from (vector))
  (sender (vector))
  subject
  body-tree
  part-index
  body-values
  (text-body (vector))
  (html-body (vector))
  (attachments (vector)))

(defun chidu-jmap-draft-observation--nullable-text (value context)
  "Return nil for JSON null or possibly empty string VALUE for CONTEXT."
  (if (eq value :json-null)
      nil
    (chidu-jmap--string value context t)))

(defun chidu-jmap-draft-observation--header-name (value context)
  "Return validated RFC 5322 field-name VALUE for CONTEXT."
  (let ((name (chidu-jmap--string value context)))
    (unless
        (cl-loop
         for character across name
         always (and (<= 33 character 126) (/= character ?:)))
      (signal 'chidu-jmap-error
              (list (format "%s is not an RFC 5322 field name" context))))
    name))

(defun chidu-jmap-draft-observation--raw-header-value (value context)
  "Return RFC-folded raw header VALUE for CONTEXT, rejecting bare line breaks."
  (let ((text (chidu-jmap--string value context t)))
    (when (string-match-p "\0" text)
      (signal 'chidu-jmap-error
              (list (format "%s contains NUL" context))))
    (let ((unfolded
           (replace-regexp-in-string
            "\r\n[ \t]+\\|\n[ \t]+" " " text t t)))
      (when (string-match-p "[\r\n]" unfolded)
        (signal 'chidu-jmap-error
                (list (format "%s contains a bare line break" context)))))
    text))

(defun chidu-jmap-draft-observation--header (wire ordinal context)
  "Decode EmailHeader WIRE at ORDINAL for CONTEXT."
  (let* ((label (format "%s header %d" context ordinal))
         (header (chidu-jmap--hash wire label))
         (name
          (chidu-jmap-draft-observation--header-name
           (chidu-jmap--required header "name" label)
           (format "%s name" label)))
         (value
          (chidu-jmap-draft-observation--raw-header-value
           (chidu-jmap--required header "value" label)
           (format "%s value" label))))
    (chidu-jmap-draft-header-create
     :name name :normalized-name (downcase name) :value value)))

(defun chidu-jmap-draft-observation--headers (wire context)
  "Return validated EmailHeader vector from WIRE for CONTEXT."
  (let ((headers (chidu-jmap--vector wire context)))
    (vconcat
     (cl-loop
      for item across headers
      for ordinal from 0
      collect (chidu-jmap-draft-observation--header item ordinal context)))))

(defun chidu-jmap-draft-observation--language (wire context)
  "Return validated nullable language vector WIRE for CONTEXT."
  (if (eq wire :json-null)
      (vector)
    (let ((items (chidu-jmap--vector wire context)))
      (vconcat
       (cl-loop
        for item across items
        collect (chidu-jmap--string item context))))))

(defun chidu-jmap-draft-observation--disposition (wire context)
  "Return normalized nullable MIME disposition WIRE for CONTEXT."
  (when-let* ((value
               (chidu-jmap-draft-observation--nullable-text wire context)))
    (setq value (downcase value))
    (unless (chidu-jmap--mime-token-p value)
      (signal 'chidu-jmap-error
              (list (format "%s is not a MIME token" context))))
    value))

(defun chidu-jmap-draft-observation--subparts-wire
    (part multipart-p context)
  "Return canonical subParts wire value from PART for CONTEXT.

MULTIPART-P decides whether omission or null is a protocol error."
  (let* ((missing (make-symbol "missing"))
         (wire (gethash "subParts" part missing)))
    (cond
     (multipart-p
      (when (or (eq wire missing) (eq wire :json-null))
        (signal 'chidu-jmap-error
                (list (format "%s multipart is missing subParts" context))))
      (chidu-jmap--vector wire (format "%s subParts" context)))
     ((or (eq wire missing) (eq wire :json-null)) (vector))
     (t
      (let ((subparts
             (chidu-jmap--vector wire (format "%s subParts" context))))
        (unless (zerop (length subparts))
          (signal 'chidu-jmap-error
                  (list (format "%s non-multipart has subParts" context))))
        subparts)))))

(cl-defun chidu-jmap-draft-observation--part
    (wire context part-index &optional (depth 0))
  "Decode EmailBodyPart WIRE for CONTEXT into PART-INDEX at recursive DEPTH."
  (when (> depth chidu-jmap-draft-observation-max-mime-depth)
    (signal 'chidu-jmap-error '("Draft MIME tree is too deeply nested")))
  (let* ((part (chidu-jmap--hash wire context))
         (media-type
          (chidu-jmap--media-type
           (chidu-jmap--required part "type" context)
           (format "%s type" context)))
         (multipart-p (string-prefix-p "multipart/" media-type))
         (part-id
          (chidu-jmap-draft-observation--nullable-text
           (chidu-jmap--required part "partId" context)
           (format "%s partId" context)))
         (blob-id
          (chidu-jmap--nullable-id
           (chidu-jmap--required part "blobId" context)
           (format "%s blobId" context)))
         (wire-size (chidu-jmap--required part "size" context))
         (size
          (unless (eq wire-size :json-null)
            (chidu-jmap--safe-nonnegative-integer
             wire-size (format "%s size" context))))
         (headers
          (chidu-jmap-draft-observation--headers
           (chidu-jmap--required part "headers" context)
           (format "%s headers" context)))
         (name
          (chidu-jmap-draft-observation--nullable-text
           (chidu-jmap--required part "name" context)
           (format "%s name" context)))
         (charset
          (chidu-jmap-draft-observation--nullable-text
           (chidu-jmap--required part "charset" context)
           (format "%s charset" context)))
         (disposition
          (chidu-jmap-draft-observation--disposition
           (chidu-jmap--required part "disposition" context)
           (format "%s disposition" context)))
         (cid
          (chidu-jmap-draft-observation--nullable-text
           (chidu-jmap--required part "cid" context)
           (format "%s cid" context)))
         (language
          (chidu-jmap-draft-observation--language
           (chidu-jmap--required part "language" context)
           (format "%s language" context)))
         (location
          (chidu-jmap-draft-observation--nullable-text
           (chidu-jmap--required part "location" context)
           (format "%s location" context)))
         (wire-subparts
          (chidu-jmap-draft-observation--subparts-wire
           part multipart-p context))
         subparts)
    (when (and charset (not (string-prefix-p "text/" media-type)))
      (signal 'chidu-jmap-error
              (list (format "%s non-text part has a charset" context))))
    (if multipart-p
        (when (or part-id blob-id)
          (signal 'chidu-jmap-error
                  (list
                   (format
                    "%s multipart must have null partId and blobId"
                    context))))
      (unless (and part-id blob-id (integerp size))
        (signal 'chidu-jmap-error
                (list
                 (format
                  "%s leaf must have partId, blobId, and size" context))))
      (when (gethash part-id part-index)
        (signal 'chidu-jmap-error
                (list "Draft MIME tree contains duplicate partId" part-id))))
    (setq
     subparts
     (vconcat
      (cl-loop
       for child across wire-subparts
       for ordinal from 0
       collect
       (chidu-jmap-draft-observation--part
        child (format "%s child %d" context ordinal)
        part-index (1+ depth)))))
    (let ((observation
           (chidu-jmap-draft-part-create
            :part-id part-id :blob-id blob-id :size size
            :headers headers :name name :media-type media-type
            :charset charset :disposition disposition :cid cid
            :language language :location location :subparts subparts)))
      (when part-id (puthash part-id observation part-index))
      observation)))

(defun chidu-jmap-draft-observation--parts (wire context)
  "Decode convenience EmailBodyPart array WIRE for CONTEXT."
  (let ((items (chidu-jmap--vector wire context))
        (seen (make-hash-table :test #'equal)))
    (vconcat
     (cl-loop
      for item across items
      for ordinal from 0
      collect
      (chidu-jmap-draft-observation--part
       item (format "%s part %d" context ordinal) seen)))))

(defun chidu-jmap-draft-observation--header-id-vector (wire context)
  "Return nullable WIRE message ids in order for CONTEXT; null becomes empty."
  (if (eq wire :json-null)
      (vector)
    (let ((items (chidu-jmap--vector wire context)))
      (when (> (length items) chidu-jmap-email-header-id-limit)
        (signal 'chidu-jmap-error
                (list (format "%s contains too many message ids" context))))
      (vconcat
       (cl-loop
        for item across items
        collect (chidu-jmap--string item context t))))))

(defun chidu-jmap-draft-observation--body-value (wire part-id)
  "Decode EmailBodyValue WIRE for PART-ID."
  (let* ((context (format "Draft body value %s" part-id))
         (value (chidu-jmap--hash wire context)))
    (chidu-jmap-draft-body-value-create
     :value
     (chidu-jmap--string
      (chidu-jmap--required value "value" context)
      (format "%s value" context) t)
     :truncated-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required value "isTruncated" context)
      (format "%s isTruncated" context))
     :encoding-problem-p
     (chidu-jmap--json-boolean
      (chidu-jmap--required value "isEncodingProblem" context)
      (format "%s isEncodingProblem" context)))))

(defun chidu-jmap-draft-observation--body-values (wire)
  "Decode bodyValues object WIRE into a part-id map."
  (let ((object (chidu-jmap--hash wire "Draft bodyValues"))
        (values (make-hash-table :test #'equal)))
    (maphash
     (lambda (part-id wire-value)
       (setq part-id
             (chidu-jmap--string part-id "Draft bodyValues partId" t))
       (puthash
        part-id
        (chidu-jmap-draft-observation--body-value wire-value part-id)
        values))
     object)
    values))

(defun chidu-jmap-draft-observation-decode
    (email email-state)
  "Decode strict remote Draft observation from EMAIL at EMAIL-STATE."
  (let* ((part-index (make-hash-table :test #'equal))
         (headers
          (chidu-jmap-draft-observation--headers
           (chidu-jmap--required email "headers" "Draft Email/get item")
           "Draft headers"))
         (body-tree
          (chidu-jmap-draft-observation--part
           (chidu-jmap--required
            email "bodyStructure" "Draft Email/get item")
           "Draft bodyStructure" part-index)))
    (unless (equal headers (chidu-jmap-draft-part-headers body-tree))
      (signal 'chidu-jmap-error
              '("Draft headers disagree with root bodyStructure headers")))
    (chidu-jmap-remote-draft-observation-create
     :remote-email-id
     (chidu-jmap--id
      (chidu-jmap--required email "id" "Draft Email/get item")
      "Draft Email id")
     :remote-blob-id
     (chidu-jmap--id
      (chidu-jmap--required email "blobId" "Draft Email/get item")
      "Draft Email blobId")
     :email-state email-state
     :mailbox-ids
     (chidu-jmap--true-map-keys
      (chidu-jmap--required email "mailboxIds" "Draft Email/get item")
      "Draft mailboxIds" t)
     :keywords
     (chidu-jmap--true-map-keys
      (chidu-jmap--required email "keywords" "Draft Email/get item")
      "Draft keywords")
     :headers headers
     :message-ids
     (chidu-jmap-draft-observation--header-id-vector
      (chidu-jmap--required email "messageId" "Draft Email/get item")
      "Draft messageId")
     :in-reply-to
     (chidu-jmap-draft-observation--header-id-vector
      (chidu-jmap--required email "inReplyTo" "Draft Email/get item")
      "Draft inReplyTo")
     :references
     (chidu-jmap-draft-observation--header-id-vector
      (chidu-jmap--required email "references" "Draft Email/get item")
      "Draft references")
     :from
     (chidu-jmap-email--address-vector
      (chidu-jmap--required email "from" "Draft Email/get item")
      "Draft From")
     :sender
     (chidu-jmap-email--address-vector
      (chidu-jmap--required email "sender" "Draft Email/get item")
      "Draft Sender")
     :subject
     (chidu-jmap-draft-observation--nullable-text
      (chidu-jmap--required email "subject" "Draft Email/get item")
      "Draft subject")
     :body-tree body-tree
     :part-index part-index
     :body-values
     (chidu-jmap-draft-observation--body-values
      (chidu-jmap--required email "bodyValues" "Draft Email/get item"))
     :text-body
     (chidu-jmap-draft-observation--parts
      (chidu-jmap--required email "textBody" "Draft Email/get item")
      "Draft textBody")
     :html-body
     (chidu-jmap-draft-observation--parts
      (chidu-jmap--required email "htmlBody" "Draft Email/get item")
      "Draft htmlBody")
     :attachments
     (chidu-jmap-draft-observation--parts
      (chidu-jmap--required email "attachments" "Draft Email/get item")
      "Draft attachments"))))

(provide 'chidu-jmap-draft-observation)

;;; chidu-jmap-draft-observation.el ends here
