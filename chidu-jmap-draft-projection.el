;;; chidu-jmap-draft-projection.el --- Remote Draft representability -*- lexical-binding: t; -*-

;;; Commentary:

;; Project a valid `chidu-jmap-remote-draft-observation' into Chidu's closed
;; editable semantic normal form.  Legal JMAP that the current compiler cannot
;; reproduce returns a typed permanent failure.  Contradictions between
;; bodyStructure and convenience projections remain wire errors.

;;; Code:

(require 'cl-lib)
(require 'mail-parse)
(require 'seq)
(require 'subr-x)
(require 'chidu-draft-semantics)
(require 'chidu-jmap-draft-observation)
(require 'chidu-result)
(require 'chidu-store)

(define-error 'chidu-jmap-draft-projection-unsupported-metadata
              "Draft body metadata cannot be represented"
              'chidu-jmap-error)

(defconst chidu-jmap-draft-projection--top-level-header-names
  '("from" "sender" "to" "cc" "bcc" "reply-to" "subject"
    "message-id" "in-reply-to" "references" "date"
    "mime-version" "content-type" "content-transfer-encoding"
    "content-disposition" "content-id" "content-language"
    "content-location")
  "Closed top-level header set understood by the current Draft compiler.")

(defconst chidu-jmap-draft-projection--text-part-header-names
  '("content-type" "content-transfer-encoding")
  "Part headers whose representation may be normalized for plain text.")

(defconst chidu-jmap-draft-projection--attachment-header-names
  '("content-type" "content-transfer-encoding" "content-disposition"
    "content-id" "content-language" "content-location")
  "Part headers fully represented or explicitly normalized for attachments.")

(defun chidu-jmap-draft-projection--unsupported (kind remote-email-id)
  "Return unsupported Draft failure KIND for REMOTE-EMAIL-ID."
  (chidu-result-failure-create
   :kind kind
   :data (list :remote-email-id remote-email-id)
   :retryable-p nil))

(defun chidu-jmap-draft-projection--unfold-header (header context)
  "Return safely unfolded raw HEADER value for CONTEXT."
  (unless (chidu-jmap-draft-header-p header)
    (signal 'wrong-type-argument
            (list 'chidu-jmap-draft-header-p header)))
  (let ((text (chidu-jmap-draft-header-value header)))
    (setq text
          (replace-regexp-in-string
           "\r\n[ \t]+\\|\n[ \t]+" " " text t t))
    (when (string-match-p "[\r\n]" text)
      (signal 'chidu-jmap-error
              (list (format "%s contains an unfolded line break" context))))
    (string-trim text)))

(defun chidu-jmap-draft-projection--header-table (headers)
  "Return normalized-name table preserving HEADERS order."
  (let ((table (make-hash-table :test #'equal))
        names)
    (cl-loop
     for header across headers
     for name = (chidu-jmap-draft-header-normalized-name header)
     do
     (unless (gethash name table)
       (push name names))
     (puthash name (cons header (gethash name table)) table))
    (dolist (name names)
      (puthash name (nreverse (gethash name table)) table))
    table))

(defun chidu-jmap-draft-projection--header-list (table name)
  "Return normalized header NAME instances from TABLE."
  (or (gethash name table) nil))

(defun chidu-jmap-draft-projection--single-header
    (table name context)
  "Return unfolded single header NAME from TABLE, or empty string."
  (let ((headers (chidu-jmap-draft-projection--header-list table name)))
    (pcase headers
      ('() "")
      (`(,header)
       (chidu-jmap-draft-projection--unfold-header header context))
      (_ nil))))

(defun chidu-jmap-draft-projection--present-empty-header-p
    (table name context)
  "Return non-nil when TABLE has one NAME empty after unfolding for CONTEXT."
  (let ((headers (chidu-jmap-draft-projection--header-list table name)))
    (and (= 1 (length headers))
         (string-empty-p
          (chidu-jmap-draft-projection--unfold-header
           (car headers) context)))))

(defun chidu-jmap-draft-projection--headers-closed-p (headers allowed)
  "Return non-nil when HEADERS are unique members of ALLOWED."
  (let ((seen (make-hash-table :test #'equal))
        (valid-p t))
    (cl-loop
     for header across headers
     for name = (chidu-jmap-draft-header-normalized-name header)
     do
     (unless (and (member name allowed) (not (gethash name seen)))
       (setq valid-p nil))
     (puthash name t seen))
    valid-p))

(defun chidu-jmap-draft-projection--header-present-p
    (headers normalized-name)
  "Return non-nil when HEADERS contain NORMALIZED-NAME."
  (cl-loop
   for header across headers
   thereis
   (equal normalized-name
          (chidu-jmap-draft-header-normalized-name header))))

(defun chidu-jmap-draft-projection--project-headers (observation)
  "Project OBSERVATION top-level headers into editable normal-form values."
  (let* ((remote-id
          (chidu-jmap-remote-draft-observation-remote-email-id observation))
         (headers (chidu-jmap-remote-draft-observation-headers observation))
         (table (chidu-jmap-draft-projection--header-table headers))
         (from (chidu-jmap-remote-draft-observation-from observation))
         (sender (chidu-jmap-remote-draft-observation-sender observation))
         (subject (chidu-jmap-remote-draft-observation-subject observation))
         (message-ids
          (chidu-jmap-remote-draft-observation-message-ids observation))
         (in-reply-to
          (chidu-jmap-remote-draft-observation-in-reply-to observation))
         (references
          (chidu-jmap-remote-draft-observation-references observation))
         (from-headers
          (chidu-jmap-draft-projection--header-list table "from"))
         (sender-headers
          (chidu-jmap-draft-projection--header-list table "sender"))
         (subject-headers
          (chidu-jmap-draft-projection--header-list table "subject"))
         (message-id-headers
          (chidu-jmap-draft-projection--header-list table "message-id"))
         (in-reply-to-headers
          (chidu-jmap-draft-projection--header-list table "in-reply-to"))
         (references-headers
          (chidu-jmap-draft-projection--header-list table "references")))
    (cond
     ((or
       (and (zerop (length from-headers)) (> (length from) 0))
       (and (zerop (length sender-headers)) (> (length sender) 0))
       (and (zerop (length message-id-headers)) (> (length message-ids) 0))
       (and (zerop (length in-reply-to-headers)) (> (length in-reply-to) 0))
       (and (zerop (length references-headers)) (> (length references) 0))
       (and (null subject) (= 1 (length subject-headers)))
       (and subject (zerop (length subject-headers))))
      (signal 'chidu-jmap-error
              '("Draft convenience headers disagree with headers property")))
     ((or
       (/= (length from-headers) 1)
       (/= (length from) 1)
       (> (length sender-headers) 0)
       (> (length sender) 0))
      (chidu-jmap-draft-projection--unsupported
       'draft-originator-unsupported remote-id))
     ((not
       (chidu-jmap-draft-projection--headers-closed-p
        headers chidu-jmap-draft-projection--top-level-header-names))
      (chidu-jmap-draft-projection--unsupported
       'draft-metadata-unsupported remote-id))
     ((or
       (> (length message-ids) 1)
       (seq-some #'string-empty-p message-ids)
       (and (= 1 (length message-id-headers))
            (zerop (length message-ids)))
       (> (length in-reply-to-headers) 0)
       (> (length in-reply-to) 0)
       (> (length references-headers) 0)
       (> (length references) 0)
       (null subject)
       (chidu-jmap-draft-projection--present-empty-header-p
        table "to" "Draft To")
       (chidu-jmap-draft-projection--present-empty-header-p
        table "cc" "Draft Cc")
       (chidu-jmap-draft-projection--present-empty-header-p
        table "bcc" "Draft Bcc")
       (chidu-jmap-draft-projection--present-empty-header-p
        table "reply-to" "Draft Reply-To"))
      (chidu-jmap-draft-projection--unsupported
       'draft-metadata-unsupported remote-id))
     (t
      (list
       :originator (aref from 0)
       :to
       (chidu-jmap-draft-projection--single-header table "to" "Draft To")
       :cc
       (chidu-jmap-draft-projection--single-header table "cc" "Draft Cc")
       :bcc
       (chidu-jmap-draft-projection--single-header table "bcc" "Draft Bcc")
       :reply-to
       (chidu-jmap-draft-projection--single-header
        table "reply-to" "Draft Reply-To")
       :subject subject)))))

(defun chidu-jmap-draft-projection--only-header (table name)
  "Return TABLE's only normalized NAME header, or nil."
  (car (chidu-jmap-draft-projection--header-list table name)))

(defun chidu-jmap-draft-projection--parameterized-header (header context)
  "Parse parameterized HEADER strictly for CONTEXT, or signal."
  (let* ((raw (chidu-jmap-draft-projection--unfold-header header context))
         (parsed
          (rfc2231-parse-string (rfc2047-decode-string raw) t)))
    (unless (and (consp parsed)
                 (stringp (car parsed))
                 (not (string-empty-p (car parsed))))
      (signal 'chidu-jmap-error
              (list (format "%s is not a parameterized MIME value" context))))
    parsed))

(defun chidu-jmap-draft-projection--parameter-contract
    (parsed allowed)
  "Return status for PARSED parameters against ALLOWED names.

The result is `ok', `unsupported', or `invalid'."
  (let (seen status)
    (dolist (entry (cdr parsed))
      (cond
       ((not (and (consp entry) (symbolp (car entry)) (stringp (cdr entry))))
        (setq status 'invalid))
       ((memq (car entry) seen)
        (setq status 'unsupported))
       ((not (memq (car entry) allowed))
        (setq status 'unsupported)))
      (push (car-safe entry) seen))
    (or status 'ok)))

(defun chidu-jmap-draft-projection--optional-downcase (value)
  "Return downcased VALUE, preserving nil."
  (and value (downcase value)))

(defun chidu-jmap-draft-projection--content-id (header)
  "Return normalized Content-ID represented by HEADER, or signal."
  (when header
    (let* ((raw
            (chidu-jmap-draft-projection--unfold-header
             header "Draft Content-ID"))
           (without-comments (mail-header-remove-comments raw))
           (compact (mail-header-remove-whitespace without-comments)))
      (unless
          (string-match
           "\\`<\\([^][()<>@,;:\\\\\"[:space:]]+@[^][()<>@,;:\\\\\"[:space:]]+\\)>\\'"
           compact)
        (signal 'chidu-jmap-draft-projection-unsupported-metadata
                '("Draft Content-ID is not a message id")))
      (match-string 1 compact))))

(defun chidu-jmap-draft-projection--content-language (header)
  "Return normalized language vector represented by HEADER, or signal."
  (if (null header)
      (vector)
    (let* ((raw
            (chidu-jmap-draft-projection--unfold-header
             header "Draft Content-Language"))
           (without-comments (mail-header-remove-comments raw))
           (items (split-string without-comments "," nil)))
      (when
          (or
           (null items)
           (cl-some
            (lambda (item)
              (not
               (string-match-p
                "\\`[[:alpha:]]\\{1,8\\}\\(?:-[[:alnum:]]\\{1,8\\}\\)*\\'"
                (string-trim item))))
            items))
        (signal 'chidu-jmap-draft-projection-unsupported-metadata
                '("Draft Content-Language is not a language list")))
      (vconcat
       (mapcar (lambda (item) (downcase (string-trim item))) items)))))

(defun chidu-jmap-draft-projection--content-location (header)
  "Return normalized Content-Location represented by HEADER, or signal."
  (when header
    (let ((value
           (string-trim
            (chidu-jmap-draft-projection--unfold-header
             header "Draft Content-Location"))))
      (when (string-empty-p value)
        (signal 'chidu-jmap-error '("Draft Content-Location is empty")))
      value)))

(defun chidu-jmap-draft-projection--part-header-contract (part)
  "Return PART header contract status: `ok', `unsupported', or `invalid'."
  (condition-case nil
      (let* ((table
              (chidu-jmap-draft-projection--header-table
               (chidu-jmap-draft-part-headers part)))
             (type-header
              (chidu-jmap-draft-projection--only-header table "content-type"))
             (disposition-header
              (chidu-jmap-draft-projection--only-header
               table "content-disposition"))
             (cid-header
              (chidu-jmap-draft-projection--only-header table "content-id"))
             (language-header
              (chidu-jmap-draft-projection--only-header
               table "content-language"))
             (location-header
              (chidu-jmap-draft-projection--only-header
               table "content-location"))
             (multipart-p
              (string-prefix-p
               "multipart/" (chidu-jmap-draft-part-media-type part)))
             type-parsed disposition-parsed parameter-status)
        (when type-header
          (setq type-parsed
                (chidu-jmap-draft-projection--parameterized-header
                 type-header "Draft Content-Type")
                parameter-status
                (chidu-jmap-draft-projection--parameter-contract
                 type-parsed
                 (if multipart-p '(boundary) '(charset name)))))
        (when disposition-header
          (setq disposition-parsed
                (chidu-jmap-draft-projection--parameterized-header
                 disposition-header "Draft Content-Disposition"))
          (let ((status
                 (chidu-jmap-draft-projection--parameter-contract
                  disposition-parsed '(filename))))
            (unless (eq status 'ok) (setq parameter-status status))))
        (cond
         ((eq parameter-status 'invalid) 'invalid)
         ((or (eq parameter-status 'unsupported)
              (and multipart-p
                   type-parsed
                   (null (rfc2231-get-value type-parsed 'boundary))))
          'unsupported)
         (t
          (let* ((wire-type
                  (if type-parsed
                      (downcase (car type-parsed))
                    "text/plain"))
                 (wire-charset
                  (if type-parsed
                      (or
                       (rfc2231-get-value type-parsed 'charset)
                       (and (string-prefix-p "text/" wire-type) "us-ascii"))
                    "us-ascii"))
                 (wire-disposition
                  (and disposition-parsed
                       (downcase (car disposition-parsed))))
                 (wire-name
                  (or
                   (and disposition-parsed
                        (rfc2231-get-value disposition-parsed 'filename))
                   (and type-parsed (rfc2231-get-value type-parsed 'name))))
                 (wire-cid
                  (chidu-jmap-draft-projection--content-id cid-header))
                 (wire-language
                  (chidu-jmap-draft-projection--content-language
                   language-header))
                 (wire-location
                  (chidu-jmap-draft-projection--content-location
                   location-header)))
            (if
                (and
                 (equal wire-type (chidu-jmap-draft-part-media-type part))
                 (equal
                  (chidu-jmap-draft-projection--optional-downcase wire-charset)
                  (chidu-jmap-draft-projection--optional-downcase
                   (chidu-jmap-draft-part-charset part)))
                 (equal wire-disposition
                        (chidu-jmap-draft-part-disposition part))
                 (equal wire-name (chidu-jmap-draft-part-name part))
                 (equal wire-cid (chidu-jmap-draft-part-cid part))
                 (equal
                  wire-language
                  (vconcat
                   (cl-loop
                    for language across
                    (chidu-jmap-draft-part-language part)
                    collect (downcase language))))
                 (equal wire-location
                        (chidu-jmap-draft-part-location part)))
                'ok
              'invalid)))))
    (chidu-jmap-draft-projection-unsupported-metadata 'unsupported)
    (error 'invalid)))

(defun chidu-jmap-draft-projection--parts-header-contract (parts)
  "Return aggregate MIME header contract status for ordered PARTS."
  (let ((status 'ok))
    (cl-loop
     for part across parts
     for current = (chidu-jmap-draft-projection--part-header-contract part)
     do
     (cond
      ((eq current 'invalid) (setq status 'invalid))
      ((and (eq current 'unsupported) (eq status 'ok))
       (setq status 'unsupported))))
    status))

(defun chidu-jmap-draft-projection--canonical-convenience
    (observation parts context)
  "Return OBSERVATION tree PARTS after exact consistency checks for CONTEXT."
  (let ((index (chidu-jmap-remote-draft-observation-part-index observation))
        (seen (make-hash-table :test #'equal))
        canonical)
    (cl-loop
     for part across parts
     for part-id = (chidu-jmap-draft-part-part-id part)
     do
     (unless part-id
       (signal 'chidu-jmap-error
               (list (format "%s contains multipart/null partId" context))))
     (when (gethash part-id seen)
       (signal 'chidu-jmap-error
               (list (format "%s contains duplicate partId" context))))
     (puthash part-id t seen)
     (let ((tree-part (gethash part-id index)))
       (unless tree-part
         (signal 'chidu-jmap-error
                 (list (format "%s references a part outside bodyStructure"
                               context))))
       (unless (equal tree-part part)
         (signal 'chidu-jmap-error
                 (list (format "%s disagrees with bodyStructure" context))))
       (push tree-part canonical)))
    (vconcat (nreverse canonical))))

(defun chidu-jmap-draft-projection--safe-text-p
    (value &optional nonempty-p)
  "Return non-nil when nullable VALUE is Store-safe text.

NONEMPTY-P rejects an empty string when VALUE is present."
  (or
   (null value)
   (and (stringp value)
        (or (not nonempty-p) (not (string-empty-p value)))
        (not (string-match-p "[\0\r\n]" value)))))

(defun chidu-jmap-draft-projection--safe-attachment-metadata-p (part)
  "Return non-nil when attachment PART metadata fits Compose resources."
  (and
   (chidu-jmap-draft-projection--safe-text-p
    (chidu-jmap-draft-part-name part) t)
   (chidu-jmap-draft-projection--safe-text-p
    (chidu-jmap-draft-part-charset part) t)
   (chidu-jmap-draft-projection--safe-text-p
    (chidu-jmap-draft-part-cid part) t)
   (chidu-jmap-draft-projection--safe-text-p
    (chidu-jmap-draft-part-location part) t)
   (cl-loop
    for language across (chidu-jmap-draft-part-language part)
    always
    (chidu-jmap-draft-projection--safe-text-p language t))))

(defun chidu-jmap-draft-projection--empty-semantic-metadata-p (part)
  "Return non-nil when PART has no user-visible optional body metadata."
  (and (null (chidu-jmap-draft-part-name part))
       (null (chidu-jmap-draft-part-disposition part))
       (null (chidu-jmap-draft-part-cid part))
       (zerop (length (chidu-jmap-draft-part-language part)))
       (null (chidu-jmap-draft-part-location part))))

(defun chidu-jmap-draft-projection--attachment-shape (part)
  "Return semantic resource shape projected from canonical attachment PART."
  (chidu-draft-resource-shape-create
   :name (chidu-jmap-draft-part-name part)
   :media-type (chidu-jmap-draft-part-media-type part)
   :size (chidu-jmap-draft-part-size part)
   :remote-blob-id (chidu-jmap-draft-part-blob-id part)
   :charset (chidu-jmap-draft-part-charset part)
   :disposition (chidu-jmap-draft-part-disposition part)
   :cid (chidu-jmap-draft-part-cid part)
   :language (copy-sequence (chidu-jmap-draft-part-language part))
   :location (chidu-jmap-draft-part-location part)))

(cl-defun chidu-jmap-draft-projection--project-body (observation header-shape)
  "Project OBSERVATION body under HEADER-SHAPE or return a failure."
  (let* ((remote-id
          (chidu-jmap-remote-draft-observation-remote-email-id observation))
         (root (chidu-jmap-remote-draft-observation-body-tree observation))
         (text-parts
          (chidu-jmap-draft-projection--canonical-convenience
           observation
           (chidu-jmap-remote-draft-observation-text-body observation)
           "Draft textBody"))
         (html-parts
          (chidu-jmap-draft-projection--canonical-convenience
           observation
           (chidu-jmap-remote-draft-observation-html-body observation)
           "Draft htmlBody"))
         (attachment-parts
          (chidu-jmap-draft-projection--canonical-convenience
           observation
           (chidu-jmap-remote-draft-observation-attachments observation)
           "Draft attachments")))
    (cond
     ((/= 1 (length text-parts))
      (chidu-jmap-draft-projection--unsupported
       'draft-body-structure-unsupported remote-id))
     ((not (equal "text/plain"
                  (chidu-jmap-draft-part-media-type (aref text-parts 0))))
      (chidu-jmap-draft-projection--unsupported
       'draft-body-structure-unsupported remote-id))
     ((cl-loop for part across html-parts
               thereis (equal "text/html"
                              (chidu-jmap-draft-part-media-type part)))
      (chidu-jmap-draft-projection--unsupported
       'draft-html-unsupported remote-id))
     ((not (or (zerop (length html-parts))
               (and (= 1 (length html-parts))
                    (equal (aref text-parts 0) (aref html-parts 0)))))
      (chidu-jmap-draft-projection--unsupported
       'draft-body-structure-unsupported remote-id))
     (t
      (let* ((text-part (aref text-parts 0))
             (root-type (chidu-jmap-draft-part-media-type root))
             expected-attachments
             root-contract text-contract attachment-contract)
        (cond
         ((equal root-type "text/plain")
          (unless (equal root text-part)
            (signal 'chidu-jmap-error
                    '("Draft textBody disagrees with leaf bodyStructure")))
          (setq expected-attachments (vector)))
         ((equal root-type "multipart/mixed")
          (let ((children (chidu-jmap-draft-part-subparts root)))
            (if (or (< (length children) 2)
                    (not (equal (aref children 0) text-part))
                    (cl-loop
                     for index from 1 below (length children)
                     thereis
                     (string-prefix-p
                      "multipart/"
                      (chidu-jmap-draft-part-media-type
                       (aref children index)))))
                (cl-return-from chidu-jmap-draft-projection--project-body
                  (chidu-jmap-draft-projection--unsupported
                   'draft-body-structure-unsupported remote-id))
              (setq expected-attachments
                    (cl-subseq children 1)))))
         (t
          (cl-return-from chidu-jmap-draft-projection--project-body
            (chidu-jmap-draft-projection--unsupported
             'draft-body-structure-unsupported remote-id))))
        (unless (equal expected-attachments attachment-parts)
          (signal 'chidu-jmap-error
                  '("Draft attachments disagree with bodyStructure order")))
        (when
            (cl-loop
             for part across expected-attachments
             thereis
             (not
              (chidu-jmap-draft-projection--safe-attachment-metadata-p
               part)))
          (cl-return-from chidu-jmap-draft-projection--project-body
            (chidu-jmap-draft-projection--unsupported
             'draft-body-metadata-unsupported remote-id)))
        (setq root-contract
              (chidu-jmap-draft-projection--part-header-contract root)
              text-contract
              (if (eq root text-part)
                  root-contract
                (chidu-jmap-draft-projection--part-header-contract text-part))
              attachment-contract
              (chidu-jmap-draft-projection--parts-header-contract
               expected-attachments))
        (when (memq 'invalid
                    (list root-contract text-contract attachment-contract))
          (signal 'chidu-jmap-error
                  '("Draft MIME headers disagree with body part properties")))
        (when (memq 'unsupported
                    (list root-contract text-contract attachment-contract))
          (cl-return-from chidu-jmap-draft-projection--project-body
            (chidu-jmap-draft-projection--unsupported
             'draft-body-metadata-unsupported remote-id)))
        (unless
            (and
             (chidu-jmap-draft-projection--empty-semantic-metadata-p text-part)
             (chidu-jmap-draft-projection--safe-text-p
              (chidu-jmap-draft-part-charset text-part) t)
             (or (eq root text-part)
                 (chidu-jmap-draft-projection--headers-closed-p
                  (chidu-jmap-draft-part-headers text-part)
                  chidu-jmap-draft-projection--text-part-header-names))
             (or (eq root text-part)
                 (and
                  (null (chidu-jmap-draft-part-name root))
                  (null (chidu-jmap-draft-part-charset root))
                  (null (chidu-jmap-draft-part-disposition root))
                  (null (chidu-jmap-draft-part-cid root))
                  (zerop (length (chidu-jmap-draft-part-language root)))
                  (null (chidu-jmap-draft-part-location root))))
             (cl-loop
              for part across expected-attachments
              always
              (and
               (chidu-jmap-draft-projection--headers-closed-p
                (chidu-jmap-draft-part-headers part)
                chidu-jmap-draft-projection--attachment-header-names)
               (chidu-jmap-draft-projection--safe-attachment-metadata-p
                part))))
          (cl-return-from chidu-jmap-draft-projection--project-body
            (chidu-jmap-draft-projection--unsupported
             'draft-body-metadata-unsupported remote-id)))
        (let* ((values
                (chidu-jmap-remote-draft-observation-body-values observation))
               (text-id (chidu-jmap-draft-part-part-id text-part))
               (body-value (gethash text-id values)))
          (unless (and body-value (= 1 (hash-table-count values)))
            (signal 'chidu-jmap-error
                    '("Draft bodyValues does not exactly cover the text body")))
          (cond
           ((chidu-jmap-draft-body-value-truncated-p body-value)
            (chidu-jmap-draft-projection--unsupported
             'draft-body-truncated remote-id))
           ((chidu-jmap-draft-body-value-encoding-problem-p body-value)
            (chidu-jmap-draft-projection--unsupported
             'draft-body-encoding-problem remote-id))
           ((string-match-p
             "\0" (chidu-jmap-draft-body-value-value body-value))
            (chidu-jmap-draft-projection--unsupported
             'draft-body-metadata-unsupported remote-id))
           (t
            (let* ((resources
                    (vconcat
                     (cl-loop
                      for part across expected-attachments
                      collect
                      (chidu-jmap-draft-projection--attachment-shape part))))
                   (shape
                    (chidu-draft-editable-shape-from-values
                     (plist-get header-shape :originator)
                     (plist-get header-shape :to)
                     (plist-get header-shape :cc)
                     (plist-get header-shape :bcc)
                     (plist-get header-shape :reply-to)
                     (plist-get header-shape :subject)
                     (chidu-jmap-draft-body-value-value body-value)
                     resources)))
              (chidu-draft-editable-snapshot-create
               :remote-email-id remote-id
               :remote-blob-id
               (chidu-jmap-remote-draft-observation-remote-blob-id
                observation)
               :shape shape))))))))))

(defun chidu-jmap-draft-project
    (observation remote-drafts-mailbox-id)
  "Project OBSERVATION for exact REMOTE-DRAFTS-MAILBOX-ID to editable form."
  (let ((remote-id
         (chidu-jmap-remote-draft-observation-remote-email-id observation))
        (mailbox-ids
         (chidu-jmap-remote-draft-observation-mailbox-ids observation))
        (keywords
         (chidu-jmap-remote-draft-observation-keywords observation)))
    (cond
     ((not (seq-contains-p mailbox-ids remote-drafts-mailbox-id #'equal))
      (chidu-result-failure-create
       :kind 'draft-left-mailbox
       :data (list :remote-email-id remote-id)
       :retryable-p t))
     ((not (seq-contains-p keywords "$draft" #'equal))
      (chidu-result-failure-create
       :kind 'email-is-not-draft
       :data (list :remote-email-id remote-id)
       :retryable-p t))
     ((or (not (equal mailbox-ids (vector remote-drafts-mailbox-id)))
          (not (equal keywords ["$draft" "$seen"])))
      (chidu-jmap-draft-projection--unsupported
       'draft-metadata-unsupported remote-id))
     (t
      (let ((header-shape
             (chidu-jmap-draft-projection--project-headers observation)))
        (if (chidu-result-failure-p header-shape)
            header-shape
          (chidu-jmap-draft-projection--project-body
           observation header-shape)))))))

(provide 'chidu-jmap-draft-projection)

;;; chidu-jmap-draft-projection.el ends here
