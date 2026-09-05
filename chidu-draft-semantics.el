;;; chidu-draft-semantics.el --- Editable Draft normal form -*- lexical-binding: t; -*-

;;; Commentary:

;; One closed semantic contract shared by remote Draft checkout and JMAP Draft
;; publication.  It deliberately excludes MIME boundaries, transfer encoding,
;; text-body charset representation, Date, Message-ID, part ids, and local
;; resource ids.  Case-insensitive MIME tokens and attachment charsets are
;; canonicalized before entering the normal form.
;; Those values may be normalized without changing the editable Draft.  Any
;; user-visible value that is not represented here must be rejected before a
;; remote Draft becomes a Compose workspace.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-store)

(chidu-define-record chidu-draft-resource-shape
    "One attachment's editable semantic metadata."
  name
  media-type
  (size 0)
  remote-blob-id
  charset
  disposition
  cid
  (language (vector))
  location)

(chidu-define-record chidu-draft-editable-shape
    "Canonical editable semantics shared by checkout and publication."
  originator
  (to "")
  (cc "")
  (bcc "")
  (reply-to "")
  (subject "")
  (body "")
  (resources (vector)))

(chidu-define-record chidu-draft-editable-snapshot
    "One representable immutable remote Draft before Identity binding."
  remote-email-id
  remote-blob-id
  shape)

(chidu-define-record chidu-draft-checkout-plan
    "One exact Identity-bound remote Draft ready for materialization."
  remote-email-id
  remote-blob-id
  identity
  shape
  document
  (resources (vector)))

(defun chidu-draft--address-name (name)
  "Return compiler-normalized address NAME."
  (and (stringp name) (not (string-empty-p name)) name))

(defun chidu-draft-identity-originator (identity)
  "Return the exact address Chidu's compiler emits for IDENTITY."
  (unless (chidu-store-identity-p identity)
    (signal 'wrong-type-argument (list 'chidu-store-identity-p identity)))
  (chidu-store-email-address-create
   :name (chidu-draft--address-name
          (chidu-store-identity-name identity))
   :email (chidu-store-identity-email identity)))

(defun chidu-draft-normalize-originator (address)
  "Return ADDRESS in the compiler's canonical originator form."
  (unless (chidu-store-email-address-p address)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-address-p address)))
  (chidu-store-email-address-create
   :name (chidu-draft--address-name
          (chidu-store-email-address-name address))
   :email (chidu-store-email-address-email address)))

(defun chidu-draft-originator-equal-p (left right)
  "Return non-nil when LEFT and RIGHT compile to the same From address."
  (and (chidu-store-email-address-p left)
       (chidu-store-email-address-p right)
       (equal (chidu-draft-normalize-originator left)
              (chidu-draft-normalize-originator right))))

(defun chidu-draft--downcase-required (value context)
  "Return lowercase string VALUE, requiring it for CONTEXT."
  (unless (and (stringp value) (not (string-empty-p value)))
    (signal 'chidu-invariant-error
            (list (format "%s must be non-empty text" context))))
  (downcase value))

(defun chidu-draft--optional-text (value context)
  "Return nullable text VALUE for CONTEXT, normalizing empty to nil."
  (cond
   ((null value) nil)
   ((not (stringp value))
    (signal 'chidu-invariant-error
            (list (format "%s must be text or nil" context))))
   ((string-empty-p value) nil)
   ((string-match-p "[\0\r\n]" value)
    (signal 'chidu-invariant-error
            (list (format "%s contains unsafe control text" context))))
   (t value)))

(defun chidu-draft--downcase-optional (value context)
  "Return lowercase optional text VALUE for CONTEXT."
  (when-let* ((text (chidu-draft--optional-text value context)))
    (downcase text)))

(defun chidu-draft--resource-size (value)
  "Return nonnegative resource size VALUE."
  (unless (and (integerp value) (>= value 0))
    (signal 'chidu-invariant-error
            (list "Compose resource size must be nonnegative" value)))
  value)

(defun chidu-draft--copy-language (language)
  "Return immutable vector copy of LANGUAGE."
  (unless (vectorp language)
    (signal 'wrong-type-argument (list 'vectorp language)))
  (copy-sequence language))

(defun chidu-draft--resource-shape-from-values
    (name media-type size remote-blob-id charset disposition cid language location)
  "Return canonical resource semantics from metadata VALUES.

NAME, MEDIA-TYPE, SIZE, REMOTE-BLOB-ID, CHARSET, DISPOSITION, CID, LANGUAGE,
and LOCATION are the exact source fields to normalize."
  (let ((language (chidu-draft--copy-language language)))
    (cl-loop
     for index below (length language)
     for value = (aref language index)
     unless
     (and (stringp value) (not (string-empty-p value))
          (not (string-match-p "[\0\r\n]" value)))
     do (signal 'chidu-invariant-error
                '("Compose resource language contains invalid text"))
     else do (aset language index (downcase value)))
    (chidu-draft-resource-shape-create
     :name (chidu-draft--optional-text name "Compose resource name")
     :media-type
     (chidu-draft--downcase-required
      media-type "Compose resource media type")
     :size (chidu-draft--resource-size size)
     :remote-blob-id
     (chidu-draft--optional-text remote-blob-id "Compose resource Blob id")
     :charset
     (chidu-draft--downcase-optional charset "Compose resource charset")
     :disposition
     (chidu-draft--downcase-optional
      disposition "Compose resource disposition")
     :cid (chidu-draft--optional-text cid "Compose resource Content-ID")
     :language language
     :location
     (chidu-draft--optional-text
      location "Compose resource Content-Location"))))

(defun chidu-draft-normalize-resource-shape (resource)
  "Return canonical semantic copy of RESOURCE shape."
  (unless (chidu-draft-resource-shape-p resource)
    (signal 'wrong-type-argument
            (list 'chidu-draft-resource-shape-p resource)))
  (chidu-draft--resource-shape-from-values
   (chidu-draft-resource-shape-name resource)
   (chidu-draft-resource-shape-media-type resource)
   (chidu-draft-resource-shape-size resource)
   (chidu-draft-resource-shape-remote-blob-id resource)
   (chidu-draft-resource-shape-charset resource)
   (chidu-draft-resource-shape-disposition resource)
   (chidu-draft-resource-shape-cid resource)
   (chidu-draft-resource-shape-language resource)
   (chidu-draft-resource-shape-location resource)))

(defun chidu-draft-resource-shape-from-observation (resource)
  "Return semantic shape for Compose resource observation RESOURCE."
  (unless (chidu-store-compose-resource-observation-p resource)
    (signal
     'wrong-type-argument
     (list 'chidu-store-compose-resource-observation-p resource)))
  (chidu-draft--resource-shape-from-values
   (chidu-store-compose-resource-observation-name resource)
   (chidu-store-compose-resource-observation-media-type resource)
   (chidu-store-compose-resource-observation-size resource)
   (chidu-store-compose-resource-observation-remote-blob-id resource)
   (chidu-store-compose-resource-observation-charset resource)
   (chidu-store-compose-resource-observation-disposition resource)
   (chidu-store-compose-resource-observation-cid resource)
   (chidu-store-compose-resource-observation-language resource)
   (chidu-store-compose-resource-observation-location resource)))

(defun chidu-draft-resource-shape-from-resource (resource)
  "Return semantic shape for registered Compose RESOURCE."
  (unless (chidu-store-compose-resource-p resource)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-resource-p resource)))
  (chidu-draft--resource-shape-from-values
   (chidu-store-compose-resource-name resource)
   (chidu-store-compose-resource-media-type resource)
   (chidu-store-compose-resource-size resource)
   (chidu-store-compose-resource-remote-blob-id resource)
   (chidu-store-compose-resource-charset resource)
   (chidu-store-compose-resource-disposition resource)
   (chidu-store-compose-resource-cid resource)
   (chidu-store-compose-resource-language resource)
   (chidu-store-compose-resource-location resource)))

(defun chidu-draft--validate-document-text (document)
  "Require DOCUMENT to contain compiler-safe structured text."
  (unless (chidu-store-compose-document-p document)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-document-p document)))
  (cl-loop
   for value in
   (list
    (chidu-store-compose-document-to document)
    (chidu-store-compose-document-cc document)
    (chidu-store-compose-document-bcc document)
    (chidu-store-compose-document-reply-to document)
    (chidu-store-compose-document-subject document)
    (chidu-store-compose-document-body document))
   unless (and (stringp value) (not (string-match-p "\0" value)))
   do (signal 'chidu-invariant-error
              '("Compose document contains invalid text")))
  document)

(defun chidu-draft-editable-shape-from-values
    (originator to cc bcc reply-to subject body resource-shapes)
  "Return normalized editable semantics from exact field VALUES.

ORIGINATOR is the compiler-visible From address.  TO, CC, BCC, REPLY-TO,
SUBJECT, and BODY are editable document fields.  RESOURCE-SHAPES is ordered
semantic attachment evidence and contains no local resource identifiers."
  (unless (chidu-store-email-address-p originator)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-address-p originator)))
  (dolist (value (list to cc bcc reply-to subject body))
    (unless (and (stringp value) (not (string-match-p "\0" value)))
      (signal 'chidu-invariant-error
              '("Draft editable text contains an invalid value"))))
  (unless (vectorp resource-shapes)
    (signal 'wrong-type-argument (list 'vectorp resource-shapes)))
  (let ((normalized-resources
         (vconcat
          (cl-loop
           for resource across resource-shapes
           collect (chidu-draft-normalize-resource-shape resource)))))
    (chidu-draft-editable-shape-create
     :originator (chidu-draft-normalize-originator originator)
     :to to :cc cc :bcc bcc :reply-to reply-to
     :subject subject :body body
     :resources normalized-resources)))

(defun chidu-draft--shape
    (originator document resource-shapes)
  "Return editable shape from ORIGINATOR, DOCUMENT, and RESOURCE-SHAPES."
  (chidu-draft--validate-document-text document)
  (chidu-draft-editable-shape-from-values
   originator
   (chidu-store-compose-document-to document)
   (chidu-store-compose-document-cc document)
   (chidu-store-compose-document-bcc document)
   (chidu-store-compose-document-reply-to document)
   (chidu-store-compose-document-subject document)
   (chidu-store-compose-document-body document)
   resource-shapes))

(defun chidu-draft-editable-shape-from-observations
    (originator document resources)
  "Return normal form for ORIGINATOR, DOCUMENT, and observation RESOURCES."
  (unless (vectorp resources)
    (signal 'wrong-type-argument (list 'vectorp resources)))
  (let ((resource-ids
         (chidu-store-compose-document-resource-ids document)))
    (unless (= (length resource-ids) (length resources))
      (signal 'chidu-invariant-error
              '("Draft resource count does not match the document")))
    (chidu-draft--shape
     originator document
     (vconcat
      (cl-loop
       for resource-id across resource-ids
       for resource across resources
       unless
       (and (chidu-store-compose-resource-observation-p resource)
            (equal
             resource-id
             (chidu-store-compose-resource-observation-resource-id resource)))
       do (signal 'chidu-invariant-error
                  '("Draft resources do not match document order"))
       collect (chidu-draft-resource-shape-from-observation resource))))))

(defun chidu-draft-editable-shape-from-resources
    (originator document resources)
  "Return normal form for ORIGINATOR, DOCUMENT, and registered RESOURCES."
  (unless (vectorp resources)
    (signal 'wrong-type-argument (list 'vectorp resources)))
  (let ((resource-ids
         (chidu-store-compose-document-resource-ids document)))
    (unless (= (length resource-ids) (length resources))
      (signal 'chidu-invariant-error
              '("Draft resource count does not match the document")))
    (chidu-draft--shape
     originator document
     (vconcat
      (cl-loop
       for resource-id across resource-ids
       for resource across resources
       unless
       (and (chidu-store-compose-resource-p resource)
            (equal resource-id
                   (chidu-store-compose-resource-resource-id resource)))
       do (signal 'chidu-invariant-error
                  '("Draft resources do not match document order"))
       collect (chidu-draft-resource-shape-from-resource resource))))))

(defun chidu-draft--identity-candidates (account originator)
  "Return available ACCOUNT identities plausibly matching ORIGINATOR."
  (let ((email (chidu-store-email-address-email originator))
        candidates)
    (cl-loop
     for identity across (chidu-store-account-identities account)
     when
     (and
      (chidu-store-identity-available-p identity)
      (string-equal-ignore-case
       email (chidu-store-identity-email identity)))
     do (push identity candidates))
    (nreverse candidates)))

(defun chidu-draft--validated-snapshot-shape (snapshot)
  "Return SNAPSHOT shape after reconstructing its closed normal form."
  (let* ((shape (chidu-draft-editable-snapshot-shape snapshot))
         (normalized
          (and
           (chidu-draft-editable-shape-p shape)
           (chidu-draft-editable-shape-from-values
            (chidu-draft-editable-shape-originator shape)
            (chidu-draft-editable-shape-to shape)
            (chidu-draft-editable-shape-cc shape)
            (chidu-draft-editable-shape-bcc shape)
            (chidu-draft-editable-shape-reply-to shape)
            (chidu-draft-editable-shape-subject shape)
            (chidu-draft-editable-shape-body shape)
            (chidu-draft-editable-shape-resources shape)))))
    (unless (and normalized (equal shape normalized))
      (signal 'chidu-invariant-error
              '("Draft snapshot is not in editable normal form")))
    shape))

(defun chidu-draft--resource-observation-from-shape (resource-id resource)
  "Return local RESOURCE-ID observation from semantic RESOURCE."
  (unless (chidu-store-local-id-p resource-id)
    (signal 'chidu-invariant-error
            (list "Draft checkout allocated an invalid resource id" resource-id)))
  (unless (chidu-draft-resource-shape-p resource)
    (signal 'wrong-type-argument
            (list 'chidu-draft-resource-shape-p resource)))
  (let ((blob-id (chidu-draft-resource-shape-remote-blob-id resource)))
    (unless (and (stringp blob-id) (not (string-empty-p blob-id))
                 (not (string-match-p "\0" blob-id)))
      (signal 'chidu-invariant-error
              '("Remote Draft resource has no usable Blob id")))
    (chidu-store-compose-resource-observation-create
     :resource-id resource-id
     :name (chidu-draft-resource-shape-name resource)
     :media-type (chidu-draft-resource-shape-media-type resource)
     :size (chidu-draft-resource-shape-size resource)
     :digest nil
     :remote-blob-id blob-id
     :charset (chidu-draft-resource-shape-charset resource)
     :disposition (chidu-draft-resource-shape-disposition resource)
     :cid (chidu-draft-resource-shape-cid resource)
     :language (copy-sequence (chidu-draft-resource-shape-language resource))
     :location (chidu-draft-resource-shape-location resource))))

(defun chidu-draft--plan-content (shape)
  "Return (DOCUMENT . RESOURCE-OBSERVATIONS) allocated for semantic SHAPE."
  (let* ((resource-shapes (chidu-draft-editable-shape-resources shape))
         (resource-ids
          (vconcat
           (cl-loop repeat (length resource-shapes)
                    collect (chidu-store-new-local-id))))
         (resources
          (vconcat
           (cl-loop
            for resource-id across resource-ids
            for resource across resource-shapes
            collect
            (chidu-draft--resource-observation-from-shape
             resource-id resource))))
         (document
          (chidu-store-compose-document-create
           :to (chidu-draft-editable-shape-to shape)
           :cc (chidu-draft-editable-shape-cc shape)
           :bcc (chidu-draft-editable-shape-bcc shape)
           :reply-to (chidu-draft-editable-shape-reply-to shape)
           :subject (chidu-draft-editable-shape-subject shape)
           :body (chidu-draft-editable-shape-body shape)
           :resource-ids resource-ids)))
    (cons document resources)))

(defun chidu-draft-bind-checkout (account snapshot)
  "Bind representable SNAPSHOT to exactly one compiler-equivalent ACCOUNT Identity.

Return a `chidu-draft-checkout-plan' or a typed non-retryable failure.  Local
resource ids are allocated only after representability and Identity binding are
both closed."
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-draft-editable-snapshot-p snapshot)
    (signal 'wrong-type-argument
            (list 'chidu-draft-editable-snapshot-p snapshot)))
  (let* ((shape (chidu-draft--validated-snapshot-shape snapshot))
         (originator (chidu-draft-editable-shape-originator shape))
         (candidates (chidu-draft--identity-candidates account originator))
         (matches
          (cl-remove-if-not
           (lambda (identity)
             (chidu-draft-originator-equal-p
              originator (chidu-draft-identity-originator identity)))
           candidates)))
    (cond
     ((null candidates)
      (chidu-result-failure-create
       :kind 'draft-identity-unavailable
       :data (list :from (chidu-store-email-address-email originator))
       :retryable-p nil))
     ((null matches)
      (chidu-result-failure-create
       :kind 'draft-originator-unsupported
       :data
       (list :from-name (chidu-store-email-address-name originator)
             :from-email (chidu-store-email-address-email originator))
       :retryable-p nil))
     ((cdr matches)
      (chidu-result-failure-create
       :kind 'draft-identity-ambiguous
       :data (list :from (chidu-store-email-address-email originator))
       :retryable-p nil))
     (t
      (let* ((content (chidu-draft--plan-content shape))
             (document (car content))
             (resources (cdr content)))
        (chidu-draft-checkout-plan-create
         :remote-email-id
         (chidu-draft-editable-snapshot-remote-email-id snapshot)
         :remote-blob-id
         (chidu-draft-editable-snapshot-remote-blob-id snapshot)
         :identity (car matches)
         :shape shape
         :document document
         :resources resources))))))

(provide 'chidu-draft-semantics)

;;; chidu-draft-semantics.el ends here
