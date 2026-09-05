;;; chidu-jmap-compose.el --- Compile Compose documents to JMAP Drafts -*- lexical-binding: t; -*-

;;; Commentary:

;; This module is a pure compiler from Chidu's shared editable Draft normal
;; form to the create object accepted by JMAP Email/set.  It does not own a
;; workspace, upload resources, perform HTTP, or interpret SetResponse outcomes.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'chidu-draft-semantics)
(require 'chidu-jmap-types)
(require 'chidu-store)

(defun chidu-jmap-compose--object (&rest pairs)
  "Return a JSON object from alternating string/value PAIRS."
  (let ((object (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) object))
    object))

(defun chidu-jmap-compose--true-set (&rest values)
  "Return a JMAP set object containing non-nil string VALUES."
  (let ((object (make-hash-table :test #'equal)))
    (dolist (value values)
      (when value
        (unless (and (stringp value) (not (string-empty-p value)))
          (signal 'chidu-jmap-error
                  (list "JMAP set member must be non-empty text" value)))
        (puthash value t object)))
    object))

(defun chidu-jmap-compose--put-raw-header (email name value)
  "Set raw header NAME to non-empty VALUE in EMAIL."
  (unless (string-empty-p value)
    (puthash (concat "header:" name) value email)))

(defun chidu-jmap-compose--address-object (address)
  "Return one JMAP EmailAddress object for semantic ADDRESS."
  (unless (chidu-store-email-address-p address)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-address-p address)))
  (let ((object
         (chidu-jmap-compose--object
          "email" (chidu-store-email-address-email address))))
    (when-let* ((name (chidu-store-email-address-name address)))
      (puthash "name" name object))
    object))

(defun chidu-jmap-compose-editable-shape (document identity resources)
  "Return the semantic normal form compiled from DOCUMENT, IDENTITY, RESOURCES."
  (chidu-draft-editable-shape-from-resources
   (chidu-draft-identity-originator identity) document resources))

(defun chidu-jmap-compose--resource-part (resource)
  "Return one JMAP EmailBodyPart for semantic RESOURCE shape."
  (unless (chidu-draft-resource-shape-p resource)
    (signal 'wrong-type-argument
            (list 'chidu-draft-resource-shape-p resource)))
  (let ((blob-id (chidu-draft-resource-shape-remote-blob-id resource))
        (part
         (chidu-jmap-compose--object
          "type" (chidu-draft-resource-shape-media-type resource))))
    (unless blob-id
      (signal 'chidu-jmap-error
              '("Compose resource has no confirmed Blob id")))
    (puthash "blobId" blob-id part)
    (dolist
        (entry
         `(("name" . ,(chidu-draft-resource-shape-name resource))
           ("charset" . ,(chidu-draft-resource-shape-charset resource))
           ("disposition" .
            ,(chidu-draft-resource-shape-disposition resource))
           ("cid" . ,(chidu-draft-resource-shape-cid resource))
           ("location" . ,(chidu-draft-resource-shape-location resource))))
      (when (cdr entry) (puthash (car entry) (cdr entry) part)))
    (let ((language (chidu-draft-resource-shape-language resource)))
      (when (> (length language) 0)
        (puthash "language" language part)))
    part))

(defun chidu-jmap-compose--resource-parts (shape)
  "Return ordered JMAP body parts for editable Draft SHAPE."
  (vconcat
   (cl-loop
    for resource across (chidu-draft-editable-shape-resources shape)
    collect (chidu-jmap-compose--resource-part resource))))

(defun chidu-jmap-compose-draft-email
    (document identity remote-drafts-mailbox-id
              &optional message-id resources)
  "Compile DOCUMENT to a JMAP Draft Email create object.

IDENTITY supplies the exact From address.  REMOTE-DRAFTS-MAILBOX-ID is the JMAP
Mailbox id with role `drafts'.  MESSAGE-ID, when non-nil, is the stable value
used to reconcile this create.  RESOURCES must exactly follow DOCUMENT resource
ids and carry confirmed Blob ids.  Recipient fields remain in raw header form
so a half-finished Draft need not parse as sendable mail."
  (unless (chidu-store-identity-p identity)
    (signal 'wrong-type-argument (list 'chidu-store-identity-p identity)))
  (setq remote-drafts-mailbox-id
        (chidu-jmap--id remote-drafts-mailbox-id "Drafts Mailbox id"))
  (let* ((shape
          (chidu-jmap-compose-editable-shape
           document identity (or resources (vector))))
         (part-id "text")
         (body-value
          (chidu-jmap-compose--object
           "value" (chidu-draft-editable-shape-body shape)))
         (body-values (chidu-jmap-compose--object part-id body-value))
         (text-part
          (chidu-jmap-compose--object
           "type" "text/plain"
           "partId" part-id))
         (resource-parts (chidu-jmap-compose--resource-parts shape))
         (body-structure
          (if (zerop (length resource-parts))
              text-part
            (chidu-jmap-compose--object
             "type" "multipart/mixed"
             "subParts" (vconcat (vector text-part) resource-parts))))
         (email
          (chidu-jmap-compose--object
           "mailboxIds"
           (chidu-jmap-compose--true-set remote-drafts-mailbox-id)
           "keywords"
           (chidu-jmap-compose--true-set "$draft" "$seen")
           "from"
           (vector
            (chidu-jmap-compose--address-object
             (chidu-draft-editable-shape-originator shape)))
           "subject" (chidu-draft-editable-shape-subject shape)
           "bodyStructure" body-structure
           "bodyValues" body-values)))
    (when message-id
      (puthash "messageId" (vector message-id) email))
    (chidu-jmap-compose--put-raw-header
     email "To" (chidu-draft-editable-shape-to shape))
    (chidu-jmap-compose--put-raw-header
     email "Cc" (chidu-draft-editable-shape-cc shape))
    (chidu-jmap-compose--put-raw-header
     email "Bcc" (chidu-draft-editable-shape-bcc shape))
    (chidu-jmap-compose--put-raw-header
     email "Reply-To" (chidu-draft-editable-shape-reply-to shape))
    email))

(provide 'chidu-jmap-compose)

;;; chidu-jmap-compose.el ends here
