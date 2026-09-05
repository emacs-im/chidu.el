;;; chidu-jscontact.el --- Selected JSContact read decoder -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure decoder for the JSContact fields projected by Chidu's read-only
;; Contacts UI.  It preserves typed map entries and preference ordering, but
;; deliberately does not claim to represent an editable complete Card.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-contact-model)
(require 'chidu-jmap-types)

(defun chidu-jscontact--nullable-text (object key context)
  "Return nullable text KEY from OBJECT for CONTEXT."
  (let ((value (gethash key object :json-null)))
    (unless (eq value :json-null)
      (chidu-jmap--string value context t))))

(defun chidu-jscontact--preference (object context)
  "Return OBJECT preference for CONTEXT, or nil."
  (let ((wire (gethash "pref" object :json-null)))
    (unless (eq wire :json-null)
      (let ((value (chidu-jmap--safe-positive-integer wire context)))
        (unless (<= value 100)
          (signal 'chidu-jmap-error
                  (list (format "%s exceeds 100" context))))
        value))))

(defun chidu-jscontact--value-less-p (left right)
  "Return non-nil when contact value LEFT sorts before RIGHT."
  (let ((left-pref (or (chidu-contact-value-pref left) 101))
        (right-pref (or (chidu-contact-value-pref right) 101)))
    (if (= left-pref right-pref)
        (string-lessp (chidu-contact-value-item-id left)
                      (chidu-contact-value-item-id right))
      (< left-pref right-pref))))

(defun chidu-jscontact--decode-map (card property decoder context)
  "Decode CARD map PROPERTY with DECODER for CONTEXT."
  (let ((wire (gethash property card :json-null)) result)
    (unless (eq wire :json-null)
      (let ((object (chidu-jmap--hash wire context)))
        (maphash
         (lambda (item-id value)
           (push
            (funcall decoder item-id (chidu-jmap--hash value context))
            result))
         object)))
    (vconcat (sort result #'chidu-jscontact--value-less-p))))

(defun chidu-jscontact--components (object context)
  "Return readable ordered components from OBJECT for CONTEXT."
  (let ((full
         (chidu-jscontact--nullable-text
          object "full" (format "%s full" context))))
    (if (and full (not (string-empty-p full)))
        full
      (let* ((wire (gethash "components" object :json-null))
             (components
              (unless (eq wire :json-null)
                (chidu-jmap--vector wire (format "%s components" context))))
             (separator
              (or
               (chidu-jscontact--nullable-text
                object "defaultSeparator"
                (format "%s defaultSeparator" context))
               " "))
             (result "")
             previous-value-p)
        (when components
          (cl-loop
           for wire-component across components
           for component =
           (chidu-jmap--hash wire-component (format "%s component" context))
           for kind =
           (chidu-jmap--string
            (chidu-jmap--required
             component "kind" (format "%s component" context))
            (format "%s component kind" context))
           for value =
           (chidu-jmap--string
            (chidu-jmap--required
             component "value" (format "%s component" context))
            (format "%s component value" context) t)
           do
           (if (equal kind "separator")
               (setq result (concat result value)
                     previous-value-p nil)
             (when (and previous-value-p (not (string-empty-p result)))
               (setq result (concat result separator)))
             (setq result (concat result value)
                   previous-value-p t))))
        (unless (string-empty-p result) result)))))

(defun chidu-jscontact--name (card context)
  "Return display name from JSContact CARD for CONTEXT, or nil."
  (let* ((label (format "%s name" context))
         (value (gethash "name" card :json-null)))
    (unless (eq value :json-null)
      (or
       (chidu-jscontact--components
        (chidu-jmap--hash value label) label)
       (signal 'chidu-jmap-error
               (list (format "%s has no full name or value component" label)))))))

(defun chidu-jscontact--common-value
    (item-id object value label qualifiers context)
  "Return one contact value from ITEM-ID, OBJECT, VALUE, and metadata.

LABEL and QUALIFIERS may be nil.  CONTEXT names validation errors."
  (chidu-contact-value-create
   :item-id item-id
   :value value
   :label
   (or label
       (chidu-jscontact--nullable-text
        object "label" (format "%s label" context)))
   :contexts
   (chidu-jmap--optional-true-map-keys
    object "contexts" (format "%s contexts" context))
   :pref (chidu-jscontact--preference
          object (format "%s pref" context))
   :qualifiers (vconcat (delq nil qualifiers))))

(defun chidu-jscontact--emails (card context)
  "Decode CARD email values for CONTEXT."
  (chidu-jscontact--decode-map
   card "emails"
   (lambda (item-id email)
     (chidu-jscontact--common-value
      item-id email
      (chidu-jmap--string
       (chidu-jmap--required email "address" context)
       (format "%s address" context))
      nil nil context))
   (format "%s emails" context)))

(defun chidu-jscontact--phones (card context)
  "Decode CARD phone values for CONTEXT."
  (chidu-jscontact--decode-map
   card "phones"
   (lambda (item-id phone)
     (chidu-jscontact--common-value
      item-id phone
      (chidu-jmap--string
       (chidu-jmap--required phone "number" context)
       (format "%s number" context))
      nil
      (append
       (chidu-jmap--optional-true-map-keys
        phone "features" (format "%s features" context))
       nil)
      context))
   (format "%s phones" context)))

(defun chidu-jscontact--organization-value (organization context)
  "Return readable ORGANIZATION text for CONTEXT."
  (let ((name
         (chidu-jscontact--nullable-text
          organization "name" (format "%s name" context)))
        (wire-units (gethash "units" organization :json-null))
        units)
    (unless (eq wire-units :json-null)
      (cl-loop
       for wire-unit across
       (chidu-jmap--vector wire-units (format "%s units" context))
       for unit = (chidu-jmap--hash wire-unit (format "%s unit" context))
       do
       (push
        (chidu-jmap--string
         (chidu-jmap--required unit "name" (format "%s unit" context))
         (format "%s unit name" context))
        units)))
    (let ((value (string-join (delq nil (cons name (nreverse units))) " · ")))
      (when (string-empty-p value)
        (signal 'chidu-jmap-error
                (list (format "%s has neither name nor units" context))))
      value)))

(defun chidu-jscontact--organizations (card context)
  "Decode CARD organizations for CONTEXT."
  (chidu-jscontact--decode-map
   card "organizations"
   (lambda (item-id organization)
     (chidu-jscontact--common-value
      item-id organization
      (chidu-jscontact--organization-value organization context)
      nil nil context))
   (format "%s organizations" context)))

(defun chidu-jscontact--titles (card context)
  "Decode CARD titles for CONTEXT."
  (chidu-jscontact--decode-map
   card "titles"
   (lambda (item-id title)
     (let ((kind
            (or
             (chidu-jscontact--nullable-text
              title "kind" (format "%s kind" context))
             "title"))
           (organization-id
            (chidu-jscontact--nullable-text
             title "organizationId"
             (format "%s organizationId" context))))
       (chidu-jscontact--common-value
        item-id title
        (chidu-jmap--string
         (chidu-jmap--required title "name" context)
         (format "%s name" context))
        nil (list kind organization-id) context)))
   (format "%s titles" context)))

(defun chidu-jscontact--addresses (card context)
  "Decode CARD postal addresses for CONTEXT."
  (chidu-jscontact--decode-map
   card "addresses"
   (lambda (item-id address)
     (let ((value (chidu-jscontact--components address context))
           (country
            (chidu-jscontact--nullable-text
             address "countryCode" (format "%s countryCode" context))))
       (unless value
         (signal 'chidu-jmap-error
                 (list (format "%s has no displayable value" context))))
       (chidu-jscontact--common-value
        item-id address value nil (list country) context)))
   (format "%s addresses" context)))

(defun chidu-jscontact--online-services (card context)
  "Decode CARD online services for CONTEXT."
  (chidu-jscontact--decode-map
   card "onlineServices"
   (lambda (item-id service)
     (let* ((service-name
             (chidu-jscontact--nullable-text
              service "service" (format "%s service" context)))
            (user
             (chidu-jscontact--nullable-text
              service "user" (format "%s user" context)))
            (uri
             (chidu-jscontact--nullable-text
              service "uri" (format "%s uri" context)))
            (value (or user uri)))
       (unless value
         (signal 'chidu-jmap-error
                 (list (format "%s has neither user nor uri" context))))
       (chidu-jscontact--common-value
        item-id service value service-name
        (and user uri (list uri)) context)))
   (format "%s onlineServices" context)))

(defun chidu-jscontact--notes (card context)
  "Decode CARD notes for CONTEXT."
  (chidu-jscontact--decode-map
   card "notes"
   (lambda (item-id note)
     (chidu-jscontact--common-value
      item-id note
      (chidu-jmap--string
       (chidu-jmap--required note "note" context)
       (format "%s note" context) t)
      nil nil context))
   (format "%s notes" context)))

(defun chidu-jscontact-decode-card (wire complete-p)
  "Decode ContactCard WIRE, marking it COMPLETE-P."
  (let* ((context "ContactCard/get item")
         (card (chidu-jmap--hash wire context))
         (kind
          (or
           (chidu-jscontact--nullable-text card "kind" "ContactCard kind")
           "individual"))
         (address-book-ids
          (chidu-jmap--true-map-keys
           (chidu-jmap--required card "addressBookIds" context)
           "ContactCard addressBookIds" t)))
    (when (= 0 (length address-book-ids))
      (signal 'chidu-jmap-error
              '("ContactCard belongs to no AddressBook")))
    (chidu-contact-card-create
     :remote-id
     (chidu-jmap--id
      (chidu-jmap--required card "id" context) "ContactCard id")
     :uid
     (chidu-jmap--string
      (chidu-jmap--required card "uid" context) "ContactCard uid")
     :kind kind
     :name (chidu-jscontact--name card "ContactCard")
     :address-book-ids address-book-ids
     :emails (chidu-jscontact--emails card "ContactCard email")
     :phones (chidu-jscontact--phones card "ContactCard phone")
     :organizations
     (chidu-jscontact--organizations card "ContactCard organization")
     :titles (chidu-jscontact--titles card "ContactCard title")
     :addresses
     (if complete-p
         (chidu-jscontact--addresses card "ContactCard address")
       (vector))
     :online-services
     (if complete-p
         (chidu-jscontact--online-services card "ContactCard service")
       (vector))
     :notes
     (if complete-p
         (chidu-jscontact--notes card "ContactCard note")
       (vector))
     :members
     (if complete-p
         (chidu-jmap--optional-true-map-keys
          card "members" "ContactCard members")
       (vector))
     :created
     (and complete-p
          (chidu-jscontact--nullable-text
           card "created" "ContactCard created"))
     :updated
     (chidu-jscontact--nullable-text
      card "updated" "ContactCard updated")
     :complete-p complete-p)))

(provide 'chidu-jscontact)

;;; chidu-jscontact.el ends here
