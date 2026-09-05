;;; chidu-contact-model.el --- Read models for JMAP Contacts -*- lexical-binding: t; -*-

;;; Commentary:

;; Small protocol-shaped read models shared by the JMAP adapter, completion,
;; and Contacts views.  These are not a second JSContact persistence model:
;; read-only views may project selected fields, while future editing keeps the
;; immutable server Card as its patch base.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'chidu-record)

(chidu-define-record chidu-contact-rights
    "Viewer rights on one JMAP AddressBook."
  may-read-p
  may-write-p
  may-share-p
  may-delete-p)

(chidu-define-record chidu-address-book
    "One JMAP AddressBook projection."
  remote-id
  name
  description
  sort-order
  default-p
  subscribed-p
  rights)

(chidu-define-record chidu-address-book-directory
    "One complete AddressBook/get snapshot."
  state
  (address-books (vector)))

(chidu-define-record chidu-contact-value
    "One labeled JSContact value projected for reading."
  item-id
  value
  label
  (contexts (vector))
  pref
  (qualifiers (vector)))

(chidu-define-record chidu-contact-card
    "One bounded read projection of a JMAP ContactCard."
  remote-id
  uid
  kind
  name
  (address-book-ids (vector))
  (emails (vector))
  (phones (vector))
  (organizations (vector))
  (titles (vector))
  (addresses (vector))
  (online-services (vector))
  (notes (vector))
  (members (vector))
  created
  updated
  complete-p)

(chidu-define-record chidu-contact-page
    "One bounded ContactCard/query page."
  query-state
  total
  position
  next-position
  query
  (cards (vector))
  maybe-more-p
  anchor-id)

(defun chidu-contact-card-primary-email (card)
  "Return CARD's preferred email value, or nil."
  (when-let* ((emails (chidu-contact-card-emails card))
              ((> (length emails) 0)))
    (aref emails 0)))

(defun chidu-contact-card-primary-phone (card)
  "Return CARD's preferred phone value, or nil."
  (when-let* ((phones (chidu-contact-card-phones card))
              ((> (length phones) 0)))
    (aref phones 0)))

(defun chidu-contact-card-in-address-books-p (card address-book-ids)
  "Return non-nil when CARD belongs to one of ADDRESS-BOOK-IDS.

ADDRESS-BOOK-IDS is a hash set keyed by remote AddressBook id."
  (seq-some
   (lambda (id) (gethash id address-book-ids))
   (append (chidu-contact-card-address-book-ids card) nil)))

(defun chidu-contact-card-display-name (card)
  "Return a non-empty display name for CARD."
  (or (and-let* ((name (chidu-contact-card-name card))
                 ((not (string-empty-p name))))
        name)
      (and-let* ((email (chidu-contact-card-primary-email card)))
        (chidu-contact-value-value email))
      (format "%s contact"
              (capitalize (or (chidu-contact-card-kind card) "unnamed")))))

(defun chidu-contact-value-metadata (value)
  "Return concise metadata text for contact VALUE."
  (string-join
   (delq nil
         (append
          (and-let* ((label (chidu-contact-value-label value))
                     ((not (string-empty-p label))))
            (list label))
          (append (chidu-contact-value-contexts value) nil)
          (append (chidu-contact-value-qualifiers value) nil)))
   " · "))

(provide 'chidu-contact-model)

;;; chidu-contact-model.el ends here
