;;; chidu-address-books.el --- JMAP AddressBook directory -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only RFC 9610 AddressBook directory.  Subscribed books are prominent,
;; explicitly requested unsubscribed books remain available, and server rights
;; govern which collections may be opened.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-surface)
(require 'chidu-contact)
(require 'chidu-contact-model)
(require 'chidu-contacts)
(require 'chidu-root)
(require 'chidu-runtime)
(require 'chidu-view-operation)
(require 'chidu-store)

(declare-function chidu "chidu" ())
(declare-function chidu--endpoint-label "chidu-root" (endpoint))
(declare-function chidu-home-endpoint-at-point "chidu-root" ())
(declare-function chidu-dispatch "chidu-transient" ())

(defvar chidu--app)

(cl-defstruct (chidu-address-books-state
               (:constructor chidu-address-books-state-create))
  "View state for one Endpoint's AddressBook directory."
  endpoint
  directory
  (phase 'initial)
  message)

(defun chidu-address-books--endpoint-label (endpoint)
  "Return a concise Contacts label for ENDPOINT."
  (format "%s · Contacts %s"
          (chidu--endpoint-label endpoint)
          (chidu-store-endpoint-primary-contacts-remote-account-id endpoint)))

(defun chidu-address-books--available-endpoints (app)
  "Return APP Endpoints with a primary Contacts Account."
  (seq-filter
   #'chidu-contact-endpoint-p
   (append (chidu--state-endpoints (chidu-app-state app)) nil)))

(defun chidu-address-books--read-endpoint (app)
  "Choose a Contacts-capable Endpoint from APP."
  (let ((at-point
         (and (derived-mode-p 'chidu-home-mode)
              (chidu-home-endpoint-at-point))))
    (if (chidu-contact-endpoint-p at-point)
        at-point
      (let* ((endpoints (chidu-address-books--available-endpoints app))
             (choices
              (mapcar
               (lambda (endpoint)
                 (cons (chidu-address-books--endpoint-label endpoint) endpoint))
               endpoints)))
        (pcase endpoints
          (`() (user-error "No Endpoint exposes JMAP Contacts"))
          (`(,endpoint) endpoint)
          (_
           (cdr
            (assoc
             (completing-read "Contacts Endpoint: " choices nil t)
             choices))))))))

(defun chidu-address-books--state (&optional view)
  "Return validated AddressBook state for VIEW or current view."
  (let* ((it (or view (appkit-current-surface)))
         (state (and it (appkit-surface-model it))))
    (unless (chidu-address-books-state-p state)
      (error "Chidu AddressBook view has invalid state"))
    state))

(defun chidu-address-books--request-sync (surface &optional _structure-p)
  "Request a committed refresh of SURFACE."
  (chidu-surface-refresh surface))

(defun chidu-address-books--status (book)
  "Return trailing status text for AddressBook BOOK."
  (let ((rights (chidu-address-book-rights book)))
    (concat
     "  "
     (string-join
      (delq nil
            (list
             (and (chidu-address-book-default-p book) "default")
             (and (chidu-address-book-subscribed-p book) "subscribed")
             (cond
              ((not (chidu-contact-rights-may-read-p rights)) "unreadable")
              ((chidu-contact-rights-may-write-p rights) "writable")
              (t "read-only"))))
      " · "))))

(defun chidu-address-books--section
    (key label books &optional default-expanded-p)
  "Return directory entries for KEY, LABEL, and BOOKS.

DEFAULT-EXPANDED-P is the initial Appkit fold state."
  (when books
    (let* ((section-key (list 'address-book-section key))
           (expanded-p
            (appkit-directory-fold-expanded-p
             (appkit-directory-surface) section-key default-expanded-p)))
      (cons
       (appkit-directory-entry-create
        :key section-key :role 'section :label label
        :foldable-p t :fold-key section-key
        :fold-default-expanded-p default-expanded-p
        :expanded-p expanded-p)
       (when expanded-p
         (mapcar
          (lambda (book)
            (let ((rights (chidu-address-book-rights book)))
              (appkit-directory-entry-create
               :key (list 'address-book (chidu-address-book-remote-id book))
               :role 'item :section-key section-key
               :label (chidu-address-book-name book)
               :trailing (chidu-address-books--status book)
               :indent 2
               :face (unless (chidu-contact-rights-may-read-p rights) 'shadow)
               :help-echo
               (or (chidu-address-book-description book)
                   (unless (chidu-contact-rights-may-read-p rights)
                     "This AddressBook is not readable"))
               :payload book)))
          books))))))

(defun chidu-address-books--entries (state)
  "Project AddressBook directory entries from STATE."
  (let* ((directory (chidu-address-books-state-directory state))
         (books
          (and directory
               (append (chidu-address-book-directory-address-books directory)
                       nil)))
         (subscribed
          (seq-filter #'chidu-address-book-subscribed-p books))
         (other
          (seq-remove #'chidu-address-book-subscribed-p books)))
    (or
     (append
      (chidu-address-books--section
       'subscribed "Subscribed Address Books" subscribed t)
      (chidu-address-books--section
       'other "Other Address Books" other nil))
     (list
      (appkit-directory-entry-create
       :key 'empty :role 'note
       :label
       (pcase (chidu-address-books-state-phase state)
         ((or 'initial 'loading) "Loading Address Books…")
         ('error
          (format "Unable to load Address Books: %s"
                  (or (chidu-address-books-state-message state)
                      "unknown error")))
         (_ "No Address Books."))
       :face
       (and (eq 'error (chidu-address-books-state-phase state)) 'error))))))

(defun chidu-address-books--update (context model message)
  "Commit a Surface MESSAGE and its presentation request."
  (chidu-surface-update context model message))

(defun chidu-address-books--loaded (view state directory)
  "Install DIRECTORY in current AddressBook VIEW STATE."
  (setf (chidu-address-books-state-directory state) directory
        (chidu-address-books-state-phase state) 'idle
        (chidu-address-books-state-message state) nil)
  (chidu-address-books--request-sync view t))

(defun chidu-address-books--failed (view state failure)
  "Install AddressBook FAILURE in VIEW STATE."
  (setf (chidu-address-books-state-phase state) 'error
        (chidu-address-books-state-message state)
        (chidu-runtime-error-message failure))
  (chidu-address-books--request-sync view t))

(defun chidu-address-books-refresh (&optional view)
  "Refresh AddressBooks in VIEW from JMAP."
  (interactive)
  (let* ((it (or view (appkit-current-surface)))
         (state (chidu-address-books--state it)))
    (setf (chidu-address-books-state-phase state) 'loading
          (chidu-address-books-state-message state) nil)
    (chidu-address-books--request-sync it t)
    (chidu-view-operation-start
     it 'address-books
     (lambda (runtime success-function error-function)
       (chidu-contact-list-address-books
        runtime
        (chidu-address-books-state-endpoint state)
        success-function error-function))
     (apply-partially #'chidu-address-books--loaded it state)
     (apply-partially #'chidu-address-books--failed it state))))

(defun chidu-address-books--activate (_surface entry)
  "Open the AddressBook carried by directory ENTRY."
  (let* ((book (appkit-directory-entry-payload entry))
         (view (or (appkit-current-surface)
                   (user-error "No live AddressBook view")))
         (state (chidu-address-books--state view)))
    (unless (chidu-address-book-p book)
      (user-error "No AddressBook at point"))
    (unless
        (chidu-contact-rights-may-read-p (chidu-address-book-rights book))
      (user-error "This AddressBook is not readable"))
    (chidu-contacts-open-list
     (appkit-surface-app view)
     (chidu-address-books-state-endpoint state)
     (chidu-address-books-state-directory state)
     book t)))

(defun chidu-address-books--fold (_surface _entry _expanded-p)
  "Reconcile the AddressBook directory after a fold change."
  (when-let* ((surface (appkit-current-surface)))
    (chidu-address-books--request-sync surface t)))

(defvar-keymap chidu-address-books-mode-map
  :doc "Keymap for `chidu-address-books-mode'."
  :parent appkit-directory-mode-map
  "?" #'chidu-dispatch
  "g" #'chidu-address-books-refresh
  "q" #'quit-window)

(define-derived-mode chidu-address-books-mode appkit-directory-mode
  "Chidu-AddressBooks"
  "Browse one JMAP Contacts Account's AddressBooks."
  (setq-local header-line-format
              '(:eval
                (when-let* ((view (appkit-current-surface))
                            (state (appkit-surface-model view))
                            ((chidu-address-books-state-p state)))
                  (format " Chidu Contacts · %s"
                          (chidu-address-books--endpoint-label
                           (chidu-address-books-state-endpoint state))))))
  (appkit-directory-configure
   (appkit-directory-surface)
   :activate-function #'chidu-address-books--activate
   :fold-function #'chidu-address-books--fold))

(defun chidu-address-books--renderer (_surface)
  "Create the generated AddressBook directory Renderer."
  (appkit-generated-renderer-create
   :mount #'ignore
   :merge (lambda (_old new) new)
   :render
   (lambda (surface _app model _request)
     (appkit-with-content-update surface
       (appkit-directory-reconcile
        (appkit-directory-surface)
        (chidu-address-books--entries model)))
     (force-mode-line-update)
     nil)
   :unmount (lambda (surface)
              (when (buffer-live-p (appkit-surface-buffer surface))
                (with-current-buffer (appkit-surface-buffer surface)
                  (appkit-directory-retire))))))

(defun chidu-address-books-open (app endpoint &optional select)
  "Open ENDPOINT AddressBooks in APP and optionally SELECT the view."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-contact-endpoint-p endpoint)
    (user-error "Endpoint does not expose JMAP Contacts"))
  (let* ((view-id
          (list 'address-books
                (chidu-store-endpoint-endpoint-id endpoint)))
         (existing (appkit-app-surface app view-id))
         (state
          (if existing
              (chidu-address-books--state existing)
            (chidu-address-books-state-create :endpoint endpoint)))
         (view
          (or existing
              (appkit-open-generated-surface
               (appkit-surface-type-create
                :name 'chidu-address-books
                :mode #'chidu-address-books-mode
                :init (lambda (_context input)
                        (appkit-next :model input
                                     :render t))
                :update #'chidu-address-books--update
                :renderer-factory #'chidu-address-books--renderer)
               :app app :identity view-id
               :buffer-name (format "*Chidu Contacts: %s*" (chidu-store-endpoint-login endpoint))
               :input state))))
    (unless existing
      (with-current-buffer (appkit-surface-buffer view)
        (chidu-address-books-refresh view)))
    (when select (pop-to-buffer (appkit-surface-buffer view)))
    (when existing
      (with-current-buffer (appkit-surface-buffer view)
        (setf (chidu-address-books-state-endpoint state) endpoint)
        (chidu-address-books-refresh view)))
    (appkit-surface-buffer view)))

;;;###autoload
(defun chidu-contacts (&optional endpoint)
  "Open JMAP AddressBooks for ENDPOINT."
  (interactive)
  (unless (appkit-app-live-p chidu--app) (chidu))
  (chidu-address-books-open
   chidu--app
   (or endpoint (chidu-address-books--read-endpoint chidu--app))
   t))

(provide 'chidu-address-books)

;;; chidu-address-books.el ends here
