;;; chidu-contacts.el --- Bounded JMAP ContactCard lists -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only, server-backed ContactCard query views.  No local Contacts mirror
;; is maintained; each view owns its exact RFC 9610 query, anchor, and cancelable
;; JMAP operation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'chidu-contact)
(require 'chidu-contact-model)
(require 'chidu-runtime)
(require 'chidu-surface-operation)
(require 'chidu-store)
(require 'chidu-text)

(declare-function chidu-compose-to-contact "chidu-compose" (endpoint card))
(declare-function chidu-contact-view-open
                  "chidu-contact-view"
                  (app endpoint address-books card &optional select))
(declare-function chidu-dispatch "chidu-transient" ())

(defgroup chidu-contacts nil
  "JMAP Contacts presentation."
  :group 'chidu)

(defcustom chidu-contacts-page-size 64
  "Maximum ContactCards requested in one list page."
  :type '(integer :tag "Cards per page")
  :group 'chidu-contacts)

(cl-defstruct (chidu-contacts-state
               (:constructor chidu-contacts-state-create))
  "View state for one bounded ContactCard list."
  endpoint
  address-books
  address-book
  (query "")
  page
  (phase 'initial)
  message)

(defun chidu-contacts--state (&optional view)
  "Return validated Contact list state for VIEW or current view."
  (let* ((it (or view (appkit-current-surface)))
         (state (and it (appkit-surface-model it))))
    (unless (chidu-contacts-state-p state)
      (error "Chidu Contact list has invalid state"))
    state))

(defun chidu-contacts--request-sync (surface &optional structure)
  "Request native projection work for SURFACE."
  (when (appkit-surface-live-p surface)
    (chidu-post-surface-message
     surface
     (list 'chidu-refresh
           (appkit-projection-change-create
            :full-p structure :frame-p t
            :position 'preserve)))))

(defun chidu-contacts--card-summary (card)
  "Return concise secondary text for Contact CARD."
  (let ((email (chidu-contact-card-primary-email card))
        (phone (chidu-contact-card-primary-phone card))
        (organization
         (and (> (length (chidu-contact-card-organizations card)) 0)
              (aref (chidu-contact-card-organizations card) 0))))
    (string-join
     (delq nil
           (list
            (and email (chidu-contact-value-value email))
            (and phone (chidu-contact-value-value phone))
            (and organization (chidu-contact-value-value organization))))
     " · ")))

(defun chidu-contacts--row-model (card)
  "Return Appkit one-line presentation for Contact CARD."
  (let ((kind (chidu-contact-card-kind card)))
    (appkit-presentation-one-line-row-create
     :context (chidu-contact-card-display-name card)
     :context-trail (and (equal kind "group") "group")
     :context-trail-face 'shadow
     :preview
     (appkit-ui-one-line-preview-create
      :text (chidu-contacts--card-summary card))
     :line-properties
     (list 'chidu-contact-card-id (chidu-contact-card-remote-id card))
     :help-echo (format "%s ContactCard"
                        (capitalize (or kind "individual"))))))

(defun chidu-contacts--print-row (projection-row)
  "Insert one ContactCard PROJECTION-ROW."
  (appkit-presentation-insert-one-line-row
   (chidu-contacts--row-model
    (appkit-projection-row-payload projection-row))
   :indent 1
   :width (or (appkit-surface-responsive-width (appkit-current-surface) 1) fill-column 100)
   :icon-slot-width 0
   :context-width-spec '(0.36 18 44)
   :time-slot-width 0))

(defun chidu-contacts--cards (state)
  "Return ContactCards committed in list STATE."
  (if-let* ((page (chidu-contacts-state-page state)))
      (chidu-contact-page-cards page)
    (vector)))

(defun chidu-contacts--project-rows (state)
  "Project ContactCard rows from STATE."
  (appkit-projection-project
   (append (chidu-contacts--cards state) nil)
   (lambda (card) (list 'contact (chidu-contact-card-remote-id card)))))

(defun chidu-contacts--header (state)
  "Return generated header for Contact list STATE."
  (let ((book (chidu-contacts-state-address-book state))
        (query (chidu-contacts-state-query state)))
    (concat
     (propertize
      (chidu-address-book-name book)
      'face '(:height 1.2 :weight bold))
     (unless (string-empty-p query)
       (propertize (format "  ·  %s" query) 'face 'shadow))
     "\n\n")))

(defun chidu-contacts--footer (state)
  "Return generated footer for Contact list STATE."
  (let* ((page (chidu-contacts-state-page state))
         (count (length (chidu-contacts--cards state)))
         (total (and page (chidu-contact-page-total page))))
    (concat
     "\n"
     (pcase (chidu-contacts-state-phase state)
       ((or 'initial 'loading) "Loading contacts…")
       ('loading-more "Loading more contacts…")
       ('error
        (format "Unable to load contacts: %s"
                (or (chidu-contacts-state-message state) "unknown error")))
       (_
        (cond
         ((zerop count) "No contacts.")
         ((and page (chidu-contact-page-maybe-more-p page))
          (format "%d of %d contacts · more available" count total))
         (t (format "%d contact%s" count (if (= count 1) "" "s"))))))
     "\n")))

(defun chidu-contacts--update (context model message)
  "Commit a Surface MESSAGE and its native projection request."
  (if (eq (car-safe message) 'chidu-refresh)
      (appkit-next :model model
                   :render (or (cadr message)
                               (appkit-projection-change-create
                                :full-p t :frame-p t)))
    (let ((next (chidu-surface-update context model message)))
      (when (and (appkit-next-p next)
                 (eq t (appkit-next-render next)))
        (setf (appkit-next-render next)
              (appkit-projection-change-create :full-p t :frame-p t)))
      next)))

(defun chidu-contacts--append-page (old-page page)
  "Return OLD-PAGE with stable PAGE appended.

Signal when PAGE cannot be proven to continue the same ordered result set."
  (unless (and
           (equal (chidu-contact-page-query-state old-page)
                  (chidu-contact-page-query-state page))
           (equal (chidu-contact-page-query old-page)
                  (chidu-contact-page-query page))
           (= (chidu-contact-page-total old-page)
              (chidu-contact-page-total page)))
    (user-error "Contacts changed; refresh before loading more"))
  (let ((expected-position (chidu-contact-page-next-position old-page))
        (seen (make-hash-table :test #'equal)))
    (unless (= expected-position (chidu-contact-page-position page))
      (user-error "Contact page position changed; refresh the list"))
    (cl-loop for card across (chidu-contact-page-cards old-page)
             do (puthash (chidu-contact-card-remote-id card) t seen))
    (cl-loop for card across (chidu-contact-page-cards page)
             for id = (chidu-contact-card-remote-id card)
             do
             (when (gethash id seen)
               (user-error "Contact page overlaps an already loaded card"))
             (puthash id t seen))
    (chidu-contact-page-with
     old-page
     :total (chidu-contact-page-total page)
     :next-position (chidu-contact-page-next-position page)
     :cards
     (vconcat (chidu-contact-page-cards old-page)
              (chidu-contact-page-cards page))
     :maybe-more-p (chidu-contact-page-maybe-more-p page)
     :anchor-id (chidu-contact-page-anchor-id page))))

(defun chidu-contacts--loaded (view state append-p page)
  "Install Contact PAGE in VIEW STATE, appending when APPEND-P."
  (condition-case error-data
      (setf
       (chidu-contacts-state-page state)
       (if append-p
           (chidu-contacts--append-page
            (or (chidu-contacts-state-page state)
                (error "Cannot append before the first Contact page"))
            page)
         page)
       (chidu-contacts-state-phase state) 'idle
       (chidu-contacts-state-message state) nil)
    (error
     (setf (chidu-contacts-state-phase state) 'error
           (chidu-contacts-state-message state)
           (error-message-string error-data))))
  (chidu-contacts--request-sync view t))

(defun chidu-contacts--failed (view state failure)
  "Install Contact page FAILURE in VIEW STATE."
  (setf
   (chidu-contacts-state-phase state) 'error
   (chidu-contacts-state-message state)
   (if (and (chidu-result-failure-p failure)
            (eq 'contact-anchor-not-found
                (chidu-result-failure-kind failure)))
       "Contacts changed; refresh before loading more"
     (chidu-runtime-error-message failure)))
  (chidu-contacts--request-sync view))

(defun chidu-contacts--page-limit (state)
  "Return bounded ContactCard page limit for list STATE."
  (unless (and (integerp chidu-contacts-page-size)
               (> chidu-contacts-page-size 0))
    (user-error "Chidu Contacts page size must be positive"))
  (min 256
       chidu-contacts-page-size
       (or
        (chidu-store-endpoint-max-objects-in-get
         (chidu-contacts-state-endpoint state))
        256)))

(defun chidu-contacts--load (view append-p)
  "Load Contact list VIEW, appending when APPEND-P."
  (let* ((state (chidu-contacts--state view))
         (page (chidu-contacts-state-page state))
         (anchor-id (and append-p page (chidu-contact-page-anchor-id page))))
    (when (and append-p
               (or (null page)
                   (not (chidu-contact-page-maybe-more-p page))))
      (user-error "No more contacts are available"))
    (setf (chidu-contacts-state-phase state)
          (if append-p 'loading-more 'loading)
          (chidu-contacts-state-message state) nil)
    (chidu-contacts--request-sync view)
    (chidu-surface-operation-start
     view 'contact-page
     (lambda (runtime success-function error-function)
       (chidu-contact-query-page
        runtime
        (chidu-contacts-state-endpoint state)
        (chidu-contacts-state-address-book state)
        (chidu-contacts-state-query state)
        (chidu-contacts--page-limit state)
        anchor-id
        success-function error-function))
     (apply-partially #'chidu-contacts--loaded view state append-p)
     (apply-partially #'chidu-contacts--failed view state))))

(defun chidu-contacts-refresh (&optional view)
  "Refresh Contact list VIEW from its exact JMAP query."
  (interactive)
  (chidu-contacts--load (or view (appkit-current-surface)) nil))

(defun chidu-contacts-load-more ()
  "Append the next ContactCard page after the current stable anchor."
  (interactive)
  (chidu-contacts--load
   (or (appkit-current-surface)
       (user-error "No live Contact list view"))
   t))

(defun chidu-contacts-search (query)
  "Replace the current Contact list filter with QUERY."
  (interactive
   (list
    (read-string
     "Search contacts: "
     (chidu-contacts-state-query (chidu-contacts--state)))))
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Contact list view")))
         (state (chidu-contacts--state view))
         (normalized (string-trim query)))
    (unless (equal normalized (chidu-contacts-state-query state))
      (setf (chidu-contacts-state-query state) normalized
            (chidu-contacts-state-page state) nil))
    (chidu-contacts-refresh view)))

(defun chidu-contacts-next ()
  "Move to the next ContactCard row."
  (interactive)
  (chidu-text-next-property-row
   'chidu-contact-card-id "No later contact"))

(defun chidu-contacts-previous ()
  "Move to the previous ContactCard row."
  (interactive)
  (chidu-text-previous-property-row
   'chidu-contact-card-id "No earlier contact"))

(defun chidu-contacts-card-at-point ()
  "Return ContactCard represented at point, or nil."
  (when-let* ((remote-id
               (or (get-text-property (point) 'chidu-contact-card-id)
                   (get-text-property
                    (line-beginning-position) 'chidu-contact-card-id))))
    (cl-find
     remote-id (chidu-contacts--cards (chidu-contacts--state))
     :key #'chidu-contact-card-remote-id :test #'equal)))

(defun chidu-contacts-open-contact ()
  "Open the ContactCard represented at point."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Contact list view")))
         (state (chidu-contacts--state view))
         (card (or (chidu-contacts-card-at-point)
                   (user-error "No contact at point"))))
    (chidu-contact-view-open
     (appkit-surface-app view)
     (chidu-contacts-state-endpoint state)
     (chidu-contacts-state-address-books state)
     card t)))

(defun chidu-contacts-compose ()
  "Compose mail to the preferred address of the ContactCard at point."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Contact list view")))
         (state (chidu-contacts--state view))
         (card (or (chidu-contacts-card-at-point)
                   (user-error "No contact at point"))))
    (chidu-compose-to-contact
     (chidu-contacts-state-endpoint state) card)))

(defvar-keymap chidu-contacts-mode-map
  :doc "Keymap for `chidu-contacts-mode'."
  :parent special-mode-map
  "?" #'chidu-dispatch
  "g" #'chidu-contacts-refresh
  "+" #'chidu-contacts-load-more
  "/" #'chidu-contacts-search
  "RET" #'chidu-contacts-open-contact
  "c" #'chidu-contacts-compose
  "n" #'chidu-contacts-next
  "p" #'chidu-contacts-previous
  "q" #'quit-window)

(define-derived-mode chidu-contacts-mode special-mode "Chidu-Contacts"
  "Browse one bounded JMAP ContactCard query."
  (setq-local truncate-lines t
              header-line-format
              '(:eval
                (when-let* ((view (appkit-current-surface))
                            (state (appkit-surface-model view))
                            ((chidu-contacts-state-p state)))
                  (format " Chidu Contacts · %s"
                          (chidu-address-book-name
                           (chidu-contacts-state-address-book state)))))))

(defun chidu-contacts--renderer (surface)
  "Create the native projection Renderer for SURFACE."
  
  (appkit-projection-renderer-create
   :project-all (lambda (_surface _app model)
                  (chidu-contacts--project-rows model))
   :project-frame (lambda (_surface _app model)
                    (cons (chidu-contacts--header model) (chidu-contacts--footer model)))
   :printer (lambda (_surface _app row) (chidu-contacts--print-row row))
   :anchor-property 'chidu-contact-card-id
   :no-separator-p t))

(defun chidu-contacts-open-list
    (app endpoint address-books address-book &optional select)
  "Open ADDRESS-BOOK ContactCards in APP.

ENDPOINT supplies the Contacts Account.  ADDRESS-BOOKS is the containing
AddressBook snapshot used by contact detail views."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-contact-endpoint-p endpoint)
    (user-error "Endpoint does not expose JMAP Contacts"))
  (unless (chidu-address-book-directory-p address-books)
    (signal 'wrong-type-argument
            (list 'chidu-address-book-directory-p address-books)))
  (unless (chidu-address-book-p address-book)
    (signal 'wrong-type-argument (list 'chidu-address-book-p address-book)))
  (let* ((view-id
          (list 'contacts
                (chidu-store-endpoint-endpoint-id endpoint)
                (chidu-address-book-remote-id address-book)))
         (existing (appkit-app-surface app view-id))
         (state
          (if existing
              (chidu-contacts--state existing)
            (chidu-contacts-state-create
             :endpoint endpoint
             :address-books address-books
             :address-book address-book)))
         (view
          (or existing
              (appkit-open-generated-surface
               (appkit-surface-type-create
                :name 'chidu-contacts
                :mode #'chidu-contacts-mode
                :init (lambda (_context input)
                        (appkit-next :model input
                                     :render (appkit-projection-change-create :full-p t :frame-p t)))
                :update #'chidu-contacts--update
                :renderer-factory #'chidu-contacts--renderer)
               :app app :identity view-id
               :buffer-name (format "*Chidu Contacts: %s*" (chidu-address-book-name address-book))
               :input state))))
    (unless existing
      (with-current-buffer (appkit-surface-buffer view)
        (appkit-surface-enable-responsive-geometry
         view
         (lambda (owner _width)
           (chidu-post-surface-message
            owner (list 'chidu-refresh
                        (appkit-projection-change-create
                         :geometry-p t :frame-p t)))))
        (chidu-contacts-refresh view)))
    (when select (pop-to-buffer (appkit-surface-buffer view)))
    (when existing
      (with-current-buffer (appkit-surface-buffer view)
        (setf (chidu-contacts-state-endpoint state) endpoint
              (chidu-contacts-state-address-books state) address-books
              (chidu-contacts-state-address-book state) address-book)
        (chidu-contacts-refresh view)))
    (appkit-surface-buffer view)))

(provide 'chidu-contacts)

;;; chidu-contacts.el ends here
