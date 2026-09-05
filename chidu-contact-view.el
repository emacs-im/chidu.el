;;; chidu-contact-view.el --- Read-only JSContact detail view -*- lexical-binding: t; -*-

;;; Commentary:

;; Read one complete ContactCard projection on demand and present selected
;; JSContact fields without pretending the projection is an editable full Card.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-presentation)
(require 'chidu-contact)
(require 'chidu-contact-model)
(require 'chidu-runtime)
(require 'chidu-view-operation)
(require 'chidu-store)

(declare-function chidu-compose-to-contact "chidu-compose" (endpoint card))
(declare-function chidu-dispatch "chidu-transient" ())

(cl-defstruct (chidu-contact-view-state
               (:constructor chidu-contact-view-state-create))
  "State for one ContactCard detail view."
  endpoint
  address-books
  summary
  card
  (phase 'initial)
  message)

(defface chidu-contact-view-name
  '((t :inherit variable-pitch :height 1.35 :weight bold))
  "Face for the primary ContactCard name."
  :group 'chidu-contacts)

(defface chidu-contact-view-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for ContactCard section headings."
  :group 'chidu-contacts)

(defun chidu-contact-view--state (&optional view)
  "Return validated ContactCard detail state for VIEW or current view."
  (let* ((it (or view (appkit-current-surface)))
         (state (and it (appkit-surface-model it))))
    (unless (chidu-contact-view-state-p state)
      (error "Chidu Contact detail has invalid state"))
    state))

(defun chidu-contact-view--request-sync (surface)
  "Request a committed refresh of SURFACE."
  (chidu-surface-refresh surface))

(defun chidu-contact-view--address-book-name (state remote-id)
  "Return STATE AddressBook name for REMOTE-ID, or REMOTE-ID."
  (or
   (when-let* ((directory (chidu-contact-view-state-address-books state)))
     (cl-loop
      for book across (chidu-address-book-directory-address-books directory)
      when (equal remote-id (chidu-address-book-remote-id book))
      return (chidu-address-book-name book)))
   remote-id))

(defun chidu-contact-view--insert-heading (text)
  "Insert Contact detail heading TEXT."
  (insert (propertize text 'face 'chidu-contact-view-heading) "\n"))

(defun chidu-contact-view--insert-line (value &optional metadata)
  "Insert one Contact detail VALUE and optional METADATA."
  (insert "  " value)
  (when (and metadata (not (string-empty-p metadata)))
    (insert (propertize (concat "  " metadata) 'face 'shadow)))
  (insert "\n"))

(defun chidu-contact-view--insert-values (heading values)
  "Insert HEADING and contact VALUES when non-empty."
  (when (> (length values) 0)
    (chidu-contact-view--insert-heading heading)
    (cl-loop
     for value across values
     do
     (chidu-contact-view--insert-line
      (chidu-contact-value-value value)
      (chidu-contact-value-metadata value)))
    (insert "\n")))

(defun chidu-contact-view--insert-notes (notes)
  "Insert ContactCard NOTES with preserved line breaks."
  (when (> (length notes) 0)
    (chidu-contact-view--insert-heading "Notes")
    (cl-loop
     for note across notes
     for text = (chidu-contact-value-value note)
     do
     (let ((start (point)))
       (insert text)
       (unless (bolp) (insert "\n"))
       (add-text-properties
        start (point)
        '(line-prefix "  " wrap-prefix "    "))))
    (insert "\n")))

(defun chidu-contact-view--insert-members (members)
  "Insert group member UIDs from MEMBERS."
  (when (> (length members) 0)
    (chidu-contact-view--insert-heading "Group Members")
    (cl-loop for uid across members
             do (chidu-contact-view--insert-line uid))
    (insert "\n")))

(defun chidu-contact-view--insert-address-books (state card)
  "Insert CARD AddressBook memberships using STATE names."
  (let ((ids (chidu-contact-card-address-book-ids card)))
    (when (> (length ids) 0)
      (chidu-contact-view--insert-heading "Address Books")
      (cl-loop for id across ids
               do
               (chidu-contact-view--insert-line
                (chidu-contact-view--address-book-name state id)))
      (insert "\n"))))

(defun chidu-contact-view--insert-dates (card)
  "Insert CARD created and updated dates."
  (let ((created (chidu-contact-card-created card))
        (updated (chidu-contact-card-updated card)))
    (when (or created updated)
      (chidu-contact-view--insert-heading "Dates")
      (when created (chidu-contact-view--insert-line created "created"))
      (when updated (chidu-contact-view--insert-line updated "updated"))
      (insert "\n"))))

(defun chidu-contact-view--insert-card (state card)
  "Insert complete Contact CARD using STATE context."
  (insert
   (propertize (chidu-contact-card-display-name card)
               'face 'chidu-contact-view-name)
   "\n")
  (insert
   (propertize
    (format "%s ContactCard · %s"
            (capitalize (or (chidu-contact-card-kind card) "individual"))
            (chidu-contact-card-uid card))
    'face 'shadow)
   "\n\n")
  (chidu-contact-view--insert-values
   "Email" (chidu-contact-card-emails card))
  (chidu-contact-view--insert-values
   "Phone" (chidu-contact-card-phones card))
  (chidu-contact-view--insert-values
   "Organization" (chidu-contact-card-organizations card))
  (chidu-contact-view--insert-values
   "Title" (chidu-contact-card-titles card))
  (chidu-contact-view--insert-values
   "Address" (chidu-contact-card-addresses card))
  (chidu-contact-view--insert-values
   "Online" (chidu-contact-card-online-services card))
  (chidu-contact-view--insert-notes (chidu-contact-card-notes card))
  (chidu-contact-view--insert-members (chidu-contact-card-members card))
  (chidu-contact-view--insert-address-books state card)
  (chidu-contact-view--insert-dates card))

(defun chidu-contact-view--render (view state)
  "Render Contact detail VIEW from STATE while preserving position."
  (appkit-position-render-preserving
   (lambda ()
     (appkit-with-content-update view
       (erase-buffer)
       (pcase (chidu-contact-view-state-phase state)
         ((or 'initial 'loading)
          (insert (propertize "Loading contact…" 'face 'shadow) "\n"))
         ('error
          (insert
           (propertize
            (format "Unable to load contact: %s"
                    (or (chidu-contact-view-state-message state)
                        "unknown error"))
            'face 'error)
           "\n"))
         (_
          (if-let* ((card (chidu-contact-view-state-card state)))
              (chidu-contact-view--insert-card state card)
            (insert "Contact is unavailable.\n"))))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun chidu-contact-view--update (context model message)
  "Commit a Surface MESSAGE and its presentation request."
  (chidu-surface-update context model message))

(defun chidu-contact-view--loaded (view state card)
  "Install complete Contact CARD in VIEW STATE."
  (setf (chidu-contact-view-state-card state) card
        (chidu-contact-view-state-phase state) 'idle
        (chidu-contact-view-state-message state) nil)
  (chidu-contact-view--request-sync view))

(defun chidu-contact-view--failed (view state failure)
  "Install Contact detail FAILURE in VIEW STATE."
  (setf (chidu-contact-view-state-phase state) 'error
        (chidu-contact-view-state-message state)
        (chidu-runtime-error-message failure))
  (chidu-contact-view--request-sync view))

(defun chidu-contact-view-refresh (&optional view)
  "Refresh ContactCard detail in VIEW."
  (interactive)
  (let* ((it (or view (appkit-current-surface)))
         (state (chidu-contact-view--state it))
         (summary (chidu-contact-view-state-summary state))
         (remote-id (chidu-contact-card-remote-id summary)))
    (setf (chidu-contact-view-state-phase state) 'loading
          (chidu-contact-view-state-message state) nil)
    (chidu-contact-view--request-sync it)
    (chidu-view-operation-start
     it 'contact-detail
     (lambda (runtime success-function error-function)
       (chidu-contact-get-detail
        runtime
        (chidu-contact-view-state-endpoint state)
        remote-id
        success-function error-function))
     (apply-partially #'chidu-contact-view--loaded it state)
     (apply-partially #'chidu-contact-view--failed it state))))

(defun chidu-contact-view-compose ()
  "Compose mail to the preferred address of the current ContactCard."
  (interactive)
  (let* ((state (chidu-contact-view--state))
         (card (or (chidu-contact-view-state-card state)
                   (chidu-contact-view-state-summary state))))
    (chidu-compose-to-contact
     (chidu-contact-view-state-endpoint state) card)))

(defvar-keymap chidu-contact-view-mode-map
  :doc "Keymap for `chidu-contact-view-mode'."
  :parent special-mode-map
  "?" #'chidu-dispatch
  "g" #'chidu-contact-view-refresh
  "c" #'chidu-contact-view-compose
  "q" #'quit-window)

(define-derived-mode chidu-contact-view-mode special-mode
  "Chidu-Contact"
  "Read one JMAP ContactCard."
  (setq-local truncate-lines nil
              word-wrap t
              header-line-format
              '(:eval
                (when-let* ((view (appkit-current-surface))
                            (state (appkit-surface-model view))
                            ((chidu-contact-view-state-p state)))
                  (format " Chidu Contact · %s"
                          (chidu-contact-card-display-name
                           (or (chidu-contact-view-state-card state)
                               (chidu-contact-view-state-summary state))))))))

(defun chidu-contact-view--renderer (_surface)
  "Create the generated Contact detail Renderer."
  (appkit-generated-renderer-create
   :mount #'ignore
   :merge (lambda (_old new) new)
   :render
   (lambda (surface _app model _request)
     (chidu-contact-view--render surface model)
     (force-mode-line-update)
     nil)
   :unmount #'ignore))

(defun chidu-contact-view-open
    (app endpoint address-books card &optional select)
  "Open Contact CARD detail in APP.

ENDPOINT owns the Contacts Account.  ADDRESS-BOOKS supplies readable membership
names.  Select the view when SELECT is non-nil."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-contact-endpoint-p endpoint)
    (user-error "Endpoint does not expose JMAP Contacts"))
  (unless (chidu-address-book-directory-p address-books)
    (signal 'wrong-type-argument
            (list 'chidu-address-book-directory-p address-books)))
  (unless (chidu-contact-card-p card)
    (signal 'wrong-type-argument (list 'chidu-contact-card-p card)))
  (let* ((view-id
          (list 'contact
                (chidu-store-endpoint-endpoint-id endpoint)
                (chidu-contact-card-remote-id card)))
         (existing (appkit-app-surface app view-id))
         (state
          (if existing
              (chidu-contact-view--state existing)
            (chidu-contact-view-state-create
             :endpoint endpoint :address-books address-books :summary card)))
         (view
          (or existing
              (appkit-open-generated-surface
               (appkit-surface-type-create
                :name 'chidu-contact-view
                :mode #'chidu-contact-view-mode
                :init (lambda (_context input)
                        (appkit-next :model input
                                     :render t))
                :update #'chidu-contact-view--update
                :renderer-factory #'chidu-contact-view--renderer)
               :app app :identity view-id
               :buffer-name (format "*Chidu Contact: %s*" (chidu-contact-card-display-name card))
               :input state))))
    (unless existing
      (with-current-buffer (appkit-surface-buffer view)
        (chidu-contact-view-refresh view)))
    (when select (pop-to-buffer (appkit-surface-buffer view)))
    (when existing
      (with-current-buffer (appkit-surface-buffer view)
        (setf (chidu-contact-view-state-endpoint state) endpoint
              (chidu-contact-view-state-address-books state) address-books
              (chidu-contact-view-state-summary state) card)
        (chidu-contact-view-refresh view)))
    (appkit-surface-buffer view)))

(provide 'chidu-contact-view)

;;; chidu-contact-view.el ends here
