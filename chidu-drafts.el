;;; chidu-drafts.el --- Canonical server Drafts view -*- lexical-binding: t; -*-

;;; Commentary:

;; A bounded local-first view of canonical server Draft Emails.  Opening a row
;; resumes an existing Compose workspace or performs one exact JMAP checkout;
;; rendering and navigation never cross the network.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'chidu-draft-checkout)
(require 'chidu-runtime)
(require 'chidu-store)
(require 'chidu-text)
(require 'chidu-surface-operation)
(declare-function chidu-compose-open-context
                  "chidu-compose" (app context))
(declare-function chidu-dispatch "chidu-transient" ())

(defcustom chidu-drafts-page-size 50
  "Number of canonical server Drafts revealed by one page."
  :type 'positive-integer
  :group 'chidu)

(cl-defstruct (chidu-drafts-state
               (:constructor chidu-drafts-state-create))
  "View-local state for one canonical Drafts Mailbox."
  account
  mailbox
  context
  (limit chidu-drafts-page-size)
  (phase 'initial)
  message)

(defun chidu-drafts--state (&optional view)
  "Return validated Drafts state for VIEW or current view."
  (let* ((it (or view (appkit-current-surface)))
         (state (and it (appkit-surface-model it))))
    (unless (chidu-drafts-state-p state)
      (error "Chidu Drafts view has invalid state"))
    state))

(defun chidu-drafts--rows (state)
  "Return canonical Draft rows committed in STATE."
  (if-let* ((context (chidu-drafts-state-context state)))
      (chidu-store-drafts-context-rows context)
    (vector)))

(defun chidu-drafts--address-label (address)
  "Return concise label for recipient ADDRESS."
  (or (and-let* ((name (chidu-store-email-address-name address))
                 ((not (string-empty-p name))))
        name)
      (and-let* ((email (chidu-store-email-address-email address))
                 ((not (string-empty-p email))))
        email)
      "(incomplete recipient)"))

(defun chidu-drafts--recipient-label (row)
  "Return concise recipient label for Draft ROW."
  (let* ((recipients (chidu-store-draft-row-recipients row))
         (count (length recipients))
         (first (and (> count 0)
                     (chidu-drafts--address-label (aref recipients 0))))
         (second (and (> count 1)
                      (chidu-drafts--address-label (aref recipients 1)))))
    (pcase count
      (0 "(no recipients)")
      (1 first)
      (2 (format "%s, %s" first second))
      (_ (format "%s, %s +%d" first second (- count 2))))))

(defun chidu-drafts--status (row)
  "Return local checkout status text for Draft ROW, or nil."
  (pcase (chidu-store-draft-row-publish-phase row)
    ('pending "saving")
    ('unknown "save outcome unknown")
    (_
     (when (chidu-store-draft-row-workspace-id row)
       (if (> (or (chidu-store-draft-row-workspace-revision row) 0)
              (or (chidu-store-draft-row-published-revision row) 0))
           "local changes"
         "checked out")))))

(defun chidu-drafts--row-model (row)
  "Return Appkit one-line presentation for Draft ROW."
  (let* ((summary (chidu-store-draft-row-summary-row row))
         (status (chidu-drafts--status row))
         (subject (chidu-email-subject summary)))
    (appkit-presentation-one-line-row-create
     :context (chidu-drafts--recipient-label row)
     :context-trail status
     :context-trail-face
     (and status
          (if (memq (chidu-store-draft-row-publish-phase row)
                    '(pending unknown))
              'warning
            'shadow))
     :preview
     (appkit-ui-one-line-preview-create
      :label subject
      :separator " —"
      :text (chidu-store-email-summary-row-preview summary))
     :time
     (chidu-email-format-time
      (chidu-store-email-summary-row-received-at summary))
     :time-face 'shadow
     :line-properties
     (list 'chidu-draft-email-id
           (chidu-store-email-summary-row-local-email-id summary))
     :mouse-face 'highlight)))

(defun chidu-drafts--print-row (projection-row)
  "Insert one Draft PROJECTION-ROW."
  (appkit-presentation-insert-one-line-row
   (chidu-drafts--row-model
    (appkit-projection-row-payload projection-row))
   :indent 1
   :width (or (appkit-surface-responsive-width (appkit-current-surface) 1) fill-column 100)
   :icon-slot-width 0
   :context-width-spec '(0.32 18 42)
   :time-slot-width 11))

(defun chidu-drafts--project-rows (state)
  "Project canonical Draft rows from STATE."
  (appkit-projection-project
   (append (chidu-drafts--rows state) nil)
   (lambda (row)
     (list 'draft
           (chidu-store-email-summary-row-local-email-id
            (chidu-store-draft-row-summary-row row))))))

(defun chidu-drafts--header (state)
  "Return generated header for Drafts STATE."
  (concat
   (propertize
    (format "%s · %s"
            (chidu-store-account-name (chidu-drafts-state-account state))
            (chidu-store-mailbox-name (chidu-drafts-state-mailbox state)))
    'face '(:height 1.2 :weight bold))
   "\n\n"))

(defun chidu-drafts--footer (state)
  "Return generated footer for Drafts STATE."
  (let* ((context (chidu-drafts-state-context state))
         (count (length (chidu-drafts--rows state))))
    (concat
     "\n"
     (pcase (chidu-drafts-state-phase state)
       ((or 'initial 'loading) "Loading canonical Drafts…")
       ('reloading "Reloading canonical Drafts…")
       ('loading-more "Loading older Drafts…")
       ('checking-out
        (or (chidu-drafts-state-message state) "Checking out Draft…"))
       ('error
        (format "Unable to load Drafts: %s"
                (or (chidu-drafts-state-message state) "unknown error")))
       (_
        (cond
         ((zerop count) "No server Drafts.")
         ((and context
               (chidu-store-drafts-context-maybe-more-p context))
          (format "%d Draft%s · more available"
                  count (if (= count 1) "" "s")))
         (t
          (format "%d Draft%s" count (if (= count 1) "" "s"))))))
     "\n")))

(defun chidu-drafts--update (context model message)
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

(defun chidu-drafts--request-sync (surface &optional structure)
  "Request native projection work for SURFACE."
  (when (appkit-surface-live-p surface)
    (chidu-post-surface-message
     surface
     (list 'chidu-refresh
           (appkit-projection-change-create
            :full-p structure :frame-p t
            :position 'preserve)))))

(defun chidu-drafts--loaded (view state limit context)
  "Install Drafts CONTEXT in VIEW STATE at LIMIT."
  (setf (chidu-drafts-state-context state) context
        (chidu-drafts-state-limit state) limit
        (chidu-drafts-state-phase state) 'idle
        (chidu-drafts-state-message state) nil)
  (chidu-drafts--request-sync view t))

(defun chidu-drafts--failed (view state failure)
  "Install Drafts FAILURE in VIEW STATE."
  (setf (chidu-drafts-state-phase state) 'error
        (chidu-drafts-state-message state)
        (chidu-runtime-error-message failure))
  (chidu-drafts--request-sync view))

(defun chidu-drafts--load (view phase limit)
  "Load canonical Drafts VIEW with PHASE and LIMIT."
  (let ((state (chidu-drafts--state view)))
    (setf (chidu-drafts-state-phase state) phase
          (chidu-drafts-state-message state) nil)
    (chidu-drafts--request-sync view)
    (chidu-surface-operation-start
     view 'drafts
     (lambda (runtime success-function error-function)
       (chidu-runtime-drafts
        runtime
        (chidu-drafts-state-account state)
        (chidu-drafts-state-mailbox state)
        limit success-function error-function))
     (apply-partially #'chidu-drafts--loaded view state limit)
     (apply-partially #'chidu-drafts--failed view state))))

(defun chidu-drafts-refresh (&optional view)
  "Reload canonical local Drafts in VIEW."
  (interactive)
  (let* ((it (or view (appkit-current-surface)))
         (state (chidu-drafts--state it)))
    (chidu-drafts--load it 'reloading (chidu-drafts-state-limit state))))

(defun chidu-drafts-load-more (&optional view)
  "Reveal the next older canonical Drafts page in VIEW."
  (interactive)
  (let* ((it (or view (appkit-current-surface)))
         (state (chidu-drafts--state it))
         (context (chidu-drafts-state-context state)))
    (unless (and context
                 (chidu-store-drafts-context-maybe-more-p context))
      (user-error "No older Drafts page is available"))
    (chidu-drafts--load
     it 'loading-more
     (+ (chidu-drafts-state-limit state) chidu-drafts-page-size))))

(defun chidu-drafts--row-at-point ()
  "Return canonical Draft row at point, or nil."
  (when-let* ((local-id
               (or (get-text-property (point) 'chidu-draft-email-id)
                   (get-text-property
                    (line-beginning-position) 'chidu-draft-email-id))))
    (cl-find
     local-id (chidu-drafts--rows (chidu-drafts--state))
     :key
     (lambda (row)
       (chidu-store-email-summary-row-local-email-id
        (chidu-store-draft-row-summary-row row)))
     :test #'equal)))

(defun chidu-drafts-next ()
  "Move to the next canonical Draft row."
  (interactive)
  (chidu-text-next-property-row
   'chidu-draft-email-id "No later Draft"))

(defun chidu-drafts-previous ()
  "Move to the previous canonical Draft row."
  (interactive)
  (chidu-text-previous-property-row
   'chidu-draft-email-id "No earlier Draft"))

(defun chidu-drafts--checkout-finished (view state context)
  "Open Compose CONTEXT after checkout in Drafts VIEW STATE."
  (setf (chidu-drafts-state-phase state) 'idle
        (chidu-drafts-state-message state) nil)
  (require 'chidu-compose)
  (chidu-compose-open-context (appkit-surface-app view) context)
  (chidu-drafts-refresh view))

(defun chidu-drafts--checkout-failed (view state failure)
  "Install checkout FAILURE in Drafts VIEW STATE."
  (setf (chidu-drafts-state-phase state) 'error
        (chidu-drafts-state-message state)
        (chidu-runtime-error-message failure))
  (chidu-drafts--request-sync view))

(defun chidu-drafts-open-draft ()
  "Resume or create the local Compose checkout for the Draft at point."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Drafts view")))
         (state (chidu-drafts--state view))
         (context (or (chidu-drafts-state-context state)
                      (user-error "Canonical Drafts are not loaded")))
         (row (or (chidu-drafts--row-at-point)
                  (user-error "No Draft at point"))))
    (setf (chidu-drafts-state-phase state) 'checking-out
          (chidu-drafts-state-message state)
          (if (chidu-store-draft-row-workspace-id row)
              "Opening local Draft checkout…"
            "Checking out server Draft…"))
    (chidu-drafts--request-sync view)
    (chidu-surface-operation-start
     view 'checkout
     (lambda (runtime success-function error-function)
       (chidu-checkout-draft
        runtime
        (chidu-store-drafts-context-endpoint context)
        (chidu-drafts-state-account state)
        (chidu-drafts-state-mailbox state)
        row success-function error-function))
     (apply-partially #'chidu-drafts--checkout-finished view state)
     (apply-partially #'chidu-drafts--checkout-failed view state))))

(defvar-keymap chidu-drafts-mode-map
  :doc "Keymap for `chidu-drafts-mode'."
  :parent special-mode-map
  "?" #'chidu-dispatch
  "g" #'chidu-drafts-refresh
  "+" #'chidu-drafts-load-more
  "RET" #'chidu-drafts-open-draft
  "n" #'chidu-drafts-next
  "p" #'chidu-drafts-previous
  "q" #'quit-window)

(define-derived-mode chidu-drafts-mode special-mode "Chidu-Drafts"
  "Browse canonical server Draft Emails."
  (setq-local truncate-lines t
              header-line-format
              '(:eval
                (when-let* ((view (appkit-current-surface))
                            (state (appkit-surface-model view))
                            ((chidu-drafts-state-p state)))
                  (format " Chidu · %s · Drafts"
                          (chidu-store-account-name
                           (chidu-drafts-state-account state)))))))

(defun chidu-drafts--renderer (surface)
  "Create the native projection Renderer for SURFACE."
  
  (appkit-projection-renderer-create
   :project-all (lambda (_surface _app model)
                  (chidu-drafts--project-rows model))
   :project-frame (lambda (_surface _app model)
                    (cons (chidu-drafts--header model) (chidu-drafts--footer model)))
   :printer (lambda (_surface _app row) (chidu-drafts--print-row row))
   :anchor-property 'chidu-draft-email-id
   :no-separator-p t))

(defun chidu-drafts-open (app account mailbox &optional select)
  "Open APP's canonical Drafts view for ACCOUNT and MAILBOX.

Select its buffer when SELECT is non-nil."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (and (chidu-store-mailbox-p mailbox)
               (equal "drafts" (chidu-store-mailbox-role mailbox)))
    (user-error "Mailbox is not the Account's Drafts Mailbox"))
  (let* ((view-id
          (list 'drafts
                (chidu-store-account-account-id account)
                (chidu-store-mailbox-mailbox-id mailbox)))
         (existing (appkit-app-surface app view-id))
         (view
          (or existing
              (appkit-open-generated-surface
               (appkit-surface-type-create
                :name 'chidu-drafts
                :mode #'chidu-drafts-mode
                :init (lambda (_context input)
                        (appkit-next :model input
                                     :render (appkit-projection-change-create :full-p t :frame-p t)))
                :update #'chidu-drafts--update
                :renderer-factory #'chidu-drafts--renderer)
               :app app :identity view-id
               :buffer-name (format "*Chidu: %s/Drafts*" (chidu-store-account-name account))
               :input (chidu-drafts-state-create :account account :mailbox mailbox)))))
    (unless existing
      (with-current-buffer (appkit-surface-buffer view)
        (appkit-surface-enable-responsive-geometry
         view
         (lambda (owner _width)
           (chidu-post-surface-message
            owner (list 'chidu-refresh
                        (appkit-projection-change-create
                         :geometry-p t :frame-p t)))))
        (chidu-drafts--load view 'loading (chidu-drafts-state-limit (chidu-drafts--state view)))))
    (when select (pop-to-buffer (appkit-surface-buffer view)))
    (when existing
      (with-current-buffer (appkit-surface-buffer view) (chidu-drafts-refresh view)))
    (appkit-surface-buffer view)))

(provide 'chidu-drafts)

;;; chidu-drafts.el ends here
