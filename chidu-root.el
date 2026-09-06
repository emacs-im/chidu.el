;;; chidu-root.el --- Appkit Account and Mailbox directory -*- lexical-binding: t; -*-

;;; Commentary:

;; Stable local-first Endpoint, Account, and Mailbox navigation.  This module
;; owns root projection state and Appkit directory rendering; lifecycle and
;; network orchestration remain in `chidu.el'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-surface)
(require 'appkit-transaction)
(require 'appkit-presentation)
(require 'chidu-store)
(require 'chidu-runtime)
(require 'chidu-surface-operation)

(declare-function chidu-refresh "chidu" ())
(declare-function chidu-restart "chidu" ())
(declare-function chidu-search-mail "chidu" (&optional account))
(declare-function chidu-drafts-open
                  "chidu-drafts" (app account mailbox &optional select))
(declare-function chidu-contacts "chidu-address-books" (&optional endpoint))
(declare-function chidu-sync-account "chidu" (&optional account))
(declare-function chidu-index-account "chidu" (&optional account))
(declare-function chidu-cancel-email-index "chidu" (&optional account))
(declare-function chidu-email-index-running-p "chidu" (&optional account))
(declare-function chidu-summary-open
                  "chidu-summary"
                  (app account mailbox &optional select))
(declare-function chidu-dispatch "chidu-transient" ())

(defconst chidu-home-buffer-name "*Chidu*"
  "Name of the Chidu root directory buffer.")

(defface chidu-home-title
  '((t :inherit variable-pitch :height 1.2 :weight bold))
  "Face for the Chidu root title."
  :group 'chidu)

(defface chidu-home-ready
  '((t :inherit success :weight semibold))
  "Face for ready Chidu state."
  :group 'chidu)

(defface chidu-home-problem
  '((t :inherit error :weight semibold))
  "Face for failed Chidu state."
  :group 'chidu)

(defface chidu-home-unread
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for unread Mailbox counts in the root directory."
  :group 'chidu)

(cl-defstruct (chidu--account-ui-state
               (:constructor chidu--account-ui-state-create))
  "Local UI projection state for one Account."
  account
  mailbox-context
  (phase 'idle)
  message)

(cl-defstruct (chidu--state (:constructor chidu--state-create))
  "Application-owned state projected by the root directory."
  phase
  store-info
  (endpoints (vector))
  (accounts (make-hash-table :test #'equal))
  message)

(defun chidu--account-id (account)
  "Return local id for ACCOUNT."
  (chidu-store-account-account-id account))

(defun chidu--account-ui-state (state account &optional create-p)
  "Return STATE's UI state for ACCOUNT; create it when CREATE-P."
  (let* ((account-id (chidu--account-id account))
         (table (chidu--state-accounts state))
         (ui (gethash account-id table)))
    (when (and (null ui) create-p)
      (setq ui (chidu--account-ui-state-create :account account))
      (puthash account-id ui table))
    (when ui
      (setf (chidu--account-ui-state-account ui) account))
    ui))

(defun chidu--current-account (app account-id)
  "Return current ACCOUNT-ID projection in APP, or nil."
  (when (appkit-app-live-p app)
    (cl-loop
     for endpoint across
     (chidu--state-endpoints (chidu-app-state app))
     thereis
     (cl-find account-id
              (chidu-store-endpoint-accounts endpoint)
              :key #'chidu-store-account-account-id :test #'equal))))

(defun chidu--invalidate-home (app &optional _structure-p)
  "Request a committed refresh of APP's generated root."
  (when (appkit-app-live-p app)
    (when-let* ((surface (appkit-app-surface app 'home)))
      (chidu-surface-refresh surface))))

(defun chidu--set-state (app state)
  "Commit APP's domain STATE; its accepted update refreshes the root."
  (when (appkit-app-live-p app)
    (chidu-post-app-message app (list :state app state)))
  state)

(defun chidu--replace-endpoint (endpoints replacement)
  "Return ENDPOINTS with REPLACEMENT inserted by stable endpoint id."
  (let ((found nil)
        rows)
    (cl-loop
     for endpoint across (or endpoints (vector))
     do
     (if (equal (chidu-store-endpoint-endpoint-id endpoint)
                (chidu-store-endpoint-endpoint-id replacement))
         (progn
           (push replacement rows)
           (setq found t))
       (push endpoint rows)))
    (unless found (push replacement rows))
    (vconcat (nreverse rows))))

(defun chidu--reconcile-account-ui-state (state)
  "Reconcile STATE's Account UI table with its Endpoint projection."
  (let ((present (make-hash-table :test #'equal))
        (table (chidu--state-accounts state)))
    (cl-loop
     for endpoint across (chidu--state-endpoints state)
     do
     (cl-loop
      for account across (chidu-store-endpoint-accounts endpoint)
      do
      (puthash (chidu--account-id account) t present)
      (chidu--account-ui-state state account t)))
    (let (stale)
      (maphash
       (lambda (account-id _ui)
         (unless (gethash account-id present) (push account-id stale)))
       table)
      (dolist (account-id stale) (remhash account-id table))))
  state)

(defun chidu--set-endpoints (app endpoints &optional message-text)
  "Set APP's ENDPOINTS and optional problem MESSAGE-TEXT."
  (let ((state (chidu-app-state app)))
    (setf (chidu--state-endpoints state) endpoints
          (chidu--state-message state) message-text)
    (chidu--reconcile-account-ui-state state)
    (chidu--set-state app state)))

(defun chidu--endpoint-label (endpoint)
  "Return concise label for ENDPOINT."
  (let* ((url (url-generic-parse-url
               (chidu-store-endpoint-session-url endpoint)))
         (host (or (url-host url) "JMAP")))
    (format "%s · %s" (chidu-store-endpoint-login endpoint) host)))

(defun chidu--endpoint-status (endpoint)
  "Return concise connection status for ENDPOINT."
  (if (chidu-store-endpoint-session-state endpoint)
      (format "%d account%s"
              (cl-count-if #'chidu-store-account-available-p
                           (chidu-store-endpoint-accounts endpoint))
              (if (= 1 (cl-count-if #'chidu-store-account-available-p
                                    (chidu-store-endpoint-accounts endpoint)))
                  "" "s"))
    "local configuration"))

(defun chidu--account-status (ui)
  "Return concise trailing status for Account UI."
  (let* ((mailbox-context (chidu--account-ui-state-mailbox-context ui))
         (mailboxes
          (and mailbox-context
               (chidu-store-mailbox-sync-context-mailboxes mailbox-context)))
         (available
          (and mailboxes
               (cl-count-if #'chidu-store-mailbox-available-p mailboxes)))
         (phase (chidu--account-ui-state-phase ui))
         (problem (chidu--account-ui-state-message ui)))
    (cond
     (problem (format "error: %s" problem))
     ((not (eq phase 'idle))
      (pcase phase
        ('loading-local "loading local state")
        ('syncing-mailboxes "syncing Mailboxes")
        ('indexing-email "building Email index")
        (_ (format "%s" phase))))
     (available
      (format "%d mailbox%s" available (if (= available 1) "" "es")))
     (t "local state ready"))))

(defun chidu--mailbox-icon (mailbox)
  "Return textual icon for MAILBOX."
  (pcase (chidu-store-mailbox-role mailbox)
    ("inbox" "▣")
    ("drafts" "✎")
    ("sent" "➤")
    ("archive" "▤")
    ("trash" "⌫")
    ("junk" "⚠")
    (_ "□")))

(defun chidu--home-insert-mailbox (_surface entry)
  "Insert Mailbox directory ENTRY."
  (let* ((payload (appkit-directory-entry-payload entry))
         (mailbox (plist-get payload :mailbox))
         (unread (chidu-store-mailbox-unread-emails mailbox))
         (total (chidu-store-mailbox-total-emails mailbox)))
    (insert (chidu--mailbox-icon mailbox) " "
            (chidu-store-mailbox-name mailbox))
    (when (> unread 0)
      (insert (propertize (format "  %d unread" unread)
                          'face 'chidu-home-unread)))
    (insert (propertize (format "  %d total" total) 'face 'shadow)
            "\n")))

(defun chidu--home-activate-mailbox (_surface entry)
  "Open Mailbox carried by directory ENTRY."
  (let* ((payload (appkit-directory-entry-payload entry))
         (account (plist-get payload :account))
         (mailbox (plist-get payload :mailbox))
         (view (or (appkit-current-surface)
                   (user-error "No live Chidu root view"))))
    (unless (and (chidu-store-account-p account)
                 (chidu-store-mailbox-p mailbox))
      (user-error "No Mailbox at point"))
    (if (equal "drafts" (chidu-store-mailbox-role mailbox))
        (progn
          (require 'chidu-drafts)
          (chidu-drafts-open (appkit-surface-app view) account mailbox t))
      (require 'chidu-summary)
      (chidu-summary-open (appkit-surface-app view) account mailbox t))))

(defun chidu--home-fold-changed (_surface _entry _expanded-p)
  "Reconcile the root after a directory fold changed."
  (when-let* ((surface (appkit-current-surface)))
    (chidu-surface-refresh surface)))

(defun chidu--home-account-entries (surface endpoint account ui)
  "Project ACCOUNT and UI below ENDPOINT for directory SURFACE."
  (let* ((endpoint-key
          (list 'endpoint (chidu-store-endpoint-endpoint-id endpoint)))
         (account-id (chidu--account-id account))
         (account-key (list 'account account-id))
         (expanded
          (appkit-directory-fold-expanded-p surface account-key t))
         (mailbox-context (chidu--account-ui-state-mailbox-context ui))
         (mailboxes
          (and mailbox-context
               (sort
                (seq-filter
                 #'chidu-store-mailbox-available-p
                 (chidu-store-mailbox-sync-context-mailboxes
                  mailbox-context))
                #'chidu-store-mailbox-less-p)))
         (entries
          (list
           (appkit-directory-entry-create
            :key account-key
            :role 'group
            :section-key endpoint-key
            :label (chidu-store-account-name account)
            :trailing (concat "  " (chidu--account-status ui))
            :face (cond
                   ((chidu--account-ui-state-message ui) 'chidu-home-problem)
                   ((not (chidu-store-account-available-p account)) 'shadow))
            :indent 2
            :foldable-p t
            :fold-key account-key
            :fold-default-expanded-p t
            :expanded-p expanded
            :payload account
            :stamp
            (list (chidu--account-status ui)
                  (chidu--account-ui-state-message ui)
                  (and mailboxes (length mailboxes)))))))
    (when expanded
      (setq entries
            (append
             entries
             (cond
              ((not (chidu-store-account-available-p account))
               (list
                (appkit-directory-entry-create
                 :key (list 'account-note account-id 'unavailable)
                 :role 'note :section-key endpoint-key :group-key account-key
                 :label "This Account is no longer available."
                 :indent 4 :face 'shadow)))
              ((null mailbox-context)
               (list
                (appkit-directory-entry-create
                 :key (list 'account-note account-id 'loading)
                 :role 'note :section-key endpoint-key :group-key account-key
                 :label (or (chidu--account-ui-state-message ui)
                            "Loading local Mailboxes…")
                 :indent 4
                 :face (and (chidu--account-ui-state-message ui) 'error))))
              ((null mailboxes)
               (list
                (appkit-directory-entry-create
                 :key (list 'account-note account-id 'empty)
                 :role 'note :section-key endpoint-key :group-key account-key
                 :label "No locally committed Mailboxes."
                 :indent 4 :face 'shadow)))
              (t
               (mapcar
                (lambda (mailbox)
                  (appkit-directory-entry-create
                   :key (list 'mailbox account-id
                              (chidu-store-mailbox-mailbox-id mailbox))
                   :role 'item
                   :section-key endpoint-key
                   :group-key account-key
                   :item-p t
                   :unread-p (> (chidu-store-mailbox-unread-emails mailbox) 0)
                   :payload (list :account account :mailbox mailbox)
                   :indent 4
                   :stamp
                   (list (chidu-store-mailbox-name mailbox)
                         (chidu-store-mailbox-total-emails mailbox)
                         (chidu-store-mailbox-unread-emails mailbox)
                         (chidu-store-mailbox-role mailbox))))
                mailboxes))))))
    entries))

(defun chidu--home-project-entries (state surface)
  "Project application STATE into directory entries for SURFACE."
  (let (entries)
    (pcase (chidu--state-phase state)
      ('starting
       (push
        (appkit-directory-entry-create
         :key 'starting :role 'note :label "Opening local Chidu Store…"
         :indent 1 :face 'shadow)
        entries))
      ('failed
       (push
        (appkit-directory-entry-create
         :key 'failed :role 'note
         :label (or (chidu--state-message state) "Chidu is unavailable")
         :indent 1 :face 'error)
        entries)))
    (cl-loop
     for endpoint across (chidu--state-endpoints state)
     for endpoint-key =
     (list 'endpoint (chidu-store-endpoint-endpoint-id endpoint))
     for expanded =
     (appkit-directory-fold-expanded-p surface endpoint-key t)
     do
     (setq entries
           (append
            entries
            (list
             (appkit-directory-entry-create
              :key endpoint-key
              :role 'section
              :label (chidu--endpoint-label endpoint)
              :trailing (concat "  " (chidu--endpoint-status endpoint))
              :face (and (chidu-store-endpoint-session-state endpoint)
                         'chidu-home-ready)
              :foldable-p t
              :fold-key endpoint-key
              :fold-default-expanded-p t
              :expanded-p expanded
              :stamp
              (list (chidu-store-endpoint-session-state endpoint)
                    (length (chidu-store-endpoint-accounts endpoint)))))))
     (when expanded
       (cl-loop
        for account across (chidu-store-endpoint-accounts endpoint)
        for ui = (chidu--account-ui-state state account t)
        do
        (setq entries
              (append entries
                      (chidu--home-account-entries
                       surface endpoint account ui))))))
    (when (and (null entries)
               (eq (chidu--state-phase state) 'ready))
      (setq entries
            (list
             (appkit-directory-entry-create
              :key 'empty :role 'note
              :label "No JMAP Endpoint configured. Set `chidu-endpoints`."
              :indent 1 :face 'shadow))))
    (when-let* ((message-text (chidu--state-message state)))
      (setq entries
            (append
             (list
              (appkit-directory-entry-create
               :key 'problem :role 'note :label message-text
               :indent 1 :face 'chidu-home-problem))
             entries)))
    entries))

(defun chidu--home-header-line ()
  "Return dynamic root header line."
  (let* ((view (appkit-current-surface))
         (app (and view (appkit-surface-app view)))
         (state (and app (chidu-app-state app)))
         (store (and state (chidu--state-store-info state))))
    (concat
     " "
     (propertize "Chidu" 'face 'chidu-home-title)
     (when store
       (format "  ·  local change %s"
               (chidu-store-runtime-change-seq store))))))

(defun chidu--home-update (context model message)
  "Commit root Surface messages."
  (chidu-surface-update context model message))

(defun chidu--home-renderer (_surface)
  "Create the generated root directory Renderer."
  (appkit-generated-renderer-create
   :mount #'ignore
   :merge (lambda (_old new) new)
   :render
   (lambda (surface app-read-view _model _request)
     (appkit-with-content-update surface
       (appkit-directory-reconcile
        (appkit-directory-surface)
        (chidu--home-project-entries
         (chidu-app-model-state
          (appkit-app-read-view-model app-read-view))
         (appkit-directory-surface))))
     (force-mode-line-update)
     nil)
   :unmount
   (lambda (surface)
     (when (buffer-live-p (appkit-surface-buffer surface))
       (with-current-buffer (appkit-surface-buffer surface)
         (appkit-directory-retire))))))

(defun chidu--open-home (app &optional select)
  "Open APP's real generated root and optionally SELECT its buffer."
  (let ((surface
         (or (appkit-app-surface app 'home)
             (appkit-open-generated-surface
              (appkit-surface-type-create
               :name 'chidu-home :mode #'chidu-home-mode
               :init (lambda (_context input)
                       (appkit-next :model input :render t))
               :update #'chidu--home-update
               :renderer-factory #'chidu--home-renderer)
              :app app :identity 'home :input nil
              :buffer-name chidu-home-buffer-name))))
    (when select (pop-to-buffer (appkit-surface-buffer surface)))
    surface))

(defun chidu-home-endpoint-at-point ()
  "Return Endpoint owning the root entry at point, or nil."
  (when-let* ((entry (appkit-directory-entry-at-point))
              (key
               (or (and (eq 'section (appkit-directory-entry-role entry))
                        (appkit-directory-entry-key entry))
                   (appkit-directory-entry-section-key entry)))
              ((and (listp key) (eq 'endpoint (car key))))
              (view (appkit-current-surface))
              (state (and view (chidu-app-state (appkit-surface-app view)))))
    (cl-find
     (cadr key)
     (chidu--state-endpoints state)
     :key #'chidu-store-endpoint-endpoint-id
     :test #'equal)))

(defun chidu-home-account-at-point ()
  "Return Account represented by the root entry at point, or nil."
  (when-let* ((entry (appkit-directory-entry-at-point)))
    (let ((payload (appkit-directory-entry-payload entry)))
      (cond
       ((chidu-store-account-p payload) payload)
       ((and (listp payload)
             (chidu-store-account-p (plist-get payload :account)))
        (plist-get payload :account))))))

(defun chidu-home-sync-account ()
  "Synchronize Mailboxes for the Account represented at point."
  (interactive)
  (chidu-sync-account
   (or (chidu-home-account-at-point)
       (user-error "No Account at point"))))

(defun chidu-home-index-account ()
  "Build the canonical Email index for the Account represented at point."
  (interactive)
  (chidu-index-account
   (or (chidu-home-account-at-point)
       (user-error "No Account at point"))))

(defun chidu-home-cancel-email-index ()
  "Cancel the Email index operation for the Account represented at point."
  (interactive)
  (chidu-cancel-email-index
   (or (chidu-home-account-at-point)
       (user-error "No Account at point"))))

(cl-defun chidu--set-account-ui
    (app account &key mailbox-context phase message-text)
  "Update ACCOUNT's local UI projection in APP."
  (when (appkit-app-live-p app)
    (let* ((state (chidu-app-state app))
           (ui (chidu--account-ui-state state account t)))
      (when mailbox-context
        (setf (chidu--account-ui-state-mailbox-context ui) mailbox-context))
      (when phase (setf (chidu--account-ui-state-phase ui) phase))
      (setf (chidu--account-ui-state-message ui) message-text)
      (chidu--set-state app state)
      ui)))

(defvar-keymap chidu-home-mode-map
  :doc "Keymap for `chidu-home-mode'."
  :parent appkit-directory-mode-map
  "?" #'chidu-dispatch
  "g" #'chidu-refresh
  "G" #'chidu-restart
  "s" #'chidu-home-sync-account
  "S" #'chidu-search-mail
  "A" #'chidu-contacts)

(define-derived-mode chidu-home-mode appkit-directory-mode "Chidu"
  "Major mode for the Chidu Account and Mailbox directory."
  (setq-local header-line-format '(:eval (chidu--home-header-line)))
  (appkit-directory-configure
   (appkit-directory-surface)
   :item-inserter #'chidu--home-insert-mailbox
   :activate-function #'chidu--home-activate-mailbox
   :fold-function #'chidu--home-fold-changed))

(provide 'chidu-root)

;;; chidu-root.el ends here
