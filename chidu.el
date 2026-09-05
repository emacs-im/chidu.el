;;; chidu.el --- JMAP-first mail client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Keywords: mail, comm
;; URL: https://github.com/emacs-im/chidu.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (appkit "0.2.19") (plz "0.9.1") (transient "0.7.0"))

;;; Commentary:

;; Chidu is an ordinary Emacs Lisp application.  Its package entry owns
;; lifecycle and user commands; `chidu-root' and `chidu-summary' own the
;; Appkit interface.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-surface)
(require 'chidu-root)
(require 'chidu-runtime)
(require 'chidu-view-operation)
(require 'chidu-mailbox-sync)
(require 'chidu-store)

(declare-function chidu-attachment-app-update
                  "chidu-attachment" (context model message))

(declare-function chidu-search-read
                  "chidu-search"
                  (app account mailboxes &optional mailbox initial-query))
(declare-function chidu-email-index-account
                  "chidu-email-sync"
                  (runtime account success-function error-function
                           &optional page-limit))
(declare-function chidu-conversation-apply-seen-change
                  "chidu-conversation" (view change))
(declare-function chidu-message-apply-seen-change
                  "chidu-message" (view change))
(declare-function chidu-search-apply-seen-change
                  "chidu-search" (view change))
(declare-function chidu-search-apply-mailbox-move-result
                  "chidu-search" (view result))
(declare-function chidu-summary-apply-seen-change
                  "chidu-summary" (view change))
(declare-function chidu-summary-apply-mailbox-move-result
                  "chidu-summary" (view result))
(declare-function chidu-summary-apply-trash-result
                  "chidu-summary" (view result))
(declare-function chidu-summary-state-p "chidu-summary" (value))
(declare-function chidu-summary-state-account "chidu-summary" (state))
(declare-function chidu-summary-reload-local "chidu-summary" (&optional view))
(declare-function chidu-search-apply-trash-result
                  "chidu-search" (view result))
(declare-function chidu-compose-ensure-app-stoppable
                  "chidu-compose" (app))
(declare-function chidu-live-watch-endpoint
                  "chidu-live" (app endpoint))
(declare-function chidu-retry-seen-intents
                  "chidu-seen" (app account))
(declare-function chidu-retry-mailbox-moves
                  "chidu-mailbox-move" (app account))
(declare-function chidu-retry-trash-operations
                  "chidu-trash" (app account))
(declare-function chidu-mailbox-move-result-committed-p
                  "chidu-mailbox-move" (result))
(declare-function chidu-trash-result-committed-p
                  "chidu-trash" (result))

(with-eval-after-load 'evil
  (require 'chidu-evil))

(defvar chidu--app nil
  "Current Chidu Appkit application, or nil.")

(defun chidu--refresh-account-mailboxes (app account-id)
  "Refresh authoritative Mailboxes for ACCOUNT-ID in APP."
  (when (appkit-app-live-p app)
    (let ((account (chidu--current-account app account-id))
          (runtime (chidu-app-runtime app)))
      (when (and account (chidu-runtime-p runtime))
        (chidu-runtime-sync-mailboxes
         runtime account
         (lambda (context)
           (when-let* ((current (chidu--current-account app account-id)))
             (chidu--set-account-ui
              app current :mailbox-context context :phase 'idle)))
         (lambda (_failure) nil))))))

(defun chidu--refresh-mailboxes-after-seen (app change)
  "Refresh authoritative Mailbox counts in APP after committed CHANGE."
  (when (and (appkit-app-live-p app)
             (eq 'committed (chidu-store-seen-change-phase change)))
    (chidu--refresh-account-mailboxes
     app
     (chidu-store-account-account-id
      (chidu-store-seen-change-account change)))))

(defun chidu--seen-changed (app change)
  "Apply explicit read-state CHANGE to every relevant live Surface in APP."
  (when (and (appkit-app-live-p app) (chidu-store-seen-change-p change))
    (maphash
     (lambda (_identity entry)
       (let ((surface (cdr entry)))
         (when (appkit-surface-live-p surface)
           (pcase (appkit-surface-type-mode (appkit-surface-type surface))
             ('chidu-summary-mode
              (chidu-summary-apply-seen-change surface change))
             ('chidu-search-mode
              (when (fboundp 'chidu-search-apply-seen-change)
                (chidu-search-apply-seen-change surface change)))
             ('chidu-conversation-mode
              (when (fboundp 'chidu-conversation-apply-seen-change)
                (chidu-conversation-apply-seen-change surface change)))
             ('chidu-message-mode
              (chidu-message-apply-seen-change surface change))))))
     (appkit-app-surfaces app))
    (chidu--refresh-mailboxes-after-seen app change)))

(defun chidu--mailbox-move-changed (app result)
  "Apply durable mailbox-move RESULT to relevant live Surfaces in APP."
  (when (and (appkit-app-live-p app)
             (chidu-store-mailbox-move-result-p result))
    (maphash
     (lambda (_identity entry)
       (let ((surface (cdr entry)))
         (when (appkit-surface-live-p surface)
           (pcase (appkit-surface-type-mode (appkit-surface-type surface))
             ('chidu-summary-mode
              (when (fboundp 'chidu-summary-apply-mailbox-move-result)
                (chidu-summary-apply-mailbox-move-result surface result)))
             ('chidu-search-mode
              (when (fboundp 'chidu-search-apply-mailbox-move-result)
                (chidu-search-apply-mailbox-move-result surface result)))))))
     (appkit-app-surfaces app))
    (when (chidu-mailbox-move-result-committed-p result)
      (chidu--refresh-account-mailboxes
       app
       (chidu-store-account-account-id
        (chidu-store-mailbox-move-context-account
         (chidu-store-mailbox-move-result-context result)))))))

(defun chidu--trash-changed (app result)
  "Apply durable trash RESULT to relevant live Surfaces in APP."
  (when (and (appkit-app-live-p app)
             (chidu-store-trash-result-p result))
    (maphash
     (lambda (_identity entry)
       (let ((surface (cdr entry)))
         (when (appkit-surface-live-p surface)
           (pcase (appkit-surface-type-mode (appkit-surface-type surface))
             ('chidu-summary-mode
              (when (fboundp 'chidu-summary-apply-trash-result)
                (chidu-summary-apply-trash-result surface result)))
             ('chidu-search-mode
              (when (fboundp 'chidu-search-apply-trash-result)
                (chidu-search-apply-trash-result surface result)))))))
     (appkit-app-surfaces app))
    (when (chidu-trash-result-committed-p result)
      (chidu--refresh-account-mailboxes
       app
       (chidu-store-account-account-id
        (chidu-store-trash-context-account
         (chidu-store-trash-result-context result)))))))

(defun chidu--runtime-shutdown (app)
  "Close the in-process runtime owned by APP."
  (when-let* ((runtime (chidu-app-runtime app)))
    (chidu-runtime-close runtime)))

(defun chidu--app-init (_context input)
  "Initialize the owning App with directory INPUT."
  (appkit-next :model (chidu-app-model-create :state input)
               :render appkit-render-none))

(defun chidu--app-update (context model message)
  "Commit Chidu MESSAGE and return all captured downstream commands."
  (let* ((chidu--transition-context context)
         (chidu--transition-commands nil)
         (next
          (pcase message
            (`(:state ,app ,state)
             (let ((updated (copy-chidu-app-model model)))
               (setf (chidu-app-model-state updated) state)
               (chidu--invalidate-home app t)
               (appkit-next :model updated :render appkit-render-none)))
            (`(:runtime ,runtime)
             (let ((updated (copy-chidu-app-model model)))
               (setf (chidu-app-model-runtime updated) runtime)
               (appkit-next :model updated :render appkit-render-none)))
            (`(chidu-attachment . ,_)
             (chidu-attachment-app-update context model message))
            (`(chidu-seen-changed ,app ,change)
             (chidu--seen-changed app change)
             (appkit-next :model model :render appkit-render-none))
            (`(chidu-mailbox-move-changed ,app ,result)
             (chidu--mailbox-move-changed app result)
             (appkit-next :model model :render appkit-render-none))
            (`(chidu-trash-changed ,app ,result)
             (chidu--trash-changed app result)
             (appkit-next :model model :render appkit-render-none))
            (_ (appkit-next :model model :render appkit-render-none)))))
    (when (appkit-next-p next)
      (setf (appkit-next-commands next)
            (nconc (appkit-next-commands next)
                   (nreverse chidu--transition-commands))))
    next))

(defconst chidu--app-type
  (appkit-app-type-create
   :name 'chidu :init #'chidu--app-init :update #'chidu--app-update
   :shutdown #'chidu--runtime-shutdown)
  "Canonical owning App descriptor for Chidu.")

(defcustom chidu-endpoints nil
  "Configured JMAP login endpoints.

Each element is a plist with required `:host' and `:user' keys.  `:port'
defaults to 443 and `:authentication' defaults to `basic'.  For example:

  ((:host \"mail.example.com\" :user \"me@example.com\"))

Chidu derives the Session URL from HOST and PORT using `/.well-known/jmap'.
These entries define which services Chidu connects to; `auth-source' is used
only to look up the corresponding secret."
  :type
  '(repeat
    (plist
     :options
     ((:host (string :tag "Host"))
      (:user (string :tag "Login"))
      (:port (integer :tag "HTTPS port"))
      (:authentication
       (choice (const :tag "HTTP Basic" basic)
               (const :tag "Bearer token" bearer))))
     :key-type symbol
     :value-type sexp))
  :group 'chidu)

(defun chidu--normalize-endpoint-spec (spec)
  "Validate endpoint SPEC and return its normalized plist."
  (unless (listp spec)
    (user-error "Chidu endpoint must be a plist: %S" spec))
  (let* ((host (plist-get spec :host))
         (user (plist-get spec :user))
         (port (if (plist-member spec :port) (plist-get spec :port) 443))
         (authentication
          (if (plist-member spec :authentication)
              (plist-get spec :authentication)
            'basic)))
    (unless (and (stringp host) (not (string-empty-p host))
                 (not (string-match-p "://\|/" host)))
      (user-error "Chidu endpoint :host must be a hostname: %S" host))
    (unless (and (stringp user) (not (string-empty-p user)))
      (user-error "Chidu endpoint :user must be nonempty: %S" user))
    (unless (and (integerp port) (> port 0) (< port 65536))
      (user-error "Chidu endpoint :port must be 1..65535: %S" port))
    (unless (memq authentication '(basic bearer))
      (user-error
       "Chidu endpoint :authentication must be basic or bearer: %S"
       authentication))
    (list :host host
          :user user
          :port port
          :authentication authentication
          :session-url
          (format "https://%s%s/.well-known/jmap"
                  host (if (= port 443) "" (format ":%d" port))))))

(defun chidu--configured-endpoint-specs ()
  "Return normalized `chidu-endpoints', rejecting duplicate logins."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (raw chidu-endpoints)
      (let* ((spec (chidu--normalize-endpoint-spec raw))
             (key (list (plist-get spec :session-url)
                        (plist-get spec :user))))
        (when (gethash key seen)
          (user-error "Duplicate Chidu endpoint: %S" raw))
        (puthash key t seen)
        (push spec result)))
    (nreverse result)))

(defun chidu--load-account-local-state (app account)
  "Load ACCOUNT's committed Mailbox and Email state into APP's root."
  (when (and (appkit-app-live-p app)
             (chidu-store-account-available-p account))
    (let ((runtime (chidu-app-runtime app))
          (account-id (chidu--account-id account)))
      (chidu--set-account-ui app account :phase 'loading-local)
      (chidu-runtime-list-mailboxes
       runtime account
       (lambda (mailbox-context)
         (when-let* ((current (chidu--current-account app account-id)))
           (chidu--set-account-ui
            app current :mailbox-context mailbox-context :phase 'idle)))
       (lambda (failure)
         (when-let* ((current (chidu--current-account app account-id)))
           (chidu--set-account-ui
            app current :phase 'error
            :message-text (chidu-runtime-error-message failure))))))))

(defun chidu--load-local-account-states (app &optional endpoints)
  "Load committed Account projections for APP and optional ENDPOINTS."
  (when (appkit-app-live-p app)
    (cl-loop
     for endpoint across
     (or endpoints
         (chidu--state-endpoints (chidu-app-state app)))
     do
     (cl-loop
      for account across (chidu-store-endpoint-accounts endpoint)
      when (chidu-store-account-available-p account)
      do (chidu--load-account-local-state app account)))))

(defun chidu--connect-endpoint (app endpoint)
  "Refresh APP's configured ENDPOINT using `auth-source'."
  (when-let* ((runtime (chidu-app-runtime app)))
    (chidu-runtime-connect-endpoint
     runtime endpoint
     (lambda (connected)
       (when (appkit-app-live-p app)
         (require 'chidu-live)
         (require 'chidu-seen)
         (require 'chidu-mailbox-move)
         (require 'chidu-trash)
         (chidu--set-endpoints
          app
          (chidu--replace-endpoint
           (chidu--state-endpoints (chidu-app-state app))
           connected))
         (chidu--load-local-account-states app (vector connected))
         (chidu-live-watch-endpoint app connected)
         (cl-loop
          for account across (chidu-store-endpoint-accounts connected)
          when (chidu-store-account-available-p account)
          do
          (progn
            (chidu-retry-seen-intents app account)
            (chidu-retry-mailbox-moves app account)
            (chidu-retry-trash-operations app account)))))
     (lambda (failure)
       (when (appkit-app-live-p app)
         (chidu--set-endpoints
          app
          (chidu--state-endpoints (chidu-app-state app))
          (chidu-runtime-error-message failure)))))))

(defun chidu--stored-endpoint-for-spec (endpoints spec)
  "Return stored Endpoint from ENDPOINTS matching normalized SPEC."
  (cl-find-if
   (lambda (endpoint)
     (and
      (equal (chidu-store-endpoint-session-url endpoint)
             (plist-get spec :session-url))
      (equal (chidu-store-endpoint-login endpoint)
             (plist-get spec :user))))
   endpoints))

(defun chidu--resolve-configured-endpoints
    (runtime stored specs success-function error-function)
  "Resolve SPECS against STORED Endpoints in RUNTIME.

Call SUCCESS-FUNCTION with a vector in configuration order, or ERROR-FUNCTION
with a typed failure."
  (let ((remaining (copy-sequence specs))
        resolved)
    (cl-labels
        ((advance
           ()
           (if-let* ((spec (pop remaining)))
               (let ((existing (chidu--stored-endpoint-for-spec stored spec)))
                 (if (and existing
                          (eq (chidu-store-endpoint-authentication existing)
                              (plist-get spec :authentication)))
                     (progn (push existing resolved) (advance))
                   (chidu-runtime-configure-endpoint
                    runtime
                    (plist-get spec :session-url)
                    (plist-get spec :user)
                    (plist-get spec :authentication)
                    (lambda (endpoint)
                      (push endpoint resolved)
                      (advance))
                    error-function)))
             (funcall success-function (vconcat (nreverse resolved))))))
      (advance))))

(defun chidu--load-endpoints (app)
  "Load configured Endpoints for APP, then refresh each Session asynchronously."
  (when-let* ((runtime (chidu-app-runtime app)))
    (condition-case error-data
        (let ((specs (chidu--configured-endpoint-specs)))
          (chidu-runtime-list-endpoints
           runtime
           (lambda (stored)
             (chidu--resolve-configured-endpoints
              runtime stored specs
              (lambda (endpoints)
                (when (appkit-app-live-p app)
                  ;; Configuration, not auth-source or historical Store rows,
                  ;; determines which Endpoints exist in the UI.
                  (chidu--set-endpoints app endpoints)
                  (chidu--load-local-account-states app endpoints)
                  (cl-loop for endpoint across endpoints
                           do (chidu--connect-endpoint app endpoint))))
              (lambda (failure)
                (when (appkit-app-live-p app)
                  (chidu--set-endpoints
                   app (vector) (chidu-runtime-error-message failure))))))
           (lambda (failure)
             (when (appkit-app-live-p app)
               (chidu--set-endpoints
                app (vector) (chidu-runtime-error-message failure))))))
      (error
       (when (appkit-app-live-p app)
         (chidu--set-endpoints
          app (vector) (error-message-string error-data)))))))

(defun chidu--runtime-ready (app store-info)
  "Mark APP ready with durable STORE-INFO and load its root projection."
  (when (appkit-app-live-p app)
    (chidu--set-state
     app
     (chidu--state-create
      :phase 'ready
      :store-info store-info
      :endpoints (vector)
      :accounts (make-hash-table :test #'equal)))
    (chidu--load-endpoints app)))

(defun chidu--start-app ()
  "Start the canonical Chidu App and attach its in-process runtime."
  (let ((app
         (appkit-app-start
          chidu--app-type
          :identity (expand-file-name chidu-data-root)
          :input (chidu--state-create
                  :phase 'starting
                  :accounts (make-hash-table :test #'equal)))))
    (condition-case error-data
        (let ((runtime (chidu-runtime-open :data-root chidu-data-root)))
          (condition-case ownership-error
              (appkit-app-send app (list :runtime runtime))
            ((error quit)
             (chidu-runtime-close runtime)
             (signal (car ownership-error) (cdr ownership-error))))
          (chidu-runtime-info
           runtime
           (lambda (store-info) (chidu--runtime-ready app store-info))
           (lambda (failure)
             (chidu--set-state
              app (chidu--state-create
                   :phase 'failed
                   :accounts (make-hash-table :test #'equal)
                   :message (chidu-runtime-error-message failure))))))
      (quit
       (appkit-app-close app)
       (signal (car error-data) (cdr error-data)))
      (error
       (chidu--set-state
        app (chidu--state-create
             :phase 'failed
             :accounts (make-hash-table :test #'equal)
             :message (error-message-string error-data)))))
    app))

(defun chidu--available-account-choices (app)
  "Return minibuffer choices for APP's available Mail Accounts."
  (cl-loop
   for endpoint across
   (chidu--state-endpoints (chidu-app-state app))
   append
   (cl-loop
    for account across (chidu-store-endpoint-accounts endpoint)
    when
    (and (chidu-store-account-available-p account)
         (seq-contains-p
          (chidu-store-account-capabilities account)
          chidu-jmap-mail-capability
          #'equal))
    collect
    (cons
     (format "%s — %s"
             (chidu-store-endpoint-login endpoint)
             (chidu-store-account-name account))
     account))))

(defun chidu--read-account (app prompt)
  "Read one available Account from APP using PROMPT."
  (let ((choices (chidu--available-account-choices app)))
    (unless choices (user-error "No available JMAP Mail Account"))
    (cdr (assoc (completing-read prompt choices nil t) choices))))

;;;###autoload
(defun chidu ()
  "Start Chidu if needed and display its Account/Mailbox directory."
  (interactive)
  (unless (appkit-app-live-p chidu--app)
    (setq chidu--app (chidu--start-app)))
  (chidu--open-home chidu--app t))

;;;###autoload
(defun chidu-stop ()
  "Stop Chidu after verifying that Compose workspaces are safe."
  (interactive)
  (when (appkit-app-live-p chidu--app)
    (require 'chidu-compose)
    (chidu-compose-ensure-app-stoppable chidu--app))
  (when (appkit-app-p chidu--app)
    (appkit-app-close chidu--app))
  (setq chidu--app nil))

;;;###autoload
(defun chidu-refresh ()
  "Reload the local root and refresh configured JMAP Sessions."
  (interactive)
  (unless (appkit-app-live-p chidu--app) (chidu))
  (chidu--load-endpoints chidu--app))

;;;###autoload
(defun chidu-sync-account (&optional account)
  "Refresh the authoritative Mailbox snapshot for ACCOUNT.

This ordinary sync is intentionally bounded.  It does not build or resume the
full canonical Email index; new-mail reconciliation remains owned by the
EventSource/polling watcher."
  (interactive)
  (unless (appkit-app-live-p chidu--app) (chidu))
  (let* ((app chidu--app)
         (runtime (chidu-app-runtime app))
         (selected
          (or account
              (and (derived-mode-p 'chidu-home-mode)
                   (chidu-home-account-at-point))
              (chidu--read-account app "Synchronize Mailboxes: "))))
    (unless (chidu-store-account-p selected)
      (user-error "No available JMAP Mail Account"))
    (chidu--set-account-ui app selected :phase 'syncing-mailboxes)
    (cl-labels
        ((failed
           (failure)
           (when (appkit-app-live-p app)
             (chidu--set-account-ui
              app selected :phase 'idle
              :message-text (chidu-runtime-error-message failure))
             (message "Chidu sync failed: %s"
                      (chidu-runtime-error-message failure))))
         (completed
           (mailbox-context)
           (when (appkit-app-live-p app)
             (chidu--set-account-ui
              app selected :mailbox-context mailbox-context :phase 'idle)
             (chidu-runtime-info
              runtime
              (lambda (store-info)
                (when (appkit-app-live-p app)
                  (let ((state (copy-chidu--state (chidu-app-state app))))
                    (setf (chidu--state-store-info state) store-info)
                    (chidu--set-state app state))))
              #'failed)
             (message "Chidu: Mailboxes synchronized"))))
      (chidu-runtime-sync-mailboxes
       runtime selected #'completed #'failed))))

(defun chidu--email-index-key (account)
  "Return APP request key for ACCOUNT's canonical Email index."
  (list 'email-index (chidu-store-account-account-id account)))

(defun chidu-email-index-running-p (&optional account)
  "Return non-nil when ACCOUNT has an active canonical index operation."
  (when (and (appkit-app-live-p chidu--app)
             (chidu-store-account-p account))
    (gethash (chidu--email-index-key account)
             (chidu-app-requests chidu--app))))

(defun chidu--reload-account-summaries (app account)
  "Reload APP Summary Surfaces that belong to ACCOUNT."
  (let ((account-id (chidu-store-account-account-id account)))
    (maphash
     (lambda (_identity entry)
       (let ((surface (cdr entry)))
         (when (and (appkit-surface-live-p surface)
                    (eq 'chidu-summary-mode
                        (appkit-surface-type-mode (appkit-surface-type surface))))
           (let ((state (appkit-surface-model surface)))
             (when (and (chidu-summary-state-p state)
                        (equal account-id
                               (chidu-store-account-account-id
                                (chidu-summary-state-account state))))
               (chidu-summary-reload-local surface))))))
     (appkit-app-surfaces app))))

;;;###autoload
(defun chidu-index-account (&optional account)
  "Build or reconcile ACCOUNT's canonical local Email index.

This is an explicit potentially full-account operation.  Ordinary Mailbox sync
and opening a Summary never start it implicitly."
  (interactive)
  (unless (appkit-app-live-p chidu--app) (chidu))
  (let* ((app chidu--app)
         (runtime (chidu-app-runtime app))
         (selected
          (or account
              (and (derived-mode-p 'chidu-home-mode)
                   (chidu-home-account-at-point))
              (chidu--read-account app "Build Email index: ")))
         (key (and (chidu-store-account-p selected)
                   (chidu--email-index-key selected)))
         (requests (chidu-app-requests app)))
    (unless (chidu-store-account-p selected)
      (user-error "No available JMAP Mail Account"))
    (when (gethash key requests)
      (user-error "Email index is already running for this Account"))
    (unless
        (yes-or-no-p
         (format
          "Build canonical Email index for %s (first run may scan the full Account)? "
          (chidu-store-account-name selected)))
      (user-error "Email index was not started"))
    (require 'chidu-email-sync)
    (puthash key 'starting requests)
    (chidu--set-account-ui app selected :phase 'indexing-email)
    (let ((operation
           (chidu-email-index-account
            runtime selected
            (lambda (_context)
              (when (appkit-app-live-p app)
                (remhash key (chidu-app-requests app))
                (chidu--set-account-ui app selected :phase 'idle)
                (chidu--reload-account-summaries app selected)
                (message "Chidu: canonical Email index is ready")))
            (lambda (failure)
              (when (appkit-app-live-p app)
                (remhash key (chidu-app-requests app))
                (chidu--set-account-ui
                 app selected :phase 'idle
                 :message-text (chidu-runtime-error-message failure))
                (message "Chidu Email index failed: %s"
                         (chidu-runtime-error-message failure)))))))
      ;; A live index may settle synchronously before the operation is returned.
      (when (eq 'starting (gethash key requests))
        (if operation
            (puthash key operation requests)
          (remhash key requests)))
      operation)))

;;;###autoload
(defun chidu-cancel-email-index (&optional account)
  "Cancel ACCOUNT's active canonical Email index operation."
  (interactive)
  (unless (appkit-app-live-p chidu--app)
    (user-error "Chidu is not running"))
  (let* ((app chidu--app)
         (runtime (chidu-app-runtime app))
         (selected
          (or account
              (and (derived-mode-p 'chidu-home-mode)
                   (chidu-home-account-at-point))
              (chidu--read-account app "Cancel Email index: ")))
         (key (chidu--email-index-key selected))
         (operation (gethash key (chidu-app-requests app))))
    (unless (chidu-runtime-operation-p operation)
      (user-error "No Email index is running for this Account"))
    (remhash key (chidu-app-requests app))
    (chidu-runtime-cancel-operation runtime operation)
    (chidu--set-account-ui app selected :phase 'idle)
    (message "Chidu: Email index canceled")))

;;;###autoload
(defun chidu-search-mail (&optional account)
  "Search mail in ACCOUNT using a bounded server-backed query view."
  (interactive)
  (unless (appkit-app-live-p chidu--app) (chidu))
  (let* ((app chidu--app)
         (selected
          (or account
              (and (derived-mode-p 'chidu-home-mode)
                   (chidu-home-account-at-point))
              (chidu--read-account app "Search account: ")))
         (ui
          (and (chidu-store-account-p selected)
               (chidu--account-ui-state (chidu-app-state app) selected)))
         (mailbox-context
          (and ui (chidu--account-ui-state-mailbox-context ui))))
    (unless mailbox-context
      (user-error "Chidu has no local Mailbox snapshot for this Account"))
    (require 'chidu-search)
    (chidu-search-read
     app selected
     (chidu-store-mailbox-sync-context-mailboxes mailbox-context))))

;;;###autoload
(defun chidu-restart ()
  "Restart Chidu and reopen its root directory."
  (interactive)
  (chidu-stop)
  (chidu))

(provide 'chidu)

;;; chidu.el ends here
