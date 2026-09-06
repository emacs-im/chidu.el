;;; chidu-summary.el --- Appkit Mailbox Summary interface -*- lexical-binding: t; -*-

;;; Commentary:

;; A local-first, Appkit-owned Mailbox Summary derived directly from the active
;; canonical Email generation.  Loading and pagination are bounded SQLite reads;
;; no render or list-navigation path crosses the network.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-position)
(require 'appkit-projection)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'chidu-mailbox-move)
(require 'chidu-runtime)
(require 'chidu-selection)
(require 'chidu-seen)
(require 'chidu-surface-operation)
(require 'chidu-store)
(require 'chidu-text)
(require 'chidu-trash)

(declare-function chidu-conversation-open
                  "chidu-conversation"
                  (app account mailbox selected-row &optional select))
(declare-function chidu-search-read
                  "chidu-search"
                  (app account mailboxes &optional mailbox initial-query))
(declare-function chidu-message-open
                  "chidu-message"
                  (app account mailbox row &optional select participants))
(declare-function chidu-dispatch "chidu-transient" ())

(defcustom chidu-summary-page-size 50
  "Number of canonical Email rows revealed by one Summary page."
  :type 'positive-integer
  :group 'chidu)

(cl-defstruct (chidu-summary-state
               (:constructor chidu-summary-state-create))
  "View-local state for one Mailbox Summary."
  account
  mailbox
  context
  (limit chidu-summary-page-size)
  (phase 'initial)
  message)

(defvar-local chidu-summary--view nil
  "Appkit view attached to the current Summary buffer.")

(defface chidu-summary-unread
  '((t :inherit bold))
  "Face for unread Summary subjects."
  :group 'chidu)

(defun chidu-summary--view-state (&optional view)
  "Return validated Summary state for VIEW or the current view."
  (let* ((it (or view (appkit-current-surface)))
         (state (and it (appkit-surface-model it))))
    (unless (chidu-summary-state-p state)
      (error "Chidu Summary view has invalid state"))
    state))

(defun chidu-summary--row-model (row)
  "Return Appkit one-line presentation model for Summary ROW."
  (let* ((unread (chidu-store-email-summary-row-unread-p row))
         (flagged (chidu-store-email-summary-row-flagged-p row))
         (attachment (chidu-store-email-summary-row-has-attachment-p row))
         (trail
          (string-join
           (delq nil
                 (list (and unread "●")
                       (and flagged "★")))
           ""))
         (subject
          (concat (and attachment "📎 ")
                  (chidu-email-subject row))))
    (appkit-presentation-one-line-row-create
     :icon-inserter
     (chidu-selection-icon-inserter
      (chidu-store-email-summary-row-local-email-id row))
     :context (chidu-text-person-label row)
     :context-trail trail
     :context-trail-face
     (cond (flagged 'chidu-email-flagged)
           (unread 'chidu-summary-unread))
     :preview
     (appkit-ui-one-line-preview-create
      :label subject
      :separator " —"
      :text (chidu-store-email-summary-row-preview row)
      :label-face (and unread 'chidu-summary-unread))
     :time (chidu-email-format-time
            (chidu-store-email-summary-row-received-at row))
     :time-face 'shadow
     :line-properties
     (list 'chidu-summary-email-id
           (chidu-store-email-summary-row-local-email-id row)
           'chidu-summary-unread-p unread
           'chidu-selection-marked-p
           (chidu-selection-marked-p
            (chidu-store-email-summary-row-local-email-id row))
           'chidu-selection-trash-flagged-p
           (chidu-selection-trash-flagged-p
            (chidu-store-email-summary-row-local-email-id row))))))

(defun chidu-summary--print-row (projection-row)
  "Insert one Summary PROJECTION-ROW."
  (appkit-presentation-insert-one-line-row
   (chidu-summary--row-model
    (appkit-projection-row-payload projection-row))
   :indent 1
   :width (or (appkit-surface-responsive-width (appkit-current-surface) 1) fill-column 100)
   :icon-slot-width 2
   :context-width-spec '(0.28 18 34)
   :time-slot-width 11))

(defun chidu-summary--project-rows (state)
  "Project committed rows from Summary STATE."
  (let ((context (chidu-summary-state-context state)))
    (appkit-projection-project
     (append
      (if context
          (chidu-store-mailbox-summary-context-rows context)
        (vector))
      nil)
     (lambda (row)
       (list 'email
             (chidu-store-email-summary-row-local-email-id row))))))

(defun chidu-summary--header (state)
  "Return generated header for Summary STATE."
  (let ((account (chidu-summary-state-account state))
        (mailbox (chidu-summary-state-mailbox state)))
    (concat
     (propertize
      (format "%s · %s"
              (chidu-store-account-name account)
              (chidu-store-mailbox-name mailbox))
      'face '(:height 1.2 :weight bold))
     "\n\n")))

(defun chidu-summary--footer (state)
  "Return generated footer for Summary STATE."
  (let* ((context (chidu-summary-state-context state))
         (rows (and context
                    (chidu-store-mailbox-summary-context-rows context)))
         (count (if rows (length rows) 0))
         (phase (chidu-summary-state-phase state))
         (message-text (chidu-summary-state-message state))
         (marked (chidu-selection-count))
         (trash-flagged (chidu-selection-trash-flag-count))
         (marker-status
          (string-join
           (delq nil
                 (list (and (> marked 0) (format "%d marked" marked))
                       (and (> trash-flagged 0)
                            (format "%d flagged for Trash" trash-flagged))))
           " · ")))
    (concat
     "\n"
     (unless (string-empty-p marker-status)
       (concat marker-status " · "))
     (pcase phase
       ('initial "Loading canonical Summary…")
       ('loading "Loading canonical Summary…")
       ('reloading "Reloading canonical Summary…")
       ('loading-more "Loading older mail…")
       ('error (format "Unable to load Summary: %s"
                       (or message-text "unknown error")))
       (_
        (cond
         ((zerop count) "No messages.")
         ((and context
               (chidu-store-mailbox-summary-context-maybe-more-p context))
          (format "%d message%s · more available"
                  count (if (= count 1) "" "s")))
         (t (format "%d message%s"
                    count (if (= count 1) "" "s"))))))
     "\n")))

(defun chidu-summary--update (context model message)
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

(defun chidu-summary--request-sync (surface &optional structure local-email-ids)
  "Request native projection work for SURFACE."
  (when (appkit-surface-live-p surface)
    (chidu-post-surface-message
     surface
     (list 'chidu-refresh
           (appkit-projection-change-create
            :full-p structure :frame-p t
            :keys (mapcar (lambda (id) (list 'email id)) local-email-ids)
            :position 'preserve)))))

(defun chidu-summary--loaded (view state limit context)
  "Install Summary CONTEXT in VIEW STATE at LIMIT."
  (chidu-summary--set-context state context)
  (setf (chidu-summary-state-limit state) limit
        (chidu-summary-state-phase state) 'idle
        (chidu-summary-state-message state) nil)
  (chidu-summary--request-sync view t))

(defun chidu-summary--failed (view state failure)
  "Install Summary FAILURE in VIEW STATE."
  (setf (chidu-summary-state-phase state) 'error
        (chidu-summary-state-message state)
        (chidu-runtime-error-message failure))
  (chidu-summary--request-sync view))

(defun chidu-summary--load (view phase limit)
  "Load VIEW's canonical Summary with PHASE and row LIMIT."
  (let ((state (chidu-summary--view-state view)))
    (setf (chidu-summary-state-phase state) phase
          (chidu-summary-state-message state) nil)
    (chidu-summary--request-sync view)
    (chidu-surface-operation-start
     view 'summary
     (lambda (runtime success-function error-function)
       (chidu-runtime-mailbox-summary
        runtime
        (chidu-summary-state-account state)
        (chidu-summary-state-mailbox state)
        limit success-function error-function))
     (apply-partially #'chidu-summary--loaded view state limit)
     (apply-partially #'chidu-summary--failed view state))))

(defun chidu-summary-refresh (&optional view)
  "Reload the current canonical Mailbox Summary VIEW from local state."
  (interactive)
  (let* ((it (or view (appkit-current-surface)))
         (state (chidu-summary--view-state it)))
    (chidu-summary--load
     it 'reloading (chidu-summary-state-limit state))))

(defun chidu-summary-load-more-available-p (&optional view)
  "Return non-nil when VIEW can reveal another canonical Summary page."
  (condition-case nil
      (let* ((state (chidu-summary--view-state (or view (appkit-current-surface))))
             (context (chidu-summary-state-context state)))
        (and context
             (eq 'idle (chidu-summary-state-phase state))
             (chidu-store-mailbox-summary-context-maybe-more-p context)))
    (error nil)))

(defun chidu-summary-load-more (&optional view)
  "Reveal the next older canonical Summary page in VIEW."
  (interactive)
  (let* ((it (or view (appkit-current-surface)))
         (state (chidu-summary--view-state it))
         (context (chidu-summary-state-context state)))
    (unless (and context
                 (chidu-store-mailbox-summary-context-maybe-more-p context))
      (user-error "No older Summary page is available"))
    (chidu-summary--load
     it 'loading-more
     (+ (chidu-summary-state-limit state) chidu-summary-page-size))))

(defun chidu-summary--load-local (view)
  "Load VIEW's bounded canonical Summary from the local Store."
  (let ((state (chidu-summary--view-state view)))
    (chidu-summary--load
     view 'loading (chidu-summary-state-limit state))))

(defun chidu-summary-reload-local (&optional view)
  "Reload canonical local state for Summary VIEW."
  (interactive)
  (chidu-summary-refresh (or view (appkit-current-surface))))

(defun chidu-summary--row-for-local-id (state local-email-id)
  "Return LOCAL-EMAIL-ID row from Summary STATE, or nil."
  (when-let* ((context (chidu-summary-state-context state)))
    (cl-find local-email-id
             (chidu-store-mailbox-summary-context-rows context)
             :key #'chidu-store-email-summary-row-local-email-id
             :test #'equal)))

(defun chidu-summary--set-context (state context)
  "Install committed Summary CONTEXT in STATE and reconcile markers."
  (setf (chidu-summary-state-context state) context)
  (when (or (hash-table-p chidu-selection--marked-ids)
            (hash-table-p chidu-selection--trash-flagged-ids))
    (chidu-selection-prune-to-ids
     (cl-loop
      for row across (chidu-store-mailbox-summary-context-rows context)
      collect (chidu-store-email-summary-row-local-email-id row))))
  context)

(defun chidu-summary--row-at-point ()
  "Return locally cached Summary row at point, or nil."
  (when-let* ((local-id
               (or (get-text-property (point) 'chidu-summary-email-id)
                   (get-text-property
                    (line-beginning-position) 'chidu-summary-email-id)))
              (state (chidu-summary--view-state))
              (context (chidu-summary-state-context state)))
    (cl-find local-id
             (chidu-store-mailbox-summary-context-rows context)
             :key #'chidu-store-email-summary-row-local-email-id
             :test #'equal)))

(defun chidu-summary-next ()
  "Move to the next Summary row."
  (interactive)
  (chidu-text-next-property-row
   'chidu-summary-email-id "No later message"))

(defun chidu-summary-previous ()
  "Move to the previous Summary row."
  (interactive)
  (chidu-text-previous-property-row
   'chidu-summary-email-id "No earlier message"))

(defun chidu-summary--selection ()
  "Return (VIEW STATE ROW) for the current Summary entry."
  (let* ((row (or (chidu-summary--row-at-point)
                  (user-error "No message at point")))
         (view (or (appkit-current-surface)
                   (user-error "No live Chidu Summary view")))
         (state (chidu-summary--view-state view)))
    (list view state row)))

(defun chidu-summary--seen-target ()
  "Return explicit read-state target for the Summary Email at point."
  (pcase-let* ((`(,view ,state ,row) (chidu-summary--selection)))
    (chidu-seen-target-create
     :app (appkit-surface-app view)
     :account (chidu-summary-state-account state)
     :local-email-id (chidu-store-email-summary-row-local-email-id row)
     :remote-email-id (chidu-store-email-summary-row-remote-email-id row)
     :unread-p (chidu-store-email-summary-row-unread-p row))))

(defun chidu-summary--seen-targets ()
  "Return read-state targets from process marks, region, or point."
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Summary view")))
         (state (chidu-summary--view-state view))
         (app (appkit-surface-app view))
         (account (chidu-summary-state-account state)))
    (mapcar
     (lambda (local-id)
       (let ((row (or (chidu-summary--row-for-local-id state local-id)
                      (user-error "Selected Summary Email disappeared"))))
         (chidu-seen-target-create
          :app app :account account
          :local-email-id local-id
          :remote-email-id
          (chidu-store-email-summary-row-remote-email-id row)
          :unread-p (chidu-store-email-summary-row-unread-p row))))
     (or (chidu-selection-selected-ids)
         (user-error "No Summary Email selected")))))

(defun chidu-summary-apply-mailbox-move-result (view result)
  "Reload local Summary VIEW after relevant Mailbox move RESULT."
  (when (appkit-surface-live-p view)
    (let ((state (chidu-summary--view-state view)))
      (when
          (and
           (chidu-mailbox-move-result-for-account-p
            result (chidu-summary-state-account state))
           (chidu-mailbox-move-result-affects-mailbox-p
            result (chidu-summary-state-mailbox state)))
        (chidu-summary--load-local view)))))

(defun chidu-summary-apply-trash-result (view result)
  "Reload local Summary VIEW after Account move-to-Trash RESULT."
  (when (appkit-surface-live-p view)
    (let ((state (chidu-summary--view-state view)))
      (when (chidu-trash-result-for-account-p
             result (chidu-summary-state-account state))
        (chidu-summary--load-local view)))))

(defun chidu-summary-apply-seen-change (view change)
  "Apply explicit read-state CHANGE to live Summary VIEW."
  (when (appkit-surface-live-p view)
    (let* ((state (chidu-summary--view-state view))
           (context (chidu-summary-state-context state)))
      (when (and context
                 (chidu-seen-change-for-account-p
                  change (chidu-summary-state-account state)))
        (let ((changed-p nil)
              rows)
          (cl-loop
           for row across (chidu-store-mailbox-summary-context-rows context)
           for updated = (chidu-seen-update-summary-row row change)
           do (unless (eq updated row) (setq changed-p t))
           do (push updated rows))
          (when changed-p
            (setf (chidu-summary-state-context state)
                  (chidu-store-mailbox-summary-context-with
                   context :rows (vconcat (nreverse rows))))
            (chidu-summary--request-sync view t)))))))

(defun chidu-summary-open-message ()
  "Open the selected Summary Email as a standalone local-first message."
  (interactive)
  (require 'chidu-message)
  (pcase-let* ((`(,view ,state ,row) (chidu-summary--selection)))
    (chidu-message-open
     (appkit-surface-app view)
     (chidu-summary-state-account state)
     (chidu-summary-state-mailbox state)
     row t)))

(defun chidu-summary-open-conversation ()
  "Open the selected Summary Email in its reply-tree Conversation."
  (interactive)
  (pcase-let* ((`(,view ,state ,row) (chidu-summary--selection)))
    (require 'chidu-conversation)
    (chidu-conversation-open
     (appkit-surface-app view)
     (chidu-summary-state-account state)
     (chidu-summary-state-mailbox state)
     row t)))

(defun chidu-summary--selected-local-email-ids ()
  "Return selected Summary local Email ids in display order."
  (vconcat
   (or (chidu-selection-selected-ids)
       (user-error "No Summary Email selected"))))

(defun chidu-summary--with-mailboxes (view continuation)
  "Call CONTINUATION with current Account Mailboxes for Summary VIEW."
  (let ((state (chidu-summary--view-state view)))
    (chidu-surface-operation-start
     view 'mailboxes
     (lambda (runtime success-function error-function)
       (chidu-runtime-list-mailboxes
        runtime (chidu-summary-state-account state)
        success-function error-function))
     (lambda (context)
       (funcall
        continuation
        (chidu-store-mailbox-sync-context-mailboxes context)))
     (lambda (failure)
       (message "Chidu: %s" (chidu-runtime-error-message failure))))))

(defun chidu-summary-archive ()
  "Move selected Summary Emails to the Account's archive Mailbox."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Summary view")))
         (state (chidu-summary--view-state view))
         (ids (chidu-summary--selected-local-email-ids))
         (source (chidu-summary-state-mailbox state)))
    (chidu-summary--with-mailboxes
     view
     (lambda (mailboxes)
       (chidu-move-emails
        (appkit-surface-app view)
        (chidu-summary-state-account state)
        source
        (chidu-mailbox-move-role-destination
         mailboxes source "archive")
        ids
        #'ignore
        (lambda (failure)
          (message "Chidu: %s" (chidu-runtime-error-message failure))))))))

(defun chidu-summary-trash-staging-available-p (&optional view)
  "Return non-nil when VIEW can stage Emails for move to Trash."
  (when-let* ((view (or view (appkit-current-surface)))
              ((appkit-surface-live-p view))
              (state (appkit-surface-model view))
              ((chidu-summary-state-p state))
              (mailbox (chidu-summary-state-mailbox state)))
    (not (equal "trash" (chidu-store-mailbox-role mailbox)))))

(defun chidu-summary-flag-trash ()
  "Flag the active region or Summary Email for a later move to Trash.

This is a view-local Dired-style marker edit.  It performs no Store or JMAP
mutation; `chidu-summary-execute-trash-flags' is the separate commit step."
  (interactive)
  (let ((view (or (appkit-current-surface)
                  (user-error "No live Chidu Summary view"))))
    (unless (chidu-summary-trash-staging-available-p view)
      (user-error "This Summary is already the Trash Mailbox"))
    (chidu-selection-flag-trash)))

(defun chidu-summary--trash-ids (view state ids)
  "Move exact Summary IDS to Trash using VIEW and STATE."
  (when (equal "trash"
               (chidu-store-mailbox-role
                (chidu-summary-state-mailbox state)))
    (user-error "This Summary is already the Trash Mailbox"))
  (unless (and (vectorp ids) (> (length ids) 0))
    (user-error "No Email selected"))
  (chidu-summary--with-mailboxes
   view
   (lambda (mailboxes)
     (chidu-trash-emails
      (appkit-surface-app view)
      (chidu-summary-state-account state)
      (chidu-trash-role-mailbox mailboxes)
      ids #'ignore
      (lambda (failure)
        (message "Chidu: %s" (chidu-runtime-error-message failure)))))))

(defun chidu-summary-execute-trash-flags ()
  "Confirm and move all `D'-flagged Summary Emails to Trash."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Summary view")))
         (state (chidu-summary--view-state view))
         (flagged (chidu-selection-trash-flagged-ids))
         (count (length flagged)))
    (when (zerop count)
      (user-error "No Emails are flagged for Trash"))
    (when (yes-or-no-p
           (format "Move %d flagged Email%s to Trash? "
                   count (if (= count 1) "" "s")))
      (chidu-summary--trash-ids view state (vconcat flagged)))))

(defun chidu-summary-move ()
  "Move selected Summary Emails to a chosen Mailbox."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Summary view")))
         (state (chidu-summary--view-state view))
         (ids (chidu-summary--selected-local-email-ids))
         (source (chidu-summary-state-mailbox state)))
    (chidu-summary--with-mailboxes
     view
     (lambda (mailboxes)
       (let ((destination
              (chidu-mailbox-move-read-mailbox
               "Move to: "
               (chidu-mailbox-move-destinations mailboxes source))))
         (chidu-move-emails
          (appkit-surface-app view)
          (chidu-summary-state-account state)
          source destination ids
          #'ignore
          (lambda (failure)
            (message "Chidu: %s"
                     (chidu-runtime-error-message failure)))))))))

(defun chidu-summary-search ()
  "Search within the current Summary Mailbox."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Summary view")))
         (state (chidu-summary--view-state view))
         (mailbox (chidu-summary-state-mailbox state)))
    (require 'chidu-search)
    (chidu-search-read
     (appkit-surface-app view)
     (chidu-summary-state-account state)
     (vector mailbox)
     mailbox)))

(defvar-keymap chidu-summary-mode-map
  :doc "Keymap for `chidu-summary-mode'."
  :parent special-mode-map
  "?" #'chidu-dispatch
  "m" #'chidu-selection-mark
  "u" #'chidu-selection-unmark
  "U" #'chidu-selection-clear
  "t" #'chidu-selection-toggle
  "M" #'chidu-selection-mark-all
  "~" #'chidu-selection-toggle-all
  "{" #'chidu-selection-previous-marked
  "}" #'chidu-selection-next-marked
  "!" #'chidu-mark-read
  "R" #'chidu-mark-unread
  "s" #'chidu-toggle-read
  "a" #'chidu-summary-archive
  "d" #'chidu-summary-flag-trash
  "x" #'chidu-summary-execute-trash-flags
  "V" #'chidu-summary-move
  "g" #'chidu-summary-refresh
  "+" #'chidu-summary-load-more
  "S" #'chidu-summary-search
  "RET" #'chidu-summary-open-conversation
  "o" #'chidu-summary-open-message
  "n" #'chidu-summary-next
  "p" #'chidu-summary-previous
  "q" #'quit-window)

(define-derived-mode chidu-summary-mode special-mode "Chidu-Summary"
  "Major mode for a local-first Chidu Mailbox Summary."
  (setq-local truncate-lines t
              chidu-seen-target-function #'chidu-summary--seen-target
              chidu-seen-targets-function #'chidu-summary--seen-targets)
  (setq-local header-line-format
              '(:eval
                (when-let* ((view (appkit-current-surface))
                            (state (appkit-surface-model view))
                            ((chidu-summary-state-p state)))
                  (format " Chidu · %s · %s"
                          (chidu-store-account-name
                           (chidu-summary-state-account state))
                          (chidu-store-mailbox-name
                           (chidu-summary-state-mailbox state)))))))

(defun chidu-summary--renderer (surface)
  "Create the native projection Renderer for SURFACE."
  (setq-local chidu-summary--view surface)
  (chidu-selection-setup
   'chidu-summary-email-id
   (apply-partially #'chidu-summary--request-sync surface nil)
   #'chidu-summary-next #'chidu-summary-previous)
  
  (appkit-projection-renderer-create
   :project-all (lambda (_surface _app model)
                  (chidu-summary--project-rows model))
   :project-frame (lambda (_surface _app model)
                    (cons (chidu-summary--header model) (chidu-summary--footer model)))
   :printer (lambda (_surface _app row) (chidu-summary--print-row row))
   :anchor-property 'chidu-summary-email-id
   :no-separator-p t))

(defun chidu-summary-open (app account mailbox &optional select)
  "Open APP's local Summary for ACCOUNT and MAILBOX.

Select its buffer when SELECT is non-nil."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-mailbox-p mailbox)
    (signal 'wrong-type-argument (list 'chidu-store-mailbox-p mailbox)))
  (let* ((view-id
          (list 'summary
                (chidu-store-account-account-id account)
                (chidu-store-mailbox-mailbox-id mailbox)))
         (existing (appkit-app-surface app view-id))
         (view
          (or existing
              (appkit-open-generated-surface
               (appkit-surface-type-create
                :name 'chidu-summary
                :mode #'chidu-summary-mode
                :init (lambda (_context input)
                        (appkit-next :model input
                                     :render (appkit-projection-change-create :full-p t :frame-p t)))
                :update #'chidu-summary--update
                :renderer-factory #'chidu-summary--renderer)
               :app app :identity view-id
               :buffer-name (format "*Chidu: %s/%s*" (chidu-store-account-name account) (chidu-store-mailbox-name mailbox))
               :input (chidu-summary-state-create :account account :mailbox mailbox)))))
    (unless existing
      (with-current-buffer (appkit-surface-buffer view)
        (appkit-surface-enable-responsive-geometry
         view
         (lambda (owner _width)
           (chidu-post-surface-message
            owner (list 'chidu-refresh
                        (appkit-projection-change-create
                         :geometry-p t :frame-p t)))))
        (chidu-summary--load-local view)))
    (when select (pop-to-buffer (appkit-surface-buffer view)))
    (when existing
      (with-current-buffer (appkit-surface-buffer view)
        (chidu-summary--load-local view)))
    (appkit-surface-buffer view)))

(provide 'chidu-summary)

;;; chidu-summary.el ends here
