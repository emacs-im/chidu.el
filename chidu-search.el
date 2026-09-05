;;; chidu-search.el --- Appkit Email search view -*- lexical-binding: t; -*-

;;; Commentary:

;; A server-backed, Store-first Email query view.  It renders the last committed
;; bounded result immediately, refreshes explicitly, and opens the same
;; Conversation/message surfaces as Mailbox Summary.

;;; Code:

(require 'cl-lib)
(require 'shr)
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
(require 'chidu-search-query)
(require 'chidu-search-sync)
(require 'chidu-selection)
(require 'chidu-view-operation)
(require 'chidu-seen)
(require 'chidu-store)
(require 'chidu-text)
(require 'chidu-trash)

(declare-function chidu-conversation-open
                  "chidu-conversation"
                  (app account mailbox selected-row &optional select))
(declare-function chidu-message-open
                  "chidu-message"
                  (app account mailbox row &optional select participants))
(declare-function chidu-dispatch "chidu-transient" ())

(cl-defstruct (chidu-search-state
               (:constructor chidu-search-state-create))
  "View-local state for one Email search."
  account
  (mailboxes (vector))
  spec
  context
  (phase 'initial)
  message)

(defvar-local chidu-search--view nil
  "Appkit view attached to the current Search buffer.")

(defun chidu-search--state (&optional view)
  "Return validated Search state for VIEW or the current view."
  (let* ((it (or view (appkit-current-surface)))
         (state (and it (appkit-surface-model it))))
    (unless (chidu-search-state-p state)
      (error "Chidu Search view has invalid state"))
    state))

(defun chidu-search--shr-mark (dom)
  "Render SearchSnippet mark DOM using the standard match face."
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'match 'append)))

(defun chidu-search--render-snippet (html fallback)
  "Render SearchSnippet HTML or return FALLBACK."
  (if (not (and (stringp html) (not (string-empty-p html))))
      fallback
    (condition-case nil
        (with-temp-buffer
          (let ((shr-inhibit-images t)
                (shr-use-fonts nil)
                (shr-width 200)
                (shr-external-rendering-functions
                 '((mark . chidu-search--shr-mark))))
            (insert "<div>" html "</div>")
            (shr-render-region (point-min) (point-max))
            (string-trim (buffer-string))))
      (error fallback))))

(defun chidu-search--row-model (row)
  "Return Appkit one-line presentation model for search ROW."
  (let* ((summary (chidu-store-search-row-summary-row row))
         (snippet (chidu-store-search-row-snippet row))
         (unread (chidu-store-email-summary-row-unread-p summary))
         (flagged (chidu-store-email-summary-row-flagged-p summary))
         (attachment
          (chidu-store-email-summary-row-has-attachment-p summary))
         (trail
          (string-join
           (delq nil (list (and unread "●") (and flagged "★"))) ""))
         (subject
          (chidu-search--render-snippet
           (and snippet (chidu-store-search-snippet-subject snippet))
           (chidu-email-subject summary)))
         (preview
          (chidu-search--render-snippet
           (and snippet (chidu-store-search-snippet-preview snippet))
           (chidu-store-email-summary-row-preview summary))))
    (appkit-presentation-one-line-row-create
     :icon-inserter
     (chidu-selection-icon-inserter
      (chidu-store-email-summary-row-local-email-id summary))
     :context (chidu-text-person-label summary)
     :context-trail trail
     :context-trail-face
     (cond (flagged 'chidu-email-flagged)
           (unread 'bold))
     :preview
     (appkit-ui-one-line-preview-create
      :label (concat (and attachment "📎 ") subject)
      :separator " —"
      :text preview
      :label-face (and unread 'bold))
     :time
     (chidu-email-format-time
      (chidu-store-email-summary-row-received-at summary))
     :time-face 'shadow
     :line-properties
     (list 'chidu-search-email-id
           (chidu-store-email-summary-row-local-email-id summary)
           'chidu-search-unread-p unread
           'chidu-selection-marked-p
           (chidu-selection-marked-p
            (chidu-store-email-summary-row-local-email-id summary))
           'chidu-selection-trash-flagged-p
           (chidu-selection-trash-flagged-p
            (chidu-store-email-summary-row-local-email-id summary)))
     :mouse-face 'highlight)))

(defun chidu-search--print-row (projection-row)
  "Insert one search PROJECTION-ROW."
  (appkit-presentation-insert-one-line-row
   (chidu-search--row-model
    (appkit-projection-row-payload projection-row))
   :indent 1
   :width (or (appkit-surface-responsive-width (appkit-current-surface) 1) fill-column 100)
   :icon-slot-width 2
   :context-width-spec '(0.28 18 34)
   :time-slot-width 11))

(defun chidu-search--rows (state)
  "Return committed result rows from Search STATE."
  (if-let* ((context (chidu-search-state-context state)))
      (chidu-store-search-context-rows context)
    (vector)))

(defun chidu-search--project-rows (state)
  "Project committed result rows from Search STATE."
  (appkit-projection-project
   (append (chidu-search--rows state) nil)
   (lambda (row)
     (list
      'email
      (chidu-store-email-summary-row-local-email-id
       (chidu-store-search-row-summary-row row))))))

(defun chidu-search--mailbox-label (state)
  "Return optional scoped Mailbox label for Search STATE."
  (when-let* ((mailbox-id
               (chidu-search-spec-mailbox-id
                (chidu-search-state-spec state)))
              (mailbox
               (cl-find mailbox-id (chidu-search-state-mailboxes state)
                        :key #'chidu-store-mailbox-mailbox-id
                        :test #'equal)))
    (chidu-store-mailbox-name mailbox)))

(defun chidu-search--header (state)
  "Return generated header for Search STATE."
  (let* ((account (chidu-search-state-account state))
         (spec (chidu-search-state-spec state))
         (context (chidu-search-state-context state))
         (count (length (chidu-search--rows state)))
         (scope (chidu-search--mailbox-label state))
         (phase (chidu-search-state-phase state))
         (problem (chidu-search-state-message state))
         (marked (chidu-selection-count))
         (trash-flagged (chidu-selection-trash-flag-count))
         (status
          (pcase phase
            ('initial "loading local result")
            ('loading "loading local result")
            ('refreshing "searching server")
            ('loading-more "loading more results")
            ('error (format "search failed: %s" (or problem "unknown error")))
            (_
             (concat
              (format "%d result%s%s%s"
                      count (if (= count 1) "" "s")
                      (if (> marked 0) (format " · %d marked" marked) "")
                      (if (> trash-flagged 0)
                          (format " · %d flagged for Trash" trash-flagged)
                        ""))
              (cond
               ((and context (chidu-store-search-context-stale-p context))
                " · changed locally; refresh to continue")
               ((and context
                     (chidu-store-search-context-maybe-more-p context))
                " · more available")))))))
    (concat
     (propertize
      (concat
       "Search · " (chidu-store-account-name account)
       (when scope (concat " · " scope)))
      'face '(:height 1.2 :weight bold))
     "\n"
     (propertize (chidu-search-spec-query-text spec) 'face 'font-lock-string-face)
     "\n"
     (propertize status 'face (if (eq phase 'error) 'error 'shadow))
     "\n\n")))

(defun chidu-search--update (context model message)
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

(defun chidu-search--request-sync (surface &optional structure local-email-ids)
  "Request native projection work for SURFACE."
  (when (appkit-surface-live-p surface)
    (chidu-post-surface-message
     surface
     (list 'chidu-refresh
           (appkit-projection-change-create
            :full-p structure :frame-p t
            :keys (mapcar (lambda (id) (list 'email id)) local-email-ids)
            :position 'preserve)))))

(defun chidu-search--loaded (view state context)
  "Install Search CONTEXT in VIEW STATE."
  (chidu-search--set-context state context)
  (setf (chidu-search-state-phase state) 'idle
        (chidu-search-state-message state) nil)
  (chidu-search--request-sync view t))

(defun chidu-search--failed (view state failure)
  "Install Search FAILURE in VIEW STATE."
  (setf (chidu-search-state-phase state) 'error
        (chidu-search-state-message state)
        (chidu-runtime-error-message failure))
  (chidu-search--request-sync view))

(defun chidu-search--loaded-local (view state refresh-empty-p context)
  "Install local Search CONTEXT in VIEW STATE.

Refresh remotely when REFRESH-EMPTY-P and CONTEXT has no revision."
  (chidu-search--set-context state context)
  (setf (chidu-search-state-phase state) 'idle)
  (chidu-search--request-sync view t)
  (when (and refresh-empty-p
             (zerop (chidu-store-search-context-revision context)))
    (chidu-search-refresh view)))

(defun chidu-search-refresh (&optional view)
  "Refresh the current Search VIEW from JMAP."
  (interactive)
  (let* ((it (or view (appkit-current-surface)))
         (state (chidu-search--state it)))
    (setf (chidu-search-state-phase state) 'refreshing
          (chidu-search-state-message state) nil)
    (chidu-search--request-sync it)
    (chidu-view-operation-start
     it 'search
     (lambda (runtime success-function error-function)
       (chidu-refresh-search
        runtime
        (chidu-search-state-account state)
        (chidu-search-state-spec state)
        success-function error-function))
     (apply-partially #'chidu-search--loaded it state)
     (apply-partially #'chidu-search--failed it state))))

(defun chidu-search-load-more-available-p (&optional view)
  "Return non-nil when VIEW can append another stable Search page."
  (condition-case nil
      (let* ((state (chidu-search--state (or view (appkit-current-surface))))
             (context (chidu-search-state-context state)))
        (and context
             (eq 'idle (chidu-search-state-phase state))
             (not (chidu-store-search-context-stale-p context))
             (chidu-store-search-context-maybe-more-p context)))
    (error nil)))

(defun chidu-search-load-more (&optional view)
  "Append the next page to the current Search VIEW."
  (interactive)
  (let* ((it (or view (appkit-current-surface)))
         (state (chidu-search--state it))
         (context (chidu-search-state-context state)))
    (when (and context (chidu-store-search-context-stale-p context))
      (user-error "Refresh this changed Search before loading more"))
    (unless (and context
                 (chidu-store-search-context-maybe-more-p context))
      (user-error "No additional search page is available"))
    (setf (chidu-search-state-phase state) 'loading-more
          (chidu-search-state-message state) nil)
    (chidu-search--request-sync it)
    (chidu-view-operation-start
     it 'search
     (lambda (runtime success-function error-function)
       (chidu-load-more-search
        runtime
        (chidu-search-state-account state)
        (chidu-search-state-spec state)
        success-function error-function))
     (apply-partially #'chidu-search--loaded it state)
     (apply-partially #'chidu-search--failed it state))))

(defun chidu-search--load-local (view &optional refresh-empty-p)
  "Load VIEW's local result; refresh remotely when REFRESH-EMPTY-P."
  (let ((state (chidu-search--state view)))
    (setf (chidu-search-state-phase state) 'loading
          (chidu-search-state-message state) nil)
    (chidu-search--request-sync view)
    (chidu-view-operation-start
     view 'search
     (lambda (runtime success-function error-function)
       (chidu-runtime-search
        runtime
        (chidu-search-state-account state)
        (chidu-search-state-spec state)
        success-function error-function))
     (apply-partially
      #'chidu-search--loaded-local view state refresh-empty-p)
     (apply-partially #'chidu-search--failed view state))))

(defun chidu-search-reload-local (&optional view)
  "Reload committed local state for Search VIEW without network access."
  (interactive)
  (chidu-search--load-local (or view (appkit-current-surface)) nil))

(defun chidu-search--row-for-local-id (state local-email-id)
  "Return LOCAL-EMAIL-ID search row from STATE, or nil."
  (cl-find
   local-email-id (chidu-search--rows state)
   :key
   (lambda (row)
     (chidu-store-email-summary-row-local-email-id
      (chidu-store-search-row-summary-row row)))
   :test #'equal))

(defun chidu-search--set-context (state context)
  "Install committed Search CONTEXT in STATE and reconcile markers."
  (setf (chidu-search-state-context state) context)
  (when (or (hash-table-p chidu-selection--marked-ids)
            (hash-table-p chidu-selection--trash-flagged-ids))
    (chidu-selection-prune-to-ids
     (cl-loop
      for row across (chidu-store-search-context-rows context)
      collect
      (chidu-store-email-summary-row-local-email-id
       (chidu-store-search-row-summary-row row)))))
  context)

(defun chidu-search--row-at-point ()
  "Return locally committed search row at point, or nil."
  (when-let* ((local-id
               (or (get-text-property (point) 'chidu-search-email-id)
                   (get-text-property
                    (line-beginning-position) 'chidu-search-email-id)))
              (state (chidu-search--state)))
    (cl-find
     local-id (chidu-search--rows state)
     :key
     (lambda (row)
       (chidu-store-email-summary-row-local-email-id
        (chidu-store-search-row-summary-row row)))
     :test #'equal)))

(defun chidu-search-next ()
  "Move to the next search result."
  (interactive)
  (chidu-text-next-property-row
   'chidu-search-email-id "No later search result"))

(defun chidu-search-previous ()
  "Move to the previous search result."
  (interactive)
  (chidu-text-previous-property-row
   'chidu-search-email-id "No earlier search result"))

(defun chidu-search--mailbox-for-row (state row)
  "Return best local Mailbox for search ROW in STATE."
  (let* ((scope-id
          (chidu-search-spec-mailbox-id
           (chidu-search-state-spec state)))
         (remote-ids
          (chidu-store-search-row-remote-mailbox-ids row))
         (matches
          (cl-loop
           for mailbox across (chidu-search-state-mailboxes state)
           when
           (and
            (chidu-store-mailbox-available-p mailbox)
            (seq-contains-p
             remote-ids
             (chidu-store-mailbox-remote-mailbox-id mailbox)
             #'equal))
           collect mailbox)))
    (or (and scope-id
             (cl-find scope-id matches
                      :key #'chidu-store-mailbox-mailbox-id
                      :test #'equal))
        (cl-find "inbox" matches
                 :key #'chidu-store-mailbox-role :test #'equal)
        (car
         (sort matches
               (lambda (left right)
                 (let ((left-order (chidu-store-mailbox-sort-order left))
                       (right-order (chidu-store-mailbox-sort-order right)))
                   (if (= left-order right-order)
                       (string-lessp
                        (chidu-store-mailbox-name left)
                        (chidu-store-mailbox-name right))
                     (< left-order right-order)))))))))

(defun chidu-search--selection ()
  "Return (VIEW STATE SEARCH-ROW SUMMARY MAILBOX) at point."
  (let* ((row (or (chidu-search--row-at-point)
                  (user-error "No search result at point")))
         (view (or (appkit-current-surface)
                   (user-error "No live Chidu Search view")))
         (state (chidu-search--state view))
         (summary (chidu-store-search-row-summary-row row))
         (mailbox
          (or (chidu-search--mailbox-for-row state row)
              (user-error "Search result has no available local Mailbox"))))
    (list view state row summary mailbox)))

(defun chidu-search--seen-target ()
  "Return explicit read-state target for the search Email at point."
  (pcase-let* ((`(,view ,state ,_search-row ,summary ,_mailbox)
                (chidu-search--selection)))
    (chidu-seen-target-create
     :app (appkit-surface-app view)
     :account (chidu-search-state-account state)
     :local-email-id
     (chidu-store-email-summary-row-local-email-id summary)
     :remote-email-id
     (chidu-store-email-summary-row-remote-email-id summary)
     :unread-p (chidu-store-email-summary-row-unread-p summary))))

(defun chidu-search--seen-targets ()
  "Return read-state targets from process marks, region, or point."
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Search view")))
         (state (chidu-search--state view))
         (app (appkit-surface-app view))
         (account (chidu-search-state-account state)))
    (mapcar
     (lambda (local-id)
       (let* ((row (or (chidu-search--row-for-local-id state local-id)
                       (user-error "Selected Search Email disappeared")))
              (summary (chidu-store-search-row-summary-row row)))
         (chidu-seen-target-create
          :app app :account account
          :local-email-id local-id
          :remote-email-id
          (chidu-store-email-summary-row-remote-email-id summary)
          :unread-p (chidu-store-email-summary-row-unread-p summary))))
     (or (chidu-selection-selected-ids)
         (user-error "No Search Email selected")))))

(defun chidu-search-apply-mailbox-move-result (view result)
  "Reload local Search VIEW after Account Mailbox move RESULT."
  (when (appkit-surface-live-p view)
    (let ((state (chidu-search--state view)))
      (when
          (chidu-mailbox-move-result-for-account-p
           result (chidu-search-state-account state))
        (chidu-search--load-local view nil)))))

(defun chidu-search-apply-trash-result (view result)
  "Reload local Search VIEW after Account move-to-Trash RESULT."
  (when (appkit-surface-live-p view)
    (let ((state (chidu-search--state view)))
      (when (chidu-trash-result-for-account-p
             result (chidu-search-state-account state))
        (chidu-search--load-local view nil)))))

(defun chidu-search-apply-seen-change (view change)
  "Apply explicit read-state CHANGE to live Search VIEW."
  (when (appkit-surface-live-p view)
    (let* ((state (chidu-search--state view))
           (context (chidu-search-state-context state)))
      (when (and context
                 (chidu-seen-change-for-account-p
                  change (chidu-search-state-account state)))
        (let ((changed-p nil)
              rows)
          (cl-loop
           for row across (chidu-store-search-context-rows context)
           for summary = (chidu-store-search-row-summary-row row)
           for updated-summary =
           (chidu-seen-update-summary-row summary change)
           for updated =
           (if (eq summary updated-summary)
               row
             (chidu-store-search-row-with
              row :summary-row updated-summary))
           do (unless (eq updated row) (setq changed-p t))
           do (push updated rows))
          (when changed-p
            (setf (chidu-search-state-context state)
                  (chidu-store-search-context-with
                   context :rows (vconcat (nreverse rows))))
            (chidu-search--request-sync view t)))))))

(defun chidu-search-open-message ()
  "Open the search result at point as a standalone message."
  (interactive)
  (require 'chidu-message)
  (pcase-let* ((`(,view ,state ,_row ,summary ,mailbox)
                (chidu-search--selection)))
    (chidu-message-open
     (appkit-surface-app view)
     (chidu-search-state-account state)
     mailbox summary t)))

(defun chidu-search-open-conversation ()
  "Open the search result at point in its reply-tree Conversation."
  (interactive)
  (pcase-let* ((`(,view ,state ,_row ,summary ,mailbox)
                (chidu-search--selection)))
    (require 'chidu-conversation)
    (chidu-conversation-open
     (appkit-surface-app view)
     (chidu-search-state-account state)
     mailbox summary t)))

(defun chidu-search--selected-rows (state)
  "Return selected Search rows from STATE in display order."
  (mapcar
   (lambda (local-id)
     (or (chidu-search--row-for-local-id state local-id)
         (user-error "Selected Search Email disappeared")))
   (or (chidu-selection-selected-ids)
       (user-error "No Search Email selected"))))

(defun chidu-search--common-source-mailboxes (state rows mailboxes)
  "Return Mailboxes common to all search ROWS and removable by STATE's Account."
  (let* ((scope-id
          (chidu-search-spec-mailbox-id
           (chidu-search-state-spec state)))
         (common
          (vconcat
           (sort
            (cl-loop
             for mailbox across mailboxes
             for remote-id =
             (chidu-store-mailbox-remote-mailbox-id mailbox)
             when
             (and
              (chidu-store-mailbox-available-p mailbox)
              (chidu-store-mailbox-rights-may-remove-items-p
               (chidu-store-mailbox-rights mailbox))
              (cl-every
               (lambda (row)
                 (seq-contains-p
                  (chidu-store-search-row-remote-mailbox-ids row)
                  remote-id #'equal))
               rows))
             collect mailbox)
            #'chidu-store-mailbox-less-p))))
    (if scope-id
        (let ((scoped
               (cl-find scope-id common
                        :key #'chidu-store-mailbox-mailbox-id
                        :test #'equal)))
          (if scoped
              (vector scoped)
            (user-error
             "Selected Emails are not all movable from the search Mailbox")))
      common)))

(defun chidu-search--read-source-mailbox
    (state rows mailboxes &optional preferred-role)
  "Resolve a common source Mailbox for ROWS in STATE.

MAILBOXES is the current Account projection.  Prefer PREFERRED-ROLE when one
common candidate has it; otherwise prompt when the source is ambiguous."
  (let* ((candidates
          (chidu-search--common-source-mailboxes state rows mailboxes))
         (preferred
          (and preferred-role
               (cl-find preferred-role candidates
                        :key #'chidu-store-mailbox-role
                        :test #'equal))))
    (or preferred
        (pcase (length candidates)
          (0 (user-error "Selected Emails share no movable Mailbox"))
          (1 (aref candidates 0))
          (_ (chidu-mailbox-move-read-mailbox
              "Move from: " candidates))))))

(defun chidu-search--with-mailboxes (view continuation)
  "Call CONTINUATION with current Account Mailboxes for Search VIEW."
  (let ((state (chidu-search--state view)))
    (chidu-view-operation-start
     view 'mailboxes
     (lambda (runtime success-function error-function)
       (chidu-runtime-list-mailboxes
        runtime (chidu-search-state-account state)
        success-function error-function))
     (lambda (context)
       (let ((mailboxes
              (chidu-store-mailbox-sync-context-mailboxes context)))
         (setf (chidu-search-state-mailboxes state) mailboxes)
         (funcall continuation mailboxes)))
     (lambda (failure)
       (message "Chidu: %s" (chidu-runtime-error-message failure))))))

(defun chidu-search-archive ()
  "Move selected Search Emails from a common source to Archive."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Search view")))
         (state (chidu-search--state view))
         (rows (chidu-search--selected-rows state))
         (ids
          (vconcat
           (mapcar
            (lambda (row)
              (chidu-store-email-summary-row-local-email-id
               (chidu-store-search-row-summary-row row)))
            rows))))
    (chidu-search--with-mailboxes
     view
     (lambda (mailboxes)
       (let* ((source
               (chidu-search--read-source-mailbox
                state rows mailboxes "inbox"))
              (destination
               (chidu-mailbox-move-role-destination
                mailboxes source "archive")))
         (chidu-move-emails
          (appkit-surface-app view)
          (chidu-search-state-account state)
          source destination ids
          #'ignore
          (lambda (failure)
            (message "Chidu: %s"
                     (chidu-runtime-error-message failure)))))))))

(defun chidu-search-flag-trash ()
  "Flag the active region or Search Email for a later move to Trash.

This is a view-local Dired-style marker edit and performs no server mutation."
  (interactive)
  (chidu-selection-flag-trash))

(defun chidu-search--trash-ids (view state ids)
  "Move exact Search IDS to Trash using VIEW and STATE."
  (unless (and (vectorp ids) (> (length ids) 0))
    (user-error "No Email selected"))
  (chidu-search--with-mailboxes
   view
   (lambda (mailboxes)
     (chidu-trash-emails
      (appkit-surface-app view)
      (chidu-search-state-account state)
      (chidu-trash-role-mailbox mailboxes)
      ids #'ignore
      (lambda (failure)
        (message "Chidu: %s"
                 (chidu-runtime-error-message failure)))))))

(defun chidu-search-execute-trash-flags ()
  "Confirm and move all `D'-flagged Search Emails to Trash."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Search view")))
         (state (chidu-search--state view))
         (flagged (chidu-selection-trash-flagged-ids))
         (count (length flagged)))
    (when (zerop count)
      (user-error "No Emails are flagged for Trash"))
    (when (yes-or-no-p
           (format "Move %d flagged Email%s to Trash? "
                   count (if (= count 1) "" "s")))
      (chidu-search--trash-ids view state (vconcat flagged)))))

(defun chidu-search-move ()
  "Move selected Search Emails between chosen common Mailboxes."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Search view")))
         (state (chidu-search--state view))
         (rows (chidu-search--selected-rows state))
         (ids
          (vconcat
           (mapcar
            (lambda (row)
              (chidu-store-email-summary-row-local-email-id
               (chidu-store-search-row-summary-row row)))
            rows))))
    (chidu-search--with-mailboxes
     view
     (lambda (mailboxes)
       (let* ((source
               (chidu-search--read-source-mailbox state rows mailboxes))
              (destination
               (chidu-mailbox-move-read-mailbox
                "Move to: "
                (chidu-mailbox-move-destinations mailboxes source))))
         (chidu-move-emails
          (appkit-surface-app view)
          (chidu-search-state-account state)
          source destination ids
          #'ignore
          (lambda (failure)
            (message "Chidu: %s"
                     (chidu-runtime-error-message failure)))))))))

(defun chidu-search-edit ()
  "Read and open another query with the current account and Mailbox scope."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "No live Chidu Search view")))
         (state (chidu-search--state view))
         (spec (chidu-search-state-spec state))
         (scope
          (when-let* ((mailbox-id (chidu-search-spec-mailbox-id spec)))
            (cl-find mailbox-id (chidu-search-state-mailboxes state)
                     :key #'chidu-store-mailbox-mailbox-id
                     :test #'equal))))
    (chidu-search-read
     (appkit-surface-app view)
     (chidu-search-state-account state)
     (chidu-search-state-mailboxes state)
     scope
     (chidu-search-spec-query-text spec))))

(defvar-keymap chidu-search-mode-map
  :doc "Keymap for `chidu-search-mode'."
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
  "a" #'chidu-search-archive
  "d" #'chidu-search-flag-trash
  "x" #'chidu-search-execute-trash-flags
  "V" #'chidu-search-move
  "RET" #'chidu-search-open-conversation
  "o" #'chidu-search-open-message
  "g" #'chidu-search-refresh
  "+" #'chidu-search-load-more
  "S" #'chidu-search-edit
  "n" #'chidu-search-next
  "p" #'chidu-search-previous
  "q" #'quit-window)

(define-derived-mode chidu-search-mode special-mode "Chidu-Search"
  "Major mode for a local-first bounded Email search."
  (setq-local truncate-lines t
              chidu-seen-target-function #'chidu-search--seen-target
              chidu-seen-targets-function #'chidu-search--seen-targets))

(defun chidu-search--renderer (surface)
  "Create the native projection Renderer for SURFACE."
  (setq-local chidu-search--view surface)
  (chidu-selection-setup
   'chidu-search-email-id
   (apply-partially #'chidu-search--request-sync surface nil)
   #'chidu-search-next #'chidu-search-previous)
  
  (appkit-projection-renderer-create
   :project-all (lambda (_surface _app model)
                  (chidu-search--project-rows model))
   :project-frame (lambda (_surface _app model)
                    (cons (chidu-search--header model) ""))
   :printer (lambda (_surface _app row) (chidu-search--print-row row))
   :anchor-property 'chidu-search-email-id
   :no-separator-p t))

(defun chidu-search-open (app account mailboxes spec &optional select)
  "Open APP's local search SPEC for ACCOUNT and MAILBOXES.

Select its buffer when SELECT is non-nil."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (vectorp mailboxes)
    (signal 'wrong-type-argument (list 'vectorp mailboxes)))
  (unless (chidu-search-spec-p spec)
    (signal 'wrong-type-argument (list 'chidu-search-spec-p spec)))
  (let* ((view-id
          (list 'search
                (chidu-store-account-account-id account)
                (chidu-search-spec-query-key spec)))
         (existing (appkit-app-surface app view-id))
         (view
          (or existing
              (appkit-open-generated-surface
               (appkit-surface-type-create
                :name 'chidu-search
                :mode #'chidu-search-mode
                :init (lambda (_context input)
                        (appkit-next :model input
                                     :render (appkit-projection-change-create :full-p t :frame-p t)))
                :update #'chidu-search--update
                :renderer-factory #'chidu-search--renderer)
               :app app :identity view-id
               :buffer-name (format "*Chidu Search: %s*" (chidu-search-spec-query-text spec))
               :input (chidu-search-state-create :account account :mailboxes mailboxes :spec spec)))))
    (unless existing
      (with-current-buffer (appkit-surface-buffer view)
        (appkit-surface-enable-responsive-geometry
         view
         (lambda (owner _width)
           (chidu-post-surface-message
            owner (list 'chidu-refresh
                        (appkit-projection-change-create
                         :geometry-p t :frame-p t)))))
        (chidu-search--load-local view t)))
    (when select (pop-to-buffer (appkit-surface-buffer view)))
    (when existing
      (with-current-buffer (appkit-surface-buffer view)
        (let ((state (chidu-search--state view)))
          (setf (chidu-search-state-account state) account
                (chidu-search-state-mailboxes state) mailboxes
                (chidu-search-state-spec state) spec))
        (chidu-search--load-local view nil)))
    (appkit-surface-buffer view)))

(defun chidu-search-read
    (app account mailboxes &optional mailbox initial-query)
  "Read a query and open it in APP for ACCOUNT and MAILBOXES.

When MAILBOX is non-nil, force the query into that Mailbox.  INITIAL-QUERY is
the minibuffer initial input."
  (let* ((query
          (read-string
           (if mailbox
               (format "Search %s: " (chidu-store-mailbox-name mailbox))
             "Search mail: ")
           initial-query 'chidu-search-history))
         (spec (chidu-search-query-compile query mailboxes mailbox)))
    (chidu-search-open app account mailboxes spec t)))

(provide 'chidu-search)

;;; chidu-search.el ends here
