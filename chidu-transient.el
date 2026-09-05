;;; chidu-transient.el --- Contextual command menus for Chidu -*- lexical-binding: t; -*-

;;; Commentary:

;; Contextual Transient menus are the discoverable command surface for Chidu.
;; Direct list-mode keys remain available for frequent mark and mail actions,
;; while `?' exposes the complete operation vocabulary and its current target.
;; Summary and Search share one list menu because both use the same
;; ordinary-process-marks > region > point target rule.  Trash flags form a
;; separate Dired-style plan consumed only by the explicit execute suffix.
;;
;; An active region is captured as stable Email ids when the list menu opens.
;; This follows the immutable-plan pattern used by emacs-qq: Transient may
;; change command-loop state, but a later mutation still acts on the exact rows
;; the user selected when opening the menu.  Ordinary marks, Trash flags, and
;; point remain dynamic because they are stable throughout the menu and marker
;; suffixes intentionally edit them in place.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'appkit-media-card)

(declare-function appkit-directory-activate "appkit-directory" ())
(declare-function chidu-conversation-close-replies "chidu-conversation" ())
(declare-function chidu-conversation-focus "chidu-conversation" ())
(declare-function chidu-conversation-next-entry "chidu-conversation" ())
(declare-function chidu-conversation-open-replies "chidu-conversation" ())
(declare-function chidu-conversation-open-standalone
                  "chidu-conversation" ())
(declare-function chidu-conversation-previous-entry "chidu-conversation" ())
(declare-function chidu-conversation-refresh
                  "chidu-conversation" (&optional view))
(declare-function chidu-conversation-toggle-body "chidu-conversation" ())
(declare-function chidu-conversation-toggle-replies "chidu-conversation" ())
(declare-function chidu-attachment-inline-toggle-available-p
                  "chidu-attachment" (&optional exact-p))
(declare-function chidu-attachment-toggle-inline-at-point
                  "chidu-attachment" ())
(declare-function chidu-home-sync-account "chidu-root" ())
(declare-function chidu-contacts "chidu-address-books" (&optional endpoint))
(declare-function chidu-address-books-refresh "chidu-address-books" (&optional view))
(declare-function chidu-contacts-refresh "chidu-contacts" (&optional view))
(declare-function chidu-contacts-load-more "chidu-contacts" ())
(declare-function chidu-contacts-search "chidu-contacts" (query))
(declare-function chidu-contacts-open-contact "chidu-contacts" ())
(declare-function chidu-contacts-compose "chidu-contacts" ())
(declare-function chidu-contact-view-refresh "chidu-contact-view" (&optional view))
(declare-function chidu-contact-view-compose "chidu-contact-view" ())
(declare-function chidu-compose "chidu-compose" (&optional account))
(declare-function chidu-open-compose-workspace "chidu-compose" ())
(declare-function chidu-compose-save-draft "chidu-compose" ())
(declare-function chidu-compose-checkpoint "chidu-compose" ())
(declare-function chidu-compose-close "chidu-compose" ())
(declare-function chidu-compose-discard-workspace "chidu-compose" ())
(declare-function chidu-drafts-open-draft "chidu-drafts" ())
(declare-function chidu-drafts-refresh "chidu-drafts" (&optional view))
(declare-function chidu-drafts-load-more "chidu-drafts" (&optional view))
(declare-function chidu-home-index-account "chidu-root" ())
(declare-function chidu-home-cancel-email-index "chidu-root" ())
(declare-function chidu-mark-read "chidu-seen" ())
(declare-function chidu-mark-unread "chidu-seen" ())
(declare-function chidu-message-refresh "chidu-message" (&optional view))
(declare-function chidu-parsed-message-refresh
                  "chidu-parsed-message" (&optional view))
(declare-function chidu-refresh "chidu" ())
(declare-function chidu-restart "chidu" ())
(declare-function chidu-search-archive "chidu-search" ())
(declare-function chidu-search-edit "chidu-search" ())
(declare-function chidu-search-load-more "chidu-search" (&optional view))
(declare-function chidu-search-load-more-available-p
                  "chidu-search" (&optional view))
(declare-function chidu-search-mail "chidu" (&optional account))
(declare-function chidu-search-move "chidu-search" ())
(declare-function chidu-search-open-conversation "chidu-search" ())
(declare-function chidu-search-open-message "chidu-search" ())
(declare-function chidu-search-refresh "chidu-search" (&optional view))
(declare-function chidu-search-flag-trash "chidu-search" ())
(declare-function chidu-search-execute-trash-flags "chidu-search" ())
(declare-function chidu-seen-target-at-point "chidu-seen" ())
(declare-function chidu-selection-clear "chidu-selection" ())
(declare-function chidu-selection-marker-count "chidu-selection" ())
(declare-function chidu-selection-trash-flag-count "chidu-selection" ())
(declare-function chidu-selection-description "chidu-selection" ())
(declare-function chidu-selection-editable-p "chidu-selection" ())
(declare-function chidu-selection-mark "chidu-selection" ())
(declare-function chidu-selection-mark-all "chidu-selection" ())
(declare-function chidu-selection-next-marked "chidu-selection" ())
(declare-function chidu-selection-previous-marked "chidu-selection" ())
(declare-function chidu-selection-row-id-at-point "chidu-selection" ())
(declare-function chidu-selection-selected-ids
                  "chidu-selection" (&optional ignore))
(declare-function chidu-selection-target-source "chidu-selection" ())
(declare-function chidu-selection-toggle "chidu-selection" ())
(declare-function chidu-selection-toggle-all "chidu-selection" ())
(declare-function chidu-selection-unmark "chidu-selection" (&optional all-p))
(declare-function chidu-selection-visible-ids "chidu-selection" ())
(declare-function chidu-summary-archive "chidu-summary" ())
(declare-function chidu-summary-load-more "chidu-summary" (&optional view))
(declare-function chidu-summary-load-more-available-p
                  "chidu-summary" (&optional view))
(declare-function chidu-summary-move "chidu-summary" ())
(declare-function chidu-summary-open-conversation "chidu-summary" ())
(declare-function chidu-summary-open-message "chidu-summary" ())
(declare-function chidu-summary-refresh "chidu-summary" (&optional view))
(declare-function chidu-summary-search "chidu-summary" ())
(declare-function chidu-summary-flag-trash "chidu-summary" ())
(declare-function chidu-summary-execute-trash-flags "chidu-summary" ())
(declare-function chidu-summary-trash-staging-available-p
                  "chidu-summary" (&optional view))
(declare-function chidu-toggle-read "chidu-seen" ())

(defvar chidu-selection-command-ids)

(cl-defstruct (chidu-transient-list-scope
               (:constructor chidu-transient-list-scope-create))
  "Immutable region plans and live source BUFFER for one list menu."
  buffer
  region-ids
  edit-region-ids)

(defun chidu-transient--capture-list-scope ()
  "Capture the current list buffer and any active-region plans."
  (unless (memq major-mode '(chidu-summary-mode chidu-search-mode))
    (user-error "This command requires a Chidu list buffer"))
  (let* ((edit-region-ids
          (when (use-region-p)
            (copy-sequence (chidu-selection-selected-ids 'marks))))
         (source (chidu-selection-target-source)))
    (chidu-transient-list-scope-create
     :buffer (current-buffer)
     ;; Mail operations preserve ordinary marks > region > point.
     :region-ids (and (eq source 'region) edit-region-ids)
     ;; Marker edits always honor an explicit region, even when ordinary
     ;; process marks already own the mail-operation target.
     :edit-region-ids edit-region-ids)))

(defun chidu-transient--list-scope ()
  "Return the active `chidu-list-transient' scope, or nil."
  (let ((scope (transient-scope 'chidu-list-transient)))
    (and (chidu-transient-list-scope-p scope) scope)))

(defun chidu-transient--list-buffer (&optional noerror)
  "Return the live source buffer owned by the list menu.

When NOERROR is non-nil, return nil instead of signaling if the scope or its
buffer is gone.  Never fall back to an unrelated current buffer."
  (let* ((scope (chidu-transient--list-scope))
         (buffer (and scope (chidu-transient-list-scope-buffer scope))))
    (cond
     ((buffer-live-p buffer) buffer)
     (noerror nil)
     (t (user-error "Chidu: the list that opened this menu is no longer live")))))

(defun chidu-transient--captured-region-ids ()
  "Return stable mail-operation region ids captured by the list menu."
  (when-let* ((scope (chidu-transient--list-scope)))
    (chidu-transient-list-scope-region-ids scope)))

(defun chidu-transient--captured-edit-region-ids ()
  "Return stable marker-edit region ids captured by the list menu."
  (when-let* ((scope (chidu-transient--list-scope)))
    (chidu-transient-list-scope-edit-region-ids scope)))

(defun chidu-transient--list-buffer-value (function &optional fallback)
  "Call zero-argument FUNCTION in the menu's source buffer.

Return FALLBACK when the scope is stale or FUNCTION signals while formatting
the menu.  Suffix commands use `chidu-transient--list-buffer' directly and
therefore never hide execution errors."
  (if-let* ((buffer (chidu-transient--list-buffer t)))
      (condition-case nil
          (with-current-buffer buffer
            (funcall function))
        (error fallback))
    fallback))

(defun chidu-transient--list-mode-p ()
  "Return non-nil when the active menu owns a Summary or Search."
  (chidu-transient--list-buffer-value
   (lambda ()
     (memq major-mode '(chidu-summary-mode chidu-search-mode)))))

(defun chidu-transient--list-operation-ids ()
  "Return exact Email ids targeted by the active list menu."
  (or (chidu-transient--captured-region-ids)
      (with-current-buffer (chidu-transient--list-buffer)
        (chidu-selection-selected-ids))))

(defun chidu-transient--list-title ()
  "Return a dynamic title for the current list action menu."
  (or
   (chidu-transient--list-buffer-value
    (lambda ()
      (let* ((region-ids (chidu-transient--captured-region-ids))
             (trash-count (chidu-selection-trash-flag-count))
             (target
              (if region-ids
                  (format "%d Email%s in region"
                          (length region-ids)
                          (if (= 1 (length region-ids)) "" "s"))
                (chidu-selection-description))))
        (format
         "%s · %s%s"
         (if (eq major-mode 'chidu-summary-mode)
             "Chidu Mailbox"
           "Chidu Search")
         target
         (if (> trash-count 0)
             (format " · %d flagged for Trash" trash-count)
           "")))))
   "Chidu list actions"))

(defun chidu-transient--edit-target-inapt-p ()
  "Return non-nil when no region or row can edit markers."
  (or
   (not (chidu-transient--list-mode-p))
   (not
    (or (chidu-transient--captured-edit-region-ids)
        (chidu-transient--list-buffer-value
         #'chidu-selection-editable-p)))))

(defun chidu-transient--operation-target-inapt-p ()
  "Return non-nil when the list menu has no mail-operation target."
  (or (not (chidu-transient--list-mode-p))
      (null
       (chidu-transient--list-buffer-value
        #'chidu-transient--list-operation-ids))))

(defun chidu-transient--markers-empty-p ()
  "Return non-nil when the current list has no marks or Trash flags."
  (or (not (chidu-transient--list-mode-p))
      (zerop
       (chidu-transient--list-buffer-value
        #'chidu-selection-marker-count 0))))

(defun chidu-transient--trash-flags-empty-p ()
  "Return non-nil when the current list has no staged Trash flags."
  (or (not (chidu-transient--list-mode-p))
      (zerop
       (chidu-transient--list-buffer-value
        #'chidu-selection-trash-flag-count 0))))

(defun chidu-transient--visible-rows-empty-p ()
  "Return non-nil when the current list has no visible Email rows."
  (or (not (chidu-transient--list-mode-p))
      (null
       (chidu-transient--list-buffer-value
        #'chidu-selection-visible-ids))))

(defun chidu-transient--point-row-empty-p ()
  "Return non-nil when point is not on a selectable Email row."
  (or (not (chidu-transient--list-mode-p))
      (null
       (chidu-transient--list-buffer-value
        #'chidu-selection-row-id-at-point))))

(defun chidu-transient--flag-trash-inapt-p ()
  "Return non-nil when the current target cannot be flagged for Trash."
  (or
   (chidu-transient--edit-target-inapt-p)
   (chidu-transient--list-buffer-value
    (lambda ()
      (and (eq major-mode 'chidu-summary-mode)
           (not (chidu-summary-trash-staging-available-p))))
    t)))

(defun chidu-transient--load-more-inapt-p ()
  "Return non-nil when the current list cannot append another page."
  (not
   (chidu-transient--list-buffer-value
    (lambda ()
      (pcase major-mode
        ('chidu-summary-mode (chidu-summary-load-more-available-p))
        ('chidu-search-mode (chidu-search-load-more-available-p))
        (_ nil))))))

(defun chidu-transient--trash-execute-description ()
  "Return a count-aware description for staged Trash execution."
  (let ((count
         (chidu-transient--list-buffer-value
          #'chidu-selection-trash-flag-count 0)))
    (if (> count 0)
        (format "Move %d flagged Email%s to Trash"
                count (if (= count 1) "" "s"))
      "Move flagged to Trash")))

(defun chidu-transient--email-at-point-inapt-p ()
  "Return non-nil when explicit read-state commands have no Email."
  (condition-case nil
      (progn (chidu-seen-target-at-point) nil)
    (error t)))

(defun chidu-transient--media-action-inapt-p (action)
  "Return non-nil when media-card ACTION is unavailable at point."
  (appkit-media-card-action-inapt-reason action))

(defun chidu-transient--media-open-inapt-p ()
  "Return non-nil when no attachment can be opened at point."
  (chidu-transient--media-action-inapt-p 'open))

(defun chidu-transient--media-download-inapt-p ()
  "Return non-nil when no attachment can be downloaded at point."
  (chidu-transient--media-action-inapt-p 'download))

(defun chidu-transient--media-cancel-inapt-p ()
  "Return non-nil when no attachment download can be canceled at point."
  (chidu-transient--media-action-inapt-p 'cancel))

(defun chidu-transient--media-save-inapt-p ()
  "Return non-nil when no attachment can be saved at point."
  (chidu-transient--media-action-inapt-p 'save-as))

(defun chidu-transient--media-inline-inapt-p ()
  "Return non-nil when no bounded attachment can toggle inline."
  (not (chidu-attachment-inline-toggle-available-p)))

(defun chidu-transient--list-surface-command (summary-command search-command)
  "Return SUMMARY-COMMAND or SEARCH-COMMAND for the current list surface."
  (with-current-buffer (chidu-transient--list-buffer)
    (pcase major-mode
      ('chidu-summary-mode summary-command)
      ('chidu-search-mode search-command)
      (_ (user-error "This command requires a Chidu list buffer")))))

(defun chidu-transient--call-list-command (summary-command search-command)
  "Call the surface command selected from SUMMARY-COMMAND and SEARCH-COMMAND."
  (with-current-buffer (chidu-transient--list-buffer)
    (call-interactively
     (chidu-transient--list-surface-command
      summary-command search-command))))

(defun chidu-transient--call-list-edit (command)
  "Call marker-edit COMMAND with the scope's immutable region plan."
  (with-current-buffer (chidu-transient--list-buffer)
    (let ((chidu-selection-command-ids
           (chidu-transient--captured-edit-region-ids)))
      (call-interactively command))))

(defun chidu-transient--call-list-operation (summary-command search-command)
  "Call SUMMARY-COMMAND or SEARCH-COMMAND with the scope's exact target ids."
  (with-current-buffer (chidu-transient--list-buffer)
    (let ((chidu-selection-command-ids
           (copy-sequence (chidu-transient--list-operation-ids))))
      (call-interactively
       (chidu-transient--list-surface-command
        summary-command search-command)))))

(transient-define-suffix chidu-list-mark-selection ()
  "Mark the active menu region or row at point."
  :key "m"
  :description "Mark"
  :transient t
  :inapt-if #'chidu-transient--edit-target-inapt-p
  (interactive)
  (chidu-transient--call-list-edit #'chidu-selection-mark))

(transient-define-suffix chidu-list-unmark-selection ()
  "Unmark the active menu region or row at point."
  :key "u"
  :description "Unmark"
  :transient t
  :inapt-if #'chidu-transient--edit-target-inapt-p
  (interactive)
  (chidu-transient--call-list-edit #'chidu-selection-unmark))

(transient-define-suffix chidu-list-toggle-selection ()
  "Toggle ordinary marks for the active menu region or row."
  :key "t"
  :description "Toggle mark"
  :transient t
  :inapt-if #'chidu-transient--edit-target-inapt-p
  (interactive)
  (chidu-transient--call-list-edit #'chidu-selection-toggle))

(transient-define-suffix chidu-list-mark-all-visible ()
  "Mark every visible row in the active list."
  :key "M"
  :description "Mark all visible"
  :transient t
  :inapt-if #'chidu-transient--visible-rows-empty-p
  (interactive)
  (chidu-transient--call-list-edit #'chidu-selection-mark-all))

(transient-define-suffix chidu-list-clear-markers ()
  "Clear every ordinary mark and Trash flag in the active list."
  :key "U"
  :description "Clear all marks / flags"
  :transient t
  :inapt-if #'chidu-transient--markers-empty-p
  (interactive)
  (chidu-transient--call-list-edit #'chidu-selection-clear))

(transient-define-suffix chidu-list-toggle-all-visible ()
  "Toggle ordinary marks across all visible rows."
  :key "~"
  :description "Toggle all visible"
  :transient t
  :inapt-if #'chidu-transient--visible-rows-empty-p
  (interactive)
  (chidu-transient--call-list-edit #'chidu-selection-toggle-all))

(transient-define-suffix chidu-list-previous-marker ()
  "Move to the previous visible marker."
  :key "{"
  :description "Previous marker"
  :transient t
  :inapt-if #'chidu-transient--markers-empty-p
  (interactive)
  (chidu-transient--call-list-edit #'chidu-selection-previous-marked))

(transient-define-suffix chidu-list-next-marker ()
  "Move to the next visible marker."
  :key "}"
  :description "Next marker"
  :transient t
  :inapt-if #'chidu-transient--markers-empty-p
  (interactive)
  (chidu-transient--call-list-edit #'chidu-selection-next-marked))

(transient-define-suffix chidu-list-mark-read ()
  "Mark the active list menu target as read."
  :key "r"
  :description "Mark read"
  :inapt-if #'chidu-transient--operation-target-inapt-p
  (interactive)
  (chidu-transient--call-list-operation
   #'chidu-mark-read #'chidu-mark-read))

(transient-define-suffix chidu-list-mark-unread ()
  "Mark the active list menu target as unread."
  :key "R"
  :description "Mark unread"
  :inapt-if #'chidu-transient--operation-target-inapt-p
  (interactive)
  (chidu-transient--call-list-operation
   #'chidu-mark-unread #'chidu-mark-unread))

(transient-define-suffix chidu-list-toggle-read ()
  "Toggle read state for the active list menu target."
  :key "s"
  :description "Toggle read"
  :inapt-if #'chidu-transient--operation-target-inapt-p
  (interactive)
  (chidu-transient--call-list-operation
   #'chidu-toggle-read #'chidu-toggle-read))

(transient-define-suffix chidu-list-archive ()
  "Archive the active list menu target."
  :key "a"
  :description "Archive"
  :inapt-if #'chidu-transient--operation-target-inapt-p
  (interactive)
  (chidu-transient--call-list-operation
   #'chidu-summary-archive #'chidu-search-archive))

(transient-define-suffix chidu-list-move ()
  "Move the active list menu target."
  :key "v"
  :description "Move…"
  :inapt-if #'chidu-transient--operation-target-inapt-p
  (interactive)
  (chidu-transient--call-list-operation
   #'chidu-summary-move #'chidu-search-move))

(transient-define-suffix chidu-list-flag-trash ()
  "Flag the active menu region or row for a later move to Trash."
  :key "d"
  :description "Flag for Trash"
  :transient t
  :inapt-if #'chidu-transient--flag-trash-inapt-p
  (interactive)
  (chidu-transient--call-list-edit
   (chidu-transient--list-surface-command
    #'chidu-summary-flag-trash #'chidu-search-flag-trash)))

(transient-define-suffix chidu-list-execute-trash-flags ()
  "Confirm and execute the active list's staged Trash flags."
  :key "x"
  :description #'chidu-transient--trash-execute-description
  :inapt-if #'chidu-transient--trash-flags-empty-p
  (interactive)
  (chidu-transient--call-list-command
   #'chidu-summary-execute-trash-flags
   #'chidu-search-execute-trash-flags))

(transient-define-suffix chidu-list-open-conversation ()
  "Open the Email at point in its Conversation."
  :key "RET"
  :description "Open Conversation"
  :inapt-if #'chidu-transient--point-row-empty-p
  (interactive)
  (chidu-transient--call-list-command
   #'chidu-summary-open-conversation
   #'chidu-search-open-conversation))

(transient-define-suffix chidu-list-open-message ()
  "Open the Email at point as a standalone message."
  :key "o"
  :description "Open standalone"
  :inapt-if #'chidu-transient--point-row-empty-p
  (interactive)
  (chidu-transient--call-list-command
   #'chidu-summary-open-message #'chidu-search-open-message))

(transient-define-suffix chidu-list-refresh ()
  "Refresh the current Summary or Search."
  :key "g"
  :description "Refresh"
  (interactive)
  (chidu-transient--call-list-command
   #'chidu-summary-refresh #'chidu-search-refresh))

(transient-define-suffix chidu-list-load-more ()
  "Append the next page to the current Summary or Search."
  :key "+"
  :description "Load more"
  :inapt-if #'chidu-transient--load-more-inapt-p
  (interactive)
  (chidu-transient--call-list-command
   #'chidu-summary-load-more #'chidu-search-load-more))

(transient-define-suffix chidu-list-search ()
  "Search the current Mailbox or edit the current Search query."
  :key "/"
  :description "Search / edit query"
  (interactive)
  (chidu-transient--call-list-command
   #'chidu-summary-search #'chidu-search-edit))

(transient-define-suffix chidu-transient-mark-read ()
  "Mark the Email at point as read."
  :key "r"
  :description "Mark read"
  :inapt-if #'chidu-transient--email-at-point-inapt-p
  (interactive)
  (call-interactively #'chidu-mark-read))

(transient-define-suffix chidu-transient-mark-unread ()
  "Mark the Email at point as unread."
  :key "R"
  :description "Mark unread"
  :inapt-if #'chidu-transient--email-at-point-inapt-p
  (interactive)
  (call-interactively #'chidu-mark-unread))

(transient-define-suffix chidu-transient-toggle-read ()
  "Toggle read state for the Email at point."
  :key "s"
  :description "Toggle read"
  :inapt-if #'chidu-transient--email-at-point-inapt-p
  (interactive)
  (call-interactively #'chidu-toggle-read))

(transient-define-suffix chidu-transient-attachment-open ()
  "Open the attachment resolved at point."
  :key "A"
  :description "Open"
  :inapt-if #'chidu-transient--media-open-inapt-p
  (interactive)
  (call-interactively #'appkit-media-card-open))

(transient-define-suffix chidu-transient-attachment-toggle-inline ()
  "Toggle inline display for the attachment resolved at point."
  :key "i"
  :description "Toggle inline"
  :inapt-if #'chidu-transient--media-inline-inapt-p
  (interactive)
  (call-interactively #'chidu-attachment-toggle-inline-at-point))

(transient-define-suffix chidu-transient-attachment-download ()
  "Download or retry the attachment resolved at point."
  :key "D"
  :description "Download / retry"
  :inapt-if #'chidu-transient--media-download-inapt-p
  (interactive)
  (call-interactively #'appkit-media-card-download))

(transient-define-suffix chidu-transient-attachment-cancel ()
  "Cancel the attachment download resolved at point."
  :key "C-d"
  :description "Cancel download"
  :inapt-if #'chidu-transient--media-cancel-inapt-p
  (interactive)
  (call-interactively #'appkit-media-card-cancel-download))

(transient-define-suffix chidu-transient-attachment-save-as ()
  "Save the attachment resolved at point to a chosen path."
  :key "S"
  :description "Save as…"
  :inapt-if #'chidu-transient--media-save-inapt-p
  (interactive)
  (call-interactively #'appkit-media-card-save-as))

(transient-define-group chidu-transient-read-actions
  ["Read state"
   (chidu-transient-mark-read)
   (chidu-transient-mark-unread)
   (chidu-transient-toggle-read)])

(transient-define-group chidu-transient-attachment-actions
  ["Attachment"
   (chidu-transient-attachment-open)
   (chidu-transient-attachment-toggle-inline)
   (chidu-transient-attachment-download)
   (chidu-transient-attachment-cancel)
   (chidu-transient-attachment-save-as)])

(transient-define-group chidu-transient-window-actions
  ["Window"
   ("q" "Quit menu" transient-quit-one)
   ("Q" "Quit window" quit-window)])

(transient-define-prefix chidu-home-transient ()
  "Command menu for the Chidu Account and Mailbox directory."
  [["Account"
    ("RET" "Open at point" appkit-directory-activate)
    ("s" "Sync Mailboxes" chidu-home-sync-account)
    ("I" "Build Email index" chidu-home-index-account)
    ("C" "Cancel Email index" chidu-home-cancel-email-index)
    ("/" "Search mail" chidu-search-mail)
    ("A" "Address Books" chidu-contacts)
    ("c" "New message" chidu-compose)
    ("d" "Resume composition" chidu-open-compose-workspace)]
   ["Application"
    ("g" "Refresh Sessions" chidu-refresh)
    ("R" "Restart Chidu" chidu-restart)]
   ["Window"
    ("q" "Quit menu" transient-quit-one)
    ("Q" "Quit window" quit-window)
    ("h" "Describe mode" describe-mode)]])

(transient-define-prefix chidu-list-transient (scope)
  "Command menu for a Chidu Summary or Search list."
  ;; Marker suffixes stay active and mutate availability.  Rebuild suffix
  ;; objects after each such command so `x', `U', and marker navigation never
  ;; use the frozen predicates from menu entry.
  :refresh-suffixes t
  :transient-non-suffix t
  [:description (lambda () (chidu-transient--list-title))]
  [["Selection"
    (chidu-list-mark-selection)
    (chidu-list-unmark-selection)
    (chidu-list-toggle-selection)
    (chidu-list-mark-all-visible)
    (chidu-list-clear-markers)
    (chidu-list-toggle-all-visible)
    (chidu-list-previous-marker)
    (chidu-list-next-marker)]
   ["Read state"
    (chidu-list-mark-read)
    (chidu-list-mark-unread)
    (chidu-list-toggle-read)]
   ["Mailbox"
    (chidu-list-archive)
    (chidu-list-move)
    (chidu-list-flag-trash)
    (chidu-list-execute-trash-flags)]
   ["View"
    (chidu-list-open-conversation)
    (chidu-list-open-message)
    (chidu-list-refresh)
    (chidu-list-load-more)
    (chidu-list-search)
    ("q" "Quit menu" transient-quit-one)
    ("Q" "Quit window" quit-window)]]
  (interactive (list (chidu-transient--capture-list-scope)))
  (unless (chidu-transient-list-scope-p scope)
    (user-error "Chidu: invalid list menu scope"))
  (transient-setup 'chidu-list-transient nil nil :scope scope))

(transient-define-prefix chidu-conversation-transient ()
  "Command menu for a Chidu Conversation."
  'chidu-transient-read-actions
  ["Email / replies"
   ("RET" "Focus Email" chidu-conversation-focus)
   ("v" "Toggle body" chidu-conversation-toggle-body)
   ("t" "Toggle replies" chidu-conversation-toggle-replies)
   ("O" "Open replies" chidu-conversation-open-replies)
   ("C" "Close replies" chidu-conversation-close-replies)
   ("o" "Open standalone" chidu-conversation-open-standalone)]
  'chidu-transient-attachment-actions
  ["Navigate"
   ("n" "Next Email" chidu-conversation-next-entry)
   ("p" "Previous Email" chidu-conversation-previous-entry)
   ("g" "Refresh" chidu-conversation-refresh)]
  'chidu-transient-window-actions)

(transient-define-prefix chidu-message-transient ()
  "Command menu for a standalone Chidu message."
  'chidu-transient-read-actions
  'chidu-transient-attachment-actions
  ["View"
   ("g" "Refresh" chidu-message-refresh)]
  'chidu-transient-window-actions)

(transient-define-prefix chidu-parsed-message-transient ()
  "Command menu for a parsed attached message."
  'chidu-transient-attachment-actions
  ["View"
   ("g" "Refresh parse" chidu-parsed-message-refresh)]
  'chidu-transient-window-actions)

(transient-define-prefix chidu-address-books-transient ()
  "Command menu for the JMAP AddressBook directory."
  [["Address Books"
    ("RET" "Open at point" appkit-directory-activate)
    ("g" "Refresh" chidu-address-books-refresh)]
   ["Window"
    ("q" "Quit menu" transient-quit-one)
    ("Q" "Quit window" quit-window)]])

(transient-define-prefix chidu-contacts-transient ()
  "Command menu for a JMAP ContactCard list."
  [["Contacts"
    ("RET" "Open contact" chidu-contacts-open-contact)
    ("c" "Compose" chidu-contacts-compose)
    ("/" "Search" chidu-contacts-search)
    ("g" "Refresh" chidu-contacts-refresh)
    ("+" "Load more" chidu-contacts-load-more)]
   ["Window"
    ("q" "Quit menu" transient-quit-one)
    ("Q" "Quit window" quit-window)]])

(transient-define-prefix chidu-contact-view-transient ()
  "Command menu for one JMAP ContactCard."
  [["Contact"
    ("c" "Compose" chidu-contact-view-compose)
    ("g" "Refresh" chidu-contact-view-refresh)]
   ["Window"
    ("q" "Quit menu" transient-quit-one)
    ("Q" "Quit window" quit-window)]])

(transient-define-prefix chidu-drafts-transient ()
  "Command menu for canonical server Drafts."
  [["Drafts"
    ("RET" "Open Draft" chidu-drafts-open-draft)
    ("g" "Refresh" chidu-drafts-refresh)
    ("+" "Load more" chidu-drafts-load-more)]
   ["Window"
    ("q" "Quit menu" transient-quit-one)
    ("Q" "Quit window" quit-window)]])

(transient-define-prefix chidu-compose-transient ()
  "Command menu for a Chidu Compose workspace."
  [["Compose"
    ("s" "Save Draft to server" chidu-compose-save-draft)
    ("l" "Update recovery copy" chidu-compose-checkpoint)
    ("a" "Attach file" chidu-compose-attach-file)
    ("d" "Remove attachment" chidu-compose-remove-attachment)
    ("i" "Change Identity" chidu-compose-change-identity)
    ("a" "Attach file" chidu-compose-attach-file)
    ("r" "Remove attachment" chidu-compose-remove-attachment)]
   ["Workspace"
    ("q" "Close" chidu-compose-close)
    ("D" "Delete local-only workspace" chidu-compose-discard-workspace)]])

;;;###autoload
(defun chidu-dispatch ()
  "Open the contextual Chidu command menu for the current buffer."
  (interactive)
  (call-interactively
   (pcase major-mode
     ('chidu-home-mode #'chidu-home-transient)
     ((or 'chidu-summary-mode 'chidu-search-mode) #'chidu-list-transient)
     ('chidu-conversation-mode #'chidu-conversation-transient)
     ('chidu-message-mode #'chidu-message-transient)
     ('chidu-parsed-message-mode #'chidu-parsed-message-transient)
     ('chidu-address-books-mode #'chidu-address-books-transient)
     ('chidu-contacts-mode #'chidu-contacts-transient)
     ('chidu-contact-view-mode #'chidu-contact-view-transient)
     ('chidu-drafts-mode #'chidu-drafts-transient)
     ('chidu-compose-mode #'chidu-compose-transient)
     (_ (user-error "No Chidu command menu for this buffer")))))

(provide 'chidu-transient)

;;; chidu-transient.el ends here
