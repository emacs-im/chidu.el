;;; chidu-evil.el --- Native Evil bindings for Chidu -*- lexical-binding: t; -*-

;;; Commentary:

;; Chidu's ordinary major-mode maps remain the Emacs-state interface.  This
;; optional integration follows Appkit, evil-collection, emacs-qq, Disco, and
;; Chirp: install deliberate application actions in Evil state maps without
;; placing an overriding application map above Evil.
;;
;; Summary and Search are read-only list modes.  Their process-mark vocabulary
;; follows evil-collection's Gnus/tablist conventions (`m', `u', `U', `t',
;; `M', and `~'), while `d' and `x' follow evil-collection Dired: flag for
;; Trash, then explicitly execute the flagged set.  Visual selections use the
;; same region target as Emacs state.  `?' opens Chidu's contextual Transient
;; command menu; `/` remains native forward search.

;;; Code:

(require 'appkit-evil)
(require 'chidu-browse)
(declare-function chidu-dispatch "chidu-transient" ())

(declare-function appkit-directory-activate "appkit-directory" ())
(declare-function appkit-directory-next-item "appkit-directory" ())
(declare-function appkit-directory-previous-item "appkit-directory" ())
(declare-function appkit-directory-tab-dwim "appkit-directory" ())
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
(declare-function chidu-conversation-evil-tab-dwim
                  "chidu-conversation" ())
(declare-function chidu-conversation-toggle-replies "chidu-conversation" ())
(declare-function chidu-attachment-toggle-inline-at-point-exact
                  "chidu-attachment" ())
(declare-function chidu-home-sync-account "chidu-root" ())
(declare-function chidu-contacts "chidu-address-books" (&optional endpoint))
(declare-function chidu-address-books-refresh "chidu-address-books" (&optional view))
(declare-function chidu-contacts-refresh "chidu-contacts" (&optional view))
(declare-function chidu-contacts-load-more "chidu-contacts" ())
(declare-function chidu-contacts-search "chidu-contacts" (query))
(declare-function chidu-contacts-open-contact "chidu-contacts" ())
(declare-function chidu-contacts-compose "chidu-contacts" ())
(declare-function chidu-contacts-next "chidu-contacts" ())
(declare-function chidu-contacts-previous "chidu-contacts" ())
(declare-function chidu-contact-view-refresh "chidu-contact-view" (&optional view))
(declare-function chidu-contact-view-compose "chidu-contact-view" ())
(declare-function chidu-drafts-open-draft "chidu-drafts" ())
(declare-function chidu-drafts-refresh "chidu-drafts" (&optional view))
(declare-function chidu-drafts-load-more "chidu-drafts" (&optional view))
(declare-function chidu-drafts-next "chidu-drafts" ())
(declare-function chidu-drafts-previous "chidu-drafts" ())
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
(declare-function chidu-search-mail "chidu" (&optional account))
(declare-function chidu-search-next "chidu-search" ())
(declare-function chidu-search-open-conversation "chidu-search" ())
(declare-function chidu-search-open-message "chidu-search" ())
(declare-function chidu-search-previous "chidu-search" ())
(declare-function chidu-search-refresh "chidu-search" (&optional view))
(declare-function chidu-search-flag-trash "chidu-search" ())
(declare-function chidu-search-execute-trash-flags "chidu-search" ())
(declare-function chidu-selection-clear "chidu-selection" ())
(declare-function chidu-selection-flag-trash "chidu-selection" ())
(declare-function chidu-selection-mark "chidu-selection" ())
(declare-function chidu-selection-mark-all "chidu-selection" ())
(declare-function chidu-selection-next-marked "chidu-selection" ())
(declare-function chidu-selection-previous-marked "chidu-selection" ())
(declare-function chidu-selection-toggle "chidu-selection" ())
(declare-function chidu-selection-toggle-all "chidu-selection" ())
(declare-function chidu-selection-unmark "chidu-selection" (&optional all-p))
(declare-function chidu-summary-archive "chidu-summary" ())
(declare-function chidu-summary-load-more "chidu-summary" (&optional view))
(declare-function chidu-summary-next "chidu-summary" ())
(declare-function chidu-summary-open-conversation "chidu-summary" ())
(declare-function chidu-summary-open-message "chidu-summary" ())
(declare-function chidu-summary-previous "chidu-summary" ())
(declare-function chidu-summary-refresh "chidu-summary" (&optional view))
(declare-function chidu-summary-search "chidu-summary" ())
(declare-function chidu-summary-flag-trash "chidu-summary" ())
(declare-function chidu-summary-execute-trash-flags "chidu-summary" ())
(declare-function chidu-toggle-read "chidu-seen" ())

(defgroup chidu-evil nil
  "Optional native Evil integration for Chidu."
  :group 'chidu
  :prefix "chidu-evil-")

(defcustom chidu-evil-enable-integration t
  "If non-nil, install Chidu's Evil bindings automatically."
  :type 'boolean
  :group 'chidu-evil)

(defcustom chidu-evil-initial-state 'normal
  "Initial Evil state used for Chidu application buffers.

When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Emacs" emacs)
          (symbol :tag "Custom state"))
  :group 'chidu-evil)

(defconst chidu-evil--application-modes
  '(chidu-home-mode
    chidu-summary-mode
    chidu-search-mode
    chidu-conversation-mode
    chidu-message-mode
    chidu-parsed-message-mode
    chidu-address-books-mode
    chidu-contacts-mode
    chidu-contact-view-mode
    chidu-drafts-mode)
  "Major modes participating in Chidu's Evil integration.")

(defconst chidu-evil--editable-modes
  '(chidu-compose-mode)
  "Editable Chidu modes with native Evil text-editing semantics.")

(defconst chidu-evil--readonly-maps
  '(chidu-home-mode-map
    chidu-summary-mode-map
    chidu-search-mode-map
    chidu-conversation-mode-map
    chidu-message-mode-map
    chidu-parsed-message-mode-map
    chidu-address-books-mode-map
    chidu-contacts-mode-map
    chidu-contact-view-mode-map
    chidu-drafts-mode-map)
  "Read-only Chidu keymaps with standard modal quit semantics.")

(defconst chidu-evil--application-states '(normal motion)
  "Evil states used by Chidu application navigation bindings.")

(defun chidu-evil--set-initial-states ()
  "Register initial Evil states for Chidu browsing and editing modes."
  (appkit-evil-set-initial-states
   chidu-evil--application-modes chidu-evil-initial-state)
  (appkit-evil-set-initial-states chidu-evil--editable-modes 'insert))

(defun chidu-evil--define-list-keys
    (map open-command open-message-command refresh-command more-command
         search-command archive-command flag-trash-command
         execute-trash-command next-command previous-command)
  "Install list interaction bindings in MAP.

OPEN-COMMAND, OPEN-MESSAGE-COMMAND, REFRESH-COMMAND, MORE-COMMAND,
SEARCH-COMMAND, ARCHIVE-COMMAND, FLAG-TRASH-COMMAND,
EXECUTE-TRASH-COMMAND, NEXT-COMMAND, and PREVIOUS-COMMAND provide the
surface-specific actions."
  ;; Navigation and contextual actions remain available in normal and motion
  ;; state.  Native `j'/`k', `gg'/`G', `/`, operators, and visual entry remain
  ;; untouched.
  (appkit-evil-define-keys chidu-evil--application-states map
    "?" #'chidu-dispatch
    "RET" open-command
    "<return>" open-command
    "g r" refresh-command
    "g +" more-command
    "g s" #'chidu-toggle-read
    "g f" search-command
    "g j" next-command
    "g k" previous-command
    "o" open-message-command)

  ;; These are the standard read-only list semantics used throughout
  ;; evil-collection.  A single mark edit advances; a visual selection applies
  ;; to all intersecting rows.
  (appkit-evil-define-keys 'normal map
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
    "a" archive-command
    "d" flag-trash-command
    "x" execute-trash-command)

  (appkit-evil-define-keys 'visual map
    "?" #'chidu-dispatch
    "m" #'chidu-selection-mark
    "u" #'chidu-selection-unmark
    "t" #'chidu-selection-toggle
    "!" #'chidu-mark-read
    "R" #'chidu-mark-unread
    "a" archive-command
    "d" flag-trash-command
    "g s" #'chidu-toggle-read))

(defun chidu-evil--define-keys ()
  "Install shared and surface-specific Chidu bindings."
  (dolist (map chidu-evil--readonly-maps)
    (appkit-evil-define-readonly-keys map))

  (appkit-evil-map
    (:map chidu-home-mode-map
     :nm
     "?" #'chidu-dispatch
     "RET" #'appkit-directory-activate
     "<return>" #'appkit-directory-activate
     "TAB" #'appkit-directory-tab-dwim
     "<tab>" #'appkit-directory-tab-dwim
     "<backtab>" #'appkit-directory-previous-item
     "g r" #'chidu-refresh
     "g R" #'chidu-restart
     "g s" #'chidu-home-sync-account
     "g f" #'chidu-search-mail
     "g a" #'chidu-contacts
     "g j" #'appkit-directory-next-item
     "g k" #'appkit-directory-previous-item))

  (chidu-evil--define-list-keys
   'chidu-summary-mode-map
   #'chidu-summary-open-conversation
   #'chidu-summary-open-message
   #'chidu-summary-refresh
   #'chidu-summary-load-more
   #'chidu-summary-search
   #'chidu-summary-archive
   #'chidu-summary-flag-trash
   #'chidu-summary-execute-trash-flags
   #'chidu-summary-next
   #'chidu-summary-previous)

  (chidu-evil--define-list-keys
   'chidu-search-mode-map
   #'chidu-search-open-conversation
   #'chidu-search-open-message
   #'chidu-search-refresh
   #'chidu-search-load-more
   #'chidu-search-edit
   #'chidu-search-archive
   #'chidu-search-flag-trash
   #'chidu-search-execute-trash-flags
   #'chidu-search-next
   #'chidu-search-previous)

  (appkit-evil-map
    (:map chidu-address-books-mode-map
     :nm
     "?" #'chidu-dispatch
     "RET" #'appkit-directory-activate
     "<return>" #'appkit-directory-activate
     "TAB" #'appkit-directory-tab-dwim
     "<tab>" #'appkit-directory-tab-dwim
     "<backtab>" #'appkit-directory-previous-item
     "g r" #'chidu-address-books-refresh
     "g j" #'appkit-directory-next-item
     "g k" #'appkit-directory-previous-item)
    (:map chidu-contacts-mode-map
     :nm
     "?" #'chidu-dispatch
     "RET" #'chidu-contacts-open-contact
     "<return>" #'chidu-contacts-open-contact
     "g r" #'chidu-contacts-refresh
     "g +" #'chidu-contacts-load-more
     "g f" #'chidu-contacts-search
     "g j" #'chidu-contacts-next
     "g k" #'chidu-contacts-previous
     "c" #'chidu-contacts-compose)
    (:map chidu-contact-view-mode-map
     :nm
     "?" #'chidu-dispatch
     "g r" #'chidu-contact-view-refresh
     "c" #'chidu-contact-view-compose)
    (:map chidu-drafts-mode-map
     :nm
     "?" #'chidu-dispatch
     "RET" #'chidu-drafts-open-draft
     "<return>" #'chidu-drafts-open-draft
     "g r" #'chidu-drafts-refresh
     "g +" #'chidu-drafts-load-more
     "g j" #'chidu-drafts-next
     "g k" #'chidu-drafts-previous)
    (:map chidu-conversation-mode-map
     :nm
     "?" #'chidu-dispatch
     "RET" #'chidu-activate-at-point
     "<return>" #'chidu-activate-at-point
     ;; Bind only the distinct GUI <tab> event so ordinary TAB/C-i keeps Evil's
     ;; jump-list meaning.  The DWIM checks an exact attachment card before the
     ;; surrounding Email body fold.
     "<tab>" #'chidu-conversation-evil-tab-dwim
     "!" #'chidu-mark-read
     "R" #'chidu-mark-unread
     "g r" #'chidu-conversation-refresh
     "g o" #'chidu-browse-at-point
     "g s" #'chidu-toggle-read
     "g j" #'chidu-conversation-next-entry
     "g k" #'chidu-conversation-previous-entry
     "o" #'chidu-conversation-open-standalone
     :n
     "z a" #'chidu-conversation-toggle-replies
     "z c" #'chidu-conversation-close-replies
     "z o" #'chidu-conversation-open-replies)
    (:map chidu-message-mode-map
     :nm
     "?" #'chidu-dispatch
     "RET" #'chidu-activate-at-point
     "<return>" #'chidu-activate-at-point
     "!" #'chidu-mark-read
     "R" #'chidu-mark-unread
     "<tab>" #'chidu-attachment-toggle-inline-at-point-exact
     "g r" #'chidu-message-refresh
     "g o" #'chidu-browse-at-point
     "g s" #'chidu-toggle-read)
    (:map chidu-parsed-message-mode-map
     :nm
     "?" #'chidu-dispatch
     "RET" #'chidu-activate-at-point
     "<return>" #'chidu-activate-at-point
     "<tab>" #'chidu-attachment-toggle-inline-at-point-exact
     "g r" #'chidu-parsed-message-refresh
     "g o" #'chidu-browse-at-point)))

(defun chidu-evil--refresh-live-buffers ()
  "Refresh Evil projections in existing Chidu application buffers."
  (appkit-evil-normalize-buffers
   (append chidu-evil--application-modes chidu-evil--editable-modes)))

;;;###autoload
(defun chidu-evil-setup ()
  "Install Chidu's native Evil integration.

Safe to call multiple times."
  (interactive)
  (when (and (featurep 'evil) chidu-evil-enable-integration)
    (chidu-evil--set-initial-states)
    (chidu-evil--define-keys)
    (chidu-evil--refresh-live-buffers)))

(with-eval-after-load 'evil
  (chidu-evil-setup))

(provide 'chidu-evil)

;;; chidu-evil.el ends here
