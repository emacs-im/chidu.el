;;; chidu-conversation.el --- Appkit reply-tree Conversation interface -*- lexical-binding: t; -*-

;;; Commentary:

;; Present one JMAP Thread as a local-first reply tree.  Thread membership is
;; committed by the Store.  Full message bodies and reply subtrees have separate
;; view-local folds: v toggles one body; Emacs TAB toggles an exact inline
;; attachment before falling back to replies, while Evil's distinct <tab>
;; preserves its earlier attachment/body fold behavior.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-discussion)
(require 'appkit-surface)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'chidu-attachment)
(require 'chidu-body-sync)
(require 'chidu-conversation-sync)
(require 'chidu-message)
(require 'chidu-runtime)
(require 'chidu-seen)
(require 'chidu-store)
(require 'chidu-text)
(require 'chidu-surface-operation)

(declare-function chidu-attachment-card-at-point-p
                  "chidu-attachment" (&optional exact-p))
(declare-function chidu-attachment-inline-toggle-available-p
                  "chidu-attachment" (&optional exact-p))
(declare-function chidu-attachment-toggle-inline-at-point-exact
                  "chidu-attachment" ())
(declare-function chidu-dispatch "chidu-transient" ())

(defcustom chidu-conversation-max-display-depth 12
  "Maximum visual nesting depth in a Conversation buffer.

The Store retains full canonical reply depth.  Presentation depth is derived
from the current focused scope; deeper focus descendants remain ordered and
carry both actual and visual depth properties, but indentation is capped."
  :type 'nonnegative-integer
  :group 'chidu)

(cl-defstruct
    (chidu-conversation-state
     (:constructor chidu-conversation-state-create))
  "View-local state for one reply-tree Conversation." account mailbox
  remote-thread-id focus-local-email-id context (phase 'initial)
  message (visible-bodies (make-hash-table :test #'equal))
  (collapsed-replies (make-hash-table :test #'equal))
  (body-contexts (make-hash-table :test #'equal))
  (body-phases (make-hash-table :test #'equal))
  (body-messages (make-hash-table :test #'equal)) media-phase
  media-message media-key)

(defun chidu-conversation--set-media-phase (model phase &optional problem key)
  "Commit media PHASE, PROBLEM and pending open KEY to reader MODEL."
  (setf (chidu-conversation-state-media-phase model) phase
        (chidu-conversation-state-media-message model) problem
        (chidu-conversation-state-media-key model) key)
  model)

(cl-defstruct (chidu-conversation--layout-row
               (:constructor chidu-conversation--layout-row-create))
  "Focus-relative metadata for one real or missing Conversation node."
  row
  role
  key
  parent-key
  depth
  connector
  missing-message-id)

(defvar-local chidu-conversation--view nil
  "Appkit view attached to the current Conversation buffer.")

(defun chidu-conversation--state (&optional view)
  "Return validated Conversation state for VIEW or the current view."
  (let*
      ((it (or view (appkit-current-surface)))
       (state (and it (appkit-surface-model it))))
    (unless (chidu-conversation-state-p state)
      (error "Chidu Conversation view has invalid state"))
    state))

(defun chidu-conversation--rows (state)
  "Return committed Conversation rows from STATE."
  (if-let* ((context (chidu-conversation-state-context state)))
      (chidu-store-conversation-context-rows context)
    (vector)))

(defun chidu-conversation--participants (state)
  "Return exact sender identities observed in Conversation STATE."
  (chidu-text-participants (chidu-conversation--rows state)))

(defun chidu-conversation--row-local-id (row)
  "Return stable local Email id from Conversation ROW."
  (chidu-store-email-summary-row-local-email-id
   (chidu-store-conversation-row-summary-row row)))

(defun chidu-conversation--row-for-local-id (state local-id)
  "Return STATE's Conversation row for LOCAL-ID, or nil."
  (cl-find local-id (chidu-conversation--rows state)
           :key #'chidu-conversation--row-local-id :test #'equal))

(defun chidu-conversation--body-visible-p (state row)
  "Return non-nil when ROW's full message body is visible in STATE."
  (and (gethash (chidu-conversation--row-local-id row)
                (chidu-conversation-state-visible-bodies state))
       t))

(defun chidu-conversation--replies-collapsed-p (state row)
  "Return non-nil when ROW's reply subtree is collapsed in STATE."
  (and (gethash (chidu-conversation--row-local-id row)
                (chidu-conversation-state-collapsed-replies state))
       t))

(defun chidu-conversation--focused-scope-rows (state)
  "Return canonical rows belonging to STATE's focused Conversation scope.

The scope is exactly the selected Email's ancestors, the selected Email, and
its descendants.  Other branches in the same JMAP Thread are deliberately
excluded from this view."
  (let* ((rows (chidu-conversation--rows state))
         (by-id (chidu-conversation--row-index rows))
         (focus-id (chidu-conversation-state-focus-local-email-id state))
         (focus (and focus-id (gethash focus-id by-id))))
    (if (null focus)
        rows
      (let ((scope (make-hash-table :test #'equal))
            result)
        (puthash focus-id t scope)
        (dolist (ancestor-id (chidu-conversation--ancestor-ids focus by-id))
          (puthash ancestor-id t scope))
        (cl-loop
         for row across rows
         when (chidu-conversation--depth-from-focus row focus-id by-id)
         do (puthash (chidu-conversation--row-local-id row) t scope))
        (cl-loop
         for row across rows
         when (gethash (chidu-conversation--row-local-id row) scope)
         do (push row result))
        (vconcat (nreverse result))))))

(defun chidu-conversation--visible-rows (state)
  "Return STATE focused rows not hidden by a collapsed ancestor reply fold."
  (let ((visible (make-hash-table :test #'equal))
        result)
    (cl-loop
     for row across (chidu-conversation--focused-scope-rows state)
     for local-id = (chidu-conversation--row-local-id row)
     for parent-id =
     (chidu-store-conversation-row-parent-local-email-id row)
     when
     (or (null parent-id)
         (and (gethash parent-id visible)
              (not
               (gethash parent-id
                        (chidu-conversation-state-collapsed-replies state)))))
     do (puthash local-id t visible)
     and do (push row result))
    (vconcat (nreverse result))))

(defun chidu-conversation--descendant-counts (rows)
  "Return local-id keyed descendant counts for canonical Conversation ROWS."
  (let ((by-id (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal)))
    (cl-loop for row across rows
             do (puthash (chidu-conversation--row-local-id row) row by-id))
    (cl-loop
     for row across rows
     for cursor =
     (chidu-store-conversation-row-parent-local-email-id row)
     do
     (let ((seen (make-hash-table :test #'equal)))
       (while (and cursor (not (gethash cursor seen)))
         (puthash cursor t seen)
         (puthash cursor (1+ (gethash cursor counts 0)) counts)
         (setq cursor
               (when-let* ((parent (gethash cursor by-id)))
                 (chidu-store-conversation-row-parent-local-email-id parent))))))
    counts))

(defun chidu-conversation--row-index (rows)
  "Return local-id keyed index for Conversation ROWS."
  (let ((index (make-hash-table :test #'equal)))
    (cl-loop for row across rows
             do (puthash (chidu-conversation--row-local-id row) row index))
    index))

(defun chidu-conversation--message-owners (rows)
  "Return Message-ID ownership index for canonical Conversation ROWS.

Unique Message-IDs map to the owning local Email id.  Duplicate Message-IDs
map to the symbol `ambiguous'."
  (let ((owners (make-hash-table :test #'equal)))
    (cl-loop
     for row across rows
     for local-id = (chidu-conversation--row-local-id row)
     do
     (cl-loop
      for message-id across (chidu-store-conversation-row-message-ids row)
      for owner = (gethash message-id owners)
      do
      (cond
       ((null owner) (puthash message-id local-id owners))
       ((not (equal owner local-id)) (puthash message-id 'ambiguous owners)))))
    owners))

(defun chidu-conversation--missing-parent-chain (row rows)
  "Return missing Message-IDs immediately above root ROW among ROWS.

This is presentation evidence only.  A canonical row with a real parent is
never given a missing ancestor.  Likewise, if any threading candidate resolves
to an existing or ambiguous Message-ID, do not claim that the root is missing;
that case represents a different kind of incomplete or ambiguous graph."
  (when (null (chidu-store-conversation-row-parent-local-email-id row))
    (let* ((owners (chidu-conversation--message-owners rows))
           (self (chidu-conversation--row-local-id row))
           (candidates
            (vconcat
             (chidu-store-conversation-row-references row)
             (chidu-store-conversation-row-in-reply-to row)))
           missing)
      (unless
          (cl-loop
           for message-id across candidates
           for owner = (gethash message-id owners)
           thereis (and owner (not (equal owner self))))
        (let ((seen (make-hash-table :test #'equal)))
          (cl-loop
           for message-id across candidates
           unless (gethash message-id seen)
           do
           (puthash message-id t seen)
           (when (null (gethash message-id owners))
             (push message-id missing))))
        (nreverse missing)))))

(defun chidu-conversation--focused-root-row (state)
  "Return the real canonical root of STATE's focused scope, or nil."
  (cl-find-if
   (lambda (row)
     (null (chidu-store-conversation-row-parent-local-email-id row)))
   (chidu-conversation--focused-scope-rows state)))

(defun chidu-conversation--ghost-layouts (state)
  "Return missing-ancestor layout nodes for STATE's focused root."
  (when-let* ((root (chidu-conversation--focused-root-row state))
              (message-ids
               (chidu-conversation--missing-parent-chain
                root (chidu-conversation--rows state))))
    (let (parent-key result)
      (dolist (message-id message-ids)
        (let ((key (list 'missing-message message-id)))
          (push
           (chidu-conversation--layout-row-create
            :role 'missing
            :key key
            :parent-key parent-key
            :depth 0
            :connector 'continue
            :missing-message-id message-id)
           result)
          (setq parent-key key)))
      (vconcat (nreverse result)))))

(defun chidu-conversation--ancestor-ids (row by-id)
  "Return ROW's canonical ancestor local ids, root first, using BY-ID."
  (let ((current-id
         (chidu-store-conversation-row-parent-local-email-id row))
        (seen (make-hash-table :test #'equal))
        result)
    (while (and current-id (not (gethash current-id seen)))
      (puthash current-id t seen)
      (if-let* ((parent (gethash current-id by-id)))
          (progn
            (push current-id result)
            (setq current-id
                  (chidu-store-conversation-row-parent-local-email-id parent)))
        (setq current-id nil)))
    result))

(defun chidu-conversation--depth-from-focus (row focus-id by-id)
  "Return ROW's canonical distance below FOCUS-ID using BY-ID, or nil."
  (let ((current-id (chidu-conversation--row-local-id row))
        (seen (make-hash-table :test #'equal))
        (depth 0)
        reached-p)
    (while (and current-id (not reached-p) (not (gethash current-id seen)))
      (puthash current-id t seen)
      (cond
       ((equal current-id focus-id)
        (setq reached-p t))
       ((gethash current-id by-id)
        (setq current-id
              (chidu-store-conversation-row-parent-local-email-id
               (gethash current-id by-id))
              depth (1+ depth)))
       (t (setq current-id nil))))
    (and reached-p depth)))

(defun chidu-conversation--layout-rows (state)
  "Return focused Appkit layout rows for Conversation STATE.

Real membership is ancestors + focus + descendants.  Missing RFC ancestors are
presentation-only ghost nodes prepended above the canonical focused root.  The
ancestor chain and focus form one depth-0 spine; only focus descendants consume
horizontal nesting depth."
  (let* ((all-rows (chidu-conversation--rows state))
         (visible-rows (chidu-conversation--visible-rows state))
         (by-id (chidu-conversation--row-index all-rows))
         (visible-by-id (chidu-conversation--row-index visible-rows))
         (focus-id (chidu-conversation-state-focus-local-email-id state))
         (focus (and focus-id (gethash focus-id visible-by-id)))
         (focus-row (and focus-id (gethash focus-id by-id)))
         (ancestor-ids
          (and focus-row
               (cl-remove-if-not
                (lambda (id) (gethash id visible-by-id))
                (chidu-conversation--ancestor-ids focus-row by-id))))
         (ancestor-set (make-hash-table :test #'equal))
         (ghosts (chidu-conversation--ghost-layouts state))
         (ghost-parent-key
          (and (> (length ghosts) 0)
               (chidu-conversation--layout-row-key
                (aref ghosts (1- (length ghosts))))))
         (focused-root (chidu-conversation--focused-root-row state))
         (focused-root-id
          (and focused-root (chidu-conversation--row-local-id focused-root)))
         result)
    (dolist (id ancestor-ids) (puthash id t ancestor-set))
    (cl-loop
     for row across visible-rows
     for id = (chidu-conversation--row-local-id row)
     for chain-p = (gethash id ancestor-set)
     for focus-p = (and focus (equal id focus-id))
     for focus-depth =
     (and focus (not chain-p) (not focus-p)
          (chidu-conversation--depth-from-focus row focus-id by-id))
     for depth = (or focus-depth 0)
     for role = (cond (chain-p 'chain)
                      (focus-p 'focus)
                      (t 'tree))
     for parent-id =
     (chidu-store-conversation-row-parent-local-email-id row)
     for key = (chidu-conversation--entry-key id)
     for parent-key =
     (cond
      (parent-id (chidu-conversation--entry-key parent-id))
      ((and ghost-parent-key (equal id focused-root-id)) ghost-parent-key))
     do
     (push
      (chidu-conversation--layout-row-create
       :row row
       :role role
       :key key
       :parent-key parent-key
       :depth depth
       :connector
       (cond (chain-p 'continue)
             ((and focus-p (or ancestor-ids ghost-parent-key)) 'end)
             (t nil)))
      result))
    (vconcat ghosts (nreverse result))))

(defun chidu-conversation--body-context (state row)
  "Return cached local body context for ROW in STATE."
  (gethash (chidu-conversation--row-local-id row)
           (chidu-conversation-state-body-contexts state)))

(defun chidu-conversation--body-phase (state row)
  "Return body materialization phase for ROW in STATE."
  (or (gethash (chidu-conversation--row-local-id row)
               (chidu-conversation-state-body-phases state))
      'idle))

(defun chidu-conversation--body-message (state row)
  "Return body materialization error for ROW in STATE."
  (gethash (chidu-conversation--row-local-id row)
           (chidu-conversation-state-body-messages state)))

(defun chidu-conversation--entry-key (local-id)
  "Return Appkit discussion key for LOCAL-ID."
  (list 'email local-id))

(defun chidu-conversation--local-id-at-point ()
  "Return local Email id represented at point, or nil."
  (pcase (appkit-discussion-key-at-point)
    (`(email ,local-id) local-id)
    (_ nil)))

(defun chidu-conversation--email-entry-position (direction)
  "Return next real Email entry position in DIRECTION, skipping ghosts."
  (let ((position (point))
        candidate
        found)
    (while (and (not found)
                (setq candidate
                      (if (eq direction 'next)
                          (appkit-discussion-next-position position)
                        (appkit-discussion-previous-position position))))
      (setq position candidate)
      (when (pcase (appkit-discussion-key-at-point candidate)
              (`(email ,_) t)
              (_ nil))
        (setq found candidate)))
    found))

(defun chidu-conversation-next-entry ()
  "Move point to the next real Email entry, skipping missing nodes."
  (interactive)
  (if-let* ((position (chidu-conversation--email-entry-position 'next)))
      (goto-char position)
    (message "Chidu: no next Email")))

(defun chidu-conversation-previous-entry ()
  "Move point to the previous real Email entry, skipping missing nodes."
  (interactive)
  (if-let* ((position (chidu-conversation--email-entry-position 'previous)))
      (goto-char position)
    (message "Chidu: no previous Email")))

(defun chidu-conversation--request-sync (surface)
  "Request presentation of the committed reader model."
  (chidu-surface-refresh surface))

(defun chidu-conversation--insert-body-region (prefix properties thunk)
  "Call THUNK, then apply Appkit PREFIX and PROPERTIES to its output."
  (let ((start (point)))
    (funcall thunk)
    (unless (bolp) (insert "\n"))
    (when (= start (point)) (insert "\n"))
    (appkit-ui-apply-line-prefix start (point) prefix)
    (add-text-properties start (point) properties)))

(defun chidu-conversation--insert-row-body
    (view state row prefix properties)
  "Insert ROW's full body for VIEW and STATE using PREFIX and PROPERTIES."
  (when (chidu-conversation--body-visible-p state row)
    (let* ((context (chidu-conversation--body-context state row))
           (body (and context
                      (chidu-store-email-body-context-body context)))
           (phase (chidu-conversation--body-phase state row))
           (problem (chidu-conversation--body-message state row))
           embedded-attachments)
      (chidu-conversation--insert-body-region
       prefix properties
       (lambda ()
         (cond
          (body
           (setq embedded-attachments
                 (chidu-message-insert-body
                  body
                  :sender (chidu-store-email-summary-row-from-email
                           (chidu-store-conversation-row-summary-row row))
                  :participants (chidu-conversation--participants state)
                  :view view
                  :context context)))
          (problem
           (insert
            (propertize (format "Unable to load full message: %s" problem)
                        'face 'error)))
          ((memq phase '(loading refreshing))
           (insert (propertize "Loading full message body…" 'face 'shadow)))
          (t
           (insert (propertize "Full message body is not available."
                               'face 'shadow))))
         (when (and body (eq phase 'refreshing))
           (insert "\n\n" (propertize "Refreshing body…" 'face 'shadow)))))
      (when (and body context)
        (chidu-attachment-insert-cards
         view context
         :prefix prefix
         :properties properties
         :embedded-attachments embedded-attachments)))))

(defun chidu-conversation--attachment-fallback-context ()
  "Return unambiguous attachment context for the Conversation Email at point."
  (when-let*
      ((view (appkit-current-surface))
       ((eq 'chidu-conversation-mode
            (appkit-surface-type-mode (appkit-surface-type view))))
       (state (chidu-conversation--state view))
       (local-id (chidu-conversation--local-id-at-point))
       (row (chidu-conversation--row-for-local-id state local-id))
       (context (chidu-conversation--body-context state row)))
    (chidu-attachment-single-card-context view context)))

(defun chidu-conversation--parent-label (state row)
  "Return reply-target label for ROW in STATE, or nil."
  (when-let* ((parent-id
               (chidu-store-conversation-row-parent-local-email-id row))
              (parent (chidu-conversation--row-for-local-id state parent-id)))
    (concat
     "↳ reply to "
     (chidu-text-person-label
      (chidu-store-conversation-row-summary-row parent)))))

(defun chidu-conversation--missing-discussion-entry (layout)
  "Return one presentation-only missing-message entry for LAYOUT."
  (let ((message-id
         (chidu-conversation--layout-row-missing-message-id layout)))
    (appkit-discussion-entry-create
     :key (chidu-conversation--layout-row-key layout)
     :parent-key (chidu-conversation--layout-row-parent-key layout)
     :depth 0
     :heading "⋯ missing message"
     :heading-face 'shadow
     :body-inserter (lambda (_prefix _properties))
     :connector (chidu-conversation--layout-row-connector layout)
     :properties
     (list
      'chidu-conversation-role 'missing
      'chidu-conversation-missing-message-id message-id
      'help-echo (format "Missing Message-ID: <%s>" message-id)
      'rear-nonsticky
      '(chidu-conversation-role
        chidu-conversation-missing-message-id)))))

(defun chidu-conversation--real-discussion-entry
    (view state layout descendant-counts)
  "Return VIEW's real Email entry for LAYOUT in STATE using reply counts."
  (let* ((row (chidu-conversation--layout-row-row layout))
         (summary (chidu-store-conversation-row-summary-row row))
         (local-id (chidu-conversation--row-local-id row))
         (actual-depth (chidu-store-conversation-row-depth row))
         (visual-depth (chidu-conversation--layout-row-depth layout))
         (display-depth
          (min visual-depth chidu-conversation-max-display-depth))
         (role (chidu-conversation--layout-row-role layout))
         (body-visible (chidu-conversation--body-visible-p state row))
         (replies-collapsed
          (chidu-conversation--replies-collapsed-p state row))
         (reply-count (gethash local-id descendant-counts 0))
         (flagged (chidu-store-email-summary-row-flagged-p summary)))
    (appkit-discussion-entry-create
     :key (chidu-conversation--layout-row-key layout)
     :parent-key (chidu-conversation--layout-row-parent-key layout)
     :depth display-depth
     :context
     (and (eq role 'tree)
          (> visual-depth 1)
          (chidu-conversation--parent-label state row))
     :context-face 'shadow
     :heading
     (concat
      (if body-visible "▾ " "▸ ")
      (when (> reply-count 0)
        (if replies-collapsed "⊞ " "⊟ "))
      (chidu-text-person-label summary)
      (and flagged (propertize "  ★" 'face 'chidu-email-flagged)))
     :heading-face nil
     :time
     (chidu-email-format-time
      (or (chidu-store-conversation-row-sent-at row)
          (chidu-store-email-summary-row-received-at summary)))
     :time-face 'shadow
     :body-inserter
     (lambda (prefix properties)
       (chidu-conversation--insert-row-body
        view state row prefix properties))
     :footer
     (when (> reply-count 0)
       (format "%d %s%s"
               reply-count
               (if (= reply-count 1) "reply" "replies")
               (if replies-collapsed " hidden" "")))
     :footer-face 'shadow
     :connector (chidu-conversation--layout-row-connector layout)
     :properties
     (list
      'chidu-conversation-email-id local-id
      'chidu-conversation-remote-email-id
      (chidu-store-email-summary-row-remote-email-id summary)
      'chidu-conversation-actual-depth actual-depth
      'chidu-conversation-visual-depth visual-depth
      'chidu-conversation-role role
      'chidu-conversation-body-visible-p body-visible
      'chidu-conversation-replies-collapsed-p replies-collapsed
      'rear-nonsticky
      '(chidu-browse-source-url
        chidu-conversation-email-id
        chidu-conversation-remote-email-id
        chidu-conversation-actual-depth
        chidu-conversation-visual-depth
        chidu-conversation-role
        chidu-conversation-body-visible-p
        chidu-conversation-replies-collapsed-p)))))

(defun chidu-conversation--discussion-entry
    (view state layout descendant-counts)
  "Return VIEW entry for focus-relative LAYOUT in STATE using reply counts."
  (if (eq 'missing (chidu-conversation--layout-row-role layout))
      (chidu-conversation--missing-discussion-entry layout)
    (chidu-conversation--real-discussion-entry
     view state layout descendant-counts)))

(defun chidu-conversation--header (state)
  "Return generated header for Conversation STATE."
  (let* ((focus
          (chidu-conversation--row-for-local-id
           state (chidu-conversation-state-focus-local-email-id state)))
         (subject
          (and focus
               (chidu-email-subject
                (chidu-store-conversation-row-summary-row focus))))
         (context (chidu-conversation-state-context state))
         (phase (chidu-conversation-state-phase state))
         (problem (chidu-conversation-state-message state))
         (status
          (cond
           ((eq phase 'refreshing) "refreshing")
           ((eq phase 'error) (or problem "refresh failed"))
           ((and context
                 (not (chidu-store-conversation-context-complete-p context)))
            "incomplete")
           (t nil))))
    (concat
     (propertize (or subject "Conversation")
                 'face '(:height 1.2 :weight bold))
     "\n"
     (propertize
      (string-join
       (delq nil
             (list
              (chidu-store-account-name
               (chidu-conversation-state-account state))
              (chidu-store-mailbox-name
               (chidu-conversation-state-mailbox state))
              status))
       " · ")
      'face 'shadow)
     "\n\n")))

(defun chidu-conversation--goto-focus (state)
  "Move point to STATE's focused Email when it is rendered."
  (let ((focus-id (chidu-conversation-state-focus-local-email-id state)))
    (goto-char (point-min))
    (when-let* ((match
                 (and focus-id
                      (text-property-search-forward
                       'chidu-conversation-email-id focus-id #'equal))))
      (goto-char (prop-match-beginning match)))))

(defun chidu-conversation--render (view)
  "Render the committed Conversation reader."
  (let*
      ((state (chidu-conversation--state view))
       (buffer (appkit-surface-buffer view)))
    (with-current-buffer buffer
      (let
          ((initial-p
            (not
             (text-property-not-all (point-min) (point-max)
                                    'chidu-conversation-email-id nil))))
        (appkit-position-render-preserving
         (lambda ()
           (let*
               ((layouts (chidu-conversation--layout-rows state))
                (descendant-counts
                 (chidu-conversation--descendant-counts
                  (chidu-conversation--focused-scope-rows state))))
             (appkit-with-content-update view
               (erase-buffer)
               (insert (chidu-conversation--header state))
               (let
                   ((width
                     (or
                      (appkit-surface-responsive-width
                       (appkit-current-surface) 1)
                      fill-column 100)))
                 (cl-loop for layout across layouts do
                          (appkit-discussion-insert-entry
                           (chidu-conversation--discussion-entry view
                                                                 state
                                                                 layout
                                                                 descendant-counts)
                           :width width :avatar-p nil :indent-width 3
                           :separate-p
                           (not
                            (eq 'missing
                                (chidu-conversation--layout-row-role
                                 layout)))))))))
         :anchor-property 'chidu-conversation-email-id
         :preserve-window-start t :after-restore
         (when initial-p
           (lambda () (chidu-conversation--goto-focus state))))))))

(defun chidu-conversation--request-key (kind local-id)
  "Return VIEW request key from KIND and LOCAL-ID."
  (list kind local-id))

(defun chidu-conversation--cancel-operations (view)
  "Cancel metadata and per-body operations owned by Conversation VIEW."
  (let ((state (chidu-conversation--state view)))
    (chidu-surface-operation-cancel view 'conversation)
    (maphash
     (lambda (local-id _phase)
       (chidu-surface-operation-cancel view
                                       (chidu-conversation--request-key
                                        'body local-id)))
     (chidu-conversation-state-body-phases state))))

(defun chidu-conversation--body-loaded
    (view state row refresh-empty-p context)
  "Install ROW body CONTEXT in VIEW STATE.

Refresh the body when REFRESH-EMPTY-P and CONTEXT has no body."
  (let ((local-id (chidu-conversation--row-local-id row)))
    (puthash local-id context
             (chidu-conversation-state-body-contexts state))
    (puthash local-id 'idle
             (chidu-conversation-state-body-phases state))
    (remhash local-id
             (chidu-conversation-state-body-messages state))
    (chidu-conversation--request-sync view)
    (when (and refresh-empty-p
               (null (chidu-store-email-body-context-body context)))
      (chidu-conversation--refresh-body view state row))))

(defun chidu-conversation--body-failed (view state row failure)
  "Install ROW body FAILURE in VIEW STATE."
  (let ((local-id (chidu-conversation--row-local-id row)))
    (puthash local-id 'error
             (chidu-conversation-state-body-phases state))
    (puthash local-id (chidu-runtime-error-message failure)
             (chidu-conversation-state-body-messages state))
    (chidu-conversation--request-sync view)))

(defun chidu-conversation--refreshed (view state context)
  "Install refreshed conversation CONTEXT in VIEW STATE."
  (setf (chidu-conversation-state-context state) context
        (chidu-conversation-state-phase state) 'idle
        (chidu-conversation-state-message state) nil)
  (let ((selected
         (chidu-conversation-state-focus-local-email-id state)))
    (unless (chidu-conversation--row-for-local-id state selected)
      (when-let* ((rows (chidu-conversation--rows state))
                  ((> (length rows) 0))
                  (first (aref rows 0)))
        (setq selected (chidu-conversation--row-local-id first))
        (setf (chidu-conversation-state-focus-local-email-id state)
              selected)
        (puthash selected t
                 (chidu-conversation-state-visible-bodies state)))))
  (chidu-conversation--request-sync view)
  (chidu-conversation--load-visible-bodies view state))

(defun chidu-conversation--failed (view state failure)
  "Install conversation FAILURE in VIEW STATE."
  (setf (chidu-conversation-state-phase state) 'error
        (chidu-conversation-state-message state)
        (chidu-runtime-error-message failure))
  (chidu-conversation--request-sync view))

(defun chidu-conversation--loaded-local
    (view state refresh-empty-p context)
  "Install local conversation CONTEXT in VIEW STATE.

Refresh remotely when REFRESH-EMPTY-P and CONTEXT has no revision."
  (setf (chidu-conversation-state-context state) context
        (chidu-conversation-state-phase state) 'idle)
  (chidu-conversation--request-sync view)
  (if (and refresh-empty-p
           (zerop (chidu-store-conversation-context-revision context)))
      (chidu-conversation-refresh view)
    (chidu-conversation--load-visible-bodies view state)))

(defun chidu-conversation--refresh-body (view state row)
  "Refresh ROW body in Conversation VIEW and STATE."
  (let* ((local-id (chidu-conversation--row-local-id row))
         (key (chidu-conversation--request-key 'body local-id)))
    (puthash local-id 'refreshing
             (chidu-conversation-state-body-phases state))
    (remhash local-id (chidu-conversation-state-body-messages state))
    (chidu-conversation--request-sync view)
    (chidu-surface-operation-start
     view key
     (lambda (runtime success-function error-function)
       (chidu-refresh-email-body
        runtime
        (chidu-conversation-state-account state)
        (chidu-store-conversation-row-summary-row row)
        success-function error-function))
     (apply-partially
      #'chidu-conversation--body-loaded view state row nil)
     (apply-partially
      #'chidu-conversation--body-failed view state row))))

(defun chidu-conversation--load-body (view state row)
  "In VIEW and STATE, load ROW body, materializing it when absent."
  (let* ((local-id (chidu-conversation--row-local-id row))
         (phase (gethash local-id
                         (chidu-conversation-state-body-phases state))))
    (unless (memq phase '(loading refreshing))
      (let ((key (chidu-conversation--request-key 'body local-id)))
        (puthash local-id 'loading
                 (chidu-conversation-state-body-phases state))
        (remhash local-id (chidu-conversation-state-body-messages state))
        (chidu-conversation--request-sync view)
        (chidu-surface-operation-start
         view key
         (lambda (runtime success-function error-function)
           (chidu-runtime-email-body
            runtime
            (chidu-conversation-state-account state)
            (chidu-store-conversation-row-summary-row row)
            success-function error-function))
         (apply-partially
          #'chidu-conversation--body-loaded view state row t)
         (apply-partially
          #'chidu-conversation--body-failed view state row))))))

(defun chidu-conversation--load-visible-bodies (view state)
  "Load bodies shown by both body and reply folds in VIEW and STATE."
  (cl-loop
   for row across (chidu-conversation--visible-rows state)
   when (chidu-conversation--body-visible-p state row)
   do (chidu-conversation--load-body view state row)))

(defun chidu-conversation-refresh (&optional view)
  "Refresh Conversation metadata for VIEW." (interactive)
  (let*
      ((it (or view (appkit-current-surface)))
       (state (chidu-conversation--state it)))
    (setf (chidu-conversation-state-phase state) 'refreshing
          (chidu-conversation-state-message state) nil)
    (chidu-conversation--request-sync it)
    (chidu-surface-operation-start it 'conversation
                                   (lambda
                                     (runtime success-function
                                              error-function)
                                     (chidu-refresh-conversation
                                      runtime
                                      (chidu-conversation-state-account
                                       state)
                                      (chidu-conversation-state-remote-thread-id
                                       state)
                                      success-function error-function))
                                   (apply-partially
                                    #'chidu-conversation--refreshed it
                                    state)
                                   (apply-partially
                                    #'chidu-conversation--failed it
                                    state))))

(defun chidu-conversation--load-local (view &optional refresh-empty-p)
  "Load VIEW's local projection and refresh when REFRESH-EMPTY-P."
  (let ((state (chidu-conversation--state view)))
    (setf (chidu-conversation-state-phase state) 'loading
          (chidu-conversation-state-message state) nil)
    (chidu-conversation--request-sync view)
    (chidu-surface-operation-start
     view 'conversation
     (lambda (runtime success-function error-function)
       (chidu-runtime-conversation
        runtime
        (chidu-conversation-state-account state)
        (chidu-conversation-state-remote-thread-id state)
        success-function error-function))
     (apply-partially
      #'chidu-conversation--loaded-local view state refresh-empty-p)
     (apply-partially #'chidu-conversation--failed view state))))

(defun chidu-conversation--seen-target ()
  "Return explicit read-state target for the Conversation Email at point."
  (let*
      ((view
        (or (appkit-current-surface)
            (user-error "No live Chidu Conversation view")))
       (state (chidu-conversation--state view))
       (local-id
        (or (chidu-conversation--local-id-at-point)
            (user-error "No Conversation Email at point")))
       (row
        (or (chidu-conversation--row-for-local-id state local-id)
            (user-error "Conversation row disappeared")))
       (summary (chidu-store-conversation-row-summary-row row)))
    (chidu-seen-target-create :app (appkit-surface-app view) :account
                              (chidu-conversation-state-account
                               state)
                              :local-email-id
                              (chidu-store-email-summary-row-local-email-id
                               summary)
                              :remote-email-id
                              (chidu-store-email-summary-row-remote-email-id
                               summary)
                              :unread-p
                              (chidu-store-email-summary-row-unread-p
                               summary))))

(defun chidu-conversation-apply-seen-change (view change)
  "Apply explicit read-state CHANGE to live Conversation VIEW."
  (when (appkit-surface-live-p view)
    (let*
        ((state (chidu-conversation--state view))
         (context (chidu-conversation-state-context state)))
      (when
          (and context
               (chidu-seen-change-for-account-p change
                                                (chidu-conversation-state-account
                                                 state)))
        (let ((changed-p nil) rows)
          (cl-loop for row across
                   (chidu-store-conversation-context-rows context)
                   for summary =
                   (chidu-store-conversation-row-summary-row row) for
                   updated-summary =
                   (chidu-seen-update-summary-row summary change) for
                   updated =
                   (if (eq summary updated-summary) row
                     (chidu-store-conversation-row-with row
                                                        :summary-row
                                                        updated-summary))
                   do (unless (eq updated row) (setq changed-p t)) do
                   (push updated rows))
          (when changed-p
            (setf (chidu-conversation-state-context state)
                  (chidu-store-conversation-context-with context
                                                         :rows
                                                         (vconcat
                                                          (nreverse
                                                           rows))))
            (chidu-conversation--request-sync view)))))))

(defun chidu-conversation-focus ()
  "Make the Conversation Email at point the current focused Email."
  (interactive)
  (let*
      ((view
        (or (appkit-current-surface)
            (user-error "No live Chidu Conversation view")))
       (state (chidu-conversation--state view))
       (local-id
        (or (chidu-conversation--local-id-at-point)
            (user-error "No Conversation Email at point"))))
    (unless (chidu-conversation--row-for-local-id state local-id)
      (user-error "Conversation row disappeared"))
    (unless
        (equal local-id
               (chidu-conversation-state-focus-local-email-id state))
      (setf (chidu-conversation-state-focus-local-email-id state)
            local-id)
      (chidu-conversation--request-sync view))))

(defun chidu-conversation-toggle-body ()
  "Show or hide the full Conversation message at point." (interactive)
  (let*
      ((view
        (or (appkit-current-surface)
            (user-error "No live Chidu Conversation view")))
       (state (chidu-conversation--state view))
       (local-id
        (or (chidu-conversation--local-id-at-point)
            (user-error "No Conversation Email at point")))
       (row
        (or (chidu-conversation--row-for-local-id state local-id)
            (user-error "Conversation row disappeared")))
       (visible-bodies
        (chidu-conversation-state-visible-bodies state)))
    (if (gethash local-id visible-bodies)
        (remhash local-id visible-bodies)
      (puthash local-id t visible-bodies))
    (chidu-conversation--request-sync view)
    (when (gethash local-id visible-bodies)
      (chidu-conversation--load-body view state row))))

(defun chidu-conversation--attachment-tab-handled-p ()
  "Handle the exact attachment card at point and return non-nil.

An inline-capable part toggles.  A non-inline attachment owns the gesture and
signals a user error rather than letting TAB mutate an unrelated fold.  Return
nil only when point is outside every exact attachment card."
  (cond
   ((chidu-attachment-inline-toggle-available-p t)
    (chidu-attachment-toggle-inline-at-point-exact)
    t)
   ((chidu-attachment-card-at-point-p t)
    (user-error "Chidu: this attachment cannot be displayed inline"))
   (t nil)))

(defun chidu-conversation-tab-dwim ()
  "Toggle an exact inline attachment, or the current reply subtree.

This preserves Conversation's Emacs-state TAB fold while letting an attachment
card at point own the same high-frequency inline/hide gesture."
  (interactive)
  (unless (chidu-conversation--attachment-tab-handled-p)
    (chidu-conversation-toggle-replies)))

(defun chidu-conversation-evil-tab-dwim ()
  "Toggle an exact inline attachment, or the current Email body.

Evil's jump-list key remains untouched; its distinct `<tab>' event
historically owns Chidu's body fold."
  (interactive)
  (unless (chidu-conversation--attachment-tab-handled-p)
    (chidu-conversation-toggle-body)))

(defun chidu-conversation--set-replies-collapsed (collapsed-p)
  "Set the reply subtree at point to COLLAPSED-P."
  (let*
      ((view
        (or (appkit-current-surface)
            (user-error "No live Chidu Conversation view")))
       (state (chidu-conversation--state view))
       (local-id
        (or (chidu-conversation--local-id-at-point)
            (user-error "No Conversation Email at point")))
       (reply-count
        (gethash local-id
                 (chidu-conversation--descendant-counts
                  (chidu-conversation--focused-scope-rows state))
                 0))
       (collapsed (chidu-conversation-state-collapsed-replies state))
       (was-collapsed-p (and (gethash local-id collapsed) t)))
    (unless (chidu-conversation--row-for-local-id state local-id)
      (user-error "Conversation row disappeared"))
    (when (zerop reply-count)
      (user-error "This message has no replies"))
    (unless (eq was-collapsed-p collapsed-p)
      (if collapsed-p (puthash local-id t collapsed)
        (remhash local-id collapsed))
      (chidu-conversation--request-sync view)
      (unless collapsed-p
        (chidu-conversation--load-visible-bodies view state)))))

(defun chidu-conversation-toggle-replies ()
  "Toggle the reply subtree below the Conversation message at point."
  (interactive)
  (let* ((state (chidu-conversation--state))
         (local-id (or (chidu-conversation--local-id-at-point)
                       (user-error "No Conversation Email at point"))))
    (chidu-conversation--set-replies-collapsed
     (not
      (gethash local-id
               (chidu-conversation-state-collapsed-replies state))))))

(defun chidu-conversation-close-replies ()
  "Collapse the reply subtree below the Conversation message at point."
  (interactive)
  (chidu-conversation--set-replies-collapsed t))

(defun chidu-conversation-open-replies ()
  "Expand the reply subtree below the Conversation message at point."
  (interactive)
  (chidu-conversation--set-replies-collapsed nil))

(defun chidu-conversation-open-standalone ()
  "Open the Conversation Email at point in a standalone reader."
  (interactive)
  (let*
      ((view
        (or (appkit-current-surface)
            (user-error "No live Chidu Conversation view")))
       (state (chidu-conversation--state view))
       (local-id
        (or (chidu-conversation--local-id-at-point)
            (user-error "No Conversation Email at point")))
       (row
        (or (chidu-conversation--row-for-local-id state local-id)
            (user-error "Conversation row disappeared"))))
    (chidu-message-open (appkit-surface-app view)
                        (chidu-conversation-state-account state)
                        (chidu-conversation-state-mailbox state)
                        (chidu-store-conversation-row-summary-row
                         row)
                        t (chidu-conversation--participants state))))

(defvar-keymap chidu-conversation-mode-map
  :doc "Keymap for `chidu-conversation-mode'."
  :parent special-mode-map
  "?" #'chidu-dispatch
  "RET" #'chidu-activate-at-point
  "<return>" #'chidu-activate-at-point
  "!" #'chidu-mark-read
  "R" #'chidu-mark-unread
  "s" #'chidu-toggle-read
  "v" #'chidu-conversation-toggle-body
  "TAB" #'chidu-conversation-tab-dwim
  "<tab>" #'chidu-conversation-tab-dwim
  "n" #'chidu-conversation-next-entry
  "p" #'chidu-conversation-previous-entry
  "g" #'chidu-conversation-refresh
  "o" #'chidu-conversation-open-standalone
  "q" #'quit-window)

(define-derived-mode chidu-conversation-mode appkit-discussion-mode "Chidu-Conversation"
  "Major mode for a local-first Chidu reply tree."
  (setq-local chidu-seen-target-function #'chidu-conversation--seen-target
              appkit-media-card-fallback-context-function
              #'chidu-conversation--attachment-fallback-context)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t))

(defun chidu-conversation--setup (view)
  "Initialize newly attached Conversation VIEW."
  (setq-local chidu-conversation--view view
              chidu-seen-target-function
              #'chidu-conversation--seen-target
              appkit-media-card-fallback-context-function
              #'chidu-conversation--attachment-fallback-context))

(defun chidu-conversation-open
    (app account mailbox focus-row &optional select)
  "Open APP's reply tree for ACCOUNT, MAILBOX, and FOCUS-ROW.\n\nDisplay the resulting buffer when SELECT is non-nil."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument
            (list 'chidu-store-account-p account)))
  (unless (chidu-store-mailbox-p mailbox)
    (signal 'wrong-type-argument
            (list 'chidu-store-mailbox-p mailbox)))
  (unless (chidu-store-email-summary-row-p focus-row)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-summary-row-p focus-row)))
  (let*
      ((remote-thread-id
        (chidu-store-email-summary-row-remote-thread-id focus-row))
       (focus-local-id
        (chidu-store-email-summary-row-local-email-id focus-row))
       (view-id
        (list 'conversation (chidu-store-account-account-id account)
              remote-thread-id))
       (existing (appkit-app-surface app view-id))
       (visible-bodies (make-hash-table :test #'equal)) state view)
    (puthash focus-local-id t visible-bodies)
    (setq state
          (chidu-conversation-state-create :account account :mailbox
                                           mailbox :remote-thread-id
                                           remote-thread-id
                                           :focus-local-email-id
                                           focus-local-id
                                           :visible-bodies
                                           visible-bodies))
    (when existing (chidu-conversation--cancel-operations existing))
    (setq view
          (or existing
              (appkit-open-generated-surface
               chidu-conversation--surface-type :app app :identity
               view-id :buffer-name
               (format "*Chidu Conversation: %s*"
                       (chidu-email-subject focus-row))
               :input state :select select)))
    (when existing
      (appkit-surface-send view (list 'chidu-reader 'replace state))
      (chidu-conversation--load-local view t))
    (when (and existing select)
      (pop-to-buffer (appkit-surface-buffer view)))
    (unless existing
      (with-current-buffer (appkit-surface-buffer view)
        (appkit-surface-enable-responsive-geometry view
                                                   (lambda
                                                     (surface _width)
                                                     (chidu-surface-refresh
                                                      surface)))
        (chidu-conversation--load-local view t)))
    (appkit-surface-buffer view)))

(provide 'chidu-conversation)

;;; chidu-conversation.el ends here

(defconst chidu-conversation--surface-type
  (appkit-surface-type-create
   :name 'chidu-conversation :mode #'chidu-conversation-mode
   :init (lambda (_context input) (appkit-next :model input :render t))
   :update #'chidu-surface-update
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount (lambda (surface _app _model) (chidu-conversation--setup surface))
      :merge (lambda (_previous next) next)
      :render (lambda (surface _app model _request)
                (chidu-conversation--render surface)
                (chidu-attachment-insert-reader-problem model)
                nil)
      :unmount (lambda (_surface) nil)))))
