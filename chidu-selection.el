;;; chidu-selection.el --- View-local Email markers -*- lexical-binding: t; -*-

;;; Commentary:

;; Summary-like views keep Gnus-style process marks keyed by stable local Email
;; ids.  Mail commands target process marks first, then an active region, then
;; the row at point.  A separate Dired-style `D' flag stages move-to-Trash until
;; `x' executes the flagged set.  Mark editing operates on the region or row at
;; point and advances after a single-row edit.  Both marker kinds are temporary
;; UI state: neither is a JMAP keyword nor a durable local annotation.

;;; Code:

(require 'cl-lib)

(defgroup chidu-selection nil
  "View-local ordinary marks and Trash flags for Chidu list surfaces."
  :group 'chidu
  :prefix "chidu-selection-")

(defface chidu-selection-mark
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for an ordinary process mark beside an Email row."
  :group 'chidu-selection)

(defface chidu-selection-trash-marker
  '((t :inherit error :weight bold))
  "Face for the Dired-style Trash flag beside an Email row."
  :group 'chidu-selection)

(defvar-local chidu-selection-row-id-property nil
  "Text property containing a stable Email id on selectable rows.")

(defvar-local chidu-selection-refresh-function nil
  "Zero-argument function that redraws the current selectable view.")

(defvar-local chidu-selection-next-function nil
  "Zero-argument function that moves to the next selectable row.")

(defvar-local chidu-selection-previous-function nil
  "Zero-argument function that moves to the previous selectable row.")

(defvar-local chidu-selection--marked-ids nil
  "Hash table containing the current view's ordinary process marks.")

(defvar-local chidu-selection--trash-flagged-ids nil
  "Hash table containing the current view's staged move-to-Trash flags.")

(defvar chidu-selection-command-ids nil
  "Dynamically bound stable Email ids for one scoped list command.

Contextual command adapters may bind this after capturing an active region.
Ordinary direct commands leave it nil and resolve marks, region, or point at
invocation time.")

(defun chidu-selection-setup
    (row-id-property refresh-function &optional next-function previous-function)
  "Configure selection markers for the current list view.

ROW-ID-PROPERTY names the stable row identity text property.
REFRESH-FUNCTION receives the changed stable Email ids and redraws their
marker indicators.  NEXT-FUNCTION and PREVIOUS-FUNCTION move between
selectable rows and are used by forward marker editing and marker navigation."
  (unless (symbolp row-id-property)
    (signal 'wrong-type-argument (list 'symbolp row-id-property)))
  (dolist (function (list refresh-function next-function previous-function))
    (unless (or (null function) (functionp function))
      (signal 'wrong-type-argument (list 'functionp function))))
  (setq-local chidu-selection-row-id-property row-id-property
              chidu-selection-refresh-function refresh-function
              chidu-selection-next-function next-function
              chidu-selection-previous-function previous-function)
  (unless (hash-table-p chidu-selection--marked-ids)
    (setq-local chidu-selection--marked-ids
                (make-hash-table :test #'equal)))
  (unless (hash-table-p chidu-selection--trash-flagged-ids)
    (setq-local chidu-selection--trash-flagged-ids
                (make-hash-table :test #'equal))))

(defun chidu-selection--table ()
  "Return the current ordinary-mark table, or signal a user error."
  (unless (and (symbolp chidu-selection-row-id-property)
               (hash-table-p chidu-selection--marked-ids))
    (user-error "This buffer has no Email process set"))
  chidu-selection--marked-ids)

(defun chidu-selection--trash-table ()
  "Return the current Trash-flag table, creating it for an old live view."
  (chidu-selection--table)
  (unless (hash-table-p chidu-selection--trash-flagged-ids)
    (setq-local chidu-selection--trash-flagged-ids
                (make-hash-table :test #'equal)))
  chidu-selection--trash-flagged-ids)

(defun chidu-selection--row-id-at (position)
  "Return the selectable row id at POSITION, or nil."
  (when (symbolp chidu-selection-row-id-property)
    (or (get-text-property position chidu-selection-row-id-property)
        (save-excursion
          (goto-char position)
          (get-text-property
           (line-beginning-position) chidu-selection-row-id-property)))))

(defun chidu-selection-row-id-at-point ()
  "Return the selectable row id at point, or nil."
  (chidu-selection--row-id-at (point)))

(defun chidu-selection--ids-between (begin end)
  "Return unique selectable ids on lines intersecting BEGIN through END."
  (let ((seen (make-hash-table :test #'equal))
        ids)
    (save-excursion
      (goto-char begin)
      (beginning-of-line)
      (while (< (point) end)
        (when-let* ((id (chidu-selection--row-id-at (point))))
          (unless (gethash id seen)
            (puthash id t seen)
            (push id ids)))
        (forward-line 1)))
    (nreverse ids)))

(defun chidu-selection-visible-ids ()
  "Return selectable Email ids in display order."
  (chidu-selection--table)
  (chidu-selection--ids-between (point-min) (point-max)))

(defun chidu-selection-marked-p (local-email-id)
  "Return non-nil when LOCAL-EMAIL-ID has an ordinary process mark."
  (and (hash-table-p chidu-selection--marked-ids)
       (gethash local-email-id chidu-selection--marked-ids)))

(defun chidu-selection-marked-ids ()
  "Return visible ordinary process-marked Email ids in display order."
  (cl-remove-if-not #'chidu-selection-marked-p
                    (chidu-selection-visible-ids)))

(defun chidu-selection-trash-flagged-p (local-email-id)
  "Return non-nil when LOCAL-EMAIL-ID is staged for move to Trash."
  (and (hash-table-p chidu-selection--trash-flagged-ids)
       (gethash local-email-id chidu-selection--trash-flagged-ids)))

(defun chidu-selection-trash-flagged-ids ()
  "Return visible Trash-flagged Email ids in display order."
  (chidu-selection--trash-table)
  (cl-remove-if-not #'chidu-selection-trash-flagged-p
                    (chidu-selection-visible-ids)))

(defun chidu-selection-count ()
  "Return the number of ordinary process marks in the current view."
  (if (hash-table-p chidu-selection--marked-ids)
      (hash-table-count chidu-selection--marked-ids)
    0))

(defun chidu-selection-trash-flag-count ()
  "Return the number of staged move-to-Trash flags in the current view."
  (if (hash-table-p chidu-selection--trash-flagged-ids)
      (hash-table-count chidu-selection--trash-flagged-ids)
    0))

(defun chidu-selection-marker-count ()
  "Return the total number of ordinary marks and Trash flags."
  (+ (chidu-selection-count) (chidu-selection-trash-flag-count)))

(defun chidu-selection-selected-ids (&optional ignore)
  "Return current operation targets in display order.

Ordinary process marks take precedence over an active region, which takes
precedence over point.  Trash flags never enter this operation target.  IGNORE
may be `marks' to ignore ordinary marks or `region' to use only point."
  (chidu-selection--table)
  (if chidu-selection-command-ids
      (copy-sequence chidu-selection-command-ids)
    (let ((marks (unless (memq ignore '(marks region))
                   (chidu-selection-marked-ids)))
          (region (unless (eq ignore 'region)
                    (when (use-region-p)
                      (chidu-selection--ids-between
                       (region-beginning) (region-end)))))
          (point-id (chidu-selection-row-id-at-point)))
      (cond (marks marks)
            (region region)
            (point-id (list point-id))))))

(defun chidu-selection-target-source ()
  "Return the active operation target source.

The result is one of `marks', `region', `point', or nil."
  (cond
   ((chidu-selection-marked-ids) 'marks)
   ((and (use-region-p)
         (chidu-selection--ids-between
          (region-beginning) (region-end)))
    'region)
   ((chidu-selection-row-id-at-point) 'point)))

(defun chidu-selection-target-count ()
  "Return the number of Emails targeted by the current operation."
  (length (or (chidu-selection-selected-ids) nil)))

(defun chidu-selection-description ()
  "Return a concise description of the current operation target."
  (let ((source (chidu-selection-target-source))
        (count (chidu-selection-target-count)))
    (pcase source
      ('marks
       (format "%d marked Email%s" count (if (= count 1) "" "s")))
      ('region
       (format "%d Email%s in region" count (if (= count 1) "" "s")))
      ('point "1 Email at point")
      (_ "no Email target"))))

(defun chidu-selection-editable-p ()
  "Return non-nil when a region or row at point can edit markers."
  (or (and (use-region-p)
           (chidu-selection--ids-between
            (region-beginning) (region-end)))
      (chidu-selection-row-id-at-point)))

(defun chidu-selection--edit-ids ()
  "Return region or point ids without consulting existing marks."
  (or (chidu-selection-selected-ids 'marks)
      (user-error "No Email row selected")))

(defun chidu-selection--refresh (local-email-ids)
  "Redraw LOCAL-EMAIL-IDS after their marker state changes."
  (when (and local-email-ids
             (functionp chidu-selection-refresh-function))
    (funcall chidu-selection-refresh-function
             (copy-sequence local-email-ids))))

(defun chidu-selection--report ()
  "Report current ordinary-mark and Trash-flag counts."
  (let ((marked (chidu-selection-count))
        (trash (chidu-selection-trash-flag-count)))
    (message "Chidu: %d marked, %d flagged for Trash" marked trash)))

(defun chidu-selection--mutate (operation)
  "Apply marker OPERATION to the active region or row at point.

OPERATION is one of `mark', `trash', `unmark', or `toggle'.  Like Dired, an
ordinary mark overwrites a Trash flag, a Trash flag overwrites an ordinary
mark, unmark clears either kind, and toggle leaves Trash flags untouched.  A
single-row operation advances before redraw."
  (let* ((table (chidu-selection--table))
         (trash-table (chidu-selection--trash-table))
         (batch-p (or (use-region-p) chidu-selection-command-ids))
         (ids (chidu-selection--edit-ids)))
    (dolist (id ids)
      (pcase operation
        ('mark
         (remhash id trash-table)
         (puthash id t table))
        ('trash
         (remhash id table)
         (puthash id t trash-table))
        ('unmark
         (remhash id table)
         (remhash id trash-table))
        ('toggle
         ;; Dired's `dired-toggle-marks' toggles only `*' and spaces; `D'
         ;; remains a deletion flag until explicitly unmarked or overwritten.
         (unless (gethash id trash-table)
           (if (gethash id table)
               (remhash id table)
             (puthash id t table))))
        (_ (error "Unknown process-marker operation: %S" operation))))
    (when (and (not batch-p) (functionp chidu-selection-next-function))
      (funcall chidu-selection-next-function))
    (chidu-selection--refresh ids))
  (chidu-selection--report))

(defun chidu-selection-mark ()
  "Mark the active region or Email at point, then advance when appropriate."
  (interactive)
  (chidu-selection--mutate 'mark))

(defun chidu-selection-toggle ()
  "Toggle ordinary marks for the active region or Email, then advance.

Trash flags remain unchanged, matching Dired's marker behavior."
  (interactive)
  (chidu-selection--mutate 'toggle))

(defun chidu-selection-flag-trash ()
  "Flag the active region or Email at point for move to Trash, then advance."
  (interactive)
  (chidu-selection--mutate 'trash))

(defun chidu-selection-unmark (&optional all-p)
  "Unmark the active region or Email at point.

With prefix argument ALL-P, clear every ordinary mark and Trash flag.  A
single-row unmark advances to the next selectable row."
  (interactive "P")
  (if all-p
      (chidu-selection-clear)
    (chidu-selection--mutate 'unmark)))

(defun chidu-selection-clear ()
  "Clear every ordinary process mark and Trash flag in the current view."
  (interactive)
  (let ((ids
         (delete-dups
          (append (chidu-selection-marked-ids)
                  (chidu-selection-trash-flagged-ids)))))
    (clrhash (chidu-selection--table))
    (clrhash (chidu-selection--trash-table))
    (chidu-selection--refresh ids))
  (chidu-selection--report))

(defun chidu-selection-mark-all ()
  "Mark every visible Email row."
  (interactive)
  (let ((ids (chidu-selection-visible-ids))
        (table (chidu-selection--table))
        (trash-table (chidu-selection--trash-table)))
    (unless ids (user-error "No visible Email rows"))
    (dolist (id ids)
      (remhash id trash-table)
      (puthash id t table))
    (chidu-selection--refresh ids))
  (chidu-selection--report))

(defun chidu-selection-toggle-all ()
  "Toggle ordinary marks for visible rows, preserving Trash flags."
  (interactive)
  (let* ((ids (chidu-selection-visible-ids))
         (table (chidu-selection--table))
         (trash-table (chidu-selection--trash-table))
         (ordinary-ids
          (cl-remove-if (lambda (id) (gethash id trash-table)) ids))
         (all-marked-p
          (and ordinary-ids
               (cl-every (lambda (id) (gethash id table)) ordinary-ids))))
    (unless ids (user-error "No visible Email rows"))
    (dolist (id ordinary-ids)
      (if all-marked-p
          (remhash id table)
        (puthash id t table)))
    (chidu-selection--refresh ordinary-ids))
  (chidu-selection--report))

(defun chidu-selection--marked-positions ()
  "Return visible marker row positions in display order."
  (let (positions)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when-let* ((id (chidu-selection--row-id-at (point)))
                    ((or (chidu-selection-marked-p id)
                         (chidu-selection-trash-flagged-p id))))
          (push (line-beginning-position) positions))
        (forward-line 1)))
    (nreverse positions)))

(defun chidu-selection-next-marked ()
  "Move to the next visible Email carrying either marker kind."
  (interactive)
  (if-let* ((position
             (cl-find-if (lambda (candidate) (> candidate (point)))
                         (chidu-selection--marked-positions))))
      (goto-char position)
    (message "No later marked or Trash-flagged Email")))

(defun chidu-selection-previous-marked ()
  "Move to the previous visible Email carrying either marker kind."
  (interactive)
  (let (position)
    (dolist (candidate (chidu-selection--marked-positions))
      (when (< candidate (line-beginning-position))
        (setq position candidate)))
    (if position
        (goto-char position)
      (message "No earlier marked or Trash-flagged Email"))))

(defun chidu-selection-prune-to-ids (valid-ids)
  "Remove markers absent from VALID-IDS and return the number removed."
  (let ((table (chidu-selection--table))
        (trash-table (chidu-selection--trash-table))
        (valid (make-hash-table :test #'equal))
        stale-marks stale-trash)
    (mapc (lambda (id) (puthash id t valid)) valid-ids)
    (maphash (lambda (id _)
               (unless (gethash id valid) (push id stale-marks)))
             table)
    (maphash (lambda (id _)
               (unless (gethash id valid) (push id stale-trash)))
             trash-table)
    (dolist (id stale-marks) (remhash id table))
    (dolist (id stale-trash) (remhash id trash-table))
    (+ (length stale-marks) (length stale-trash))))

(defun chidu-selection-icon-inserter (local-email-id)
  "Return an icon inserter for LOCAL-EMAIL-ID's Dired-style marker."
  (lambda ()
    (cond
     ((chidu-selection-trash-flagged-p local-email-id)
      (insert (propertize "D" 'face 'chidu-selection-trash-marker)))
     ((chidu-selection-marked-p local-email-id)
      (insert (propertize "*" 'face 'chidu-selection-mark))))))

(provide 'chidu-selection)

;;; chidu-selection.el ends here
