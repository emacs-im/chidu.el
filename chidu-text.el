;;; chidu-text.el --- Email text presentation primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Thread-scoped participant colouring and source-preserving quote presentation.
;; Canonical Email text remains unchanged: quote markers stay in the buffer and
;; in copied text while Appkit projects vertical bars, wrap prefixes, and subtle
;; depth-specific backgrounds.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'appkit-name-color)
(require 'appkit-ui)
(require 'chidu-store)

(defface chidu-email-flagged
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for flagged Emails."
  :group 'chidu)

(defun chidu-email-format-time (date)
  "Return concise display time for DATE."
  (condition-case nil
      (format-time-string "%m-%d %H:%M" (date-to-time date))
    (error date)))

(defun chidu-email-sender (row)
  "Return human-readable sender for Email Summary ROW."
  (let ((name (chidu-store-email-summary-row-from-name row))
        (email (chidu-store-email-summary-row-from-email row)))
    (cond
     ((and (stringp name) (not (string-empty-p name))) name)
     ((and (stringp email) (not (string-empty-p email))) email)
     (t "Unknown sender"))))

(defun chidu-email-subject (row)
  "Return nonempty subject label for Email Summary ROW."
  (let ((subject (chidu-store-email-summary-row-subject row)))
    (if (string-empty-p subject) "(no subject)" subject)))

(defun chidu-text-next-property-row (property no-result)
  "Move to the next non-nil PROPERTY row, or report NO-RESULT."
  (if-let* ((match (text-property-search-forward property nil nil t)))
      (goto-char (prop-match-beginning match))
    (message "%s" no-result)))

(defun chidu-text-previous-property-row (property no-result)
  "Move to the previous non-nil PROPERTY row, or report NO-RESULT."
  (if-let* ((match (text-property-search-backward property nil nil t)))
      (goto-char (prop-match-beginning match))
    (message "%s" no-result)))

(defcustom chidu-text-highlight-participants t
  "When non-nil, colour exact names and addresses of thread participants."
  :type 'boolean
  :group 'chidu)

(defcustom chidu-text-quote-background-alpha 0.10
  "Accent fraction used for quote-depth background tinting."
  :type 'number
  :group 'chidu)

(cl-defstruct (chidu-text-participant
               (:constructor chidu-text-participant-create))
  "One presentation identity observed in the current mail thread."
  identity
  name
  email
  face)

(defun chidu-text-email-address-identity (address)
  "Return stable presentation identity for Store Email ADDRESS."
  (unless (chidu-store-email-address-p address)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-address-p address)))
  (let ((email (chidu-store-email-address-email address))
        (name (chidu-store-email-address-name address)))
    (cond
     ((and (stringp email) (not (string-empty-p email)))
      (concat "mail:" (downcase (string-trim email))))
     ((and (stringp name) (not (string-empty-p name)))
      (concat "name:" (downcase (string-trim name)))))))

(defun chidu-text-email-address-label (address)
  "Return copyable label for Store Email ADDRESS with identity presentation."
  (unless (chidu-store-email-address-p address)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-address-p address)))
  (let* ((name (chidu-store-email-address-name address))
         (email (chidu-store-email-address-email address))
         (name-present (and (stringp name) (not (string-empty-p name))))
         (email-present (and (stringp email) (not (string-empty-p email))))
         (label
          (cond
           ((and name-present email-present) (format "%s <%s>" name email))
           (name-present name)
           (email-present email)
           (t "Unknown address")))
         (identity (chidu-text-email-address-identity address))
         (face (and identity (appkit-name-color-face identity)))
         (result (copy-sequence label)))
    (when identity
      (add-text-properties
       0 (length result)
       (list 'chidu-person-identity identity
             'help-echo (if email-present email label)
             'rear-nonsticky '(chidu-person-identity help-echo))
       result))
    (when face
      (add-face-text-property 0 (length result) face 'append result))
    result))

(defun chidu-text-email-address-list (addresses)
  "Return comma-separated copyable presentation for ADDRESSES vector."
  (unless (vectorp addresses)
    (signal 'wrong-type-argument (list 'vectorp addresses)))
  (mapconcat #'chidu-text-email-address-label (append addresses nil) ", "))

(defun chidu-text-address-participants (&rest address-vectors)
  "Return unique participants observed in ADDRESS-VECTORS."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (addresses address-vectors)
      (when (vectorp addresses)
        (cl-loop
         for address across addresses
         for identity = (chidu-text-email-address-identity address)
         when (and identity (not (gethash identity seen)))
         do
         (puthash identity t seen)
         (push
          (chidu-text-participant-create
           :identity identity
           :name (chidu-store-email-address-name address)
           :email (chidu-store-email-address-email address)
           :face (appkit-name-color-face identity))
          result))))
    (nreverse result)))

(defun chidu-text--summary-row (item)
  "Return Email Summary row represented by ITEM, or nil."
  (cond
   ((chidu-store-email-summary-row-p item) item)
   ((chidu-store-conversation-row-p item)
    (chidu-store-conversation-row-summary-row item))))

(defun chidu-text-person-identity (row)
  "Return stable presentation identity for sender of Summary ROW."
  (let ((email (chidu-store-email-summary-row-from-email row))
        (name (chidu-store-email-summary-row-from-name row)))
    (cond
     ((and (stringp email) (not (string-empty-p email)))
      (concat "mail:" (downcase (string-trim email))))
     ((and (stringp name) (not (string-empty-p name)))
      (concat "name:" (downcase (string-trim name)))))))

(defun chidu-text-person-label (row)
  "Return ROW sender label carrying its deterministic participant face."
  (let* ((name (chidu-store-email-summary-row-from-name row))
         (email (chidu-store-email-summary-row-from-email row))
         (label
          (cond
           ((and (stringp name) (not (string-empty-p name))) name)
           ((and (stringp email) (not (string-empty-p email))) email)
           (t "Unknown sender")))
         (identity (chidu-text-person-identity row))
         (face (and identity (appkit-name-color-face identity)))
         (result (copy-sequence label)))
    (when identity
      (add-text-properties
       0 (length result)
       (list 'chidu-person-identity identity
             'help-echo
             (if (and (stringp email) (not (string-empty-p email)))
                 email
               label)
             'rear-nonsticky '(chidu-person-identity help-echo))
       result))
    (when face
      (add-face-text-property 0 (length result) face 'append result))
    result))

(defun chidu-text-participants (rows)
  "Return unique participants observed in Summary or Conversation ROWS."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (seq-doseq (item rows)
      (when-let* ((row (chidu-text--summary-row item))
                  (identity (chidu-text-person-identity row)))
        (unless (gethash identity seen)
          (let ((participant
                 (chidu-text-participant-create
                  :identity identity
                  :name (chidu-store-email-summary-row-from-name row)
                  :email (chidu-store-email-summary-row-from-email row)
                  :face (appkit-name-color-face identity))))
            (puthash identity participant seen)
            (push participant result)))))
    (nreverse result)))

(defun chidu-text--clean-participant-name (name)
  "Return compact human NAME for body matching."
  (when (and (stringp name) (not (string-empty-p name)))
    (let ((clean (string-trim name)))
      ;; Mailman may rewrite a sender as “Name viaList name”.  Keep the wire
      ;; display name elsewhere, but recover the human prefix as an additional
      ;; body alias.
      (setq clean
            (replace-regexp-in-string
             "[[:space:]]+via[[:space:]]*.+\\'" "" clean t t))
      (string-trim clean))))

(defun chidu-text--participant-name-aliases (participant)
  "Return conservative name aliases for PARTICIPANT."
  (when-let* ((clean
               (chidu-text--clean-participant-name
                (chidu-text-participant-name participant))))
    (let* ((original (string-trim (chidu-text-participant-name participant)))
           (parts (split-string clean "[[:space:]]+" t))
           (first (car parts))
           (last (car (last parts)))
           aliases)
      (dolist (alias (delete-dups (list original clean first last)))
        (when (and (stringp alias)
                   (not (string-empty-p alias))
                   (or (string-match-p "[[:space:]]" alias)
                       (>= (length alias) 3))
                   (not
                    (member (downcase alias)
                            '("me" "you" "unknown sender"))))
          (push alias aliases)))
      (nreverse aliases))))

(defun chidu-text--participant-alias-index (participants)
  "Return (ALIASES . INDEX) for unambiguous PARTICIPANTS.

INDEX maps a downcased alias onto one participant or the symbol `ambiguous'."
  (let ((index (make-hash-table :test #'equal))
        aliases)
    (dolist (participant participants)
      (dolist (alias
               (append
                (chidu-text--participant-name-aliases participant)
                (when-let* ((email (chidu-text-participant-email participant))
                            ((not (string-empty-p email))))
                  (list (string-trim email)))))
        (let* ((normalized (downcase alias))
               (existing (gethash normalized index)))
          (cond
           ((null existing)
            (puthash normalized participant index)
            (push alias aliases))
           ((and (chidu-text-participant-p existing)
                 (not
                  (equal (chidu-text-participant-identity existing)
                         (chidu-text-participant-identity participant))))
            (puthash normalized 'ambiguous index))))))
    (cons (delete-dups aliases) index)))

(defun chidu-text--wordish-char-p (character)
  "Return non-nil when CHARACTER is a word-like boundary character."
  (and character (memq (char-syntax character) '(?w ?_))))

(defun chidu-text--participant-match-boundary-p (start end)
  "Return non-nil when START..END is not embedded in a larger word."
  (and
   (or (not (chidu-text--wordish-char-p (char-after start)))
       (not (chidu-text--wordish-char-p (char-before start))))
   (or (not (chidu-text--wordish-char-p (char-before end)))
       (not (chidu-text--wordish-char-p (char-after end))))))

(defun chidu-text-apply-participant-highlights (start end participants)
  "Colour exact PARTICIPANTS aliases between START and END."
  (when (and chidu-text-highlight-participants
             (< start end)
             participants)
    (pcase-let* ((`(,aliases . ,index)
                  (chidu-text--participant-alias-index participants)))
      (when aliases
        (let ((regexp (regexp-opt aliases))
              (case-fold-search t))
          (save-excursion
            (goto-char start)
            (while (re-search-forward regexp end t)
              (let* ((match-start (match-beginning 0))
                     (match-end (match-end 0))
                     (participant
                      (gethash
                       (downcase (match-string-no-properties 0)) index)))
                (when (and (chidu-text-participant-p participant)
                           (chidu-text--participant-match-boundary-p
                            match-start match-end))
                  (when-let* ((face
                               (chidu-text-participant-face participant)))
                    (add-face-text-property
                     match-start match-end face 'append))
                  (add-text-properties
                   match-start match-end
                   (list
                    'chidu-person-identity
                    (chidu-text-participant-identity participant)
                    'help-echo
                    (or (chidu-text-participant-email participant)
                        (chidu-text-participant-name participant))
                    'rear-nonsticky
                    '(chidu-person-identity help-echo))))))))))))

(defun chidu-text--quote-accent-face (level)
  "Return palette accent face for quote LEVEL."
  (when (> (length appkit-name-color-palette) 0)
    (seq-elt appkit-name-color-palette
             (mod (1- level) (length appkit-name-color-palette)))))

(defun chidu-text--quote-background-face (level)
  "Return background-only face for quote LEVEL, or nil."
  (when-let* ((accent (chidu-text--quote-accent-face level)))
    (appkit-ui-tinted-background-face
     accent :alpha chidu-text-quote-background-alpha)))

(defun chidu-text--quote-line-info (line-start line-end)
  "Return quote marker information for LINE-START..LINE-END, or nil."
  (save-excursion
    (goto-char line-start)
    (skip-chars-forward " \t" line-end)
    (when (eq (char-after) ?>)
      (let ((depth 0)
            source-end
            done)
        (while (and (not done) (< (point) line-end))
          (if (not (eq (char-after) ?>))
              (setq done t)
            (cl-incf depth)
            (forward-char 1)
            (let ((spaces-start (point)))
              (skip-chars-forward " \t" line-end)
              (if (eq (char-after) ?>)
                  nil
                ;; Normalize one separator space after the final marker, but
                ;; leave additional whitespace intact for quoted code blocks.
                (setq source-end
                      (if (< spaces-start (point))
                          (1+ spaces-start)
                        spaces-start)
                      done t)))))
        (and source-end (> depth 0)
             (list :source-end source-end :depth depth))))))

(defun chidu-text--quote-visual-prefix (start end background-face)
  "Return visual quote prefix for source START..END using BACKGROUND-FACE."
  (let ((position start)
        (level 0)
        pieces)
    (while (< position end)
      (let ((character (char-after position)))
        (if (eq character ?>)
            (let* ((accent (chidu-text--quote-accent-face (cl-incf level)))
                   (bar (appkit-ui-vbar-string accent)))
              (when background-face
                (add-face-text-property
                 0 (length bar) background-face 'append bar))
              (push bar pieces))
          (push (char-to-string character) pieces)))
      (cl-incf position))
    (let ((prefix (apply #'concat (nreverse pieces))))
      (when (and background-face (> (length prefix) 0))
        (add-face-text-property
         0 (length prefix) background-face 'append prefix))
      prefix)))

(defun chidu-text-markup-quote-style (depth)
  "Return Appkit quote styling matching plain mail at DEPTH."
  (let* ((background (chidu-text--quote-background-face depth))
         (prefix (concat (appkit-ui-vbar-string
                          (chidu-text--quote-accent-face depth)) " ")))
    (when background
      (add-face-text-property 0 (length prefix) background 'append prefix))
    (list :prefix prefix :face background)))

(defun chidu-text-apply-quote-presentation (start end)
  "Present RFC-style quote markers between START and END.

The underlying `>' markers remain untouched and copyable.  They are hidden in
the buffer display and replaced by depth-coloured vertical bars.  Quote text
keeps its normal foreground; only its background is tinted by depth."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let* ((line-start (line-beginning-position))
             (line-end (min end (line-end-position)))
             (span-end
              (if (and (< line-end end) (eq (char-after line-end) ?\n))
                  (1+ line-end)
                line-end))
             (info (chidu-text--quote-line-info line-start line-end)))
        (when info
          (let* ((source-end (plist-get info :source-end))
                 (depth (plist-get info :depth))
                 (background (chidu-text--quote-background-face depth))
                 (prefix
                  (chidu-text--quote-visual-prefix
                   line-start source-end background)))
            (when (< line-start span-end)
              (when background
                (add-face-text-property
                 line-start span-end background 'append))
              (add-text-properties
               line-start span-end
               (list 'chidu-message-quote-depth depth
                     'rear-nonsticky '(chidu-message-quote-depth))))
            (appkit-ui-apply-source-line-prefix
             line-start span-end line-start source-end prefix
             :continuation-prefix prefix)))
        (goto-char span-end)))))

(defun chidu-text-present-region (start end participants)
  "Apply quote and PARTICIPANTS presentation to START..END."
  (chidu-text-apply-quote-presentation start end)
  (chidu-text-apply-participant-highlights start end participants))

(provide 'chidu-text)

;;; chidu-text.el ends here
