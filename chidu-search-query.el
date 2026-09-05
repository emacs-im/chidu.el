;;; chidu-search-query.el --- Compile Chidu search text to JMAP -*- lexical-binding: t; -*-

;;; Commentary:

;; A small explicit query language for account- or Mailbox-scoped server search.
;; The compiled JMAP Filter is immutable view state and has a stable content key;
;; UI code never reparses it during rendering.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'time-date)
(require 'chidu-record)
(require 'chidu-store)

(chidu-define-record chidu-search-spec
    "One normalized Email search request."
  query-key
  query-text
  filter
  filter-json
  mailbox-id
  remote-mailbox-id)

(defvar chidu-search-history nil
  "Minibuffer history for Chidu Email searches.")

(defun chidu-search-query--present-string (value label)
  "Return trimmed nonempty string VALUE for LABEL, or signal a user error."
  (unless (stringp value)
    (user-error "%s must be a string" label))
  (let ((text (string-trim value)))
    (when (string-empty-p text)
      (user-error "%s must not be empty" label))
    text))

(defun chidu-search-query--date (value operator)
  "Return UTCDate search VALUE for OPERATOR."
  (let ((text (chidu-search-query--present-string value operator)))
    (condition-case nil
        (if (string-match-p
             "\\`[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}\\'" text)
            (concat text "T00:00:00Z")
          (format-time-string "%Y-%m-%dT%H:%M:%SZ" (date-to-time text) t))
      (error (user-error "%s needs YYYY-MM-DD or an ISO date" operator)))))

(defun chidu-search-query--available-mailboxes (mailboxes)
  "Return available Mailboxes from MAILBOXES as a list."
  (unless (vectorp mailboxes)
    (signal 'wrong-type-argument (list 'vectorp mailboxes)))
  (cl-loop for mailbox across mailboxes
           when (chidu-store-mailbox-available-p mailbox)
           collect mailbox))

(defun chidu-search-query--resolve-mailbox (mailboxes designator)
  "Resolve Mailbox DESIGNATOR in MAILBOXES, or signal a user error."
  (let* ((needle (downcase
                  (chidu-search-query--present-string designator "in:")))
         (available (chidu-search-query--available-mailboxes mailboxes))
         (role-matches
          (cl-remove-if-not
           (lambda (mailbox)
             (equal needle (chidu-store-mailbox-role mailbox)))
           available))
         (name-matches
          (cl-remove-if-not
           (lambda (mailbox)
             (equal needle (downcase (chidu-store-mailbox-name mailbox))))
           available))
         (matches (or role-matches name-matches)))
    (pcase (length matches)
      (0 (user-error "No available Mailbox matches in:%s" designator))
      (1 (car matches))
      (_ (user-error "Mailbox name in:%s is ambiguous" designator)))))

(defun chidu-search-query--keyword-condition (value)
  "Return JMAP keyword condition for is: VALUE."
  (pcase (downcase value)
    ("unread" (list :notKeyword "$seen"))
    ("read" (list :hasKeyword "$seen"))
    ("flagged" (list :hasKeyword "$flagged"))
    ("unflagged" (list :notKeyword "$flagged"))
    ("answered" (list :hasKeyword "$answered"))
    ("unanswered" (list :notKeyword "$answered"))
    ("draft" (list :hasKeyword "$draft"))
    (_ (user-error "Unknown is: search value: %s" value))))

(defun chidu-search-query--attachment-condition (value)
  "Return JMAP attachment condition for has: VALUE."
  (pcase (downcase value)
    ("attachment" (list :hasAttachment t))
    ((or "no-attachment" "noattachment") (list :hasAttachment :json-false))
    (_ (user-error "Unknown has: search value: %s" value))))

(defun chidu-search-query--header-condition (value)
  "Return JMAP header condition for header: VALUE."
  (let* ((text (chidu-search-query--present-string value "header:"))
         (separator (string-search "=" text))
         (name (string-trim (if separator (substring text 0 separator) text)))
         (match (and separator (string-trim (substring text (1+ separator))))))
    (when (string-empty-p name)
      (user-error "Header: needs a field name"))
    (list :header
          (if separator
              (vector name match)
            (vector name)))))

(defun chidu-search-query--operator-condition (operator value)
  "Return JMAP condition for OPERATOR and VALUE, or nil when not an operator."
  (pcase operator
    ((or "text" "from" "to" "cc" "bcc" "subject" "body")
     (list (intern (concat ":" operator))
           (chidu-search-query--present-string value (concat operator ":"))))
    ("before" (list :before (chidu-search-query--date value "before:")))
    ("after" (list :after (chidu-search-query--date value "after:")))
    ("is" (chidu-search-query--keyword-condition value))
    ("has" (chidu-search-query--attachment-condition value))
    ("list"
     (list :header
           (vector "List-Id"
                   (chidu-search-query--present-string value "list:"))))
    ("header" (chidu-search-query--header-condition value))
    (_ nil)))

(defun chidu-search-query--and-filter (conditions)
  "Return canonical JMAP AND filter for CONDITIONS."
  (pcase conditions
    (`() (user-error "Email search needs at least one condition"))
    (`(,condition) condition)
    (_ (list :operator "AND" :conditions (vconcat conditions)))))

(defun chidu-search-query-compile (query mailboxes &optional forced-mailbox)
  "Compile QUERY against MAILBOXES and optional FORCED-MAILBOX.

Bare words use the JMAP `text' filter.  Supported operators are `from:',
`to:', `cc:', `bcc:', `subject:', `body:', `text:', `before:', `after:',
`is:', `has:', `list:', `header:', and `in:'.  Return a
`chidu-search-spec'."
  (unless (or (null forced-mailbox)
              (chidu-store-mailbox-p forced-mailbox))
    (signal 'wrong-type-argument
            (list 'chidu-store-mailbox-p forced-mailbox)))
  (let* ((query-text (chidu-search-query--present-string query "Search query"))
         (tokens (split-string-and-unquote query-text))
         (scope-mailbox forced-mailbox)
         conditions
         bare)
    (dolist (token tokens)
      (if (string-match "\\`\\([[:alpha:]-]+\\):\\(.*\\)\\'" token)
          (let* ((operator (downcase (match-string 1 token)))
                 (value (match-string 2 token)))
            (if (equal operator "in")
                (let ((mailbox
                       (chidu-search-query--resolve-mailbox mailboxes value)))
                  (when (and scope-mailbox
                             (not
                              (equal
                               (chidu-store-mailbox-mailbox-id scope-mailbox)
                               (chidu-store-mailbox-mailbox-id mailbox))))
                    (user-error
                     "Search is already scoped to %s"
                     (chidu-store-mailbox-name scope-mailbox)))
                  (setq scope-mailbox mailbox))
              (if-let* ((condition
                         (chidu-search-query--operator-condition
                          operator value)))
                  (push condition conditions)
                (push token bare))))
        (push token bare)))
    (when bare
      (push (list :text (string-join (nreverse bare) " ")) conditions))
    (when scope-mailbox
      (push
       (list :inMailbox
             (chidu-store-mailbox-remote-mailbox-id scope-mailbox))
       conditions))
    (let* ((filter (chidu-search-query--and-filter (nreverse conditions)))
           (filter-json
            (json-serialize filter :null-object :json-null
                            :false-object :json-false))
           (query-key
            (secure-hash
             'sha256
             (encode-coding-string
              (concat "chidu-search-v1\0" filter-json) 'utf-8-unix))))
      (chidu-search-spec-create
       :query-key query-key
       :query-text query-text
       :filter filter
       :filter-json filter-json
       :mailbox-id
       (and scope-mailbox
            (chidu-store-mailbox-mailbox-id scope-mailbox))
       :remote-mailbox-id
       (and scope-mailbox
            (chidu-store-mailbox-remote-mailbox-id scope-mailbox))))))

(provide 'chidu-search-query)

;;; chidu-search-query.el ends here
