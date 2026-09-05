;;; chidu-jmap-types.el --- Shared JMAP wire validators -*- lexical-binding: t; -*-

;;; Commentary:

;; Mechanical validation shared by Session and method adapters.  These helpers
;; copy accepted strings and never mutate Store or application state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(define-error 'chidu-jmap-error
              "Invalid or unavailable JMAP result")

(defconst chidu-jmap-core-capability "urn:ietf:params:jmap:core")
(defconst chidu-jmap-mail-capability "urn:ietf:params:jmap:mail")
(defconst chidu-jmap-submission-capability
  "urn:ietf:params:jmap:submission")
(defconst chidu-jmap-contacts-capability
  "urn:ietf:params:jmap:contacts")
(defconst chidu-jmap-safe-integer-max 9007199254740991)

(defun chidu-jmap--json-boolean (value context)
  "Decode JSON Boolean VALUE for CONTEXT."
  (cond
   ((eq value t) t)
   ((eq value :json-false) nil)
   (t
    (signal 'chidu-jmap-error
            (list (format "%s must be a JSON Boolean" context))))))

(defun chidu-jmap--string (value context &optional allow-empty)
  "Return copied string VALUE for CONTEXT; permit empty when ALLOW-EMPTY."
  (unless (and (stringp value)
               (or allow-empty (not (string-empty-p value))))
    (signal 'chidu-jmap-error
            (list (format "%s must be %sa string"
                          context (if allow-empty "" "a nonempty ")))))
  (copy-sequence value))

(defun chidu-jmap--id (value context)
  "Return RFC 8620 Id VALUE for CONTEXT, or signal."
  (let ((text (chidu-jmap--string value context)))
    (unless (and (<= (string-bytes text) 255)
                 (string-match-p "\\`[A-Za-z0-9_-]+\\'" text))
      (signal 'chidu-jmap-error
              (list
               (format
                "%s must be a 1-255 octet base64url Id without padding"
                context))))
    text))

(defun chidu-jmap--hash (value context)
  "Return hash-table VALUE for CONTEXT."
  (unless (hash-table-p value)
    (signal 'chidu-jmap-error
            (list (format "%s must be a JSON object" context))))
  value)

(defun chidu-jmap--vector (value context)
  "Return vector VALUE for CONTEXT."
  (unless (vectorp value)
    (signal 'chidu-jmap-error
            (list (format "%s must be a JSON array" context))))
  value)

(defun chidu-jmap--required (object key context)
  "Return KEY from hash OBJECT, requiring presence for CONTEXT."
  (let* ((missing (make-symbol "missing"))
         (value (gethash key object missing)))
    (when (eq value missing)
      (signal 'chidu-jmap-error
              (list (format "%s is missing %s" context key))))
    value))

(defun chidu-jmap--safe-positive-integer (value context)
  "Return safe positive integer VALUE for CONTEXT."
  (unless (and (integerp value)
               (> value 0)
               (<= value chidu-jmap-safe-integer-max))
    (signal 'chidu-jmap-error
            (list (format "%s must be a safe positive integer" context))))
  value)

(defun chidu-jmap--safe-nonnegative-integer (value context)
  "Return safe nonnegative integer VALUE for CONTEXT."
  (unless (and (integerp value)
               (>= value 0)
               (<= value chidu-jmap-safe-integer-max))
    (signal 'chidu-jmap-error
            (list
             (format "%s must be a safe nonnegative integer" context))))
  value)

(defun chidu-jmap--nullable-string (value context)
  "Return nil for JSON null or copied string VALUE for CONTEXT."
  (if (eq value :json-null)
      nil
    (chidu-jmap--string value context)))

(defun chidu-jmap--nullable-id (value context)
  "Return nil for JSON null or RFC 8620 Id VALUE for CONTEXT."
  (if (eq value :json-null)
      nil
    (chidu-jmap--id value context)))

(defun chidu-jmap--nullable-vector (value context)
  "Return an empty vector for JSON null or vector VALUE for CONTEXT."
  (if (eq value :json-null)
      (vector)
    (chidu-jmap--vector value context)))

(defun chidu-jmap--true-map-keys (value context &optional ids-p)
  "Return sorted true-valued object keys from VALUE for CONTEXT.

Validate keys as RFC 8620 Ids when IDS-P is non-nil."
  (let ((object (chidu-jmap--hash value context))
        keys)
    (maphash
     (lambda (key present-p)
       (unless (eq present-p t)
         (signal 'chidu-jmap-error
                 (list (format "%s contains a non-true value" context))))
       (push (if ids-p (chidu-jmap--id key context) key) keys))
     object)
    (vconcat (sort keys #'string-lessp))))

(defun chidu-jmap--optional-true-map-keys
    (object key context &optional ids-p)
  "Return optional true-map KEY values from OBJECT for CONTEXT.

Validate keys as RFC 8620 Ids when IDS-P is non-nil."
  (let ((value (gethash key object :json-null)))
    (if (eq value :json-null)
        (vector)
      (chidu-jmap--true-map-keys value context ids-p))))

(defun chidu-jmap-patch-path-component (value context)
  "Return VALUE escaped as one JMAP PatchObject path component for CONTEXT."
  (let ((text (chidu-jmap--string value context)))
    (string-replace "/" "~1" (string-replace "~" "~0" text))))

(defun chidu-jmap--mime-token-p (value)
  "Return non-nil when VALUE is one RFC 2045 MIME token."
  (and
   (stringp value)
   (> (length value) 0)
   (cl-loop
    for character across value
    always
    (and (<= 33 character 126)
         (not
          (memq character
                '(?\( ?\) ?< ?> ?@ ?, ?\; ?: ?\\ ?\" ?/ ?\[ ?\] ?? ?=)))))))

(defun chidu-jmap--media-type (value context)
  "Return normalized parameter-free MIME media type VALUE for CONTEXT."
  (let* ((text (downcase (chidu-jmap--string value context)))
         (separator (string-search "/" text)))
    (unless
        (and separator
             (> separator 0)
             (< separator (1- (length text)))
             (null (string-search "/" text (1+ separator)))
             (chidu-jmap--mime-token-p (substring text 0 separator))
             (chidu-jmap--mime-token-p (substring text (1+ separator))))
      (signal 'chidu-jmap-error
              (list (format "%s is not a parameter-free media type" context))))
    text))

(defun chidu-jmap--capability-names (value context)
  "Return sorted capability-name vector from object VALUE for CONTEXT."
  (let ((object (chidu-jmap--hash value context))
        names)
    (maphash
     (lambda (key _arguments)
       (push (chidu-jmap--string key context) names))
     object)
    (vconcat (sort names #'string<))))

(defun chidu-jmap--parse-url (value context)
  "Parse safe absolute HTTPS URL VALUE for CONTEXT."
  (let* ((text (chidu-jmap--string value context))
         (url (url-generic-parse-url text)))
    (unless (and (equal "https" (url-type url))
                 (stringp (url-host url))
                 (not (string-empty-p (url-host url)))
                 (null (url-user url))
                 (null (url-password url))
                 (null (url-target url)))
      (signal 'chidu-jmap-error
              (list
               (format
                "%s must be absolute HTTPS without userinfo or fragment"
                context))))
    url))

(defun chidu-jmap--origin (url)
  "Return normalized origin tuple for parsed URL."
  (list (downcase (url-host url)) (or (url-port url) 443)))

(defun chidu-jmap--same-origin-p (left right)
  "Return non-nil when parsed URLs LEFT and RIGHT share an HTTPS origin."
  (equal (chidu-jmap--origin left) (chidu-jmap--origin right)))

(defun chidu-jmap--https-template (value context)
  "Validate HTTPS template string VALUE for CONTEXT and copy it."
  (let ((text (chidu-jmap--string value context)))
    (unless (string-prefix-p "https://" text)
      (signal 'chidu-jmap-error
              (list (format "%s must use HTTPS" context))))
    text))

(defun chidu-jmap--expand-url-template
    (template context values required)
  "Expand a level-1 HTTPS URI TEMPLATE for CONTEXT.

VALUES is an alist of variable-name strings to nonempty string values.
REQUIRED is the list of variable names that must occur in TEMPLATE.  Only
level-1 simple string expansion is accepted; operators, explode, and prefix
modifiers are rejected."
  (setq template (chidu-jmap--https-template template context))
  (unless (proper-list-p values)
    (signal 'chidu-jmap-error
            (list (format "%s values must be an alist" context))))
  (unless (proper-list-p required)
    (signal 'chidu-jmap-error
            (list (format "%s required variables must be a list" context))))
  (let ((known nil))
    (dolist (entry values)
      (unless (and (consp entry)
                   (stringp (car entry))
                   (string-match-p "\\`[A-Za-z][A-Za-z0-9_]*\\'"
                                   (car entry))
                   (stringp (cdr entry))
                   (not (string-empty-p (cdr entry))))
        (signal 'chidu-jmap-error
                (list (format "%s contains an invalid variable value"
                              context))))
      (when (member (car entry) known)
        (signal 'chidu-jmap-error
                (list (format "%s repeats variable %s"
                              context (car entry)))))
      (push (car entry) known))
    (dolist (name required)
      (unless (and (stringp name)
                   (member name known))
        (signal 'chidu-jmap-error
                (list (format "%s requires unknown variable %S"
                              context name)))))
    (let ((position 0)
          seen
          pieces)
      (while (string-match "{\\([^{}]+\\)}" template position)
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (name (match-string 1 template))
               (entry (assoc name values)))
          (unless
              (string-match-p "\\`[A-Za-z][A-Za-z0-9_]*\\'" name)
            (signal 'chidu-jmap-error
                    (list
                     (format
                      "%s contains a non-level-1 URI template expression"
                      context))))
          (unless entry
            (signal 'chidu-jmap-error
                    (list (format "%s uses unsupported variable %s"
                                  context name))))
          (push (substring template position match-start) pieces)
          (push (url-hexify-string (cdr entry)) pieces)
          (cl-pushnew name seen :test #'equal)
          (setq position match-end)))
      (push (substring template position) pieces)
      (dolist (name required)
        (unless (member name seen)
          (signal 'chidu-jmap-error
                  (list (format "%s is missing {%s}" context name)))))
      (let ((expanded (apply #'concat (nreverse pieces))))
        (when (string-match-p "[{}]" expanded)
          (signal 'chidu-jmap-error
                  (list (format "%s contains an unmatched template expression"
                                context))))
        (chidu-jmap--parse-url expanded (format "expanded %s" context))
        expanded))))

(defun chidu-jmap--parse-json-object (text context)
  "Decode JSON TEXT as an object for CONTEXT."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (let ((value
         (condition-case error-data
             (json-parse-string
              text
              :object-type 'hash-table
              :array-type 'array
              :null-object :json-null
              :false-object :json-false)
           (error
            (signal 'chidu-jmap-error
                    (list
                     (format "%s JSON is invalid: %s"
                             context
                             (error-message-string error-data))))))))
    (chidu-jmap--hash value context)))

(provide 'chidu-jmap-types)

;;; chidu-jmap-types.el ends here
