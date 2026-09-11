;;; chidu-message-github.el --- Native GitHub notification presentation -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Adapt notification HTML directly to Appkit semantics.  HTML is presentation
;; input, not executable content; the stored MIME alternatives remain canonical.
;; No SHR, Markdown reparsing, or remote resource acquisition is involved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dom)
(require 'appkit-markup)
(require 'appkit-markup-ui)
(require 'appkit-fontify)
(require 'appkit-name-color)
(require 'chidu-store)
(require 'chidu-browse)
(require 'chidu-text)

(defcustom chidu-message-github-sender-addresses '("notifications@github.com")
  "Parsed From addresses whose bodies use GitHub presentation.
Matching is case-insensitive and exact, never against display names.
For Enterprise notifications also configure `chidu-message-github-site-url'."
  :type '(repeat string)
  :group 'chidu)

(defcustom chidu-message-github-site-url "https://github.com"
  "GitHub site used for notification source links and username navigation."
  :type 'string
  :group 'chidu)

(defcustom chidu-message-github-code-modes
  '(("diff" . diff-mode) ("patch" . diff-mode)
    ("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode)
    ("python" . python-mode) ("ruby" . ruby-mode)
    ("javascript" . js-mode) ("js" . js-mode) ("json" . js-json-mode)
    ("sh" . sh-mode) ("bash" . sh-mode) ("c" . c-mode) ("cpp" . c++-mode))
  "Trusted modes for explicit notification code-language labels.
Unknown or unavailable languages remain fixed-pitch.  HTML never supplies a
function name and rendering never installs a grammar."
  :type '(alist :key-type string :value-type function)
  :group 'chidu)

(defconst chidu-message-github--discard-tags
  '(head script style iframe frame frameset object embed form textarea
    select option button video audio source track canvas svg math link meta
    base template noscript)
  "Elements whose complete subtrees are not notification text.")

(defconst chidu-message-github--block-tags
  '(html body div section article header footer main aside p pre blockquote
    h1 h2 h3 h4 h5 h6 ul ol li hr table thead tbody tfoot tr td th dl dt dd)
  "Elements establishing notification block boundaries.")

(defun chidu-message-github--sender-p (sender)
  "Return non-nil when parsed From address SENDER selects this adapter."
  (and (stringp sender)
       (member-ignore-case (string-trim sender)
                           chidu-message-github-sender-addresses)))

(defun chidu-message-github--site-prefix ()
  "Return the configured site's URL prefix, including its path separator."
  (concat (string-remove-suffix "/" chidu-message-github-site-url) "/"))

(defun chidu-message-github--site-url-p (url)
  "Return non-nil for a web URL within the configured GitHub site."
  (and (chidu-browse-web-url-p url)
       (string-prefix-p (chidu-message-github--site-prefix) url)))

(defun chidu-message-github--text (node &optional depth)
  "Extract literal descendant text of NODE at DEPTH, excluding active content."
  (when (> (or depth 0) 64) (error "GitHub HTML nesting limit exceeded"))
  (cond
   ((stringp node) (substring-no-properties node))
   ((not (consp node)) "")
   ((memq (dom-tag node) chidu-message-github--discard-tags) "")
   ((eq (dom-tag node) 'br) "\n")
   (t (mapconcat (lambda (child)
                   (chidu-message-github--text child (1+ (or depth 0))))
                 (dom-children node) ""))))

(defun chidu-message-github--inlines (nodes depth &optional styles)
  "Adapt inline DOM NODES at DEPTH, inheriting semantic STYLES."
  (when (> depth 64) (error "GitHub HTML nesting limit exceeded"))
  (cl-mapcan
   (lambda (node)
     (cond
      ((stringp node)
       (list (appkit-markup-text
              (replace-regexp-in-string "[ \t\r\n\f]+" " " node) styles)))
      ((not (consp node)) nil)
      ((memq (dom-tag node) chidu-message-github--discard-tags) nil)
      ((eq (dom-tag node) 'br) (list (appkit-markup-line-break)))
      ((eq (dom-tag node) 'input)
       ;; Retain passive task state, never a form control or its attributes.
       ;; CHECKED is a boolean HTML attribute: presence, not its string value.
       (when (equal (downcase (or (dom-attr node 'type) "")) "checkbox")
         (list (appkit-markup-text
                (if (assq 'checked (dom-attributes node)) "[x]" "[ ]")
                styles))))
      ((eq (dom-tag node) 'img)
       ;; No URL is retained or fetched, including notification tracking pixels.
       (when-let* ((alt (dom-attr node 'alt)) ((not (string-empty-p alt))))
         (list (appkit-markup-text
                (replace-regexp-in-string "[\r\n]+" " " alt) styles))))
      ((eq (dom-tag node) 'code)
       (let (result)
         (dolist (line (split-string (chidu-message-github--text node) "\n"))
           (when result (push (appkit-markup-line-break) result))
           (push (appkit-markup-text line (cons 'code styles)) result))
         (nreverse result)))
      ((eq (dom-tag node) 'a)
       (let* ((url (dom-attr node 'href))
              (children (chidu-message-github--inlines
                         (dom-children node) (1+ depth) styles))
              ;; One link owns one action span; flatten richer label structures.
              (label (mapcar
                      (lambda (child)
                        (if (appkit-markup-text-p child) child
                          (appkit-markup-text " " styles)))
                      children))
              (text (mapconcat #'appkit-markup-text-text label "")))
         (cond
          ((not (chidu-browse-web-url-p url)) children)
          ((and (string-match "\\`@\\([A-Za-z0-9-]+\\)\\'" text)
                (equal url (concat (chidu-message-github--site-prefix)
                                   (match-string 1 text))))
           (list (appkit-markup-object
                  (list 'github-user (substring text 1) url) label styles)))
          (t (list (appkit-markup-link url label))))))
      (t
       (chidu-message-github--inlines
        (dom-children node) (1+ depth)
        (append (pcase (dom-tag node)
                  ((or 'b 'strong) '(bold))
                  ((or 'i 'em) '(italic))
                  ('u '(underline))
                  ((or 's 'del 'strike) '(strike)))
                styles)))))
   nodes))

(defun chidu-message-github--language (node)
  "Return NODE's explicit, opaque code-language label, if present."
  (or (dom-attr node 'data-code-language)
      (cl-loop for class in (split-string (or (dom-attr node 'class) ""))
               when (string-match "\\`\\(?:language-\\|lang-\\|highlight-source-\\)\\(.+\\)\\'"
                                  class)
               return (match-string 1 class))))

(defun chidu-message-github--review-context-p (node)
  "Return non-nil for a review's file-context paragraph NODE."
  (and (eq (dom-tag node) 'p)
       (string-match-p "\\`In .+:[ \t\r\n]*\\'" (chidu-message-github--text node))
       (cl-some (lambda (anchor)
                  (let ((url (dom-attr anchor 'href)))
                    (and (chidu-message-github--site-url-p url)
                         (string-match-p "/pull/[0-9]+" url))))
                (dom-by-tag node 'a))))

(defun chidu-message-github--blocks (nodes depth &optional language)
  "Adapt DOM NODES to Appkit blocks at DEPTH, inheriting LANGUAGE metadata."
  (when (> depth 64) (error "GitHub HTML nesting limit exceeded"))
  (let (blocks pending previous review-p)
    (cl-labels ((flush ()
                 (when pending
                   (push (appkit-markup-paragraph pending) blocks)
                   (setq pending nil))))
      (dolist (node nodes)
        (cond
         ((and (stringp node) (string-match-p "\\`[ \t\r\n]*\\'" node))
          (when pending (setq pending (append pending (list (appkit-markup-text " "))))))
         ((and (consp node) (memq (dom-tag node) chidu-message-github--discard-tags)))
         ((and (consp node) (memq (dom-tag node) chidu-message-github--block-tags))
          (flush)
          (let* ((tag (dom-tag node))
                 (children (dom-children node))
                 (adapted
                  (pcase tag
                    ('p
                     (when (and (not blocks)
                                (string-match-p "commented on this pull request"
                                                (chidu-message-github--text node)))
                       (setq review-p t))
                     (list (appkit-markup-paragraph
                            (chidu-message-github--inlines children (1+ depth)))))
                    ((or 'h1 'h2 'h3 'h4 'h5 'h6)
                     (list (appkit-markup-heading
                            (string-to-number (substring (symbol-name tag) 1))
                            (chidu-message-github--inlines children (1+ depth)))))
                    ('blockquote
                     (list (appkit-markup-quote
                            (chidu-message-github--blocks children (1+ depth)))))
                    ((or 'ul 'ol)
                     (list (appkit-markup-list
                            (if (eq tag 'ol) 'ordered 'unordered)
                            (cl-loop for child in children
                                     when (and (consp child) (eq (dom-tag child) 'li))
                                     collect (appkit-markup-list-item
                                              (chidu-message-github--blocks
                                               (dom-children child) (1+ depth))))
                            :start (when-let* ((start (dom-attr node 'start))
                                               ((string-match-p "\\`[1-9][0-9]*\\'" start)))
                                     (string-to-number start)))))
                    ('pre
                     (let ((code (car (dom-by-tag node 'code))))
                       (list (appkit-markup-preformatted
                              (chidu-message-github--text node)
                              (or (chidu-message-github--language node)
                                  (and code (chidu-message-github--language code))
                                  language
                                  (and review-p previous
                                       (chidu-message-github--review-context-p previous)
                                       "diff"))))))
                    ('hr (list (appkit-markup-paragraph
                                (list (appkit-markup-text "────────")))))
                    (_ (chidu-message-github--blocks
                        children (1+ depth)
                        (or (chidu-message-github--language node) language))))))
            ;; Ignore empty template paragraphs without losing the review header.
            (setq blocks (nconc (reverse
                                (appkit-markup-document-blocks
                                 (appkit-markup-document adapted))) blocks)))
          (setq previous node))
         (t (setq pending
                  (nconc pending (chidu-message-github--inlines (list node) (1+ depth)))))))
      (flush))
    (nreverse blocks)))

(defun chidu-message-github-parse (html)
  "Return (DOCUMENT . SOURCE-URL) adapted from notification HTML, or nil.
Active content and remote images are never evaluated or acquired.  Invalid,
oversized, or unsupported HTML falls back at the caller to the MIME text part."
  (condition-case nil
      (when (and (<= (length html) (* 2 1024 1024))
                 (fboundp 'libxml-parse-html-region)
                 (libxml-available-p))
        (let* ((tree (with-temp-buffer
                       (insert html)
                       (libxml-parse-html-region (point-min) (point-max))))
               (body (car (dom-by-tag tree 'body)))
               ;; Validate traversal depth before using recursive DOM helpers.
               (_ (chidu-message-github--text body))
               (footer (car (last (dom-by-tag body 'p))))
               (source
                (when (and footer
                           (string-match-p "Reply to this email directly"
                                           (chidu-message-github--text footer))
                           (string-match-p "You are receiving this because"
                                           (chidu-message-github--text footer)))
                  (cl-loop for anchor in (dom-by-tag footer 'a)
                           for url = (dom-attr anchor 'href)
                           when (and (equal (string-trim (chidu-message-github--text anchor))
                                            "view it on GitHub")
                                     (chidu-message-github--site-url-p url))
                           return url)))
               (document (appkit-markup-document
                          (chidu-message-github--blocks (dom-children body) 0))))
          (when (appkit-markup-document-blocks document)
            (cons document source))))
    (error nil)))

(defun chidu-message-github--user-link (start end name url)
  "Apply profile URL and identity color for NAME to START..END."
  (when (chidu-browse-add-link start end url)
    (when-let* ((face (appkit-name-color-face (concat "github:" (downcase name)))))
      (add-face-text-property start end face))))

(defun chidu-message-github--insert-object (node)
  "Insert the visible fallback and profile action for GitHub object NODE."
  (pcase-let ((`(github-user ,name ,url) (appkit-markup-object-value node))
              (start (point)))
    (appkit-markup-ui-insert-document
     (appkit-markup-document
      (list (appkit-markup-paragraph (appkit-markup-object-fallback node))))
     :final-newline-p nil)
    (chidu-message-github--user-link start (point) name url)))

(defun chidu-message-github--insert-code (node)
  "Insert exact preformatted NODE text with optional native highlighting."
  (let* ((text (appkit-markup-preformatted-text node))
         (language (appkit-markup-preformatted-language node))
         (mode (and (stringp language)
                    (cdr (assoc-string language chidu-message-github-code-modes t))))
         (start (point)))
    (insert (or (and mode (appkit-fontify-string text mode)) text))
    (add-face-text-property start (point) 'appkit-markup-preformatted-face 'append)))

(defun chidu-message-github--set-source (start end url)
  "Associate original-post URL with START..END without leaking to later mail."
  (when (and url (< start end))
    ;; Reader lookup searches within the current Email row.  One interior
    ;; marker avoids inheritance even when outer row insertion replaces the
    ;; body's rear-nonsticky policy.
    (put-text-property start (1+ start) 'chidu-browse-source-url url)
    (put-text-property start (1+ start) 'rear-nonsticky t)))

(defun chidu-message-github-render (body sender _view _context &optional format)
  "Insert GitHub HTML from BODY selected by SENDER, without SHR.
Return (html) when handled, with no inline attachment consumption.  Images
remain on Chidu's attachment path.  Return nil to use the ordinary MIME text
fallback, unless FORMAT explicitly requests HTML."
  (when (chidu-message-github--sender-p sender)
    (let* ((html (chidu-store-email-body-html-content body))
           (parsed (and (not (string-empty-p html))
                        (chidu-message-github-parse html)))
           (start (point)))
      (cond
       (parsed
        (appkit-markup-ui-insert-document
         (car parsed) :final-newline-p nil :interactive-p t
         :link-action (lambda (url) (apply-partially #'chidu-browse-open url))
         :object-inserter #'chidu-message-github--insert-object
         :preformatted-inserter #'chidu-message-github--insert-code
         :quote-style #'chidu-text-markup-quote-style)
        (save-excursion
          (goto-char start)
          (when (looking-at
                 "@?\\([A-Za-z0-9-]+\\) \\(?:left a comment\\|commented\\|opened\\|closed\\|reopened\\|merged\\)\\b")
            (let ((name (match-string-no-properties 1)) (end (match-end 1)))
              (unless (appkit-ui-action-at start)
                (chidu-message-github--user-link
                 start end name (concat (chidu-message-github--site-prefix) name))))))
        (chidu-message-github--set-source
         start (point)
         (or (cdr parsed)
             (with-temp-buffer
               (insert (chidu-store-email-body-text-content body))
               (nth 3 (chidu-message-github--footer)))))
        '(html))
       ((and (not (string-empty-p html))
             (or (eq format 'html)
                 (string-empty-p (chidu-store-email-body-text-content body))))
        (insert (propertize "Unable to display this HTML notification." 'face 'warning))
        '(html))))))

(defun chidu-message-github--footer ()
  "Find the complete trailing plain-text notification footer.
Return (FOOTER-START URL-START URL-END URL), preserving any comment anchor."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^--[ \t]*\r?\n"
                   "Reply to this email directly or view it on GitHub:\r?\n"
                   "\\(https?://[^[:space:]]+\\)\r?\n"
                   "You are receiving this because [^\n]*\n\r?\n"
                   "Message ID: <[^>\n]+>[ \t\r\n]*\\'")
           nil t)
      (let ((url (match-string-no-properties 1))
            (start (match-beginning 0))
            (url-start (match-beginning 1)) (url-end (match-end 1)))
        (when (chidu-message-github--site-url-p url)
          (list start url-start url-end url))))))

(defun chidu-message-github-annotate (start end sender)
  "Annotate only the recognized footer of a plain-text fallback from SENDER.
Never guess Markdown/code boundaries or change characters in START..END."
  (when (chidu-message-github--sender-p sender)
    (save-restriction
      (narrow-to-region start end)
      (when-let* ((footer (chidu-message-github--footer)))
        (chidu-browse-add-link (nth 1 footer) (nth 2 footer) (nth 3 footer))
        (chidu-message-github--set-source start end (nth 3 footer))))))

(provide 'chidu-message-github)
;;; chidu-message-github.el ends here
