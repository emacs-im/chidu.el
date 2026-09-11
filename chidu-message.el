;;; chidu-message.el --- Local-first Email display helpers and reader -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared presentation for Summary rows, standalone Email readers, and inline
;; Conversation entries.  Rendering consumes committed Store values only;
;; network materialization is owned by explicit view operations.

;;; Code:

(require 'subr-x)
(require 'time-date)
(require 'shr)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'chidu-message-github)
(require 'appkit-presentation)
(require 'chidu-attachment)
(require 'chidu-body-sync)
(require 'chidu-runtime)
(require 'chidu-seen)
(require 'chidu-store)
(require 'chidu-text)
(require 'chidu-surface-operation)

(declare-function chidu-attachment-toggle-inline-at-point-exact
                  "chidu-attachment" ())
(declare-function chidu-dispatch "chidu-transient" ())

(defun chidu-message--prefix-region-as-quote (start end)
  "Prefix nonempty lines in START..END with a literal HTML quote marker."
  (let ((limit (copy-marker end t)))
    (save-excursion
      (goto-char start)
      (while (< (point) limit)
        (unless (= (line-beginning-position) (line-end-position))
          (insert "> "))
        (forward-line 1)))
    (set-marker limit nil)))

(defun chidu-message--shr-blockquote (dom)
  "Render HTML blockquote DOM into copyable RFC-style quote lines."
  (shr-ensure-paragraph)
  (let ((start (copy-marker (point))))
    ;; `shr-external-rendering-functions' receives the complete blockquote
    ;; node.  Descending that node would dispatch to this renderer again;
    ;; render its children through SHR's generic walker instead.  Nested
    ;; blockquotes still dispatch here when the walker reaches their nodes.
    (shr-generic dom)
    (let ((end (copy-marker (point) t)))
      (chidu-message--prefix-region-as-quote start end)
      (goto-char end)
      (set-marker end nil))
    (set-marker start nil))
  (shr-ensure-paragraph))

(defun chidu-message--shr-image-alt (dom)
  "Insert safe fallback text for unmatched HTML image DOM."
  (let ((alt (dom-attr dom 'alt)))
    (insert
     (if (and (stringp alt) (not (string-empty-p (string-trim alt))))
         (string-trim alt)
       "[image]"))))

(defun chidu-message-insert-html (html view context)
  "Render stored HTML for VIEW and CONTEXT without arbitrary network access.\n\nReturn the list of JMAP attachments consumed as embedded image resources."
  (let*
      ((start (point)) embedded-attachments (shr-inhibit-images t)
       (shr-use-fonts nil)
       (shr-width
        (max 20
             (or
              (appkit-surface-responsive-width
               (appkit-current-surface) 2)
              fill-column 80)))
       (image-renderer
        (lambda (dom)
          (let
              ((attachment
                (and view context
                     (chidu-attachment-insert-embedded-image view
                                                             context
                                                             (dom-attr
                                                              dom
                                                              'src)
                                                             (dom-attr
                                                              dom
                                                              'alt)))))
            (if attachment
                (cl-pushnew attachment embedded-attachments :test #'eq)
              (chidu-message--shr-image-alt dom)))))
       (shr-external-rendering-functions
        (let
            ((renderers (copy-tree shr-external-rendering-functions)))
          (setq renderers (assq-delete-all 'img renderers) renderers
                (assq-delete-all 'blockquote renderers))
          (cons (cons 'img image-renderer)
                (cons '(blockquote . chidu-message--shr-blockquote)
                      renderers)))))
    (insert html) (shr-render-region start (point))
    (nreverse embedded-attachments)))

(defcustom chidu-message-body-render-functions '(chidu-message-github-render)
  "Functions offering sender-specific presentation of committed mail bodies.
Each receives (BODY SENDER VIEW CONTEXT FORMAT), where SENDER is the parsed
From address and FORMAT is nil for automatic selection or `html' for an
explicit HTML choice.  An explicit `plain' choice bypasses these functions.
Return nil without inserting to decline, or (SELECTED . ATTACHMENTS) after
insertion, where SELECTED is `plain' or `html'.  List attachments represented
inline; first handled result wins.  Renderers preserve stored bodies and reader
modes, and never perform external actions.  Set to nil to use ordinary MIME
presentation for every sender."
  :type 'hook
  :group 'chidu)

(defcustom chidu-message-body-annotate-functions '(chidu-message-github-annotate)
  "Functions adding provider actions to an inserted plain-text mail body.
Each receives (START END SENDER) and may add text properties, but must not
change source characters or perform external actions.  HTML is excluded.
Set this option to nil to disable provider-specific links."
  :type 'hook
  :group 'chidu)

(defun chidu-message-body-formats (body)
  "Return the available `plain' and `html' representations of cached BODY."
  (when body
    (delq nil
          (list (unless (string-empty-p (chidu-store-email-body-text-content body))
                  'plain)
                (unless (string-empty-p (chidu-store-email-body-html-content body))
                  'html)))))

(defun chidu-message-check-body-format (body format)
  "Require cached BODY to support FORMAT, or nil for automatic selection."
  (unless (and body (or (null format) (memq format (chidu-message-body-formats body))))
    (user-error "This body representation is no longer available")))

(defun chidu-message--insert-body-selector (formats selected select)
  "Insert FORMATS with SELECTED active; SELECT receives the chosen format."
  (insert (propertize "Body: " 'face 'shadow))
  (dolist (format formats)
    (unless (eq format (car formats)) (insert "   "))
    (let ((start (point))
          (label (if (eq format 'plain) "text/plain" "text/html")))
      (insert (format "(%s) %s" (if (eq format selected) "*" " ") label))
      (appkit-ui-add-action
       start (point) (apply-partially select format)
       :help-echo (concat "Display cached " label " body")
       :face (if (eq format selected) 'bold 'link))))
  (insert "\n\n"))

(cl-defun chidu-message-insert-body
    (body &key sender participants view context format on-format-change)
  "Insert locally committed Email BODY for VIEW and CONTEXT.

SENDER is the parsed From email address supplied to body renderers.
PARTICIPANTS supplies thread-local identity highlighting for ordinary mail.
FORMAT is nil for automatic selection, or an explicit `plain' or `html'.
If that representation disappears after refresh, use automatic selection.
ON-FORMAT-CHANGE receives a new choice and enables the inline selector
when both representations exist.  It must re-render the reader, including
attachment cards.  Return attachments represented inline by the chosen body."
  (when (chidu-store-email-body-encoding-problem-p body)
    (insert (propertize
             "Some body text could not be decoded cleanly.\n\n"
             'face 'warning)))
  (when (chidu-store-email-body-truncated-p body)
    (insert (propertize
             "This body was truncated at the configured per-part limit.\n\n"
             'face 'warning)))
  (let* ((text (chidu-store-email-body-text-content body))
         (html (chidu-store-email-body-html-content body))
         (formats (chidu-message-body-formats body))
         (requested (and (memq format formats) format))
         (start (point))
         (rendered (unless (eq requested 'plain)
                     (run-hook-with-args-until-success
                      'chidu-message-body-render-functions
                      body sender view context requested)))
         (selected (car rendered))
         (embedded-attachments (cdr rendered)))
    (unless rendered
      (cond
       ((and (not (eq requested 'html)) (memq 'plain formats))
        (setq selected 'plain)
        (insert text))
       ((memq 'html formats)
        (setq selected 'html
              embedded-attachments (chidu-message-insert-html html view context)))
       (t (insert (propertize "No displayable text body." 'face 'shadow))))
      (chidu-text-present-region start (point) participants)
      (when (eq selected 'plain)
        (run-hook-with-args 'chidu-message-body-annotate-functions
                           start (point) sender)))
    ;; Insert after rendering so the indicator reports the representation that
    ;; actually succeeded, including a provider's automatic plain fallback.
    (when (and on-format-change (> (length formats) 1))
      (save-excursion
        (goto-char start)
        (chidu-message--insert-body-selector formats selected on-format-change)))
    embedded-attachments))

(cl-defstruct
    (chidu-message-state (:constructor chidu-message-state-create))
  "View-local state for one selected Email." row account mailbox
  body-context participants (phase 'initial) message media-phase
  media-message media-key body-format)

(defun chidu-message--select-body-format (view format)
  "Select FORMAT for the live standalone VIEW without fetching mail."
  (unless (appkit-surface-live-p view) (user-error "Reader is no longer open"))
  (let* ((state (chidu-message--state view))
         (context (chidu-message-state-body-context state)))
    (chidu-message-check-body-format
     (and context (chidu-store-email-body-context-body context)) format)
    (setf (chidu-message-state-body-format state) format)
    (chidu-surface-refresh view)))

(defun chidu-message--set-media-phase (model phase &optional problem key)
  "Commit media PHASE, PROBLEM and pending open KEY to reader MODEL."
  (setf (chidu-message-state-media-phase model) phase
        (chidu-message-state-media-message model) problem
        (chidu-message-state-media-key model) key)
  model)

(defvar-local chidu-message--view nil
  "Appkit view attached to the current local message buffer.")

(defun chidu-message--state (&optional view)
  "Return validated message state for VIEW or the current view."
  (let*
      ((it (or view (appkit-current-surface)))
       (state (and it (appkit-surface-model it))))
    (unless (chidu-message-state-p state)
      (error "Chidu message view has invalid state"))
    state))

(defun chidu-message--render-content (view)
  "Render selected Email VIEW entirely from local state."
  (let*
      ((state (chidu-message--state view))
       (row (chidu-message-state-row state))
       (account (chidu-message-state-account state))
       (mailbox (chidu-message-state-mailbox state))
       (context (chidu-message-state-body-context state))
       (body
        (and context (chidu-store-email-body-context-body context)))
       (phase (chidu-message-state-phase state))
       (problem (chidu-message-state-message state)))
    (with-current-buffer (appkit-surface-buffer view)
      (let ((initial-p (= (point-min) (point-max))))
        (appkit-position-render-preserving
         (lambda ()
           (appkit-with-content-update view
             (erase-buffer)
             (insert
              (propertize (chidu-email-subject row) 'face
                          '(:height 1.25 :weight bold))
              "\n\n")
             (insert
              (propertize "From: " 'face 'font-lock-keyword-face)
              (chidu-text-person-label row) "\n")
             (insert
              (propertize "Date: " 'face 'font-lock-keyword-face)
              (chidu-store-email-summary-row-received-at row) "\n")
             (insert
              (propertize "Account: " 'face 'font-lock-keyword-face)
              (chidu-store-account-name account) "\n")
             (insert
              (propertize "Mailbox: " 'face 'font-lock-keyword-face)
              (chidu-store-mailbox-name mailbox) "\n")
             (when (chidu-store-email-summary-row-flagged-p row)
               (insert
                (propertize "Flagged\n" 'face 'chidu-email-flagged)))
             (when
                 (chidu-store-email-summary-row-has-attachment-p row)
               (insert (propertize "Has attachments\n" 'face 'shadow)))
             (insert "\n")
             (let (embedded-attachments)
               (cond
                (body
                 (setq embedded-attachments
                       (chidu-message-insert-body
                        body :sender (chidu-store-email-summary-row-from-email row)
                        :participants (chidu-message-state-participants state)
                        :view view :context context
                        :format (chidu-message-state-body-format state)
                        :on-format-change
                        (apply-partially #'chidu-message--select-body-format view))))
                (problem
                 (insert (chidu-store-email-summary-row-preview row)
                         "\n\n"
                         (propertize
                          (format "Unable to load body: %s" problem)
                          'face 'error)))
                ((memq phase '(loading refreshing))
                 (insert (chidu-store-email-summary-row-preview row)
                         "\n\n"
                         (propertize "Loading full message body…"
                                     'face 'shadow)))
                (t
                 (insert (chidu-store-email-summary-row-preview row)
                         "\n\n"
                         (propertize
                          "Full message body is not cached." 'face
                          'shadow))))
               (when (and body context)
                 (unless (bolp) (insert "\n")) (insert "\n")
                 (chidu-attachment-insert-cards view context
                                                :embedded-attachments
                                                embedded-attachments)))
             (when (and body (eq phase 'refreshing))
               (insert "\n"
                       (propertize "Refreshing body…" 'face 'shadow)))
             (insert "\n")))
         :anchor-property 'chidu-attachment-key
         :preserve-window-start t :after-restore
         (when initial-p (lambda () (goto-char (point-min)))))))))

(defun chidu-message--request-sync (surface)
  "Request presentation of the committed reader model."
  (chidu-surface-refresh surface))

(defun chidu-message--attachment-fallback-context ()
  "Return the standalone Email's unambiguous attachment card context."
  (when-let*
      ((view (appkit-current-surface))
       ((eq 'chidu-message-mode
            (appkit-surface-type-mode (appkit-surface-type view))))
       (state (chidu-message--state view)))
    (chidu-attachment-single-card-context view
                                          (chidu-message-state-body-context
                                           state))))

(defun chidu-message--failed (view state failure)
  "Install body FAILURE in message VIEW STATE."
  (setf (chidu-message-state-phase state) 'error
        (chidu-message-state-message state)
        (chidu-runtime-error-message failure))
  (chidu-message--request-sync view))

(defun chidu-message--refreshed (view state context)
  "Install refreshed body CONTEXT in message VIEW STATE."
  (setf (chidu-message-state-body-context state) context
        (chidu-message-state-phase state) 'idle
        (chidu-message-state-message state) nil)
  (chidu-message--request-sync view))

(defun chidu-message--loaded-local
    (view state refresh-empty-p context)
  "Install local body CONTEXT in VIEW STATE.

Refresh its remote body when REFRESH-EMPTY-P and CONTEXT has no body."
  (setf (chidu-message-state-body-context state) context
        (chidu-message-state-phase state) 'idle)
  (chidu-message--request-sync view)
  (when (and refresh-empty-p
             (null (chidu-store-email-body-context-body context)))
    (chidu-message-refresh view)))

(defun chidu-message-refresh (&optional view)
  "Refresh the selected Email body for VIEW." (interactive)
  (let*
      ((it (or view (appkit-current-surface)))
       (state (chidu-message--state it)))
    (setf (chidu-message-state-phase state) 'refreshing
          (chidu-message-state-message state) nil)
    (chidu-message--request-sync it)
    (chidu-surface-operation-start it 'body
                                   (lambda
                                     (runtime success-function
                                              error-function)
                                     (chidu-refresh-email-body runtime
                                                               (chidu-message-state-account
                                                                state)
                                                               (chidu-message-state-row
                                                                state)
                                                               success-function
                                                               error-function))
                                   (apply-partially
                                    #'chidu-message--refreshed it state)
                                   (apply-partially
                                    #'chidu-message--failed it state))))

(defun chidu-message--load-local (view &optional refresh-empty-p)
  "Load VIEW's local body and refresh when REFRESH-EMPTY-P."
  (let ((state (chidu-message--state view)))
    (setf (chidu-message-state-phase state) 'loading
          (chidu-message-state-message state) nil)
    (chidu-message--request-sync view)
    (chidu-surface-operation-start
     view 'body
     (lambda (runtime success-function error-function)
       (chidu-runtime-email-body
        runtime
        (chidu-message-state-account state)
        (chidu-message-state-row state)
        success-function error-function))
     (apply-partially
      #'chidu-message--loaded-local view state refresh-empty-p)
     (apply-partially #'chidu-message--failed view state))))

(defun chidu-message--seen-target ()
  "Return explicit read-state target for the standalone Email."
  (let*
      ((view
        (or (appkit-current-surface)
            (user-error "No live Chidu message view")))
       (state (chidu-message--state view))
       (row (chidu-message-state-row state)))
    (chidu-seen-target-create :app (appkit-surface-app view) :account
                              (chidu-message-state-account state)
                              :local-email-id
                              (chidu-store-email-summary-row-local-email-id
                               row)
                              :remote-email-id
                              (chidu-store-email-summary-row-remote-email-id
                               row)
                              :unread-p
                              (chidu-store-email-summary-row-unread-p
                               row))))

(defun chidu-message-apply-seen-change (view change)
  "Apply explicit read-state CHANGE to standalone message VIEW."
  (when (appkit-surface-live-p view)
    (let*
        ((state (chidu-message--state view))
         (row (chidu-message-state-row state)))
      (when
          (chidu-seen-change-for-account-p change
                                           (chidu-message-state-account
                                            state))
        (let ((updated (chidu-seen-update-summary-row row change)))
          (unless (eq updated row)
            (setf (chidu-message-state-row state) updated)))))))

(defvar-keymap chidu-message-mode-map
  :doc "Keymap for `chidu-message-mode'."
  :parent special-mode-map
  "?" #'chidu-dispatch
  "g" #'chidu-message-refresh
  "RET" #'chidu-activate-at-point
  "<return>" #'chidu-activate-at-point
  "!" #'chidu-mark-read
  "R" #'chidu-mark-unread
  "s" #'chidu-toggle-read
  "TAB" #'chidu-attachment-toggle-inline-at-point-exact
  "<tab>" #'chidu-attachment-toggle-inline-at-point-exact
  "q" #'quit-window)

(define-derived-mode chidu-message-mode special-mode "Chidu-Message"
  "Major mode for a local-first Chidu message."
  (setq-local truncate-lines nil
              chidu-seen-target-function #'chidu-message--seen-target
              appkit-media-card-fallback-context-function
              #'chidu-message--attachment-fallback-context)
  (setq-local filter-buffer-substring-function
              #'appkit-ui-buffer-substring-filter))

(defun chidu-message--setup (view)
  "Initialize newly attached message VIEW."
  (setq-local chidu-message--view view chidu-seen-target-function
              #'chidu-message--seen-target
              appkit-media-card-fallback-context-function
              #'chidu-message--attachment-fallback-context))

(defun chidu-message-open
    (app account mailbox row &optional select participants)
  "Open APP's local-first reader for ACCOUNT, MAILBOX, and Summary ROW.\n\nDisplay the buffer when SELECT is non-nil.  PARTICIPANTS supplies the exact\nthread-local identities eligible for inline name highlighting."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument
            (list 'chidu-store-account-p account)))
  (unless (chidu-store-mailbox-p mailbox)
    (signal 'wrong-type-argument
            (list 'chidu-store-mailbox-p mailbox)))
  (unless (chidu-store-email-summary-row-p row)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-summary-row-p row)))
  (let*
      ((participants
        (or participants (chidu-text-participants (vector row))))
       (local-id (chidu-store-email-summary-row-local-email-id row))
       (view-id
        (list 'message (chidu-store-account-account-id account)
              local-id))
       (existing (appkit-app-surface app view-id))
       (view
        (or existing
            (appkit-open-generated-surface
             chidu-message--surface-type :app app :identity view-id
             :buffer-name
             (format "*Chidu: %s*" (chidu-email-subject row)) :input
             (chidu-message-state-create :row row :account account
                                         :mailbox mailbox
                                         :participants participants)
             :select select))))
    (when existing
      (let ((state (chidu-message--state view)))
        (setf (chidu-message-state-row state) row
              (chidu-message-state-account state) account
              (chidu-message-state-mailbox state) mailbox
              (chidu-message-state-participants state) participants))
      (chidu-message--load-local view t))
    nil
    (when (and existing select)
      (pop-to-buffer (appkit-surface-buffer view)))
    (unless existing
      (with-current-buffer (appkit-surface-buffer view)
        (appkit-surface-enable-responsive-geometry view
                                                   (lambda
                                                     (surface _width)
                                                     (chidu-surface-refresh
                                                      surface)))
        (chidu-message--load-local view t)))
    (appkit-surface-buffer view)))

(provide 'chidu-message)

;;; chidu-message.el ends here

(defconst chidu-message--surface-type
  (appkit-surface-type-create
   :name 'chidu-message :mode #'chidu-message-mode
   :init (lambda (_context input) (appkit-next :model input :render t))
   :update #'chidu-surface-update
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount (lambda (surface _app _model) (chidu-message--setup surface))
      :merge (lambda (_previous next) next)
      :render (lambda (surface _app model _request)
                (chidu-message--render-content surface)
                (chidu-attachment-insert-reader-problem model)
                nil)
      :unmount (lambda (_surface) nil)))))
