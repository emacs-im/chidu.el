;;; chidu-parsed-message.el --- Read-only JMAP attached-message reader -*- lexical-binding: t; -*-

;;; Commentary:

;; Present one account-scoped Blob parsed through JMAP `Email/parse'.  This is
;; deliberately not a top-level Email view: it has no mailbox membership,
;; keywords, receivedAt, read-state mutation, or EmailRecord identity.  The
;; structured parse result is Store-first and may itself contain attachments,
;; including recursively parseable attached messages.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'chidu-attachment)
(require 'chidu-jmap-body)
(require 'chidu-jmap-parse)
(require 'chidu-message)
(require 'chidu-parse-sync)
(require 'chidu-runtime)
(require 'chidu-store)
(require 'chidu-text)
(require 'chidu-surface-operation)
(declare-function chidu-dispatch "chidu-transient" ())

(cl-defstruct
    (chidu-parsed-message-state
     (:constructor chidu-parsed-message-state-create))
  "View-local state for one parsed attached message." account blob-id
  profile-version source-name context (phase 'initial) problem
  media-phase media-message media-key)

(defun chidu-parsed-message--set-media-phase (model phase &optional problem key)
  "Commit media PHASE, PROBLEM and pending open KEY to reader MODEL."
  (setf (chidu-parsed-message-state-media-phase model) phase
        (chidu-parsed-message-state-media-message model) problem
        (chidu-parsed-message-state-media-key model) key)
  model)

(defvar-local chidu-parsed-message--view nil
  "Appkit view attached to the current parsed-message buffer.")

(defun chidu-parsed-message--state (&optional view)
  "Return validated parsed-message state for VIEW or current view."
  (let*
      ((it (or view (appkit-current-surface)))
       (state (and it (appkit-surface-model it))))
    (unless (chidu-parsed-message-state-p state)
      (error "Chidu parsed-message view has invalid state"))
    state))

(defun chidu-parsed-message--subject (state message)
  "Return display subject for STATE and optional parsed MESSAGE."
  (let ((subject (and message
                      (chidu-store-parsed-message-subject message))))
    (if (and (stringp subject) (not (string-empty-p subject)))
        subject
      (chidu-parsed-message-state-source-name state))))

(defun chidu-parsed-message--insert-address-field (label addresses)
  "Insert LABEL and nonempty ADDRESSES vector."
  (when (and (vectorp addresses) (> (length addresses) 0))
    (insert (propertize (concat label ": ")
                        'face 'font-lock-keyword-face)
            (chidu-text-email-address-list addresses)
            "\n")))

(defun chidu-parsed-message--participants (message)
  "Return body-highlight participants from parsed MESSAGE."
  (chidu-text-address-participants
   (chidu-store-parsed-message-sender message)
   (chidu-store-parsed-message-from message)
   (chidu-store-parsed-message-to message)
   (chidu-store-parsed-message-cc message)
   (chidu-store-parsed-message-bcc message)
   (chidu-store-parsed-message-reply-to message)))

(defun chidu-parsed-message--render-content (view)
  "Render parsed-message VIEW entirely from local state."
  (let*
      ((state (chidu-parsed-message--state view))
       (context (chidu-parsed-message-state-context state))
       (message
        (and context
             (chidu-store-parsed-blob-context-message context)))
       (body (and message (chidu-store-parsed-message-body message)))
       (phase (chidu-parsed-message-state-phase state))
       (problem (chidu-parsed-message-state-problem state)))
    (with-current-buffer (appkit-surface-buffer view)
      (let ((initial-p (= (point-min) (point-max))))
        (appkit-position-render-preserving
         (lambda ()
           (appkit-with-content-update view
             (erase-buffer)
             (insert
              (propertize
               (chidu-parsed-message--subject state message) 'face
               '(:height 1.25 :weight bold))
              "\n\n")
             (insert
              (propertize "Attached message: " 'face
                          'font-lock-keyword-face)
              (chidu-parsed-message-state-source-name state) "\n")
             (insert
              (propertize "Account: " 'face 'font-lock-keyword-face)
              (chidu-store-account-name
               (chidu-parsed-message-state-account state))
              "\n")
             (when message
               (chidu-parsed-message--insert-address-field "Sender"
                                                           (chidu-store-parsed-message-sender
                                                            message))
               (chidu-parsed-message--insert-address-field "From"
                                                           (chidu-store-parsed-message-from
                                                            message))
               (chidu-parsed-message--insert-address-field "To"
                                                           (chidu-store-parsed-message-to
                                                            message))
               (chidu-parsed-message--insert-address-field "Cc"
                                                           (chidu-store-parsed-message-cc
                                                            message))
               (chidu-parsed-message--insert-address-field "Bcc"
                                                           (chidu-store-parsed-message-bcc
                                                            message))
               (chidu-parsed-message--insert-address-field "Reply-To"
                                                           (chidu-store-parsed-message-reply-to
                                                            message))
               (when-let*
                   ((sent-at
                     (chidu-store-parsed-message-sent-at message)))
                 (insert
                  (propertize "Date: " 'face 'font-lock-keyword-face)
                  sent-at "\n")))
             (insert "\n")
             (let (embedded-attachments)
               (cond
                (body
                 (setq embedded-attachments
                       (chidu-message-insert-body
                        body
                        :sender (when-let* ((from (seq-first (chidu-store-parsed-message-from message))))
                                  (chidu-store-email-address-email from))
                        :participants (chidu-parsed-message--participants message)
                        :view view :context context)))
                (problem
                 (insert
                  (when message
                    (concat
                     (chidu-store-parsed-message-preview message)
                     "\n\n"))
                  (propertize
                   (format "Unable to parse attached message: %s"
                           problem)
                   'face 'error)))
                ((memq phase '(loading refreshing))
                 (insert
                  (propertize "Parsing attached message…" 'face
                              'shadow)))
                (t
                 (insert
                  (propertize "Attached message is not cached." 'face
                              'shadow))))
               (when (and body context)
                 (unless (bolp) (insert "\n")) (insert "\n")
                 (chidu-attachment-insert-cards view context
                                                :embedded-attachments
                                                embedded-attachments)))
             (when (and body (eq phase 'refreshing))
               (insert "\n"
                       (propertize "Refreshing parsed message…" 'face
                                   'shadow)))
             (insert "\n")))
         :anchor-property 'chidu-attachment-key
         :preserve-window-start t :after-restore
         (when initial-p (lambda () (goto-char (point-min)))))))))

(defun chidu-parsed-message--request-sync (surface)
  "Request presentation of the committed reader model."
  (chidu-surface-refresh surface))

(defun chidu-parsed-message--attachment-fallback-context ()
  "Return parsed message's unambiguous attachment card context."
  (when-let*
      ((view (appkit-current-surface))
       ((eq 'chidu-parsed-message-mode
            (appkit-surface-type-mode (appkit-surface-type view))))
       (state (chidu-parsed-message--state view)))
    (chidu-attachment-single-card-context view
                                          (chidu-parsed-message-state-context
                                           state))))

(defun chidu-parsed-message--failed (view state failure)
  "Install parsed Blob FAILURE in VIEW STATE."
  (setf (chidu-parsed-message-state-phase state) 'error
        (chidu-parsed-message-state-problem state)
        (chidu-runtime-error-message failure))
  (chidu-parsed-message--request-sync view))

(defun chidu-parsed-message--refreshed (view state context)
  "Install refreshed parsed Blob CONTEXT in VIEW STATE."
  (setf (chidu-parsed-message-state-context state) context
        (chidu-parsed-message-state-phase state) 'idle
        (chidu-parsed-message-state-problem state) nil)
  (chidu-parsed-message--request-sync view))

(defun chidu-parsed-message--loaded-local
    (view state refresh-empty-p context)
  "Install local parsed Blob CONTEXT in VIEW STATE.

Refresh its remote Blob when REFRESH-EMPTY-P and CONTEXT has no message."
  (setf (chidu-parsed-message-state-context state) context
        (chidu-parsed-message-state-phase state) 'idle)
  (chidu-parsed-message--request-sync view)
  (when (and refresh-empty-p
             (null (chidu-store-parsed-blob-context-message context)))
    (chidu-parsed-message-refresh view)))

(defun chidu-parsed-message-refresh (&optional view)
  "Refresh the attached message parsed by VIEW." (interactive)
  (let*
      ((it (or view (appkit-current-surface)))
       (state (chidu-parsed-message--state it)))
    (setf (chidu-parsed-message-state-phase state) 'refreshing
          (chidu-parsed-message-state-problem state) nil)
    (chidu-parsed-message--request-sync it)
    (chidu-surface-operation-start it 'parsed-message
                                   (lambda
                                     (runtime success-function
                                              error-function)
                                     (chidu-refresh-parsed-blob runtime
                                                                (chidu-parsed-message-state-account
                                                                 state)
                                                                (chidu-parsed-message-state-blob-id
                                                                 state)
                                                                success-function
                                                                error-function
                                                                chidu-email-body-value-byte-limit))
                                   (apply-partially
                                    #'chidu-parsed-message--refreshed
                                    it state)
                                   (apply-partially
                                    #'chidu-parsed-message--failed it
                                    state))))

(defun chidu-parsed-message--load-local (view &optional refresh-empty-p)
  "Load VIEW's local parsed Blob and refresh when REFRESH-EMPTY-P."
  (let ((state (chidu-parsed-message--state view)))
    (setf (chidu-parsed-message-state-phase state) 'loading
          (chidu-parsed-message-state-problem state) nil)
    (chidu-parsed-message--request-sync view)
    (chidu-surface-operation-start
     view 'parsed-message
     (lambda (runtime success-function error-function)
       (chidu-runtime-parsed-blob
        runtime
        (chidu-parsed-message-state-account state)
        (chidu-parsed-message-state-blob-id state)
        (chidu-parsed-message-state-profile-version state)
        success-function error-function))
     (apply-partially
      #'chidu-parsed-message--loaded-local view state refresh-empty-p)
     (apply-partially #'chidu-parsed-message--failed view state))))

(defvar-keymap chidu-parsed-message-mode-map
  :doc "Keymap for `chidu-parsed-message-mode'."
  :parent special-mode-map
  "?" #'chidu-dispatch
  "g" #'chidu-parsed-message-refresh
  "TAB" #'chidu-attachment-toggle-inline-at-point-exact
  "<tab>" #'chidu-attachment-toggle-inline-at-point-exact
  "q" #'quit-window)

(define-derived-mode chidu-parsed-message-mode special-mode
  "Chidu-Attached"
  "Major mode for a local-first read-only parsed attached message."
  (setq-local truncate-lines nil
              appkit-media-card-fallback-context-function
              #'chidu-parsed-message--attachment-fallback-context)
  (setq-local filter-buffer-substring-function
              #'appkit-ui-buffer-substring-filter)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t))

(defun chidu-parsed-message--setup (view)
  "Initialize newly attached parsed-message VIEW."
  (setq-local chidu-parsed-message--view view
              appkit-media-card-fallback-context-function
              #'chidu-parsed-message--attachment-fallback-context))

(defun chidu-parsed-message-open
    (app source-context attachment &optional select)
  "Open APP's parsed view for ATTACHMENT from SOURCE-CONTEXT.\n\nDisplay the resulting buffer when SELECT is non-nil."
  (unless (appkit-app-live-p app)
    (user-error "Chidu application is not running"))
  (unless (chidu-attachment-context-p source-context)
    (signal 'wrong-type-argument
            (list 'chidu-attachment-context-p source-context)))
  (unless
      (and (chidu-store-email-attachment-p attachment)
           (chidu-attachment-attached-message-p attachment))
    (user-error "Chidu: attachment is not an attached message"))
  (let*
      ((account (chidu-attachment-context-account source-context))
       (blob-id (chidu-store-email-attachment-blob-id attachment))
       (profile-version
        (chidu-jmap-parse-profile-version
         chidu-email-body-value-byte-limit))
       (source-name (chidu-attachment-display-name attachment))
       (view-id
        (list 'parsed-message
              (chidu-store-account-account-id account) blob-id
              profile-version))
       (existing (appkit-app-surface app view-id))
       (view
        (or existing
            (appkit-open-generated-surface
             chidu-parsed-message--surface-type :app app :identity
             view-id :buffer-name
             (format "*Chidu Attached: %s*" source-name) :input
             (chidu-parsed-message-state-create :account account
                                                :blob-id blob-id
                                                :profile-version
                                                profile-version
                                                :source-name
                                                source-name)
             :select select))))
    (when existing
      (let ((state (chidu-parsed-message--state view)))
        (setf (chidu-parsed-message-state-account state) account
              (chidu-parsed-message-state-source-name state)
              source-name))
      (chidu-parsed-message--load-local view t))
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
        (chidu-parsed-message--load-local view t)))
    (appkit-surface-buffer view)))

(provide 'chidu-parsed-message)

;;; chidu-parsed-message.el ends here

(defconst chidu-parsed-message--surface-type
  (appkit-surface-type-create
   :name 'chidu-parsed-message :mode #'chidu-parsed-message-mode
   :init (lambda (_context input) (appkit-next :model input :render t))
   :update #'chidu-surface-update
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount (lambda (surface _app _model) (chidu-parsed-message--setup surface))
      :merge (lambda (_previous next) next)
      :render (lambda (surface _app model _request)
                (chidu-parsed-message--render-content surface)
                (chidu-attachment-insert-reader-problem model)
                nil)
      :unmount (lambda (_surface) nil)))))
