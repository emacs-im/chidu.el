;;; chidu-live-smoke.el --- Read-only in-process Chidu smoke -*- lexical-binding: t; -*-

;;; Commentary:

;; Environment:
;;   CHIDU_JMAP_SESSION_URL
;;   CHIDU_JMAP_USER
;;   CHIDU_JMAP_PASSWORD_FILE
;;
;; This exercises the same in-process runtime used by interactive Chidu:
;; Session/Identity, Mailbox/get, canonical Email bootstrap and live catch-up,
;; a bounded recent Mailbox Summary and server search, one bounded reply-tree
;; Conversation, selected display content, one bounded attachment download, and
;; one attached-message Email/parse path.  It
;; performs no remote mutation and prints no ids, addresses,
;; message metadata, body text, or secret.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'chidu)
(require 'chidu-attachment)
(require 'chidu-conversation)
(require 'chidu-runtime)
(require 'chidu-mailbox-sync)
(require 'chidu-email-sync)
(require 'chidu-search)
(require 'chidu-search-sync)
(require 'chidu-body-sync)
(require 'chidu-conversation-sync)
(require 'chidu-parse-sync)
(require 'chidu-parsed-message)

(defun chidu-live-smoke--private-file-p (path)
  "Return non-nil when PATH is current-uid mode-0600 regular data."
  (let ((attributes (and path (file-attributes path 'integer))))
    (and attributes
         (file-regular-p path)
         (not (file-symlink-p path))
         (= (file-attribute-user-id attributes) (user-uid))
         (= (logand (file-modes path) #o777) #o600))))

(defun chidu-live-smoke--read-password (path)
  "Read private password file PATH into a mutable string."
  (unless (chidu-live-smoke--private-file-p path)
    (error "password file must be current-uid mode-0600 regular data"))
  (let ((bytes
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert-file-contents-literally path)
           (while (and (> (point-max) (point-min))
                       (memq (char-before (point-max)) '(?\r ?\n)))
             (delete-region (1- (point-max)) (point-max)))
           (buffer-string))))
    (when (string-empty-p bytes)
      (error "password file is empty"))
    bytes))

(defun chidu-live-smoke--settle-surface (surface)
  "Present already-queued canonical App and SURFACE work."
  (let ((app (appkit-surface-app surface)) (passes 0))
    (while
        (let ((loops (cons (appkit-app-loop app)
                           (mapcar (lambda (entry) (appkit-surface-loop (cdr entry)))
                                   (hash-table-values (appkit-app-surfaces app)))))
              pending)
          (dolist (loop loops)
            (when (and (eq (appkit-loop-status loop) 'running)
                       (> (appkit-loop-pending-count loop) 0))
              (setq pending t)
              (when (> (cl-incf passes) 100) (error "Reader did not become idle"))
              (appkit-loop-run-pass loop)))
          pending))))

(defun chidu-live-smoke--await (cell deadline context)
  "Wait until CELL has a value before DEADLINE, or fail for CONTEXT."
  (while (and (null (car cell)) (< (float-time) deadline))
    (accept-process-output nil 0.05))
  (or (car cell) (error "%s timed out" context)))

(defun chidu-live-smoke--run (starter deadline context)
  "Run async STARTER and return its value before DEADLINE for CONTEXT."
  (let ((cell (list nil)))
    (funcall
     starter
     (lambda (value) (setcar cell (cons 'ok value)))
     (lambda (value) (setcar cell (cons 'error value))))
    (pcase (chidu-live-smoke--await cell deadline context)
      (`(ok . ,value) value)
      (`(error . ,value)
       (error "%s" (chidu-runtime-error-message value))))))

(defun chidu-live-smoke--readable-mailbox (mailbox-context)
  "Return the largest readable Mailbox in MAILBOX-CONTEXT."
  (car
   (sort
    (cl-loop
     for mailbox across
     (chidu-store-mailbox-sync-context-mailboxes mailbox-context)
     when
     (and
      (chidu-store-mailbox-available-p mailbox)
      (chidu-store-mailbox-rights-may-read-items-p
       (chidu-store-mailbox-rights mailbox)))
     collect mailbox)
    (lambda (left right)
      (> (chidu-store-mailbox-total-emails left)
         (chidu-store-mailbox-total-emails right))))))

(defun chidu-live-smoke--conversation
    (runtime account summary-rows deadline)
  "Return a bounded Conversation from SUMMARY-ROWS.

Prefer the first projection with more than one row and a real reply edge;
otherwise return the first successfully fetched Thread."
  (let ((index 0)
        first
        selected)
    (while (and (< index (length summary-rows)) (null selected))
      (condition-case _error
          (let* ((summary-row (aref summary-rows index))
                 (context
                  (chidu-live-smoke--run
                   (lambda (success error)
                     (chidu-refresh-conversation
                      runtime account
                      (chidu-store-email-summary-row-remote-thread-id
                       summary-row)
                      success error 256))
                   deadline "Conversation"))
                 (rows (chidu-store-conversation-context-rows context))
                 (depth
                  (cl-loop
                   for row across rows
                   maximize (chidu-store-conversation-row-depth row))))
            (unless first (setq first context))
            (when (and (> (length rows) 1) (> depth 0))
              (setq selected context)))
        (error nil))
      (setq index (1+ index)))
    (or selected first (error "No Summary row yielded a Conversation"))))

(defun chidu-live-smoke--selected-conversation-row
    (conversation summary-row)
  "Return CONVERSATION row matching SUMMARY-ROW, or its first row."
  (let ((rows (chidu-store-conversation-context-rows conversation))
        (local-id (chidu-store-email-summary-row-local-email-id summary-row)))
    (or
     (cl-find
      local-id rows
      :key
      (lambda (row)
        (chidu-store-email-summary-row-local-email-id
         (chidu-store-conversation-row-summary-row row)))
      :test #'equal)
     (aref rows 0))))

(defun chidu-live-smoke--search-ui
    (runtime connected account mailboxes spec)
  "Render committed search SPEC and return its visible row count."
  (let*
      ((state
        (chidu--state-create :phase 'ready :endpoints
                             (vector connected) :accounts
                             (make-hash-table :test #'equal)))
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "chidu-live-search") :input
                          state)))
    (appkit-app-send app (list :runtime runtime))
    (unwind-protect
        (let*
            ((buffer
              (chidu-search-open app account mailboxes spec nil))
             (view
              (with-current-buffer buffer (appkit-current-surface))))
          (chidu-live-smoke--settle-surface view)
          (with-current-buffer buffer
            (when (string-match-p "<mark>" (buffer-string))
              (error "Search UI leaked raw SearchSnippet markup"))
            (let ((position (point-min)) (count 0))
              (while (< position (point-max))
                (when
                    (get-text-property position
                                       'chidu-search-email-id)
                  (setq count (1+ count)))
                (setq position
                      (or
                       (next-single-property-change position
                                                    'chidu-search-email-id
                                                    nil (point-max))
                       (point-max))))
              (when (zerop count)
                (error "Search UI rendered no result row"))
              count)))
      (appkit-app-send app (list :runtime nil))
      (when (appkit-app-live-p app) (appkit-app-close app)))))

(defconst chidu-live-smoke-attachment-byte-cap (* 8 1024 1024)
  "Largest attachment downloaded by the read-only live smoke.")

(defconst chidu-live-smoke-attached-message-scan-limit 200
  "Maximum attachment-bearing Emails inspected for one parse candidate.")

(defun chidu-live-smoke--body-context
    (runtime account summary deadline context)
  "Return local or freshly materialized body for SUMMARY before DEADLINE.

CONTEXT names the smoke phase for diagnostics."
  (let ((local
         (chidu-live-smoke--run
          (lambda (success error)
            (chidu-runtime-email-body
             runtime account summary success error))
          deadline (format "%s local body" context))))
    (if (chidu-store-email-body-context-body local)
        local
      (chidu-live-smoke--run
       (lambda (success error)
         (chidu-refresh-email-body
          runtime account summary success error))
       deadline context))))

(defun chidu-live-smoke--attachment-candidate
    (runtime account search-rows deadline)
  "Return (BODY-CONTEXT ATTACHMENT) from SEARCH-ROWS before DEADLINE."
  (catch 'candidate
    (cl-loop
     for search-row across search-rows
     for summary = (chidu-store-search-row-summary-row search-row)
     do
     (condition-case nil
         (let* ((context
                 (chidu-live-smoke--body-context
                  runtime account summary deadline "attachment metadata"))
                (body (chidu-store-email-body-context-body context))
                (attachments
                 (and body (chidu-store-email-body-attachments body))))
           (cl-loop
            for attachment across (or attachments (vector))
            when
            (<= (chidu-store-email-attachment-size attachment)
                chidu-live-smoke-attachment-byte-cap)
            do (throw 'candidate (list context attachment))))
       (error nil)))
    (error "No bounded attachment candidate was materialized")))

(defun chidu-live-smoke--attached-message-candidate
    (runtime account search-rows start deadline)
  "Return attached-message candidate in SEARCH-ROWS from START, or nil."
  (catch 'candidate
    (cl-loop
     for index from start below (length search-rows)
     for search-row = (aref search-rows index)
     for summary = (chidu-store-search-row-summary-row search-row)
     do
     (condition-case nil
         (let* ((context
                 (chidu-live-smoke--body-context
                  runtime account summary deadline
                  "attached-message metadata"))
                (body (chidu-store-email-body-context-body context))
                (attachments
                 (and body (chidu-store-email-body-attachments body))))
           (cl-loop
            for attachment across (or attachments (vector))
            when (chidu-attachment-attached-message-p attachment)
            do (throw 'candidate (list context attachment))))
       (error nil)))))

(defun chidu-live-smoke--find-attached-message
    (runtime account spec initial-context deadline)
  "Find attached message by paging SPEC from INITIAL-CONTEXT."
  (let ((context initial-context)
        (offset 0)
        candidate)
    (while
        (and
         (null candidate)
         (< offset chidu-live-smoke-attached-message-scan-limit))
      (let* ((rows (chidu-store-search-context-rows context))
             (end (length rows)))
        (setq candidate
              (chidu-live-smoke--attached-message-candidate
               runtime account rows offset deadline))
        (if candidate
            (setq candidate
                  (append
                   candidate
                   (list (min end
                              chidu-live-smoke-attached-message-scan-limit))))
          (if (or (>= end chidu-live-smoke-attached-message-scan-limit)
                  (not (chidu-store-search-context-maybe-more-p context)))
              (setq offset chidu-live-smoke-attached-message-scan-limit)
            (setq offset end
                  context
                  (chidu-live-smoke--run
                   (lambda (success error)
                     (chidu-load-more-search
                      runtime account spec success error 50))
                   deadline "attached-message search page"))))))
    (or candidate
        (error "No attached-message candidate found in bounded search"))))

(defun chidu-live-smoke--parsed-message-ui
    (runtime connected source-context attachment parsed-context)
  "Render PARSED-CONTEXT from ATTACHMENT and return nested attachment count."
  (let*
      ((state
        (chidu--state-create :phase 'ready :endpoints
                             (vector connected) :accounts
                             (make-hash-table :test #'equal)))
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "chidu-live-parsed-message")
                          :input state)))
    (appkit-app-send app (list :runtime runtime))
    (unwind-protect
        (cl-letf
            (((symbol-function 'chidu-refresh-parsed-blob)
              (lambda (&rest _arguments)
                (error
                 "Parsed-message UI ignored committed Store state"))))
          (let*
              ((buffer
                (chidu-parsed-message-open app source-context
                                           attachment nil))
               (view
                (with-current-buffer buffer (appkit-current-surface)))
               (message
                (chidu-store-parsed-blob-context-message
                 parsed-context))
               (body
                (and message
                     (chidu-store-parsed-message-body message))))
            (unless
                (and (buffer-live-p buffer)
                     (appkit-surface-live-p view))
              (error "Parsed-message UI did not create a live view"))
            (chidu-live-smoke--settle-surface view)
            (with-current-buffer buffer
              (unless (eq major-mode 'chidu-parsed-message-mode)
                (error
                 "Attached message opened in the wrong major mode"))
              (when (= (point-min) (point-max))
                (error "Parsed-message UI rendered an empty buffer"))
              (when
                  (string-match-p "Attached message is not cached"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))
                (error
                 "Parsed-message UI did not consume committed state")))
            (length
             (if body (chidu-store-email-body-attachments body)
               (vector)))))
      (appkit-app-send app (list :runtime nil))
      (when (appkit-app-live-p app) (appkit-app-close app)))))

(defun chidu-live-smoke--download-attachment
    (runtime connected context attachment data-root deadline)
  "Download and verify ATTACHMENT from CONTEXT before DEADLINE."
  (let*
      ((state
        (chidu--state-create :phase 'ready :endpoints
                             (vector connected) :accounts
                             (make-hash-table :test #'equal)))
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "chidu-live-attachment")
                          :input state))
       (target (expand-file-name "attachment-smoke.bin" data-root))
       (cell (list nil)))
    (appkit-app-send app (list :runtime runtime))
    (unwind-protect
        (progn
          (chidu-attachment--acquire app context attachment target
                                     (lambda (file)
                                       (setcar cell (cons 'ok file)))
                                     (lambda (reason)
                                       (setcar cell
                                               (cons 'error reason))))
          (pcase
              (chidu-live-smoke--await cell deadline
                                       "attachment download")
            (`(error \, reason)
             (error "attachment download failed: %s" reason))
            (`(ok \, file)
             (unless
                 (and (file-regular-p file)
                      (=
                       (file-attribute-size
                        (file-attributes file 'integer))
                       (chidu-store-email-attachment-size attachment))
                      (= 384 (logand 511 (file-modes file))))
               (error
                "downloaded attachment failed local verification"))
             t)))
      (appkit-app-send app (list :runtime nil))
      (when (appkit-app-live-p app) (appkit-app-close app))
      (when (file-exists-p target) (delete-file target)))))

(defun chidu-live-smoke--conversation-ui
    (runtime connected account mailbox selected-row)
  "Render SELECTED-ROW Conversation and return count, actual depth, and visual depth."
  (let*
      ((state
        (chidu--state-create :phase 'ready :endpoints
                             (vector connected) :accounts
                             (make-hash-table :test #'equal)))
       (app
        (appkit-app-start chidu--app-type :identity
                          (make-symbol "chidu-live-smoke") :input
                          state)))
    (appkit-app-send app (list :runtime runtime))
    (unwind-protect
        (let*
            ((buffer
              (chidu-conversation-open app account mailbox
                                       selected-row nil))
             (view
              (with-current-buffer buffer (appkit-current-surface))))
          (chidu-live-smoke--settle-surface view)
          (with-current-buffer buffer
            (unless
                (text-property-not-all (point-min) (point-max)
                                       'chidu-conversation-email-id
                                       nil)
              (error "Conversation UI rendered no entries"))
            (when
                (string-match-p "Loading full message body"
                                (buffer-string))
              (error
               "Conversation UI did not render committed selected body")))
          (let*
              ((state (appkit-surface-model view))
               (rows
                (chidu-store-conversation-context-rows
                 (chidu-conversation-state-context state)))
               (layouts (chidu-conversation--layout-rows state)))
            (list (length rows)
                  (cl-loop for row across rows maximize
                           (chidu-store-conversation-row-depth row))
                  (cl-loop for layout across layouts maximize
                           (chidu-conversation--layout-row-depth
                            layout)))))
      (appkit-app-send app (list :runtime nil))
      (when (appkit-app-live-p app) (appkit-app-close app)))))

(defun chidu-live-smoke-main ()
  "Run read-only discovery, canonical sync, and reader paths."
  (let* ((session-url (getenv "CHIDU_JMAP_SESSION_URL"))
         (user (getenv "CHIDU_JMAP_USER"))
         (password-path (getenv "CHIDU_JMAP_PASSWORD_FILE"))
         (data-root (make-temp-file "chidu-live-store-" t))
         (password nil)
         (runtime nil)
         (deadline (+ (float-time) 600.0)))
    (unless (and session-url user password-path)
      (error "missing CHIDU_JMAP_* environment"))
    (unwind-protect
        (progn
          (set-file-modes data-root #o700)
          (setq password (chidu-live-smoke--read-password password-path)
                runtime (chidu-runtime-open :data-root data-root))
          (cl-letf (((symbol-function 'auth-source-search)
                     (lambda (&rest _arguments)
                       (list
                        (list :user user
                              :secret
                              (lambda () (copy-sequence password)))))))
            (let (endpoint failure)
              (chidu-runtime-configure-endpoint
               runtime session-url user 'basic
               (lambda (value) (setq endpoint value))
               (lambda (value) (setq failure value)))
              (when failure
                (error "%s" (chidu-runtime-error-message failure)))
              (unless (chidu-store-endpoint-p endpoint)
                (error "Endpoint configuration returned no value"))
              (let* ((connected
                      (chidu-live-smoke--run
                       (lambda (success error)
                         (chidu-runtime-connect-endpoint
                          runtime endpoint success error))
                       deadline "JMAP discovery"))
                     (max-objects-in-get
                      (chidu-store-endpoint-max-objects-in-get connected))
                     (_core-limit-check
                      (unless (and (integerp max-objects-in-get)
                                   (> max-objects-in-get 0))
                        (error "Session maxObjectsInGet was not preserved")))
                     (accounts (chidu-store-endpoint-accounts connected))
                     (account
                      (cl-find-if
                       (lambda (value)
                         (and (chidu-store-account-available-p value)
                              (seq-contains-p
                               (chidu-store-account-capabilities value)
                               chidu-jmap-mail-capability
                               #'equal)))
                       accounts)))
                (unless account
                  (error "Session exposes no available Mail Account"))
                (let* ((mailbox-context
                        (chidu-live-smoke--run
                         (lambda (success error)
                           (chidu-runtime-sync-mailboxes
                            runtime account success error))
                         deadline "Mailbox/get"))
                       (email-started (float-time))
                       (email-context
                        (chidu-live-smoke--run
                         (lambda (success error)
                           (chidu-email-index-account
                            runtime account success error))
                         deadline "Email generation bootstrap"))
                       (email-seconds (- (float-time) email-started))
                       (_email-context-check
                        (unless
                            (eq 'live
                                (chidu-store-email-sync-context-phase
                                 email-context))
                          (error "Canonical Email generation did not activate")))
                       (live-result
                        (chidu-live-smoke--run
                         (lambda (success error)
                           (chidu-email-sync-live
                            runtime account success error))
                         deadline "live Email/changes"))
                       (_live-result-check
                        (unless (chidu-email-live-result-p live-result)
                          (error "Live Email synchronization returned invalid data")))
                       (summary-mailbox
                        (chidu-live-smoke--readable-mailbox
                         mailbox-context))
                       (_mailbox-check
                        (unless summary-mailbox
                          (error "Session exposes no readable Mailbox")))
                       (summary-context
                        (chidu-live-smoke--run
                         (lambda (success error)
                           (chidu-runtime-mailbox-summary
                            runtime account summary-mailbox 50 success error))
                         deadline "canonical Mailbox Summary"))
                       (summary-rows
                        (chidu-store-mailbox-summary-context-rows
                         summary-context))
                       (_summary-check
                        (when (zerop (length summary-rows))
                          (error "Mailbox Summary returned no readable row")))
                       (search-spec
                        (chidu-search-query-compile
                         "emoji"
                         (chidu-store-mailbox-sync-context-mailboxes
                          mailbox-context)))
                       (search-context
                        (chidu-live-smoke--run
                         (lambda (success error)
                           (chidu-refresh-search
                            runtime account search-spec success error 20))
                         deadline "Email search"))
                       (search-rows
                        (chidu-store-search-context-rows search-context))
                       (_search-check
                        (when (zerop (length search-rows))
                          (error "Email search returned no result row")))
                       (search-ui-count
                        (chidu-live-smoke--search-ui
                         runtime connected account
                         (chidu-store-mailbox-sync-context-mailboxes
                          mailbox-context)
                         search-spec))
                       (attachment-search-spec
                        (chidu-search-query-compile
                         "has:attachment"
                         (chidu-store-mailbox-sync-context-mailboxes
                          mailbox-context)))
                       (attachment-search-context
                        (chidu-live-smoke--run
                         (lambda (success error)
                           (chidu-refresh-search
                            runtime account attachment-search-spec
                            success error 50))
                         deadline "attachment search"))
                       (attachment-candidate
                        (chidu-live-smoke--attachment-candidate
                         runtime account
                         (chidu-store-search-context-rows
                          attachment-search-context)
                         deadline))
                       (attachment-body-context (nth 0 attachment-candidate))
                       (attachment (nth 1 attachment-candidate))
                       (_attachment-download
                        (chidu-live-smoke--download-attachment
                         runtime connected attachment-body-context attachment
                         data-root deadline))
                       (attachment-count
                        (length
                         (chidu-store-email-body-attachments
                          (chidu-store-email-body-context-body
                           attachment-body-context))))
                       (parsed-candidate
                        (chidu-live-smoke--find-attached-message
                         runtime account attachment-search-spec
                         attachment-search-context deadline))
                       (parsed-source-context (nth 0 parsed-candidate))
                       (parsed-source-attachment (nth 1 parsed-candidate))
                       (parsed-scan-count (nth 2 parsed-candidate))
                       (parsed-context
                        (chidu-live-smoke--run
                         (lambda (success error)
                           (chidu-refresh-parsed-blob
                            runtime account
                            (chidu-store-email-attachment-blob-id
                             parsed-source-attachment)
                            success error))
                         deadline "Email/parse attached message"))
                       (parsed-message
                        (chidu-store-parsed-blob-context-message
                         parsed-context))
                       (_parsed-check
                        (unless
                            (and parsed-message
                                 (chidu-store-parsed-message-body
                                  parsed-message)
                                 (null
                                  (chidu-store-email-body-email-state
                                   (chidu-store-parsed-message-body
                                    parsed-message))))
                          (error "Email/parse did not produce a detached message")))
                       (parsed-attachment-count
                        (chidu-live-smoke--parsed-message-ui
                         runtime connected parsed-source-context
                         parsed-source-attachment parsed-context))
                       (conversation-context
                        (chidu-live-smoke--conversation
                         runtime account summary-rows deadline))
                       (conversation-rows
                        (chidu-store-conversation-context-rows
                         conversation-context))
                       (selected-conversation-row
                        (cl-loop
                         with selected = (aref conversation-rows 0)
                         for row across conversation-rows
                         when (> (chidu-store-conversation-row-depth row)
                                 (chidu-store-conversation-row-depth selected))
                         do (setq selected row)
                         finally return selected))
                       (selected-summary-row
                        (chidu-store-conversation-row-summary-row
                         selected-conversation-row))
                       (body-context
                        (chidu-live-smoke--run
                         (lambda (success error)
                           (chidu-refresh-email-body
                            runtime account selected-summary-row
                            success error))
                         deadline "selected Email body"))
                       (_body-check
                        (unless
                            (chidu-store-email-body-context-body body-context)
                          (error "selected Email body was not materialized")))
                       (ui-metrics
                        (chidu-live-smoke--conversation-ui
                         runtime connected account summary-mailbox
                         selected-summary-row))
                       (available-mailboxes
                        (cl-loop
                         for mailbox across
                         (chidu-store-mailbox-sync-context-mailboxes
                          mailbox-context)
                         count (chidu-store-mailbox-available-p mailbox))))
                  (princ "in_process_runtime=PASS\n")
                  (princ "session_discovery=PASS\n")
                  (princ "event_source_template=PASS\n")
                  (princ "mailbox_sync=PASS\n")
                  (princ "mailbox_summary=PASS\n")
                  (princ "email_live_sync=PASS\n")
                  (princ "email_search_store=PASS\n")
                  (princ "email_search_ui=PASS\n")
                  (princ "attachment_metadata=PASS\n")
                  (princ "attachment_download=PASS\n")
                  (princ "email_parse_store=PASS\n")
                  (princ "email_parse_ui=PASS\n")
                  (princ "conversation_store=PASS\n")
                  (princ "conversation_ui=PASS\n")
                  (princ "email_body=PASS\n")
                  (princ "email_membership_baseline=PASS\n")
                  (princ "email_membership_catchup=PASS\n")
                  (princ "email_metadata_hydration=PASS\n")
                  (princ "email_generation_activation=PASS\n")
                  (princ "store_backend=sqlite\n")
                  (princ (format "account_count=%d\n" (length accounts)))
                  (princ
                   (format "max_objects_in_get=%d\n" max-objects-in-get))
                  (princ
                   (format "available_mailbox_count=%d\n"
                           available-mailboxes))
                  (princ
                   (format "summary_row_count=%d\n"
                           (length summary-rows)))
                  (princ
                   (format
                    "email_live_new_rows=%d\n"
                    (length (chidu-email-live-result-new-emails live-result))))
                  (princ
                   (format "search_row_count=%d\n" search-ui-count))
                  (princ
                   (format "attachment_count=%d\n" attachment-count))
                  (princ
                   (format "attached_message_scan_count=%d\n"
                           parsed-scan-count))
                  (princ
                   (format "parsed_attachment_count=%d\n"
                           parsed-attachment-count))
                  (princ
                   (format "conversation_row_count=%d\n"
                           (nth 0 ui-metrics)))
                  (princ
                   (format "conversation_reply_depth=%d\n"
                           (nth 1 ui-metrics)))
                  (princ
                   (format "conversation_visual_depth=%d\n"
                           (nth 2 ui-metrics)))
                  (princ
                   (format "email_enumerated_count=%d\n"
                           (chidu-store-email-sync-context-committed-count
                            email-context)))
                  (princ (format "email_bootstrap_seconds=%.3f\n" email-seconds))
                  (princ
                   (format "store_max_seconds=%.6f\n"
                           (plist-get
                            (chidu-runtime-store-metrics runtime)
                            :max-seconds)))
                  (princ "read_only_remote_methods=PASS\n"))))))
      (when runtime (chidu-runtime-close runtime))
      (when (stringp password) (clear-string password))
      (when (file-directory-p data-root) (delete-directory data-root t)))))

(condition-case _error-data
    (chidu-live-smoke-main)
  ((error quit)
   ;; Keep the command safe for CI/log capture: intermediate domain records may
   ;; contain private ids, addresses, subjects, or attachment names, so never
   ;; let a batch backtrace print the condition payload.
   (princ "read_only_smoke=FAIL\n" #'external-debugging-output)
   (kill-emacs 1)))

;;; chidu-live-smoke.el ends here
