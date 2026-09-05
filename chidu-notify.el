;;; chidu-notify.el --- Desktop notification policy for Chidu -*- lexical-binding: t; -*-

;;; Commentary:

;; Decide whether final canonical new-email rows warrant desktop presentation.
;; This module owns no EventSource, polling timer, or Email checkpoint.

;;; Code:

(require 'cl-lib)
(require 'notifications nil t)
(require 'seq)
(require 'subr-x)
(require 'appkit-surface)
(require 'chidu-email-sync)
(require 'chidu-store)
(require 'chidu-text)

(declare-function notifications-notify "notifications" (&rest params))
(declare-function chidu-conversation-open
                  "chidu-conversation"
                  (app account mailbox selected-row &optional select))
(declare-function chidu-summary-open
                  "chidu-summary"
                  (app account mailbox &optional select))

(defcustom chidu-new-mail-notifications t
  "When non-nil, present desktop notifications for eligible new mail."
  :type 'boolean
  :group 'chidu)

(defcustom chidu-new-mail-individual-limit 3
  "Maximum eligible messages shown as separate desktop notifications."
  :type 'positive-integer
  :group 'chidu)

(defcustom chidu-notification-function #'notifications-notify
  "Function called with `notifications-notify' keyword arguments.

When the function is unavailable or signals, Chidu falls back to an echo-area
message."
  :type 'function
  :group 'chidu)

(defun chidu-notify--view-visible-p (app id)
  "Return non-nil when APP view ID is displayed in any live window."
  (when-let* ((view (appkit-app-surface app id)))
    (and (appkit-surface-live-p view)
         (get-buffer-window (appkit-surface-buffer view) t))))

(defun chidu-notify--inbox-for-row (mailbox-context row)
  "Return Inbox in MAILBOX-CONTEXT containing new Email ROW, or nil."
  (let ((remote-mailbox-ids
         (chidu-store-new-email-row-remote-mailbox-ids row)))
    (cl-find-if
     (lambda (mailbox)
       (and (chidu-store-mailbox-available-p mailbox)
            (equal "inbox" (chidu-store-mailbox-role mailbox))
            (seq-contains-p
             remote-mailbox-ids
             (chidu-store-mailbox-remote-mailbox-id mailbox)
             #'equal)))
     (chidu-store-mailbox-sync-context-mailboxes mailbox-context))))

(defun chidu-notify--visible-p (app account inbox summary)
  "Return non-nil when new Email SUMMARY is already visible in APP.

ACCOUNT and INBOX identify its Conversation, standalone reader, and Summary
surfaces."
  (let ((account-id (chidu-store-account-account-id account)))
    (or
     (chidu-notify--view-visible-p
      app
      (list 'conversation account-id
            (chidu-store-email-summary-row-remote-thread-id summary)))
     (chidu-notify--view-visible-p
      app
      (list 'message account-id
            (chidu-store-email-summary-row-local-email-id summary)))
     (chidu-notify--view-visible-p
      app
      (list 'summary account-id
            (chidu-store-mailbox-mailbox-id inbox))))))

(defun chidu-notify--open-email (app account inbox summary)
  "Open new Email SUMMARY in APP for ACCOUNT and INBOX."
  (when (appkit-app-live-p app)
    (require 'chidu-conversation)
    (chidu-conversation-open app account inbox summary t)))

(defun chidu-notify--open-inbox (app account inbox)
  "Open ACCOUNT INBOX in APP."
  (when (appkit-app-live-p app)
    (require 'chidu-summary)
    (chidu-summary-open app account inbox t)))

(defun chidu-notify--one-line (value)
  "Return VALUE collapsed into one display line."
  (string-trim
   (replace-regexp-in-string "[[:space:]\n\r]+" " " (or value ""))))

(defun chidu-notify--send (&rest arguments)
  "Send desktop notification with ARGUMENTS, or fall back to `message'."
  (condition-case nil
      (if (functionp chidu-notification-function)
          (apply chidu-notification-function arguments)
        (error "Notification function is unavailable"))
    (error
     (message "Chidu: %s — %s"
              (or (plist-get arguments :title) "New mail")
              (or (plist-get arguments :body) "")))))

(defun chidu-notify--individual (app account inbox row)
  "Notify one eligible new Email ROW for APP, ACCOUNT, and INBOX."
  (let* ((summary (chidu-store-new-email-row-summary-row row))
         (sender (chidu-email-sender summary))
         (subject (chidu-email-subject summary))
         (preview
          (chidu-notify--one-line
           (chidu-store-email-summary-row-preview summary)))
         (body
          (if (string-empty-p preview)
              subject
            (format "%s\n%s" subject preview))))
    (chidu-notify--send
     :title sender
     :body body
     :app-name "Chidu"
     :category "email.arrived"
     :urgency 'normal
     :actions '("default" "Open")
     :on-action
     (lambda (_notification-id action)
       (when (equal action "default")
         (chidu-notify--open-email app account inbox summary))))))

(defun chidu-notify--aggregate
    (app account inbox rows eligible-count truncated-p)
  "Notify eligible messages represented by ROWS.

ELIGIBLE-COUNT is exact unless TRUNCATED-P, in which case Chidu deliberately
avoids claiming an unknown count.  APP, ACCOUNT, and INBOX supply the action
that opens the receiving Mailbox."
  (let ((body
         (string-join
          (cl-loop
           for row in rows
           for summary = (chidu-store-new-email-row-summary-row row)
           collect
           (format "%s — %s"
                   (chidu-email-sender summary)
                   (chidu-email-subject summary)))
          "\n")))
    (chidu-notify--send
     :title
     (if truncated-p
         "New messages"
       (format "%d new messages" eligible-count))
     :body body
     :app-name "Chidu"
     :category "email.arrived"
     :urgency 'normal
     :actions '("default" "Open Inbox")
     :on-action
     (lambda (_notification-id action)
       (when (equal action "default")
         (chidu-notify--open-inbox app account inbox))))))

(defun chidu-notify-present (app account mailbox-context result)
  "Present eligible live Email RESULT for APP and ACCOUNT.

MAILBOX-CONTEXT resolves Inbox membership."
  (when (appkit-app-live-p app)
    (let* ((rows (chidu-email-live-result-new-emails result))
           (truncated-p
            (chidu-email-live-result-truncated-p result))
           eligible
           first-inbox)
      (cl-loop
       for row across rows
       for summary = (chidu-store-new-email-row-summary-row row)
       for inbox = (chidu-notify--inbox-for-row mailbox-context row)
       when (and inbox
                 (chidu-store-email-summary-row-unread-p summary)
                 (not (chidu-notify--visible-p
                       app account inbox summary)))
       do
       (unless first-inbox (setq first-inbox inbox))
       (push (cons inbox row) eligible))
      (setq eligible (nreverse eligible))
      (when (and chidu-new-mail-notifications eligible)
        (if (and (not truncated-p)
                 (<= (length eligible) chidu-new-mail-individual-limit))
            (dolist (entry eligible)
              (chidu-notify--individual
               app account (car entry) (cdr entry)))
          (chidu-notify--aggregate
           app account first-inbox
           (mapcar #'cdr
                   (seq-take eligible chidu-new-mail-individual-limit))
           (length eligible)
           truncated-p))))))

(provide 'chidu-notify)

;;; chidu-notify.el ends here
