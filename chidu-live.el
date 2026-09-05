;;; chidu-live.el --- Canonical Email watcher for Chidu -*- lexical-binding: t; -*-

;;; Commentary:

;; Own EventSource and low-frequency polling for connected Endpoints.  Wakes are
;; coalesced per Account and run the canonical active-generation Email reducer.
;; Desktop notification policy is a downstream consumer, never a sync owner.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'appkit-surface)
(require 'chidu-email-sync)
(require 'chidu-jmap-event-source)
(require 'chidu-notify)
(require 'chidu-root)
(require 'chidu-runtime)
(require 'chidu-store)

(declare-function chidu-summary-reload-local
                  "chidu-summary" (&optional view))
(declare-function chidu-search-reload-local
                  "chidu-search" (&optional view))

(defcustom chidu-event-source-enabled t
  "When non-nil, use JMAP EventSource as a canonical Email wake hint."
  :type 'boolean
  :group 'chidu)

(defcustom chidu-event-source-timeout 300
  "Seconds allowed for one bounded EventSource long-poll."
  :type 'positive-integer
  :group 'chidu)

(defcustom chidu-live-sweep-interval 900
  "Seconds between canonical Email sweeps despite EventSource."
  :type 'positive-integer
  :group 'chidu)

(defcustom chidu-event-source-max-backoff 60
  "Maximum seconds between failed EventSource reconnect attempts."
  :type 'positive-integer
  :group 'chidu)

(defcustom chidu-live-account-busy-delay 1
  "Seconds before retrying a wake while another Account operation is active."
  :type 'number
  :group 'chidu)

(cl-defstruct (chidu-live-watcher
               (:constructor chidu-live-watcher-create))
  "Lifecycle state for one connected Endpoint watcher."
  app
  endpoint
  handle
  request
  reconnect-timer
  sweep-timer
  dispatch-timer
  (backoff 1)
  (pending (make-hash-table :test #'equal))
  (operations (make-hash-table :test #'equal))
  last-error
  closed-p)

(defun chidu-live--watch-key (endpoint)
  "Return Appkit request-table key for ENDPOINT's watcher."
  (list 'email-live-watch (chidu-store-endpoint-endpoint-id endpoint)))

(defun chidu-live--live-p (watcher)
  "Return non-nil when WATCHER and its Appkit application are live."
  (and (chidu-live-watcher-p watcher)
       (not (chidu-live-watcher-closed-p watcher))
       (appkit-app-live-p (chidu-live-watcher-app watcher))))

(defun chidu-live--accounts (watcher)
  "Return available Mail Accounts currently owned by WATCHER."
  (cl-loop
   for account across
   (chidu-store-endpoint-accounts
    (chidu-live-watcher-endpoint watcher))
   when
   (and (chidu-store-account-available-p account)
        (seq-contains-p
         (chidu-store-account-capabilities account)
         chidu-jmap-mail-capability
         #'equal))
   collect account))

(defun chidu-live--account-by-remote-id (watcher remote-account-id)
  "Return WATCHER Account matching REMOTE-ACCOUNT-ID, or nil."
  (cl-find remote-account-id (chidu-live--accounts watcher)
           :key #'chidu-store-account-remote-account-id :test #'equal))

(defun chidu-live--cancel-timer (timer)
  "Cancel TIMER when live."
  (when (timerp timer) (cancel-timer timer)))

(defun chidu-live--close (watcher)
  "Close WATCHER and cancel all of its owned work."
  (when (and (chidu-live-watcher-p watcher)
             (not (chidu-live-watcher-closed-p watcher)))
    (setf (chidu-live-watcher-closed-p watcher) t)
    (when-let* ((cancel (chidu-live-watcher-request watcher)))
      (when (functionp cancel) (funcall cancel)))
    (setf (chidu-live-watcher-request watcher) nil)
    (dolist (timer
             (list (chidu-live-watcher-reconnect-timer watcher)
                   (chidu-live-watcher-sweep-timer watcher)
                   (chidu-live-watcher-dispatch-timer watcher)))
      (chidu-live--cancel-timer timer))
    (setf (chidu-live-watcher-reconnect-timer watcher) nil
          (chidu-live-watcher-sweep-timer watcher) nil
          (chidu-live-watcher-dispatch-timer watcher) nil)
    (let ((runtime
           (and (appkit-app-p (chidu-live-watcher-app watcher))
                (chidu-app-runtime
                 (chidu-live-watcher-app watcher)))))
      (when (chidu-runtime-p runtime)
        (maphash
         (lambda (_account-id operation)
           (when (chidu-runtime-operation-p operation)
             (chidu-runtime-cancel-operation runtime operation)))
         (chidu-live-watcher-operations watcher))))
    (clrhash (chidu-live-watcher-operations watcher))
    (clrhash (chidu-live-watcher-pending watcher))
    (let* ((app (chidu-live-watcher-app watcher))
           (key
            (chidu-live--watch-key
             (chidu-live-watcher-endpoint watcher))))
      (when (and (appkit-app-p app)
                 (eq watcher (gethash key (chidu-app-requests app))))
        (remhash key (chidu-app-requests app))))
    t))

(defun chidu-live--mailbox-context (app account)
  "Return APP's committed Mailbox context for ACCOUNT, or nil."
  (when-let* ((ui
               (chidu--account-ui-state
                (chidu-app-state app) account)))
    (chidu--account-ui-state-mailbox-context ui)))

(defun chidu-live--reload-account-views (app account)
  "Reload APP list Surfaces derived from ACCOUNT's canonical Email state."
  (let ((account-id (chidu-store-account-account-id account)))
    (maphash
     (lambda (identity entry)
       (let ((surface (cdr entry)))
         (when (and (appkit-surface-live-p surface)
                    (consp identity)
                    (equal account-id (nth 1 identity)))
           (pcase (car identity)
             ('summary
              (require 'chidu-summary)
              (chidu-summary-reload-local surface))
             ('search
              (require 'chidu-search)
              (chidu-search-reload-local surface))))))
     (appkit-app-surfaces app))))

(defun chidu-live--present-with-mailboxes (watcher account result)
  "Present WATCHER live Email RESULT after obtaining ACCOUNT Mailboxes."
  (let* ((app (chidu-live-watcher-app watcher))
         (runtime (and (appkit-app-live-p app) (chidu-app-runtime app)))
         (local (and (appkit-app-live-p app)
                     (chidu-live--mailbox-context app account))))
    (cond
     (local
      (chidu-notify-present app account local result))
     ((chidu-runtime-p runtime)
      (chidu-runtime-list-mailboxes
       runtime account
       (lambda (context)
         (when (chidu-live--live-p watcher)
           (chidu--set-account-ui app account :mailbox-context context)
           (chidu-notify-present app account context result)))
       (lambda (_failure) nil))))))

(defun chidu-live--operation-finished
    (watcher account-id &optional delay)
  "Retire WATCHER operation for ACCOUNT-ID and drain a pending wake.

DELAY postpones the next dispatch when the Account lane is busy."
  (remhash account-id (chidu-live-watcher-operations watcher))
  (when (gethash account-id (chidu-live-watcher-pending watcher))
    (chidu-live--schedule-dispatch watcher delay)))

(defun chidu-live--start-account (watcher account)
  "Start canonical Email reconciliation for WATCHER ACCOUNT."
  (when (chidu-live--live-p watcher)
    (let* ((app (chidu-live-watcher-app watcher))
           (runtime (chidu-app-runtime app))
           (account-id (chidu-store-account-account-id account))
           operation
           completed-p)
      (remhash account-id (chidu-live-watcher-pending watcher))
      (setq
       operation
       (chidu-email-sync-live
        runtime account
        (lambda (result)
          (setq completed-p t)
          (when (chidu-live--live-p watcher)
            (setf (chidu-live-watcher-last-error watcher) nil)
            (chidu-live--operation-finished watcher account-id)
            (when (or (chidu-email-live-result-changed-p result)
                      (chidu-email-live-result-rebuilt-p result))
              (chidu-live--reload-account-views app account))
            (when (> (length (chidu-email-live-result-new-emails result)) 0)
              (chidu-live--present-with-mailboxes
               watcher account result))))
        (lambda (failure)
          (setq completed-p t)
          (when (chidu-live--live-p watcher)
            (let ((busy-p
                   (and (chidu-result-failure-p failure)
                        (eq 'account-busy
                            (chidu-result-failure-kind failure)))))
              (setf (chidu-live-watcher-last-error watcher)
                    (chidu-runtime-error-message failure))
              (when busy-p
                (puthash account-id account
                         (chidu-live-watcher-pending watcher)))
              (chidu-live--operation-finished
               watcher account-id
               (and busy-p chidu-live-account-busy-delay)))))))
      (unless completed-p
        (puthash account-id operation
                 (chidu-live-watcher-operations watcher))))))

(defun chidu-live--dispatch (watcher)
  "Start pending Account reconciliations for WATCHER."
  (when (chidu-live--live-p watcher)
    (setf (chidu-live-watcher-dispatch-timer watcher) nil)
    (let (accounts)
      (maphash
       (lambda (_account-id account) (push account accounts))
       (chidu-live-watcher-pending watcher))
      (dolist (account accounts)
        (let ((account-id (chidu-store-account-account-id account)))
          (unless (gethash account-id
                           (chidu-live-watcher-operations watcher))
            (chidu-live--start-account watcher account)))))))

(defun chidu-live--schedule-dispatch (watcher &optional delay)
  "Coalesce a pending-work dispatch for WATCHER after DELAY seconds."
  (when (and (chidu-live--live-p watcher)
             (null (chidu-live-watcher-dispatch-timer watcher)))
    (setf
     (chidu-live-watcher-dispatch-timer watcher)
     (run-at-time (or delay 0) nil #'chidu-live--dispatch watcher))))

(defun chidu-live--schedule-account (watcher account)
  "Schedule authoritative reconciliation for WATCHER ACCOUNT."
  (when (and (chidu-live--live-p watcher)
             (chidu-store-account-p account))
    (puthash
     (chidu-store-account-account-id account) account
     (chidu-live-watcher-pending watcher))
    (chidu-live--schedule-dispatch watcher)))

(defun chidu-live--schedule-all (watcher)
  "Schedule every available Mail Account owned by WATCHER."
  (when (chidu-live--live-p watcher)
    (dolist (account (chidu-live--accounts watcher))
      (chidu-live--schedule-account watcher account))))

(defun chidu-live--schedule-wakes (watcher wakes)
  "Schedule WATCHER Accounts named by EventSource WAKES."
  (cl-loop
   for wake across wakes
   for account =
   (chidu-live--account-by-remote-id
    watcher (chidu-jmap-event-wake-remote-account-id wake))
   when account do (chidu-live--schedule-account watcher account)))

(defun chidu-live--schedule-reconnect (watcher delay)
  "Reconnect WATCHER EventSource after DELAY seconds."
  (when (chidu-live--live-p watcher)
    (chidu-live--cancel-timer
     (chidu-live-watcher-reconnect-timer watcher))
    (setf
     (chidu-live-watcher-reconnect-timer watcher)
     (run-at-time delay nil #'chidu-live--start-event-source watcher))))

(defun chidu-live--event-source-result (watcher result)
  "Handle one EventSource RESULT for WATCHER."
  (when (chidu-live--live-p watcher)
    (setf (chidu-live-watcher-request watcher) nil)
    (cond
     ((chidu-result-ok-p result)
      (let ((wakes (chidu-result-ok-value result)))
        (setf (chidu-live-watcher-backoff watcher) 1
              (chidu-live-watcher-last-error watcher) nil)
        (if (> (length wakes) 0)
            (chidu-live--schedule-wakes watcher wakes)
          (chidu-live--schedule-all watcher))
        (chidu-live--schedule-reconnect watcher 0)))
     ((chidu-result-failure-p result)
      ;; A dropped or timed-out EventSource may have lost a hint.  Sweep every
      ;; account before reconnecting; the durable checkpoint makes this cheap.
      (setf (chidu-live-watcher-last-error watcher)
            (chidu-runtime-error-message result))
      (chidu-live--schedule-all watcher)
      (let ((delay (chidu-live-watcher-backoff watcher)))
        (setf
         (chidu-live-watcher-backoff watcher)
         (min chidu-event-source-max-backoff (max 1 (* 2 delay))))
        (chidu-live--schedule-reconnect watcher delay))))))

(defun chidu-live--start-event-source (watcher)
  "Start one bounded EventSource long-poll for WATCHER."
  (when (chidu-live--live-p watcher)
    (setf (chidu-live-watcher-reconnect-timer watcher) nil)
    (when (and chidu-event-source-enabled
               (stringp
                (chidu-store-endpoint-event-source-url
                 (chidu-live-watcher-endpoint watcher))))
      (let* ((endpoint (chidu-live-watcher-endpoint watcher))
             secret
             cancel
             completed-p)
        (condition-case error-data
            (setq secret (chidu-runtime--endpoint-secret endpoint))
          (error
           (setf (chidu-live-watcher-last-error watcher)
                 (error-message-string error-data))))
        (if (null secret)
            (chidu-live--schedule-reconnect
             watcher (chidu-live-watcher-backoff watcher))
          (condition-case error-data
              (progn
                (setq
                 cancel
                 (chidu-jmap-event-source-fetch
                  endpoint secret chidu-event-source-timeout
                  (lambda (result)
                    (setq completed-p t)
                    (chidu-live--event-source-result watcher result))))
                ;; The adapter owns and clears SECRET after this point.
                (setq secret nil)
                (unless completed-p
                  (setf (chidu-live-watcher-request watcher) cancel)))
            (error
             (when secret (clear-string secret))
             (setf (chidu-live-watcher-last-error watcher)
                   (error-message-string error-data))
             (chidu-live--schedule-all watcher)
             (chidu-live--schedule-reconnect
              watcher (chidu-live-watcher-backoff watcher)))))))))

(defun chidu-live-watch-endpoint (app endpoint)
  "Start or replace APP's canonical Email watcher for connected ENDPOINT."
  (unless (appkit-app-live-p app)
    (signal 'wrong-type-argument (list 'appkit-app-live-p app)))
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (let* ((key (chidu-live--watch-key endpoint))
         (existing (gethash key (chidu-app-requests app))))
    (when (chidu-live-watcher-p existing)
      (if-let* ((handle (chidu-live-watcher-handle existing)))
          (appkit-cancel-handle handle)
        (chidu-live--close existing)))
    (let ((watcher
           (chidu-live-watcher-create
            :app app :endpoint endpoint)))
      (setf
       (chidu-live-watcher-handle watcher)
       (appkit-register-handle
        app 'function (apply-partially #'chidu-live--close watcher))
       (chidu-live-watcher-sweep-timer watcher)
       (run-at-time
        chidu-live-sweep-interval
        chidu-live-sweep-interval
        #'chidu-live--schedule-all watcher))
      (puthash key watcher (chidu-app-requests app))
      ;; Reconcile changes that arrived while Emacs was not running before
      ;; waiting for the next EventSource wake.  Desktop notification policy is
      ;; applied only after the canonical Store transition.
      (chidu-live--schedule-all watcher)
      (chidu-live--start-event-source watcher)
      watcher)))

(provide 'chidu-live)

;;; chidu-live.el ends here
