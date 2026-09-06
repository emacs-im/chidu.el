;;; chidu-jmap-email-catchup.el --- JMAP Email catch-up hydration -*- lexical-binding: t; -*-

;;; Commentary:

;; After Email/changes has been normalized, fetch created/full and
;; updated/mutable targets in one JMAP request.  The two Email/get states remain
;; independent evidence for state-matched round closure.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-api)
(require 'chidu-jmap-email)
(require 'chidu-jmap-email-hydration)
(require 'chidu-jmap-response)
(require 'chidu-record)
(require 'chidu-store)

(chidu-define-record chidu-jmap-email-catchup-hydration
    "Profile-specific Email/get results for one canonical catch-up round."
  full
  mutable)

(defun chidu-jmap-email-catchup--method (remote-account-id kind ids call-id)
  "Return one Email/get invocation for REMOTE-ACCOUNT-ID, KIND, IDS, and CALL-ID."
  (vector
   "Email/get"
   (list :accountId remote-account-id
         :ids ids
         :properties (chidu-jmap-email-hydration--properties kind))
   call-id))

(defun chidu-jmap-email-catchup--request
    (remote-account-id created updated)
  "Return REMOTE-ACCOUNT-ID hydration request for CREATED and UPDATED ids."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
    :methodCalls
    ,(vconcat
      (delq
       nil
       (list
        (and (> (length created) 0)
             (chidu-jmap-email-catchup--method
              remote-account-id 'full created "email-catchup-created"))
        (and (> (length updated) 0)
             (chidu-jmap-email-catchup--method
              remote-account-id 'mutable updated "email-catchup-updated")))))))

(defun chidu-jmap-email-catchup--expected
    (remote-account-id created updated)
  "Return REMOTE-ACCOUNT-ID response specs for CREATED and UPDATED."
  (delq
   nil
   (list
    (and (> (length created) 0)
         (list "Email/get" "email-catchup-created" remote-account-id))
    (and (> (length updated) 0)
         (list "Email/get" "email-catchup-updated" remote-account-id)))))

(defun chidu-jmap-email-catchup--decode
    (bytes remote-account-id created updated)
  "Decode BYTES for REMOTE-ACCOUNT-ID and exact CREATED/UPDATED ids."
  (let* ((responses
          (chidu-jmap-parse-method-responses
           bytes
           (chidu-jmap-email-catchup--expected
            remote-account-id created updated)))
         (index 0)
         full
         mutable)
    (when (> (length created) 0)
      (setq full
            (chidu-jmap-email-hydration--decode-arguments
             (chidu-jmap-method-response-arguments (aref responses index))
             'full created)
            index (1+ index)))
    (when (> (length updated) 0)
      (setq mutable
            (chidu-jmap-email-hydration--decode-arguments
             (chidu-jmap-method-response-arguments (aref responses index))
             'mutable updated)))
    (chidu-jmap-email-catchup-hydration-create
     :full full :mutable mutable)))

(defun chidu-jmap-email-fetch-catchup-hydration
    (context secret created updated deliver)
  "Fetch CREATED/full and UPDATED/mutable Email metadata for CONTEXT.

Both nonempty profiles share one JMAP request.  SECRET remains owned by the
caller, and DELIVER receives one typed result."
  (unless (chidu-store-email-sync-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-sync-context-p context)))
  (unless (and (vectorp created) (vectorp updated)
               (> (+ (length created) (length updated)) 0))
    (signal 'wrong-type-argument
            (list 'nonempty-email-catchup-targets-p created updated)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let* ((endpoint (chidu-store-email-sync-context-endpoint context))
         (account (chidu-store-email-sync-context-account context))
         (remote-account-id
          (chidu-store-account-remote-account-id account)))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-email-catchup--request
      remote-account-id created updated)
     (lambda (bytes)
       (chidu-jmap-email-catchup--decode
        bytes remote-account-id created updated))
     deliver)))

(provide 'chidu-jmap-email-catchup)

;;; chidu-jmap-email-catchup.el ends here
