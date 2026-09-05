;;; chidu-jmap-trash.el --- JMAP move-to-Trash updates -*- lexical-binding: t; -*-

;;; Commentary:

;; RFC 8621 move-to-Trash semantics replace the complete `mailboxIds' set with
;; the unique role=trash Mailbox.  This differs deliberately from an ordinary
;; move, which removes only one source membership.  Authoritative preflight
;; membership evidence and durable retry policy belong to `chidu-trash.el'.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-set)
(require 'chidu-store)

(defun chidu-jmap-trash--updates (trash-remote-mailbox-id intents)
  "Return Trash-only update map for TRASH-REMOTE-MAILBOX-ID and INTENTS."
  (let ((updates (make-hash-table :test #'equal)))
    (cl-loop
     for intent across intents
     for remote-email-id = (chidu-store-trash-intent-remote-email-id intent)
     do
     (let ((patch (make-hash-table :test #'equal))
           (mailbox-ids (make-hash-table :test #'equal)))
       ;; Replacing the complete property is the operation's semantics.  It is
       ;; also robust if another membership appears after the preflight get.
       (puthash trash-remote-mailbox-id t mailbox-ids)
       (puthash "mailboxIds" mailbox-ids patch)
       (puthash remote-email-id patch updates)))
    updates))

(defun chidu-jmap-trash--request
    (remote-account-id trash-remote-mailbox-id intents)
  "Return REMOTE-ACCOUNT-ID move-to-Trash request for INTENTS.

TRASH-REMOTE-MAILBOX-ID is the sole resulting Mailbox membership."
  (chidu-jmap-set-update-request
   remote-account-id "move-to-trash"
   (chidu-jmap-trash--updates trash-remote-mailbox-id intents)))

(defun chidu-jmap-trash-request-size (context intents)
  "Return encoded move-to-Trash request size for CONTEXT and INTENTS."
  (let ((account (chidu-store-trash-context-account context))
        (trash-mailbox (chidu-store-trash-context-trash-mailbox context)))
    (unless (and account trash-mailbox)
      (signal 'chidu-invariant-error '("Trash context is incomplete")))
    (chidu-jmap-set-update-request-size
     (chidu-store-account-remote-account-id account)
     "move-to-trash"
     (chidu-jmap-trash--updates
      (chidu-store-mailbox-remote-mailbox-id trash-mailbox)
      intents))))

(defun chidu-jmap-move-to-trash-batch (context secret intents deliver)
  "Move bounded Trash INTENTS through CONTEXT using owned SECRET.

Call DELIVER with the strict per-target Set result.  Return a cancellation
function; cancellation leaves durable targets unresolved."
  (unless (chidu-store-trash-context-p context)
    (signal 'wrong-type-argument (list 'chidu-store-trash-context-p context)))
  (unless (and (vectorp intents) (> (length intents) 0)
               (cl-loop for intent across intents
                        always
                        (and (chidu-store-trash-intent-p intent)
                             (vectorp
                              (chidu-store-trash-intent-original-remote-mailbox-ids
                               intent)))))
    (signal 'wrong-type-argument
            (list 'hydrated-trash-intent-vector-p intents)))
  (let* ((endpoint (chidu-store-trash-context-endpoint context))
         (account (chidu-store-trash-context-account context))
         (trash-mailbox (chidu-store-trash-context-trash-mailbox context))
         (limit (chidu-store-endpoint-max-objects-in-set endpoint)))
    (unless (and (integerp limit) (> limit 0))
      (signal 'chidu-invariant-error '("JMAP Session has no maxObjectsInSet")))
    (when (> (length intents) limit)
      (signal 'chidu-invariant-error
              (list "Trash batch exceeds maxObjectsInSet"
                    (length intents) limit)))
    (unless trash-mailbox
      (signal 'chidu-invariant-error '("Trash context has no destination")))
    (chidu-jmap-set-update
     endpoint account secret "move-to-trash"
     (chidu-jmap-trash--updates
      (chidu-store-mailbox-remote-mailbox-id trash-mailbox)
      intents)
     deliver)))

(provide 'chidu-jmap-trash)

;;; chidu-jmap-trash.el ends here
