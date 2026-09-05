;;; chidu-jmap-mailbox-move.el --- Batched JMAP Mailbox delta -*- lexical-binding: t; -*-

;;; Commentary:

;; Move a bounded batch of Email objects by applying one atomic per-object
;; PatchObject: add the destination Mailbox id and remove the source Mailbox id.
;; Unrelated memberships are never replaced.  Transport and exact SetResponse
;; coverage live in `chidu-jmap-set'; this module only owns move semantics.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-set)
(require 'chidu-jmap-types)
(require 'chidu-store)

(defun chidu-jmap-mailbox-move--updates
    (source-remote-mailbox-id destination-remote-mailbox-id intents)
  "Return Email/set updates moving INTENTS.

Move from SOURCE-REMOTE-MAILBOX-ID to DESTINATION-REMOTE-MAILBOX-ID."
  (let* ((source-path
          (concat
           "mailboxIds/"
           (chidu-jmap-patch-path-component
            source-remote-mailbox-id "source Mailbox id")))
         (destination-path
          (concat
           "mailboxIds/"
           (chidu-jmap-patch-path-component
            destination-remote-mailbox-id "destination Mailbox id")))
         (updates (make-hash-table :test #'equal)))
    (cl-loop
     for intent across intents
     for remote-email-id =
     (chidu-store-mailbox-move-intent-remote-email-id intent)
     do
     (let ((patch (make-hash-table :test #'equal)))
       ;; Both keys belong to one PatchObject, so the target never passes
       ;; through a state with no Mailbox and unrelated memberships survive.
       (puthash destination-path t patch)
       (puthash source-path :json-null patch)
       (puthash remote-email-id patch updates)))
    updates))

(defun chidu-jmap-mailbox-move--request
    (remote-account-id source-remote-mailbox-id
                       destination-remote-mailbox-id intents)
  "Return REMOTE-ACCOUNT-ID Email/set request moving INTENTS.

Move from SOURCE-REMOTE-MAILBOX-ID to DESTINATION-REMOTE-MAILBOX-ID."
  (chidu-jmap-set-update-request
   remote-account-id "mailbox-move"
   (chidu-jmap-mailbox-move--updates
    source-remote-mailbox-id destination-remote-mailbox-id intents)))

(defun chidu-jmap-mailbox-move-request-size (context intents)
  "Return encoded request size for moving INTENTS from CONTEXT."
  (let ((account (chidu-store-mailbox-move-context-account context))
        (source
         (chidu-store-mailbox-move-context-source-mailbox context))
        (destination
         (chidu-store-mailbox-move-context-destination-mailbox context)))
    (unless (and account source destination)
      (signal 'chidu-invariant-error
              '("Mailbox move context is incomplete")))
    (chidu-jmap-set-update-request-size
     (chidu-store-account-remote-account-id account)
     "mailbox-move"
     (chidu-jmap-mailbox-move--updates
      (chidu-store-mailbox-remote-mailbox-id source)
      (chidu-store-mailbox-remote-mailbox-id destination)
      intents))))

(defun chidu-jmap-move-mailbox-batch (context secret intents deliver)
  "Move bounded Mailbox INTENTS from CONTEXT using owned SECRET.

Call DELIVER with the typed result.  INTENTS must not exceed the Endpoint's
maxObjectsInSet.  Return a cancel function; cancellation leaves durable Store
targets unresolved."
  (unless (chidu-store-mailbox-move-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-mailbox-move-context-p context)))
  (unless (and (vectorp intents) (> (length intents) 0)
               (cl-loop for intent across intents
                        always (chidu-store-mailbox-move-intent-p intent)))
    (signal 'wrong-type-argument
            (list 'nonempty-mailbox-move-intent-vector-p intents)))
  (let* ((endpoint (chidu-store-mailbox-move-context-endpoint context))
         (account (chidu-store-mailbox-move-context-account context))
         (source
          (chidu-store-mailbox-move-context-source-mailbox context))
         (destination
          (chidu-store-mailbox-move-context-destination-mailbox context))
         (limit (chidu-store-endpoint-max-objects-in-set endpoint)))
    (unless (and (integerp limit) (> limit 0))
      (signal 'chidu-invariant-error
              '("JMAP Session has no maxObjectsInSet")))
    (when (> (length intents) limit)
      (signal 'chidu-invariant-error
              (list "Mailbox move batch exceeds maxObjectsInSet"
                    (length intents) limit)))
    (unless (and source destination)
      (signal 'chidu-invariant-error
              '("Mailbox move context lacks source or destination")))
    (chidu-jmap-set-update
     endpoint account secret "mailbox-move"
     (chidu-jmap-mailbox-move--updates
      (chidu-store-mailbox-remote-mailbox-id source)
      (chidu-store-mailbox-remote-mailbox-id destination)
      intents)
     deliver)))

(provide 'chidu-jmap-mailbox-move)

;;; chidu-jmap-mailbox-move.el ends here
