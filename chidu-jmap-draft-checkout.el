;;; chidu-jmap-draft-checkout.el --- Exact server Draft checkout -*- lexical-binding: t; -*-

;;; Commentary:

;; Thin JMAP adapter for one remote Draft checkout.  Strict wire decoding lives
;; in `chidu-jmap-draft-observation'; representability projection lives in
;; `chidu-jmap-draft-projection'; Identity binding and editable semantics live
;; in `chidu-draft-semantics'.

;;; Code:

(require 'chidu-jmap-api)
(require 'chidu-jmap-draft-observation)
(require 'chidu-jmap-draft-projection)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-result)
(require 'chidu-store)

(defconst chidu-jmap-draft-checkout-properties
  ["id" "blobId" "mailboxIds" "keywords" "headers"
   "messageId" "inReplyTo" "references" "from" "sender" "subject"
   "bodyStructure" "bodyValues" "textBody" "htmlBody" "attachments"]
  "Email properties required to decide exact Draft representability.")

(defconst chidu-jmap-draft-checkout-body-properties
  ["partId" "blobId" "size" "headers" "name" "type" "charset"
   "disposition" "cid" "language" "location" "subParts"]
  "EmailBodyPart properties required for strict Draft projection.")

(defun chidu-jmap-draft-checkout-request
    (remote-account-id remote-email-id)
  "Return exact checkout request for REMOTE-EMAIL-ID in REMOTE-ACCOUNT-ID."
  (setq remote-account-id
        (chidu-jmap--id remote-account-id "Draft Account id")
        remote-email-id
        (chidu-jmap--id remote-email-id "Draft Email id"))
  (let ((arguments
         (list
          :accountId remote-account-id
          :ids (vector remote-email-id)
          :properties chidu-jmap-draft-checkout-properties
          :bodyProperties chidu-jmap-draft-checkout-body-properties
          :fetchTextBodyValues t
          :maxBodyValueBytes 0)))
    (list
     :using
     (vector chidu-jmap-core-capability chidu-jmap-mail-capability)
     :methodCalls
     (vector (vector "Email/get" arguments "draft-checkout")))))

(defun chidu-jmap-draft-checkout-validate-response
    (bytes remote-account-id remote-email-id remote-drafts-mailbox-id)
  "Decode and project Draft checkout response BYTES.

REMOTE-ACCOUNT-ID, REMOTE-EMAIL-ID, and REMOTE-DRAFTS-MAILBOX-ID are exact
request evidence.  Return a representable snapshot or a typed failure."
  (let* ((response
          (chidu-jmap-parse-single-method-response
           bytes "Email/get" "draft-checkout" remote-account-id))
         (arguments (chidu-jmap-method-response-arguments response))
         (email-state
          (chidu-jmap--string
           (chidu-jmap--required arguments "state" "Email/get")
           "Draft Email state"))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "Email/get")
           "Draft Email/get list"))
         (not-found
          (chidu-jmap--nullable-vector
           (chidu-jmap--required arguments "notFound" "Email/get")
           "Draft Email/get notFound")))
    (cond
     ((and (zerop (length wire-list))
           (= 1 (length not-found))
           (equal remote-email-id
                  (chidu-jmap--id
                   (aref not-found 0) "Draft notFound id")))
      (chidu-result-failure-create
       :kind 'draft-not-found
       :data (list :remote-email-id remote-email-id)
       :retryable-p nil))
     ((not (and (= 1 (length wire-list))
                (zerop (length not-found))))
      (signal 'chidu-jmap-error
              '("Draft Email/get returned incomplete coverage")))
     (t
      (let* ((email
              (chidu-jmap--hash
               (aref wire-list 0) "Draft Email/get item"))
             (observation
              (chidu-jmap-draft-observation-decode
               email email-state)))
        (unless
            (equal
             remote-email-id
             (chidu-jmap-remote-draft-observation-remote-email-id observation))
          (signal 'chidu-jmap-error
                  '("Draft Email/get returned a different id")))
        (chidu-jmap-draft-project
         observation remote-drafts-mailbox-id))))))

(defun chidu-jmap-draft-checkout
    (endpoint account drafts-mailbox remote-email-id secret deliver)
  "Fetch REMOTE-EMAIL-ID checkout through ENDPOINT and ACCOUNT.

DRAFTS-MAILBOX is exact request evidence.  Use SECRET and call DELIVER with one
typed result.  Return a cancellation function when the request remains live."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-mailbox-p drafts-mailbox)
    (signal 'wrong-type-argument
            (list 'chidu-store-mailbox-p drafts-mailbox)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let ((remote-account-id (chidu-store-account-remote-account-id account))
        (remote-mailbox-id
         (chidu-store-mailbox-remote-mailbox-id drafts-mailbox)))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-draft-checkout-request remote-account-id remote-email-id)
     (lambda (bytes)
       (chidu-jmap-draft-checkout-validate-response
        bytes remote-account-id remote-email-id remote-mailbox-id))
     deliver)))

(provide 'chidu-jmap-draft-checkout)

;;; chidu-jmap-draft-checkout.el ends here
