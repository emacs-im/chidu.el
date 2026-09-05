;;; chidu-draft-checkout.el --- Server Draft checkout workflow -*- lexical-binding: t; -*-

;;; Commentary:

;; Resolve one canonical server Draft to an exact JMAP Identity, materialize
;; every remote attachment into Chidu's private content-addressed resource
;; tree, and atomically create its local Compose checkout.  Existing checkouts
;; are Store-first and do not fetch the immutable remote Email or Blobs again.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-draft-semantics)
(require 'chidu-jmap-download)
(require 'chidu-jmap-draft-checkout)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)

(defun chidu-draft-checkout--materialized-resource-p (before after)
  "Return non-nil when AFTER is BEFORE plus exact local digest evidence."
  (and
   (chidu-store-compose-resource-observation-p before)
   (chidu-store-compose-resource-observation-p after)
   (let ((digest
          (chidu-store-compose-resource-observation-digest after)))
     (and digest
          (equal
           after
           (chidu-store-compose-resource-observation-with
            before :digest digest))))))

(defun chidu-checkout-draft
    (runtime endpoint account drafts-mailbox row
             success-function error-function)
  "Open or create ROW's local Draft checkout in RUNTIME.

ENDPOINT, ACCOUNT, and DRAFTS-MAILBOX own ROW.  Call SUCCESS-FUNCTION with a
`chidu-store-compose-context' or ERROR-FUNCTION with a typed failure.  Return a
cancelable runtime operation."
  (unless (chidu-runtime-p runtime)
    (signal 'wrong-type-argument (list 'chidu-runtime-p runtime)))
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (chidu-store-account-p account)
    (signal 'wrong-type-argument (list 'chidu-store-account-p account)))
  (unless (chidu-store-mailbox-p drafts-mailbox)
    (signal 'wrong-type-argument
            (list 'chidu-store-mailbox-p drafts-mailbox)))
  (unless (chidu-store-draft-row-p row)
    (signal 'wrong-type-argument (list 'chidu-store-draft-row-p row)))
  (dolist (function (list success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (if-let* ((workspace-id (chidu-store-draft-row-workspace-id row)))
      (chidu-runtime-get-compose-workspace
       runtime workspace-id success-function error-function)
    (let* ((operation (chidu-runtime--begin-operation runtime))
           (summary (chidu-store-draft-row-summary-row row))
           (remote-email-id
            (chidu-store-email-summary-row-remote-email-id summary))
           plan
           resources
           (resource-index 0))
      (cl-labels
          ((current-p ()
             (chidu-runtime--operation-current-p runtime operation))
           (clear-cancel ()
             (when (current-p)
               (setf
                (chidu-runtime-operation-cancel-function operation) nil)))
           (finish (result)
             (chidu-runtime--deliver-result
              runtime operation result success-function error-function))
           (condition-failure (kind error-data retryable-p)
             (finish
              (chidu-runtime--condition-failure
               kind error-data retryable-p)))
           (missing-cancel (kind)
             (finish
              (chidu-result-failure-create
               :kind kind
               :data '(request-remained-live-without-cancellation)
               :retryable-p t)))
           (commit ()
             (chidu-runtime--store-call
              runtime
              (chidu-store-op-checkout-draft-create
               :workspace-id (chidu-store-new-local-id)
               :account-id (chidu-store-account-account-id account)
               :identity-id
               (chidu-store-identity-identity-id
                (chidu-draft-checkout-plan-identity plan))
               :drafts-mailbox-id
               (chidu-store-mailbox-mailbox-id drafts-mailbox)
               :local-email-id
               (chidu-store-email-summary-row-local-email-id summary)
               :remote-email-id
               (chidu-draft-checkout-plan-remote-email-id plan)
               :remote-blob-id
               (chidu-draft-checkout-plan-remote-blob-id plan)
               :document
               (chidu-draft-checkout-plan-document plan)
               :resources resources)
              #'finish))
           (resource-finished (index before result)
             (when (current-p)
               (clear-cancel)
               (cond
                ((chidu-result-failure-p result) (finish result))
                ((chidu-result-ok-p result)
                 (let ((after (chidu-result-ok-value result)))
                   (if (and (= index resource-index)
                            (chidu-draft-checkout--materialized-resource-p
                             before after))
                       (progn
                         (aset resources index after)
                         (setq resource-index (1+ resource-index))
                         (materialize-next))
                     (finish
                      (chidu-result-failure-create
                       :kind 'invalid-compose-resource-materialization
                       :data
                       (list
                        :resource-id
                        (chidu-store-compose-resource-observation-resource-id
                         before))
                       :retryable-p nil)))))
                (t
                 (finish
                  (chidu-result-failure-create
                   :kind 'invalid-result :data (list :value result)
                   :retryable-p nil))))))
           (materialize-resource (resource index)
             (let (secret cancel (pending-p t))
               (condition-case error-data
                   (setq secret (chidu-runtime--endpoint-secret endpoint))
                 (error
                  (condition-failure 'credential-error error-data t)))
               (when (and secret (current-p))
                 (condition-case error-data
                     (progn
                       (setq
                        cancel
                        (chidu-jmap-download-compose-resource
                         endpoint account resource
                         (chidu-runtime-data-root runtime) secret
                         (lambda (result)
                           ;; A test adapter or startup failure may settle
                           ;; synchronously.  Never let the previous stage's
                           ;; returned cancel overwrite a newly started stage.
                           (setq pending-p nil)
                           (resource-finished index resource result))))
                       (clear-string secret)
                       (setq secret nil)
                       (when (and pending-p (current-p))
                         (if cancel
                             (chidu-runtime--set-operation-cancel
                              runtime operation cancel)
                           (missing-cancel
                            'jmap-download-did-not-settle))))
                   (error
                    (when secret (clear-string secret))
                    (condition-failure
                     'jmap-download-failed error-data t))))))
           (materialize-next ()
             (when (current-p)
               (if (>= resource-index (length resources))
                   (commit)
                 (let ((resource (aref resources resource-index)))
                   (if
                       (chidu-store-compose-resource-observation-digest
                        resource)
                       (progn
                         (setq resource-index (1+ resource-index))
                         (materialize-next))
                     (materialize-resource resource resource-index))))))
           (accept-snapshot (value)
             (if (chidu-draft-editable-snapshot-p value)
                 (let ((resolved (chidu-draft-bind-checkout account value)))
                   (cond
                    ((chidu-result-failure-p resolved)
                     (finish resolved))
                    ((not
                      (equal
                       remote-email-id
                       (chidu-draft-checkout-plan-remote-email-id resolved)))
                     (finish
                      (chidu-result-failure-create
                       :kind 'invalid-result
                       :data
                       (list
                        :expected-remote-email-id remote-email-id
                        :actual-remote-email-id
                        (chidu-draft-checkout-plan-remote-email-id resolved))
                       :retryable-p nil)))
                    (t
                     (setq plan resolved
                           resources
                           (copy-sequence
                            (chidu-draft-checkout-plan-resources resolved))
                           resource-index 0)
                     (materialize-next))))
               (finish
                (chidu-result-failure-create
                 :kind 'invalid-result :data (list :value value)
                 :retryable-p nil))))
           (remote-finished (result)
             (when (current-p)
               (clear-cancel)
               (cond
                ((chidu-result-failure-p result) (finish result))
                ((chidu-result-ok-p result)
                 (accept-snapshot (chidu-result-ok-value result)))
                (t
                 (finish
                  (chidu-result-failure-create
                   :kind 'invalid-result :data (list :value result)
                   :retryable-p nil))))))
           (start-remote ()
             (let (secret cancel (pending-p t))
               (condition-case error-data
                   (setq secret (chidu-runtime--endpoint-secret endpoint))
                 (error
                  (condition-failure 'credential-error error-data t)))
               (when (and secret (current-p))
                 (condition-case error-data
                     (progn
                       (setq
                        cancel
                        (chidu-jmap-draft-checkout
                         endpoint account drafts-mailbox remote-email-id secret
                         (lambda (result)
                           (setq pending-p nil)
                           (remote-finished result))))
                       (clear-string secret)
                       (setq secret nil)
                       (when (and pending-p (current-p))
                         (if cancel
                             (chidu-runtime--set-operation-cancel
                              runtime operation cancel)
                           (missing-cancel
                            'jmap-request-did-not-settle))))
                   (error
                    (when secret (clear-string secret))
                    (condition-failure
                     'jmap-request-failed error-data t)))))))
        (start-remote)
        operation))))

(provide 'chidu-draft-checkout)

;;; chidu-draft-checkout.el ends here
