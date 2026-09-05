;;; chidu-jmap-api.el --- Cancelable JMAP API request boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Run one bounded JMAP API request through an Endpoint.  Object adapters own
;; request construction and response decoding; this module owns HTTP policy,
;; cancellation, and exactly-once typed delivery.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chidu-jmap-http)
(require 'chidu-jmap-types)
(require 'chidu-result)
(require 'chidu-store)

(cl-defstruct (chidu-jmap-api-request
               (:constructor chidu-jmap-api-request-create))
  "One cancelable JMAP API request."
  process
  deliver
  completed-p
  canceled-p)

(defun chidu-jmap-api--finish (request result)
  "Complete REQUEST exactly once with typed RESULT."
  (unless (chidu-jmap-api-request-completed-p request)
    (setf (chidu-jmap-api-request-completed-p request) t)
    (unless (chidu-jmap-api-request-canceled-p request)
      (funcall (chidu-jmap-api-request-deliver request) result))))

(defun chidu-jmap-api-start (endpoint secret body decoder deliver)
  "Send BODY through ENDPOINT using SECRET and decode with DECODER.

DELIVER receives one typed result.  SECRET remains owned by the caller so a
workflow may reuse or clear it.  Return a cancellation function, or nil when
startup settles synchronously."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (dolist (function (list decoder deliver))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let ((request (chidu-jmap-api-request-create :deliver deliver)))
    (condition-case error-data
        (progn
          (setf
           (chidu-jmap-api-request-process request)
           (chidu-jmap-http-request
            (chidu-store-endpoint-api-url endpoint)
            (chidu-store-endpoint-login endpoint)
            (chidu-store-endpoint-authentication endpoint)
            secret
            (lambda (result)
              (cond
               ((chidu-result-failure-p result)
                (chidu-jmap-api--finish request result))
               ((chidu-result-ok-p result)
                (condition-case validation-error
                    (let ((response (chidu-result-ok-value result)))
                      (if (= 200 (chidu-jmap-http-response-status response))
                          (let ((decoded
                                 (funcall
                                  decoder
                                  (chidu-jmap-http-response-body response))))
                            (chidu-jmap-api--finish
                             request
                             (if (chidu-result-failure-p decoded)
                                 decoded
                               (chidu-result-ok-create :value decoded))))
                        (chidu-jmap-api--finish
                         request
                         (chidu-result-failure-create
                          :kind 'unexpected-http-status
                          :data
                          (list :status
                                (chidu-jmap-http-response-status response))
                          :retryable-p nil))))
                  (error
                   (chidu-jmap-api--finish
                    request
                    (chidu-result-failure-create
                     :kind 'invalid-jmap-response
                     :data
                     (list :message
                           (error-message-string validation-error))
                     :retryable-p nil)))))
               (t (chidu-jmap-api--finish request result))))
            :body body
            :max-request-bytes
            (chidu-store-endpoint-max-size-request endpoint)
            :byte-cap chidu-jmap-api-byte-cap))
          (unless (chidu-jmap-api-request-completed-p request)
            (lambda ()
              (unless (chidu-jmap-api-request-completed-p request)
                (setf (chidu-jmap-api-request-canceled-p request) t)
                (when-let* ((process
                             (chidu-jmap-api-request-process request)))
                  (chidu-jmap-http-cancel process))
                (chidu-jmap-api--finish
                 request
                 (chidu-result-failure-create
                  :kind 'canceled :data nil :retryable-p nil))))))
      (error
       (chidu-jmap-api--finish
        request
        (chidu-result-failure-create
         :kind 'jmap-request-failed
         :data (list :message (error-message-string error-data))
         :retryable-p nil))
       nil))))

(provide 'chidu-jmap-api)

;;; chidu-jmap-api.el ends here
