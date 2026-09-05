;;; chidu-contact.el --- JMAP Contacts workflows -*- lexical-binding: t; -*-

;;; Commentary:

;; Cancelable read workflows shared by Compose completion and Contacts views.
;; Object adapters own JMAP shapes; this module owns capability selection,
;; credentials, and runtime operation fencing.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'chidu-jmap-contact)
(require 'chidu-result)
(require 'chidu-runtime)
(require 'chidu-store)

(defun chidu-contact--remote-account-id (endpoint)
  "Return ENDPOINT's primary Contacts Account id, or nil."
  (when
      (seq-contains-p
       (chidu-store-endpoint-capabilities endpoint)
       chidu-jmap-contacts-capability #'equal)
    (chidu-store-endpoint-primary-contacts-remote-account-id endpoint)))

(defun chidu-contact-endpoint-p (endpoint)
  "Return non-nil when ENDPOINT exposes a primary JMAP Contacts Account."
  (and (chidu-store-endpoint-p endpoint)
       (chidu-contact--remote-account-id endpoint)
       t))

(defun chidu-contact--deliver
    (runtime operation result success-function error-function)
  "Deliver RESULT for current RUNTIME OPERATION."
  (when (chidu-runtime--operation-current-p runtime operation)
    (setf (chidu-runtime-operation-cancel-function operation) nil)
    (chidu-runtime--deliver-result
     runtime operation result success-function error-function)))

(defun chidu-contact--request
    (runtime endpoint start-function success-function error-function)
  "Run one Contacts START-FUNCTION through RUNTIME and ENDPOINT.

START-FUNCTION receives remote Account id, secret, and typed delivery function.
Return a cancelable runtime operation."
  (unless (chidu-runtime-p runtime)
    (signal 'wrong-type-argument (list 'chidu-runtime-p runtime)))
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (dolist (function (list start-function success-function error-function))
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function))))
  (let* ((operation (chidu-runtime--begin-operation runtime))
         (remote-account-id (chidu-contact--remote-account-id endpoint)))
    (if (null remote-account-id)
        (chidu-contact--deliver
         runtime operation
         (chidu-result-failure-create
          :kind 'contacts-unavailable :data nil :retryable-p nil)
         success-function error-function)
      (let (secret)
        (condition-case error-data
            (setq secret (chidu-runtime--endpoint-secret endpoint))
          (error
           (chidu-contact--deliver
            runtime operation
            (chidu-runtime--condition-failure
             'credential-error error-data t)
            success-function error-function)))
        (when (and secret
                   (chidu-runtime--operation-current-p runtime operation))
          (condition-case error-data
              (let ((cancel
                     (funcall
                      start-function remote-account-id secret
                      (lambda (result)
                        (chidu-contact--deliver
                         runtime operation result
                         success-function error-function)))))
                (clear-string secret)
                (setq secret nil)
                (chidu-runtime--set-operation-cancel
                 runtime operation cancel))
            (error
             (when secret (clear-string secret))
             (chidu-contact--deliver
              runtime operation
              (chidu-runtime--condition-failure
               'jmap-request-failed error-data t)
              success-function error-function))))))
    operation))

(defun chidu-contact-list-address-books
    (runtime endpoint success-function error-function)
  "Read ENDPOINT AddressBooks through RUNTIME."
  (chidu-contact--request
   runtime endpoint
   (lambda (remote-account-id secret deliver)
     (chidu-jmap-contact-address-books
      endpoint remote-account-id secret deliver))
   success-function error-function))

(defun chidu-contact-query-page
    (runtime endpoint address-book query limit anchor-id
             success-function error-function)
  "Read one ContactCard page for ADDRESS-BOOK through RUNTIME and ENDPOINT.

QUERY is an RFC 9610 text filter.  LIMIT bounds this page; ANCHOR-ID resumes
after a previously returned card when non-nil."
  (unless (chidu-address-book-p address-book)
    (signal 'wrong-type-argument (list 'chidu-address-book-p address-book)))
  (unless (stringp query)
    (signal 'wrong-type-argument (list 'stringp query)))
  (unless (and (integerp limit) (> limit 0) (<= limit 256))
    (signal 'wrong-type-argument (list 'bounded-positive-integer-p limit)))
  (chidu-contact--request
   runtime endpoint
   (lambda (remote-account-id secret deliver)
     (chidu-jmap-contact-page
      endpoint remote-account-id
      (chidu-address-book-remote-id address-book)
      query limit anchor-id secret deliver))
   success-function error-function))

(defun chidu-contact-get-detail
    (runtime endpoint remote-contact-id success-function error-function)
  "Read REMOTE-CONTACT-ID detail through RUNTIME and ENDPOINT."
  (chidu-contact--request
   runtime endpoint
   (lambda (remote-account-id secret deliver)
     (chidu-jmap-contact-detail
      endpoint remote-account-id remote-contact-id secret deliver))
   success-function error-function))

(defun chidu-complete-compose-addresses
    (runtime context query limit success-function error-function)
  "Complete CONTEXT recipient QUERY through JMAP Contacts in RUNTIME.

Return at most LIMIT `chidu-store-email-address' values through
SUCCESS-FUNCTION."
  (unless (chidu-store-compose-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-context-p context)))
  (unless (stringp query)
    (signal 'wrong-type-argument (list 'stringp query)))
  (unless (and (integerp limit) (> limit 0) (<= limit 256))
    (signal 'wrong-type-argument (list 'bounded-positive-integer-p limit)))
  (let ((endpoint (chidu-store-compose-context-endpoint context)))
    (chidu-contact--request
     runtime endpoint
     (lambda (remote-account-id secret deliver)
       (chidu-jmap-contact-search
        endpoint remote-account-id query limit secret deliver))
     success-function error-function)))

(provide 'chidu-contact)

;;; chidu-contact.el ends here
