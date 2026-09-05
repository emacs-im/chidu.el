;;; chidu-jmap-event-source.el --- JMAP EventSource wake hints -*- lexical-binding: t; -*-

;;; Commentary:

;; Expand the Session eventSourceUrl template, perform one bounded EventSource
;; long-poll, and decode StateChange events.  The resulting account/type values
;; are wake hints only; callers must reconcile with authoritative JMAP methods.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'chidu-jmap-http)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-store)

(defconst chidu-jmap-event-source-byte-cap (* 256 1024)
  "Maximum bytes accepted from one EventSource long-poll.")

(defconst chidu-jmap-event-source-line-cap (* 64 1024)
  "Maximum UTF-8 characters accepted in one EventSource line.")

(chidu-define-record chidu-jmap-event-wake
    "One Account wake hint decoded from StateChange."
  remote-account-id
  (types (vector)))

(cl-defstruct (chidu-jmap-event-source-fetch
               (:constructor chidu-jmap-event-source-fetch-create))
  "Mechanical state for one cancelable EventSource long-poll."
  endpoint
  secret
  deliver
  request
  completed-p
  canceled-p)

(defun chidu-jmap-event-source-url (endpoint)
  "Expand and validate ENDPOINT's EventSource URI template.

The request asks for one state event concerning Email or EmailDelivery and
suppresses ping events.  The expanded URL must remain HTTPS and same-origin with
the Endpoint API URL."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (let* ((template
          (chidu-store-endpoint-event-source-url endpoint))
         (values
          '(("types" . "Email,EmailDelivery")
            ("closeafter" . "state")
            ("ping" . "0")))
         (position 0)
         pieces)
    (unless (and (stringp template) (not (string-empty-p template)))
      (signal 'chidu-jmap-error
              '("JMAP Session has no eventSourceUrl")))
    (while (string-match "{\\([^{}]+\\)}" template position)
      (let* ((name (match-string 1 template))
             (entry (assoc name values)))
        (unless entry
          (signal 'chidu-jmap-error
                  (list (format "Unsupported EventSource template expression: %s"
                                name))))
        (push (substring template position (match-beginning 0)) pieces)
        (push (url-hexify-string (cdr entry)) pieces)
        (setq position (match-end 0))))
    (push (substring template position) pieces)
    (let* ((expanded (apply #'concat (nreverse pieces)))
           (event-url
            (chidu-jmap--parse-url expanded "expanded eventSourceUrl"))
           (api-url
            (chidu-jmap--parse-url
             (chidu-store-endpoint-api-url endpoint) "Endpoint apiUrl")))
      (when (string-match-p "[{}]" expanded)
        (signal 'chidu-jmap-error
                '("EventSource template contains an unmatched expression")))
      (unless (chidu-jmap--same-origin-p event-url api-url)
        (signal 'chidu-jmap-error
                '("eventSourceUrl must share the JMAP API origin")))
      expanded)))

(defun chidu-jmap-event-source--state-change (data)
  "Decode one StateChange JSON DATA string into wake records."
  (let* ((bytes (encode-coding-string data 'utf-8-unix t))
         (object
          (chidu-jmap--parse-json-object bytes "EventSource state data"))
         (type
          (chidu-jmap--string
           (chidu-jmap--required object "@type" "StateChange")
           "StateChange @type"))
         (changed
          (chidu-jmap--hash
           (chidu-jmap--required object "changed" "StateChange")
           "StateChange changed"))
         (by-account (make-hash-table :test #'equal)))
    (unless (equal type "StateChange")
      (signal 'chidu-jmap-error
              '("EventSource state event is not a StateChange")))
    (maphash
     (lambda (wire-account-id wire-types)
       (let ((account-id
              (chidu-jmap--id wire-account-id "StateChange accountId"))
             (types
              (chidu-jmap--hash wire-types "StateChange account types"))
             selected)
         (maphash
          (lambda (wire-type wire-state)
            (let ((data-type
                   (chidu-jmap--string wire-type "StateChange data type")))
              ;; Validate every state value even when the type is not currently
              ;; actionable; malformed wake data must not silently pass.
              (chidu-jmap--string wire-state "StateChange state")
              (when (member data-type '("Email" "EmailDelivery"))
                (push data-type selected))))
          types)
         (when selected
           (let ((existing (gethash account-id by-account)))
             (puthash
              account-id
              (delete-dups (append selected existing))
              by-account)))))
     changed)
    (vconcat
     (mapcar
      (lambda (account-id)
        (chidu-jmap-event-wake-create
         :remote-account-id account-id
         :types
         (vconcat (sort (copy-sequence (gethash account-id by-account))
                        #'string<))))
      (sort (hash-table-keys by-account) #'string<)))))

(defun chidu-jmap-event-source-parse (bytes)
  "Parse bounded EventSource BYTES and return account wake records."
  (let* ((text (decode-coding-string bytes 'utf-8-unix))
         (roundtrip (encode-coding-string text 'utf-8-unix t)))
    (unless (equal bytes roundtrip)
      (signal 'chidu-jmap-error
              '("EventSource response is not valid UTF-8")))
    (setq text (replace-regexp-in-string "\r\n?" "\n" text t t))
    (let ((events nil)
          (event-type nil)
          (data-lines nil))
      (cl-labels
          ((dispatch
             ()
             (when data-lines
               (let ((data (string-join (nreverse data-lines) "\n")))
                 (when (or (null event-type) (equal event-type "state"))
                   (cl-loop
                    for wake across
                    (chidu-jmap-event-source--state-change data)
                    do (push wake events))))
               (setq event-type nil
                     data-lines nil))))
        (dolist (line (split-string text "\n" nil))
          (when (> (length line) chidu-jmap-event-source-line-cap)
            (signal 'chidu-jmap-error
                    '("EventSource line exceeds the configured cap")))
          (cond
           ((string-empty-p line) (dispatch))
           ((string-prefix-p ":" line) nil)
           (t
            (let* ((separator (string-search ":" line))
                   (field (if separator (substring line 0 separator) line))
                   (value
                    (if separator
                        (let ((raw (substring line (1+ separator))))
                          (if (string-prefix-p " " raw)
                              (substring raw 1)
                            raw))
                      "")))
              (pcase field
                ("event" (setq event-type value))
                ("data" (push value data-lines))
                (_ nil)))))
          (dispatch))
        (let ((by-account (make-hash-table :test #'equal)))
          (dolist (wake events)
            (let* ((account-id
                    (chidu-jmap-event-wake-remote-account-id wake))
                   (types (gethash account-id by-account)))
              (cl-loop
               for type across (chidu-jmap-event-wake-types wake)
               do (cl-pushnew type types :test #'equal))
              (puthash account-id types by-account)))
          (vconcat
           (mapcar
            (lambda (account-id)
              (chidu-jmap-event-wake-create
               :remote-account-id account-id
               :types
               (vconcat
                (sort (copy-sequence (gethash account-id by-account))
                      #'string<))))
            (sort (hash-table-keys by-account) #'string<))))))))

(defun chidu-jmap-event-source--content-type-p (value)
  "Return non-nil when HTTP Content-Type VALUE is an event stream."
  (and (stringp value)
       (string-match-p
        "\\`[[:space:]]*text/event-stream\\(?:[[:space:]]*;\\|[[:space:]]*\\\'\\)"
        (downcase value))))

(defun chidu-jmap-event-source--finish (fetch result)
  "Complete FETCH exactly once with RESULT and clear its credential."
  (unless (chidu-jmap-event-source-fetch-completed-p fetch)
    (setf (chidu-jmap-event-source-fetch-completed-p fetch) t
          (chidu-jmap-event-source-fetch-request fetch) nil)
    (when-let* ((secret (chidu-jmap-event-source-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-event-source-fetch-secret fetch) nil))
    (unless (chidu-jmap-event-source-fetch-canceled-p fetch)
      (funcall (chidu-jmap-event-source-fetch-deliver fetch) result))))

(defun chidu-jmap-event-source--cancel (fetch)
  "Cancel FETCH and clear its credential."
  (unless (chidu-jmap-event-source-fetch-completed-p fetch)
    (setf (chidu-jmap-event-source-fetch-canceled-p fetch) t
          (chidu-jmap-event-source-fetch-completed-p fetch) t)
    (when-let* ((request (chidu-jmap-event-source-fetch-request fetch)))
      (chidu-jmap-http-cancel request)
      (setf (chidu-jmap-event-source-fetch-request fetch) nil))
    (when-let* ((secret (chidu-jmap-event-source-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-event-source-fetch-secret fetch) nil))))

(defun chidu-jmap-event-source-fetch (endpoint secret timeout deliver)
  "Perform one EventSource long-poll for ENDPOINT.

The adapter owns and clears SECRET.  TIMEOUT is the bounded long-poll duration.
DELIVER receives a typed result containing a vector of wake records.  Return a
zero-argument cancellation function, or nil after synchronous failure."
  (unless (chidu-store-endpoint-p endpoint)
    (signal 'wrong-type-argument (list 'chidu-store-endpoint-p endpoint)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (and (numberp timeout) (> timeout 0))
    (signal 'wrong-type-argument (list 'positive-number-p timeout)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (let ((fetch
         (chidu-jmap-event-source-fetch-create
          :endpoint endpoint :secret secret :deliver deliver)))
    (condition-case error-data
        (setf
         (chidu-jmap-event-source-fetch-request fetch)
         (chidu-jmap-http-request
          (chidu-jmap-event-source-url endpoint)
          (chidu-store-endpoint-login endpoint)
          (chidu-store-endpoint-authentication endpoint)
          secret
          (lambda (result)
            (cond
             ((chidu-result-failure-p result)
              (chidu-jmap-event-source--finish fetch result))
             ((chidu-result-ok-p result)
              (condition-case validation-error
                  (let ((response (chidu-result-ok-value result)))
                    (cond
                     ((/= 200 (chidu-jmap-http-response-status response))
                      (chidu-jmap-event-source--finish
                       fetch
                       (chidu-result-failure-create
                        :kind 'unexpected-http-status
                        :data
                        (list :status
                              (chidu-jmap-http-response-status response))
                        :retryable-p t)))
                     ((not
                       (chidu-jmap-event-source--content-type-p
                        (chidu-jmap-http-response-content-type response)))
                      (chidu-jmap-event-source--finish
                       fetch
                       (chidu-result-failure-create
                        :kind 'unexpected-content-type
                        :data
                        (list :content-type
                              (chidu-jmap-http-response-content-type response))
                        :retryable-p t)))
                     (t
                      (chidu-jmap-event-source--finish
                       fetch
                       (chidu-result-ok-create
                        :value
                        (chidu-jmap-event-source-parse
                         (chidu-jmap-http-response-body response)))))))
                (error
                 (chidu-jmap-event-source--finish
                  fetch
                  (chidu-result-failure-create
                   :kind 'invalid-event-source
                   :data
                   (list :message (error-message-string validation-error))
                   :retryable-p t)))))
             (t (chidu-jmap-event-source--finish fetch result))))
          :accept "text/event-stream"
          :timeout timeout
          :byte-cap chidu-jmap-event-source-byte-cap))
      (error
       (chidu-jmap-event-source--finish
        fetch
        (chidu-result-failure-create
         :kind 'event-source-startup-failed
         :data (list :message (error-message-string error-data))
         :retryable-p t))))
    (unless (chidu-jmap-event-source-fetch-completed-p fetch)
      (apply-partially #'chidu-jmap-event-source--cancel fetch))))

(provide 'chidu-jmap-event-source)

;;; chidu-jmap-event-source.el ends here
