;;; chidu-jmap-http.el --- Bounded HTTP policy for JMAP -*- lexical-binding: t; -*-

;;; Commentary:

;; Thin policy wrapper around `plz'.  plz owns the mechanical curl process;
;; Chidu fixes HTTPS-only redirect policy, byte caps, credentials, cancellation
;; semantics, and typed results.

;;; Code:

(require 'cl-lib)
(require 'json)
;; plz 0.9.1's debug macro references byte-compile state during eager
;; macro-expansion on Emacs 32; package managers normally load this already.
(require 'bytecomp)
(require 'plz)
(require 'subr-x)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-store)

(defconst chidu-jmap-session-byte-cap (* 2 1024 1024)
  "Maximum JMAP Session response body size in bytes.")

(defconst chidu-jmap-api-byte-cap (* 8 1024 1024)
  "Maximum ordinary JMAP API response body size in bytes.")

(defconst chidu-jmap-upload-response-byte-cap (* 1024 1024)
  "Maximum JMAP upload response body size in bytes.")

(defconst chidu-jmap-max-redirects 5
  "Maximum HTTPS redirects allowed for one JMAP HTTP request.")

(defconst chidu-jmap-connect-timeout 10
  "Maximum seconds allowed to establish a JMAP HTTP connection.")

(defconst chidu-jmap-request-timeout 30
  "Maximum seconds allowed for one bounded JMAP HTTP request.")

(defconst chidu-jmap-http-minimum-curl-version "8.4.0"
  "Minimum curl version with a streaming `--max-filesize' hard bound.")

(defvar chidu-jmap-http--curl-version-cache nil
  "Cached (PROGRAM . VERSION) for the configured curl executable.")

(defun chidu-jmap-http--curl-version ()
  "Return the configured curl version string, or signal."
  (let ((program plz-curl-program))
    (if (and chidu-jmap-http--curl-version-cache
             (equal program (car chidu-jmap-http--curl-version-cache)))
        (cdr chidu-jmap-http--curl-version-cache)
      (let (version)
        (condition-case error-data
            (with-temp-buffer
              (unless (zerop (call-process program nil t nil "--version"))
                (signal 'chidu-jmap-error
                        (list "curl --version failed" program)))
              (goto-char (point-min))
              (unless
                  (looking-at
                   "curl \\([0-9]+\\(?:\\.[0-9]+\\)\\{1,2\\}\\)")
                (signal 'chidu-jmap-error
                        (list "curl returned an invalid version banner"
                              program)))
              (setq version (match-string-no-properties 1)))
          (file-error
           (signal 'chidu-jmap-error
                   (list "curl executable is unavailable"
                         program
                         (error-message-string error-data)))))
        (setq chidu-jmap-http--curl-version-cache (cons program version))
        version))))

(defun chidu-jmap-http--assert-curl-version ()
  "Require curl with a reliable streaming file-size bound."
  (let ((actual (chidu-jmap-http--curl-version)))
    (when (version< actual chidu-jmap-http-minimum-curl-version)
      (signal
       'chidu-jmap-error
       (list
        (format "curl %s or newer is required; found %s"
                chidu-jmap-http-minimum-curl-version actual))))))

(defun chidu-jmap-http--safe-config-header-value-p (value)
  "Return non-nil when VALUE cannot escape plz curl-config quoting."
  (and
   (stringp value)
   (> (length value) 0)
   (cl-loop for character across value
            always
            (and (<= 32 character 126)
                 (not (memq character '(?\" ?\\)))))))

(defun chidu-jmap-http--validate-headers (headers)
  "Return HEADERS after validating their curl-config boundary."
  (dolist (header headers)
    (unless (and (consp header)
                 (stringp (car header))
                 (string-match-p "\\`[A-Za-z0-9-]+\\'" (car header))
                 (chidu-jmap-http--safe-config-header-value-p (cdr header)))
      (signal 'chidu-jmap-error
              (list "HTTP header is unsafe for curl config" (car-safe header)))))
  headers)

(chidu-define-record chidu-jmap-http-response
    "Bounded HTTP response returned by the JMAP HTTP policy layer."
  status
  body
  content-type)

(defun chidu-jmap-http--safe-config-url-p (url)
  "Return non-nil when URL cannot escape plz's curl config quoting."
  (and (stringp url)
       (not (string-search "\"" url))
       (not (string-search "\\" url))
       (not (string-search "\r" url))
       (not (string-search "\n" url))
       (not (string-search "\t" url))))

(defun chidu-jmap-http--authorization (login authentication secret)
  "Return mutable Authorization value for LOGIN, AUTHENTICATION, and SECRET."
  (pcase authentication
    ('basic
     (let* ((plain
             (encode-coding-string (concat login ":" secret) 'utf-8-unix t))
            (encoded (base64-encode-string plain t))
            (authorization (concat "Basic " encoded)))
       (clear-string plain)
       (clear-string encoded)
       authorization))
    ('bearer
     ;; RFC 6750 b64token.  Validate without regexp escaping so a credential
     ;; can never become curl-config syntax through an accidental parser gap.
     (let* ((padding (string-search "=" secret))
            (payload-end (or padding (length secret))))
       (unless
           (and (> payload-end 0)
                (cl-loop for character across (substring secret 0 payload-end)
                         always
                         (or (and (<= ?A character) (<= character ?Z))
                             (and (<= ?a character) (<= character ?z))
                             (and (<= ?0 character) (<= character ?9))
                             (memq character '(?- ?. ?_ ?~ ?+ ?/))))
                (or (null padding)
                    (cl-loop for character across (substring secret padding)
                             always (= character ?=))))
         (signal 'chidu-jmap-error
                 '("Bearer credential is not a valid b64token"))))
     (concat "Bearer " secret))
    (_
     (signal 'chidu-jmap-error
             (list "Unsupported JMAP authentication kind" authentication)))))

(defun chidu-jmap-http--response (response byte-cap)
  "Convert plz RESPONSE to a bounded JMAP response using BYTE-CAP."
  (let* ((body
          (encode-coding-string (or (plz-response-body response) "") 'binary t))
         (status (plz-response-status response)))
    (when (> (string-bytes body) byte-cap)
      (signal 'chidu-jmap-error
              (list (format "HTTP body exceeds %d bytes" byte-cap))))
    (chidu-jmap-http-response-create
     :status status
     :body body
     :content-type (alist-get 'content-type (plz-response-headers response)))))

(defun chidu-jmap-http--http-result (response byte-cap)
  "Return typed Chidu result for plz RESPONSE using BYTE-CAP."
  (if (> (string-bytes (or (plz-response-body response) "")) byte-cap)
      (chidu-result-failure-create
       :kind 'response-too-large :data (list :byte-cap byte-cap) :retryable-p nil)
    (condition-case error-data
        (let* ((http (chidu-jmap-http--response response byte-cap))
               (status (chidu-jmap-http-response-status http)))
          (cond
           ((memq status '(401 403))
            (chidu-result-failure-create
             :kind 'authentication-rejected
             :data (list :status status)
             :retryable-p nil))
           ((>= status 400)
            (chidu-result-failure-create
             :kind 'http-error
             :data (list :status status)
             :retryable-p (>= status 500)))
           (t (chidu-result-ok-create :value http))))
      (error
       (chidu-result-failure-create
        :kind 'invalid-http-response
        :data (list :message (error-message-string error-data))
        :retryable-p nil)))))

(defun chidu-jmap-http--error-result (process error-data byte-cap)
  "Map plz ERROR-DATA for PROCESS to a typed result using BYTE-CAP."
  (ignore process)
  (cond
   ((plz-error-response error-data)
    (chidu-jmap-http--http-result (plz-error-response error-data) byte-cap))
   ((equal 63 (car-safe (plz-error-curl-error error-data)))
    (chidu-result-failure-create
     :kind 'response-too-large :data (list :byte-cap byte-cap) :retryable-p nil))
   (t
    (chidu-result-failure-create
     :kind 'network-error
     :data (list :curl-error (plz-error-curl-error error-data)
                 :message (plz-error-message error-data))
     :retryable-p t))))

(defun chidu-jmap-http-cancel (process)
  "Cancel asynchronous plz PROCESS if it is still live."
  (when (and (processp process) (process-live-p process))
    (delete-process process)
    t))

(defun chidu-jmap-http-encode-body (body)
  "Encode JMAP JSON BODY as unibyte UTF-8."
  (encode-coding-string
   (json-serialize body :null-object :json-null :false-object :json-false)
   'utf-8-unix t))

(cl-defun chidu-jmap-http-request
    (url login authentication secret callback
         &key body (byte-cap chidu-jmap-session-byte-cap)
         max-request-bytes
         (accept "application/json")
         (timeout chidu-jmap-request-timeout))
  "Start one bounded asynchronous JMAP HTTP request.

URL must be HTTPS.  LOGIN, AUTHENTICATION, and SECRET supply credentials.
CALLBACK receives one typed result.  BODY is an optional JMAP JSON object.
BYTE-CAP bounds the response body.  MAX-REQUEST-BYTES, when non-nil, bounds the
encoded request body using the Session's maxSizeRequest.  ACCEPT controls the
Accept header and TIMEOUT is the total request timeout in seconds.  Return the
plz curl process, or nil when startup fails and CALLBACK is invoked
synchronously."
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (chidu-jmap-http--assert-curl-version)
  (chidu-jmap--parse-url url "request URL")
  (unless (chidu-jmap-http--safe-config-url-p url)
    (signal 'chidu-jmap-error '("request URL contains unsafe curl-config syntax")))
  (chidu-store-validate-login login)
  (chidu-store-validate-authentication authentication)
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (and (integerp byte-cap) (> byte-cap 0))
    (signal 'wrong-type-argument (list 'positive-integer-p byte-cap)))
  (unless (or (null max-request-bytes)
              (and (integerp max-request-bytes) (> max-request-bytes 0)))
    (signal 'wrong-type-argument
            (list 'positive-integer-or-nil-p max-request-bytes)))
  (unless (chidu-jmap-http--safe-config-header-value-p accept)
    (signal 'wrong-type-argument (list 'safe-http-header-p accept)))
  (unless (and (numberp timeout) (> timeout 0))
    (signal 'wrong-type-argument (list 'positive-number-p timeout)))
  (let* ((authorization
          (chidu-jmap-http--authorization login authentication secret))
         (headers
          (chidu-jmap-http--validate-headers
           (append
            (list (cons "Accept" accept)
                  (cons "Accept-Encoding" "identity")
                  (cons "Authorization" authorization))
            (when body (list (cons "Content-Type" "application/json"))))))
         (request-body
          (when body (chidu-jmap-http-encode-body body)))
         process)
    (unwind-protect
        (if (and request-body max-request-bytes
                 (> (string-bytes request-body) max-request-bytes))
            (progn
              (funcall
               callback
               (chidu-result-failure-create
                :kind 'request-too-large
                :data
                (list :actual-bytes (string-bytes request-body)
                      :max-size-request max-request-bytes)
                :retryable-p nil))
              nil)
          (condition-case error-data
              (let ((plz-curl-default-args
                     (append
                      (list "--disable" "--silent"
                            "--location"
                            "--proto" "=https"
                            "--proto-redir" "=https"
                            "--max-redirs"
                            (number-to-string chidu-jmap-max-redirects)
                            "--max-filesize" (number-to-string byte-cap))
                      (when body
                        (list "--post301" "--post302" "--post303")))))
                (setq process
                      (plz (if body 'post 'get) url
                        :headers headers
                        :body request-body
                        :body-type 'binary
                        :as 'response
                        :decode nil
                        :connect-timeout chidu-jmap-connect-timeout
                        :timeout timeout
                        :noquery t
                        :then
                        (lambda (response)
                          (funcall
                           callback
                           (chidu-jmap-http--http-result response byte-cap)))
                        :else
                        (lambda (plz-error)
                          (funcall
                           callback
                           (chidu-jmap-http--error-result
                            process plz-error byte-cap)))))
                ;; plz keeps its original call arguments on the process for
                ;; debugging.  JMAP arguments contain credentials and message
                ;; data, so Chidu intentionally removes that diagnostic copy.
                (process-put process :plz-args nil)
                process)
            (error
             (funcall
              callback
              (chidu-result-failure-create
               :kind 'transport-unavailable
               :data (list :message (error-message-string error-data))
               :retryable-p t))
             nil)))
      ;; plz has synchronously written its curl config/body to stdin before
      ;; returning.  Remove caller-visible copies that contain credentials or
      ;; request JSON; plz's private config string then becomes ordinary GC data.
      (clear-string authorization)
      (when (stringp request-body) (clear-string request-body)))))

(cl-defun chidu-jmap-http-upload-file
    (url login authentication secret file media-type callback
         &key max-upload-bytes
         (byte-cap chidu-jmap-upload-response-byte-cap)
         (timeout chidu-jmap-request-timeout))
  "Upload FILE to JMAP URL as MEDIA-TYPE and call CALLBACK.

LOGIN, AUTHENTICATION, and SECRET supply credentials.  MAX-UPLOAD-BYTES, when
non-nil, bounds the exact regular file.  BYTE-CAP bounds the JSON response.
Return the plz curl process, or nil after synchronous startup settlement."
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (chidu-jmap-http--assert-curl-version)
  (chidu-jmap--parse-url url "upload URL")
  (unless (chidu-jmap-http--safe-config-url-p url)
    (signal 'chidu-jmap-error
            '("upload URL contains unsafe curl-config syntax")))
  (chidu-store-validate-login login)
  (chidu-store-validate-authentication authentication)
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (setq media-type
        (chidu-jmap--media-type media-type "upload media type"))
  (unless (and (file-regular-p file) (not (file-symlink-p file)))
    (signal 'chidu-invariant-error
            (list "JMAP upload source is not regular file data" file)))
  (let* ((attributes (file-attributes file 'integer))
         (size (and attributes (file-attribute-size attributes))))
    (unless (and (integerp size) (>= size 0))
      (signal 'chidu-invariant-error
              (list "JMAP upload source has no stable size" file)))
    (when (and max-upload-bytes (> size max-upload-bytes))
      (funcall
       callback
       (chidu-result-failure-create
        :kind 'request-too-large
        :data (list :actual-bytes size
                    :max-size-upload max-upload-bytes)
        :retryable-p nil))
      (cl-return-from chidu-jmap-http-upload-file nil)))
  (let* ((authorization
          (chidu-jmap-http--authorization login authentication secret))
         (headers
          (chidu-jmap-http--validate-headers
           (list (cons "Accept" "application/json")
                 (cons "Accept-Encoding" "identity")
                 (cons "Authorization" authorization)
                 (cons "Content-Type" media-type))))
         process)
    (unwind-protect
        (condition-case error-data
            (let ((plz-curl-default-args
                   (list "--disable" "--silent"
                         "--location"
                         "--proto" "=https"
                         "--proto-redir" "=https"
                         "--max-redirs"
                         (number-to-string chidu-jmap-max-redirects)
                         "--max-filesize" (number-to-string byte-cap)
                         "--post301" "--post302" "--post303")))
              (setq process
                    (plz 'post url
                      :headers headers
                      :body (list 'file file)
                      :body-type 'binary
                      :as 'response
                      :decode nil
                      :connect-timeout chidu-jmap-connect-timeout
                      :timeout timeout
                      :noquery t
                      :then
                      (lambda (response)
                        (funcall
                         callback
                         (chidu-jmap-http--http-result response byte-cap)))
                      :else
                      (lambda (plz-error)
                        (funcall
                         callback
                         (chidu-jmap-http--error-result
                          process plz-error byte-cap)))))
              (process-put process :plz-args nil)
              process)
          (error
           (funcall
            callback
            (chidu-result-failure-create
             :kind 'transport-unavailable
             :data (list :message (error-message-string error-data))
             :retryable-p t))
           nil))
      (clear-string authorization))))

(cl-defun chidu-jmap-http-download-file
    (url login authentication secret file callback
         &key max-download-bytes
         (accept "application/octet-stream")
         (timeout chidu-jmap-request-timeout))
  "Download URL into non-existent FILE and call CALLBACK with a typed result.

LOGIN, AUTHENTICATION, and SECRET supply credentials.  MAX-DOWNLOAD-BYTES is a
required positive hard bound for the file.  ACCEPT controls the requested media
type.  The response is streamed by curl directly to FILE and is never buffered
as a Lisp string.  Return the plz curl process, or nil after synchronous startup
settlement."
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (chidu-jmap-http--assert-curl-version)
  (chidu-jmap--parse-url url "download URL")
  (unless (chidu-jmap-http--safe-config-url-p url)
    (signal 'chidu-jmap-error
            '("download URL contains unsafe curl-config syntax")))
  (chidu-store-validate-login login)
  (chidu-store-validate-authentication authentication)
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (and (integerp max-download-bytes) (> max-download-bytes 0))
    (signal 'wrong-type-argument
            (list 'positive-integer-p max-download-bytes)))
  (unless (chidu-jmap-http--safe-config-header-value-p accept)
    (signal 'wrong-type-argument (list 'safe-http-header-p accept)))
  (unless (and (numberp timeout) (> timeout 0))
    (signal 'wrong-type-argument (list 'positive-number-p timeout)))
  (setq file (expand-file-name file))
  (when (file-exists-p file)
    (signal 'chidu-invariant-error
            (list "JMAP download target already exists" file)))
  (unless (file-directory-p (file-name-directory file))
    (signal 'chidu-invariant-error
            (list "JMAP download target directory is unavailable" file)))
  (let* ((authorization
          (chidu-jmap-http--authorization login authentication secret))
         (headers
          (chidu-jmap-http--validate-headers
           (list (cons "Accept" accept)
                 (cons "Accept-Encoding" "identity")
                 (cons "Authorization" authorization))))
         process)
    (unwind-protect
        (condition-case error-data
            (let ((plz-curl-default-args
                   (list "--disable" "--silent"
                         "--location"
                         "--proto" "=https"
                         "--proto-redir" "=https"
                         "--max-redirs"
                         (number-to-string chidu-jmap-max-redirects)
                         "--max-filesize"
                         (number-to-string max-download-bytes))))
              (setq process
                    (plz 'get url
                      :headers headers
                      :as (list 'file file)
                      :decode nil
                      :connect-timeout chidu-jmap-connect-timeout
                      :timeout timeout
                      :noquery t
                      :then
                      (lambda (downloaded)
                        (let* ((attributes
                                (and (file-regular-p downloaded)
                                     (file-attributes downloaded 'integer)))
                               (size
                                (and attributes
                                     (file-attribute-size attributes))))
                          (cond
                           ((not (and (integerp size) (>= size 0)))
                            (when (file-exists-p downloaded)
                              (ignore-errors (delete-file downloaded)))
                            (funcall
                             callback
                             (chidu-result-failure-create
                              :kind 'invalid-http-response
                              :data '(file-size-unavailable)
                              :retryable-p nil)))
                           ((> size max-download-bytes)
                            (ignore-errors (delete-file downloaded))
                            (funcall
                             callback
                             (chidu-result-failure-create
                              :kind 'response-too-large
                              :data (list :actual-bytes size
                                          :byte-cap max-download-bytes)
                              :retryable-p nil)))
                           (t
                            (funcall
                             callback
                             (chidu-result-ok-create :value downloaded))))))
                      :else
                      (lambda (plz-error)
                        (when (file-exists-p file)
                          (ignore-errors (delete-file file)))
                        (funcall
                         callback
                         (chidu-jmap-http--error-result
                          process plz-error max-download-bytes)))))
              (process-put process :plz-args nil)
              process)
          (error
           (when (file-exists-p file)
             (ignore-errors (delete-file file)))
           (funcall
            callback
            (chidu-result-failure-create
             :kind 'transport-unavailable
             :data (list :message (error-message-string error-data))
             :retryable-p t))
           nil))
      (clear-string authorization))))

(provide 'chidu-jmap-http)

;;; chidu-jmap-http.el ends here
