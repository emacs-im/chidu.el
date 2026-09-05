;;; chidu-jmap-download-test.el --- Blob materialization tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'cl-lib)
(require 'ert)
(require 'chidu-compose-resource)
(require 'chidu-jmap-download)
(require 'chidu-jmap-http)
(require 'chidu-result)
(require 'chidu-store)

(defun chidu-jmap-download-test--root ()
  "Return one private temporary data root."
  (let ((root (make-temp-file "chidu-jmap-download-test-" t)))
    (set-file-modes root #o700)
    root))

(defun chidu-jmap-download-test--endpoint (&optional template)
  "Return one download-capable Endpoint using TEMPLATE."
  (chidu-store-endpoint-create
   :endpoint-id "endpoint"
   :session-url "https://mail.example.test/.well-known/jmap"
   :login "me@example.test"
   :authentication 'basic
   :download-url
   (or template
       (concat
        "https://download.example.test/{accountId}/{blobId}/{name}"
        "?type={type}"))))

(defun chidu-jmap-download-test--account ()
  "Return one available JMAP Account."
  (chidu-store-account-create
   :account-id "account"
   :remote-account-id "remote-account"
   :name "Mail"
   :available-p t))

(defun chidu-jmap-download-test--resource (&optional size name)
  "Return one remote-only Compose resource of SIZE and NAME."
  (chidu-store-compose-resource-observation-create
   :resource-id (chidu-store-new-local-id)
   :name (or name "note.txt")
   :media-type "text/plain"
   :size (or size 4)
   :digest nil
   :remote-blob-id "blob-note"
   :charset "utf-8"
   :disposition "attachment"
   :cid nil
   :language ["en"]
   :location nil))

(defun chidu-jmap-download-test--write-bytes (file bytes)
  "Write exact unibyte BYTES to FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'binary))
      (write-region (point-min) (point-max) file nil 'silent))))

(ert-deftest chidu-jmap-download-expands-level-one-session-template ()
  "downloadUrl accepts one variable per level-1 expression."
  (let ((endpoint
         (chidu-jmap-download-test--endpoint
          (concat
           "https://download.example.test/{accountId}/{blobId}/{name}"
           "?type={type}"))))
    (should
     (equal
      (concat
       "https://download.example.test/remote-account/blob-note/"
       "note%20%2F%202026.txt?type=text%2Fplain")
      (chidu-jmap-download-url
       endpoint "remote-account" "blob-note" "text/plain"
       "note / 2026.txt")))
    (should-error
     (chidu-jmap-download-url
      (chidu-jmap-download-test--endpoint
       (concat
        "https://download.example.test/{accountId,blobId}/{name}"
        "?type={type}"))
      "remote-account" "blob-note" "text/plain" "note.txt")
     :type 'chidu-jmap-error)
    (should-error
     (chidu-jmap-download-url
      (chidu-jmap-download-test--endpoint
       "https://download.example.test/{accountId}/{blobId}/{name}")
      "remote-account" "blob-note" "text/plain" "note.txt")
     :type 'chidu-jmap-error)
    (should-error
     (chidu-jmap-download-url
      (chidu-jmap-download-test--endpoint
       (concat
        "https://download.example.test/{accountId}/{blobId}/{+name}"
        "?type={type}"))
      "remote-account" "blob-note" "text/plain" "note.txt")
     :type 'chidu-jmap-error)))

(ert-deftest chidu-jmap-http-requires-stream-bounded-curl ()
  (let ((chidu-jmap-http--curl-version-cache
         (cons plz-curl-program "8.3.0")))
    (should-error
     (chidu-jmap-http--assert-curl-version)
     :type 'chidu-jmap-error))
  (let ((chidu-jmap-http--curl-version-cache
         (cons plz-curl-program "8.4.0")))
    (should-not (chidu-jmap-http--assert-curl-version))))

(ert-deftest chidu-jmap-http-rejects-curl-config-header-escapes ()
  (should
   (chidu-jmap-http--safe-config-header-value-p "application/json"))
  (dolist (value
           (list "text/plain\nX-Injected: yes"
                 "text/plain\rX-Injected: yes"
                 "text/plain\tX-Injected: yes"
                 (concat "text/plain" (string 0))
                 "text/pl\\ain"
                 "text/pl\"ain"))
    (should-not (chidu-jmap-http--safe-config-header-value-p value))
    (should-error
     (chidu-jmap-http--validate-headers
      (list (cons "Accept" value)))
     :type 'chidu-jmap-error))
  (should (equal "text/plain"
                 (chidu-jmap--media-type "Text/Plain" "test media type")))
  (dolist (value '("text/pl:ain" "text/pl\\ain" "text/pl\"ain"
                   "text/(plain)" "text/plain; charset=utf-8"))
    (should-error
     (chidu-jmap--media-type value "test media type")
     :type 'chidu-jmap-error)))

(ert-deftest chidu-jmap-http-download-streams-to-a-bounded-file ()
  "The HTTP policy must make curl own a bounded non-decoded output file."
  (let* ((root (chidu-jmap-download-test--root))
         (target (expand-file-name "payload" root))
         captured-args
         captured-curl-args
         process)
    (unwind-protect
        (cl-letf
            (((symbol-function 'plz)
              (lambda (&rest arguments)
                (setq captured-args arguments
                      captured-curl-args
                      (copy-sequence plz-curl-default-args)
                      process
                      (make-pipe-process
                       :name "chidu-download-plz-test" :noquery t))
                (process-put process :plz-args arguments)
                process)))
          (let* ((secret (copy-sequence "password"))
                 (returned
                  (chidu-jmap-http-download-file
                   "https://download.example.test/blob"
                   "me@example.test" 'basic secret target #'ignore
                   :max-download-bytes 4
                   :accept "text/plain")))
            (should (eq process returned))
            (should (equal "password" secret)))
          (let* ((properties (nthcdr 2 captured-args))
                 (headers (plist-get properties :headers)))
            (should (equal `(file ,target) (plist-get properties :as)))
            (should-not (plist-get properties :decode))
            (should (equal "identity"
                           (cdr (assoc "Accept-Encoding" headers))))
            (should (equal "text/plain" (cdr (assoc "Accept" headers))))
            (should (cl-every
                     #'zerop (cdr (assoc "Authorization" headers)))))
          (should (member "--max-filesize" captured-curl-args))
          (should (member "--location" captured-curl-args))
          (should-not (member "--compressed" captured-curl-args))
          (should-not (member "--location-trusted" captured-curl-args))
          (should-not (process-get process :plz-args))
          (should-not (file-exists-p target)))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest chidu-jmap-download-materializes-exact-private-bytes ()
  "A remote resource gains a digest only after exact CAS installation."
  (let* ((root (chidu-jmap-download-test--root))
         (endpoint (chidu-jmap-download-test--endpoint))
         (account (chidu-jmap-download-test--account))
         (resource
          (chidu-jmap-download-test--resource 4 "note / 2026.txt"))
         result
         requested-url
         requested-limit)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chidu-jmap-http-download-file)
              (lambda (url login authentication secret file callback
                           &rest arguments)
                (setq requested-url url
                      requested-limit
                      (plist-get arguments :max-download-bytes))
                (should (equal "me@example.test" login))
                (should (eq 'basic authentication))
                (should (equal "password" secret))
                (should (equal "text/plain"
                               (plist-get arguments :accept)))
                (should-not (file-exists-p file))
                (chidu-jmap-download-test--write-bytes file "note")
                (funcall callback (chidu-result-ok-create :value file))
                nil)))
          (should-not
           (chidu-jmap-download-compose-resource
            endpoint account resource root (copy-sequence "password")
            (lambda (value) (setq result value))))
          (should (chidu-result-ok-p result))
          (let* ((materialized (chidu-result-ok-value result))
                 (digest (secure-hash 'sha256 "note"))
                 (file (chidu-compose-resource-path root digest)))
            (should (equal digest
                           (chidu-store-compose-resource-observation-digest
                            materialized)))
            (should
             (equal "blob-note"
                    (chidu-store-compose-resource-observation-remote-blob-id
                     materialized)))
            (should (equal resource
                           (chidu-store-compose-resource-observation-with
                            materialized :digest nil)))
            (should (file-regular-p file))
            (should (= #o600 (logand (file-modes file) #o777)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file)
              (should (equal "note" (buffer-string)))))
          (should (= 4 requested-limit))
          (should
           (equal
            (concat
             "https://download.example.test/remote-account/blob-note/"
             "note%20%2F%202026.txt?type=text%2Fplain")
            requested-url))
          (should-not
           (directory-files
            (chidu-compose-resource-root root) nil "\\`\\.download-")))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest chidu-jmap-download-rejects-size-mismatch-and-local-overflow ()
  "Mismatched and locally oversized Blobs never gain durable byte evidence."
  (let* ((root (chidu-jmap-download-test--root))
         (endpoint (chidu-jmap-download-test--endpoint))
         (account (chidu-jmap-download-test--account))
         mismatch
         overflow
         dispatched)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'chidu-jmap-http-download-file)
                (lambda (_url _login _authentication _secret file callback
                              &rest _arguments)
                  (chidu-jmap-download-test--write-bytes file "note")
                  (funcall callback (chidu-result-ok-create :value file))
                  nil)))
            (chidu-jmap-download-compose-resource
             endpoint account
             (chidu-jmap-download-test--resource 5)
             root (copy-sequence "password")
             (lambda (value) (setq mismatch value))))
          (should (chidu-result-failure-p mismatch))
          (should
           (eq 'compose-resource-materialization-failed
               (chidu-result-failure-kind mismatch)))
          (let ((chidu-compose-resource-max-bytes 3))
            (cl-letf
                (((symbol-function 'chidu-jmap-http-download-file)
                  (lambda (&rest _arguments)
                    (setq dispatched t)
                    (ert-fail "oversized resource reached HTTP"))))
              (chidu-jmap-download-compose-resource
               endpoint account
               (chidu-jmap-download-test--resource 4)
               root (copy-sequence "password")
               (lambda (value) (setq overflow value)))))
          (should-not dispatched)
          (should (chidu-result-failure-p overflow))
          (should
           (eq 'compose-resource-too-large
               (chidu-result-failure-kind overflow)))
          (should-not
           (directory-files
            (chidu-compose-resource-root root) nil "\\`\\.download-")))
      (when (file-directory-p root)
        (delete-directory root t)))))

(provide 'chidu-jmap-download-test)

;;; chidu-jmap-download-test.el ends here
