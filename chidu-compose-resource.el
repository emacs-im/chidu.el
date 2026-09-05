;;; chidu-compose-resource.el --- Stable local Compose resources -*- lexical-binding: t; -*-

;;; Commentary:

;; Freeze one user-selected file into Chidu's private content-addressed tree.
;; Durable Compose state retains only typed metadata and the SHA-256 digest; it
;; never retains the user's original pathname.  Publication resolves and
;; verifies the managed file again before handing it to the bounded HTTP layer.

;;; Code:

(require 'cl-lib)
(require 'mailcap)
(require 'subr-x)
(require 'chidu-store)

(defcustom chidu-compose-resource-max-bytes (* 64 1024 1024)
  "Maximum size of one local Compose resource in bytes."
  :type 'positive-integer
  :group 'chidu)

(defun chidu-compose-resource--private-directory-p (path)
  "Return non-nil when PATH is current-uid mode 0700 directory data."
  (let ((attributes (file-attributes path 'integer)))
    (and attributes
         (file-directory-p path)
         (not (file-symlink-p path))
         (= (file-attribute-user-id attributes) (user-uid))
         (= (logand (file-modes path) #o777) #o700))))

(defun chidu-compose-resource--private-directory (directory)
  "Create DIRECTORY and require owner-only directory data."
  (setq directory (expand-file-name directory))
  (cond
   ((file-symlink-p directory)
    (signal 'chidu-invariant-error
            (list "Compose resource directory must not be a symbolic link"
                  directory)))
   ((file-exists-p directory)
    (unless (chidu-compose-resource--private-directory-p directory)
      (signal 'chidu-invariant-error
              (list "Compose resource directory must be current-uid mode 0700"
                    directory))))
   (t
    (make-directory directory t)
    (set-file-modes directory #o700)
    (unless (chidu-compose-resource--private-directory-p directory)
      (signal 'chidu-invariant-error
              (list "Failed to create private Compose resource directory"
                    directory)))))
  directory)

(defun chidu-compose-resource--private-file-p (path)
  "Return non-nil when PATH is current-uid mode 0600 regular data."
  (let ((attributes (file-attributes path 'integer)))
    (and attributes
         (file-regular-p path)
         (not (file-symlink-p path))
         (= (file-attribute-user-id attributes) (user-uid))
         (= (logand (file-modes path) #o777) #o600))))

(defun chidu-compose-resource-root (data-root)
  "Return the private content-addressed resource root below DATA-ROOT."
  (unless (and (stringp data-root) (not (string-empty-p data-root)))
    (signal 'wrong-type-argument (list 'nonempty-string-p data-root)))
  (let* ((compose-root
          (chidu-compose-resource--private-directory
           (expand-file-name "compose-resources/" data-root)))
         (digest-root
          (chidu-compose-resource--private-directory
           (expand-file-name "sha256/" compose-root))))
    digest-root))

(defun chidu-compose-resource--digest-p (value)
  "Return non-nil when VALUE is a lowercase SHA-256 digest."
  (and (stringp value)
       (= 64 (length value))
       (string-match-p "\\`[0-9a-f]+\\'" value)))

(defun chidu-compose-resource-path (data-root digest)
  "Return DATA-ROOT's managed content path for SHA-256 DIGEST."
  (unless (chidu-compose-resource--digest-p digest)
    (signal 'chidu-invariant-error
            (list "Compose resource digest is invalid" digest)))
  (expand-file-name
   (format "%s/%s/%s"
           (substring digest 0 2)
           (substring digest 2 4)
           digest)
   (chidu-compose-resource-root data-root)))

(defun chidu-compose-resource-download-target (data-root)
  "Allocate a non-existent private download target below DATA-ROOT."
  (let* ((root (chidu-compose-resource-root data-root))
         (directory
          (make-temp-file (expand-file-name ".download-" root) t)))
    (set-file-modes directory #o700)
    (unless (chidu-compose-resource--private-directory-p directory)
      (signal 'chidu-invariant-error
              (list "Compose download directory is not private" directory)))
    (expand-file-name "payload" directory)))

(defun chidu-compose-resource--download-directory (data-root target)
  "Return validated private staging directory for DATA-ROOT TARGET."
  (let* ((root (file-name-as-directory
                (file-truename (chidu-compose-resource-root data-root))))
         (target (expand-file-name target))
         (directory (file-name-directory target))
         (parent (file-name-directory (directory-file-name directory)))
         (name (file-name-nondirectory (directory-file-name directory))))
    (unless
        (and (equal "payload" (file-name-nondirectory target))
             (string-prefix-p ".download-" name)
             (file-directory-p directory)
             (chidu-compose-resource--private-directory-p directory)
             (file-equal-p parent root))
      (signal 'chidu-invariant-error
              (list "Compose download target escaped its private staging root"
                    target)))
    directory))

(defun chidu-compose-resource-discard-download-target (data-root target)
  "Delete private staging TARGET and its empty directory below DATA-ROOT."
  (let ((directory
         (chidu-compose-resource--download-directory data-root target)))
    (when (file-exists-p target)
      (delete-file target))
    (when (file-directory-p directory)
      (delete-directory directory))))

(defun chidu-compose-resource--name (file)
  "Return a safe attachment name derived from FILE."
  (let ((name (file-name-nondirectory (directory-file-name file))))
    (when (or (string-empty-p name)
              (string-match-p "[\0\r\n]" name))
      (user-error "Attachment filename is invalid"))
    name))

(defun chidu-compose-resource--media-type (name)
  "Return a normalized parameter-free media type inferred from NAME."
  (let ((candidate
         (downcase
          (or (and (fboundp 'mailcap-file-name-to-mime-type)
                   (mailcap-file-name-to-mime-type name))
              (and-let* ((extension (file-name-extension name)))
                (mailcap-extension-to-mime extension))
              "application/octet-stream"))))
    (if (string-match-p
         "\\`[^[:space:]/;]+/[^[:space:]/;]+\\'" candidate)
        candidate
      "application/octet-stream")))

(defun chidu-compose-resource-byte-limit (server-limit)
  "Return the effective local bound constrained by SERVER-LIMIT."
  (unless (and (integerp chidu-compose-resource-max-bytes)
               (> chidu-compose-resource-max-bytes 0))
    (signal 'chidu-invariant-error
            '("Compose resource local size limit must be positive")))
  (when (and server-limit
             (not (and (integerp server-limit) (> server-limit 0))))
    (signal 'chidu-invariant-error
            (list "JMAP maxSizeUpload is invalid" server-limit)))
  (if server-limit
      (min chidu-compose-resource-max-bytes server-limit)
    chidu-compose-resource-max-bytes))

(defun chidu-compose-resource--file-size (file)
  "Return regular FILE size, or nil."
  (when-let* ((attributes
               (and (file-regular-p file)
                    (file-attributes file 'integer))))
    (file-attribute-size attributes)))

(defun chidu-compose-resource--hash-file (file)
  "Return lowercase SHA-256 digest of exact FILE bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun chidu-compose-resource--verify-file (file size digest)
  "Require FILE to contain exact SIZE bytes identified by DIGEST."
  (unless (chidu-compose-resource--private-file-p file)
    (signal 'chidu-invariant-error
            (list "Compose resource must be current-uid mode 0600" file)))
  (unless (= size (or (chidu-compose-resource--file-size file) -1))
    (signal 'chidu-invariant-error
            (list "Compose resource size does not match durable evidence"
                  file)))
  (unless (equal digest (chidu-compose-resource--hash-file file))
    (signal 'chidu-invariant-error
            (list "Compose resource digest does not match durable evidence"
                  file)))
  file)

(defun chidu-compose-resource--install-file
    (data-root temporary size digest)
  "Install TEMPORARY below DATA-ROOT at SIZE and DIGEST, returning its path."
  (let* ((target (chidu-compose-resource-path data-root digest))
         (directory
          (chidu-compose-resource--private-directory
           (file-name-directory target))))
    (ignore directory)
    (cond
     ((file-exists-p target)
      (chidu-compose-resource--verify-file target size digest)
      (delete-file temporary))
     (t
      (condition-case error-data
          (rename-file temporary target nil)
        (file-already-exists
         (chidu-compose-resource--verify-file target size digest)
         (delete-file temporary))
        (error (signal (car error-data) (cdr error-data))))
      (set-file-modes target #o600)
      (chidu-compose-resource--verify-file target size digest)))
    target))

(defun chidu-compose-resource-install-download
    (data-root temporary expected-size)
  "Install downloaded TEMPORARY bytes below DATA-ROOT.

EXPECTED-SIZE is immutable JMAP attachment evidence.  Return the lowercase
SHA-256 digest after exact size verification and atomic content-addressed
installation."
  (unless (and (integerp expected-size) (>= expected-size 0))
    (signal 'wrong-type-argument
            (list 'nonnegative-integer-p expected-size)))
  (let ((limit (chidu-compose-resource-byte-limit nil)))
    (when (> expected-size limit)
      (signal 'chidu-overloaded
              (list "Compose resource exceeds the local byte limit"
                    :actual-bytes expected-size :byte-cap limit))))
  (chidu-compose-resource--download-directory data-root temporary)
  (unless (and (file-regular-p temporary)
               (not (file-symlink-p temporary)))
    (signal 'chidu-invariant-error
            (list "Compose resource download is not regular file data"
                  temporary)))
  (let ((actual-size (chidu-compose-resource--file-size temporary)))
    (unless (and (integerp actual-size) (= actual-size expected-size))
      (signal 'chidu-invariant-error
              (list "Compose resource download size mismatch"
                    :expected-bytes expected-size
                    :actual-bytes actual-size))))
  (set-file-modes temporary #o600)
  (let ((digest (chidu-compose-resource--hash-file temporary)))
    (chidu-compose-resource--install-file
     data-root temporary expected-size digest)
    digest))

(defun chidu-compose-resource-import (data-root file &optional server-limit)
  "Freeze FILE below DATA-ROOT and return a resource observation.

SERVER-LIMIT, when non-nil, further bounds the accepted byte size.  The source
pathname is never retained."
  (setq file (expand-file-name file))
  (unless (and (file-exists-p file)
               (file-regular-p file)
               (not (file-symlink-p file)))
    (user-error "Attachment is not regular file data: %s" file))
  (let* ((limit (chidu-compose-resource-byte-limit server-limit))
         (source-size (chidu-compose-resource--file-size file)))
    (unless (and (integerp source-size) (>= source-size 0))
      (user-error "Attachment size is unavailable: %s" file))
    (when (> source-size limit)
      (user-error "Attachment exceeds the %s byte limit" limit))
    (let* ((root (chidu-compose-resource-root data-root))
           (temporary
            (make-temp-file (expand-file-name ".import-" root)))
           size digest)
      (unwind-protect
          (progn
            (copy-file file temporary t nil nil nil)
            (set-file-modes temporary #o600)
            (setq size (or (chidu-compose-resource--file-size temporary)
                           (error "Attachment snapshot disappeared")))
            (when (> size limit)
              (user-error "Attachment exceeds the %s byte limit" limit))
            (setq digest (chidu-compose-resource--hash-file temporary))
            (chidu-compose-resource--install-file
             data-root temporary size digest)
            (setq temporary nil)
            (chidu-store-compose-resource-observation-create
             :resource-id (chidu-store-new-local-id)
             :name (chidu-compose-resource--name file)
             :media-type
             (chidu-compose-resource--media-type
              (chidu-compose-resource--name file))
             :size size
             :digest digest
             :remote-blob-id nil
             :charset nil
             :disposition "attachment"
             :cid nil
             :language (vector)
             :location nil))
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary))))))

(defun chidu-compose-resource-local-file (data-root resource)
  "Return RESOURCE's verified managed file below DATA-ROOT."
  (unless (chidu-store-compose-resource-p resource)
    (signal 'wrong-type-argument
            (list 'chidu-store-compose-resource-p resource)))
  (let ((digest (chidu-store-compose-resource-digest resource)))
    (unless digest
      (signal 'chidu-invariant-error
              (list "Compose resource has no local byte evidence"
                    (chidu-store-compose-resource-resource-id resource))))
    (chidu-compose-resource--verify-file
     (chidu-compose-resource-path data-root digest)
     (chidu-store-compose-resource-size resource)
     digest)))

(provide 'chidu-compose-resource)

;;; chidu-compose-resource.el ends here
