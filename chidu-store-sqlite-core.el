;;; chidu-store-sqlite-core.el --- SQLite lifecycle and shared primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Connection ownership, common codecs, change sequencing, stable Email
;; identity, and backend lifecycle for Chidu's sole Store implementation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'sqlite)
(require 'subr-x)
(require 'chidu-sql)
(require 'chidu-store)

(defconst chidu-store-sqlite--metadata-query
  (chidu-sql
   [:select [store-id change-seq]
    :from store-metadata
    :where [:= singleton 1]])
  "Read the sole Store metadata row.")

(defconst chidu-store-sqlite--change-seq-query
  (chidu-sql
   [:select [change-seq]
    :from store-metadata
    :where [:= singleton 1]])
  "Read the Store change sequence.")

(defconst chidu-store-sqlite--increment-change-seq-statement
  (chidu-sql
   [:update store-metadata
    :set [[change-seq [:+ change-seq 1]]]
    :where [:= singleton 1]])
  "Increment the Store change sequence.")

(cl-defstruct (chidu-store-sqlite-state
               (:constructor chidu-store-sqlite-state-create))
  "Mutable SQLite backend state hidden behind the Store capability."
  root
  owner-path
  owner-connection
  database-path
  connection
  store-id
  closed-p)

(defun chidu-store-sqlite--private-directory-p (path)
  "Return non-nil when PATH is current-uid mode 0700, real directory data."
  (let ((attributes (file-attributes path 'integer)))
    (and attributes
         (file-directory-p path)
         (not (file-symlink-p path))
         (= (file-attribute-user-id attributes) (user-uid))
         (= (logand (file-modes path) #o777) #o700))))

(defun chidu-store-sqlite--prepare-root (path)
  "Create or validate private Store root PATH and return its absolute name."
  (let ((root (expand-file-name path)))
    (cond
     ((file-exists-p root)
      (unless (chidu-store-sqlite--private-directory-p root)
        (signal 'chidu-invariant-error
                '("SQLite data root must be current-uid mode 0700"))))
     ((file-symlink-p root)
      (signal 'chidu-invariant-error
              '("SQLite data root must not be a symbolic link")))
     (t
      (make-directory root t)
      (set-file-modes root #o700)
      (unless (chidu-store-sqlite--private-directory-p root)
        (signal 'chidu-invariant-error
                '("failed to create private SQLite data root")))))
    root))

(defun chidu-store-sqlite--private-file-p (path)
  "Return non-nil when PATH is current-uid mode 0600 regular data."
  (let ((attributes (file-attributes path 'integer)))
    (and attributes
         (file-regular-p path)
         (not (file-symlink-p path))
         (= (file-attribute-user-id attributes) (user-uid))
         (= (logand (file-modes path) #o777) #o600))))

(defun chidu-store-sqlite--prepare-database (root)
  "Create or validate the SQLite database below ROOT and return its path."
  (let ((path (expand-file-name "store.sqlite3" root)))
    (cond
     ((file-symlink-p path)
      (signal 'chidu-invariant-error
              '("SQLite database must not be a symbolic link")))
     ((file-exists-p path)
      (unless (chidu-store-sqlite--private-file-p path)
        (signal 'chidu-invariant-error
                '("SQLite database must be current-uid mode 0600"))))
     (t
      (let ((coding-system-for-write 'binary)
            (make-backup-files nil))
        (with-file-modes #o600
          (write-region "" nil path nil 'silent nil 'excl)))
      (unless (chidu-store-sqlite--private-file-p path)
        (signal 'chidu-invariant-error
                '("failed to create private SQLite database")))))
    path))

(defun chidu-store-sqlite--prepare-owner-database (root)
  "Create or validate the private owner database below ROOT."
  (let ((path (expand-file-name "owner.sqlite3" root)))
    (cond
     ((file-symlink-p path)
      (signal 'chidu-invariant-error
              '("SQLite owner database must not be a symbolic link")))
     ((file-exists-p path)
      (unless (chidu-store-sqlite--private-file-p path)
        (signal 'chidu-invariant-error
                '("SQLite owner database must be current-uid mode 0600"))))
     (t
      (let ((coding-system-for-write 'binary)
            (make-backup-files nil))
        (with-file-modes #o600
          (write-region "" nil path nil 'silent nil 'excl)))
      (unless (chidu-store-sqlite--private-file-p path)
        (signal 'chidu-invariant-error
                '("failed to create private SQLite owner database")))))
    path))

(defun chidu-store-sqlite--acquire-owner (path)
  "Open PATH and hold a lifetime exclusive SQLite transaction."
  (let ((database (sqlite-open path)))
    (condition-case error-data
        (progn
          (sqlite-execute database "PRAGMA busy_timeout = 0")
          (sqlite-execute database "PRAGMA journal_mode = DELETE")
          (sqlite-execute
           database
           "CREATE TABLE IF NOT EXISTS chidu_owner_lock (
              singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
              token INTEGER NOT NULL
            )")
          (sqlite-execute
           database "INSERT OR IGNORE INTO chidu_owner_lock VALUES (1, 0)")
          (sqlite-execute database "BEGIN EXCLUSIVE")
          (sqlite-execute
           database "UPDATE chidu_owner_lock SET token = token + 1 WHERE singleton = 1")
          database)
      (error
       (ignore-errors (sqlite-close database))
       (signal 'chidu-invariant-error
               (list "data root is already owned by another Chidu Store instance"
                     (error-message-string error-data)))))))

(defun chidu-store-sqlite--release-owner (database)
  "Release and close owner DATABASE."
  (when database
    (ignore-errors (sqlite-execute database "ROLLBACK"))
    (ignore-errors (sqlite-close database))))

(defun chidu-store-sqlite--configure-database (database)
  "Configure connection-local safety/performance pragmas for DATABASE."
  (sqlite-execute database "PRAGMA foreign_keys = ON")
  (sqlite-execute database "PRAGMA busy_timeout = 5000")
  (sqlite-execute database "PRAGMA journal_mode = WAL"))

(defun chidu-store-sqlite--metadata (database)
  "Return (STORE-ID CHANGE-SEQ) from DATABASE."
  (or (car
       (chidu-sql-select database chidu-store-sqlite--metadata-query))
      (signal 'chidu-invariant-error
              '("SQLite store metadata is missing"))))

(defun chidu-store-sqlite--assert-open (state)
  "Return STATE's connection, or signal if STATE is closed."
  (when (chidu-store-sqlite-state-closed-p state)
    (signal 'chidu-invariant-error '("SQLite Store is closed")))
  (chidu-store-sqlite-state-connection state))

(defun chidu-store-sqlite--capability-names (value context)
  "Decode capability-object JSON VALUE into a string vector for CONTEXT."
  (if (null value)
      (vector)
    (let ((decoded
           (condition-case error-data
               (json-parse-string value :object-type 'hash-table)
             (error
              (signal 'chidu-invariant-error
                      (list (format "%s JSON is invalid: %s"
                                    context
                                    (error-message-string error-data))))))))
      (unless (hash-table-p decoded)
        (signal 'chidu-invariant-error
                (list (format "%s JSON is not an object" context))))
      (chidu-store-validate-string-vector
       (vconcat (sort (hash-table-keys decoded) #'string<)) context))))

(defun chidu-store-sqlite--capability-json (value context)
  "Encode capability-name vector VALUE as an object for CONTEXT."
  (let ((names (chidu-store-validate-string-vector value context))
        (object (make-hash-table :test #'equal)))
    (cl-loop for name across names
             do (puthash name (make-hash-table :test #'equal) object))
    (json-serialize object)))

(defun chidu-store-sqlite--string-vector-json (value context)
  "Encode validated string vector VALUE as JSON for CONTEXT."
  (json-serialize (chidu-store-validate-string-vector value context)))

(defun chidu-store-sqlite--string-vector-from-json (value context)
  "Decode JSON array VALUE as a validated string vector for CONTEXT."
  (condition-case error-data
      (chidu-store-validate-string-vector
       (json-parse-string value :array-type 'array)
       context)
    (error
     (signal 'chidu-invariant-error
             (list (format "%s JSON is invalid: %s"
                           context (error-message-string error-data)))))))

(defun chidu-store-sqlite--email-address-vector-json (addresses)
  "Encode typed Email ADDRESSES as JSON."
  (json-serialize
   (vconcat
    (cl-loop
     for address across addresses
     collect
     (vector
      (or (chidu-store-email-address-name address) :json-null)
      (chidu-store-email-address-email address))))
   :null-object :json-null))

(defun chidu-store-sqlite--email-address-vector-from-json (value context)
  "Decode JSON VALUE as Store EmailAddress vector for CONTEXT."
  (let ((wire
         (condition-case error-data
             (json-parse-string
              value :array-type 'array :null-object :json-null)
           (error
            (signal 'chidu-invariant-error
                    (list (format "%s JSON is invalid: %s"
                                  context
                                  (error-message-string error-data)))))))
        result)
    (unless (vectorp wire)
      (signal 'chidu-invariant-error
              (list (format "%s JSON is not an array" context))))
    (cl-loop
     for item across wire
     do
     (unless (and (vectorp item) (= (length item) 2)
                  (or (eq (aref item 0) :json-null)
                      (stringp (aref item 0)))
                  (stringp (aref item 1)))
       (signal 'chidu-invariant-error
               (list (format "%s contains an invalid address" context))))
     (push
      (chidu-store-email-address-create
       :name (unless (eq (aref item 0) :json-null)
               (aref item 0))
       :email (aref item 1))
      result))
    (vconcat (nreverse result))))

(defun chidu-store-sqlite--bool (value)
  "Decode SQLite integer VALUE as Boolean."
  (pcase value
    (0 nil)
    (1 t)
    (_
     (signal 'chidu-invariant-error
             (list "SQLite Boolean is invalid" value)))))

(defun chidu-store-sqlite--integer-bool (value)
  "Encode Lisp Boolean VALUE as SQLite integer."
  (if value 1 0))

(defun chidu-store-sqlite--change-seq (database)
  "Return DATABASE change sequence as a nonnegative integer."
  (let ((value
         (caar
          (chidu-sql-select
              database chidu-store-sqlite--change-seq-query))))
    (unless (and (integerp value) (>= value 0))
      (signal 'chidu-invariant-error
              '("SQLite change_seq is invalid")))
    value))

(defun chidu-store-sqlite--increment-change-seq (database)
  "Increment and return DATABASE change sequence."
  (chidu-sql-execute
      database chidu-store-sqlite--increment-change-seq-statement)
  (chidu-store-sqlite--change-seq database))

(defun chidu-store-sqlite--email-record-id
    (database account-id remote-email-id change-seq)
  "Return stable Email id from DATABASE for ACCOUNT-ID and REMOTE-EMAIL-ID.

Use CHANGE-SEQ when creating the identity."
  (or (caar
       (chidu-sql-select database
         [:select [local-email-id]
          :from jmap-email-record
          :where [:and
                  [:= account-id [:bind account-id]]
                  [:= remote-email-id [:bind remote-email-id]]]]))
      (let ((local-id (chidu-store-new-local-id)))
        (chidu-sql-execute database
          [:insert :into jmap-email-record
           :row
           [[local-email-id [:bind local-id]]
            [account-id [:bind account-id]]
            [remote-email-id [:bind remote-email-id]]
            [created-change-seq [:bind change-seq]]]])
        local-id)))

(defun chidu-store-sqlite--runtime (state)
  "Return runtime metadata for SQLite STATE."
  (let ((database (chidu-store-sqlite--assert-open state)))
    (chidu-store-runtime-create
     :store-id (chidu-store-sqlite-state-store-id state)
     :change-seq
     (number-to-string
      (chidu-store-sqlite--change-seq database)))))

(defun chidu-store-sqlite--close (state)
  "Close SQLite STATE and release its lifetime owner lock once."
  (unless (chidu-store-sqlite-state-closed-p state)
    (setf (chidu-store-sqlite-state-closed-p state) t)
    (ignore-errors
      (sqlite-close (chidu-store-sqlite-state-connection state)))
    (chidu-store-sqlite--release-owner
     (chidu-store-sqlite-state-owner-connection state))))

(provide 'chidu-store-sqlite-core)

;;; chidu-store-sqlite-core.el ends here
