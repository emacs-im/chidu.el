;;; chidu-store-sqlite-schema-test.el --- Current-format Store tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'chidu-store-sqlite)

(defun chidu-store-sqlite-schema-test--root ()
  "Return fresh private Store root."
  (let ((root (make-temp-file "chidu-schema-contract-" t)))
    (set-file-modes root #o700)
    root))

(defun chidu-store-sqlite-schema-test--drift (mutator)
  "Create a current Store, apply MUTATOR, and return open failure data."
  (let ((root (chidu-store-sqlite-schema-test--root))
        store
        database
        failure)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (chidu-store-close store)
          (setq store nil
                database
                (sqlite-open (expand-file-name "store.sqlite3" root)))
          (funcall mutator database)
          (sqlite-close database)
          (setq database nil)
          (condition-case error-data
              (progn
                (setq store (chidu-store-sqlite-create root))
                (ert-fail "schema drift was accepted"))
            (chidu-invariant-error
             (setq failure (cddr error-data))))
          failure)
      (when database (sqlite-close database))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-store-sqlite-fresh-schema-matches-the-manifest ()
  (let ((root (chidu-store-sqlite-schema-test--root))
        store
        database)
    (unwind-protect
        (progn
          (setq store (chidu-store-sqlite-create root))
          (chidu-store-close store)
          (setq store nil
                database
                (sqlite-open (expand-file-name "store.sqlite3" root)))
          (should
           (equal
            (chidu-store-sqlite--expected-schema-snapshot)
            (chidu-store-sqlite--schema-snapshot database))))
      (when database (sqlite-close database))
      (when store (chidu-store-close store))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest chidu-store-sqlite-rejects-column-index-and-foreign-key-drift ()
  (let ((cases
         (list
          (cons
           'columns
           (lambda (database)
             (sqlite-execute
              database
              "ALTER TABLE jmap_email_preview DROP COLUMN observed_change_seq")))
          (cons
           'indexes
           (lambda (database)
             (sqlite-execute database "DROP INDEX jmap_account_endpoint")))
          (cons
           'foreign-keys
           (lambda (database)
             (sqlite-execute database "PRAGMA foreign_keys = OFF")
             (sqlite-execute
              database
              "ALTER TABLE jmap_email_preview RENAME TO old_email_preview")
             (sqlite-execute
              database
              "CREATE TABLE jmap_email_preview (
                 account_id TEXT NOT NULL,
                 local_email_id TEXT NOT NULL,
                 value TEXT NOT NULL,
                 observed_change_seq INTEGER NOT NULL
                   CHECK (observed_change_seq >= 0),
                 PRIMARY KEY (account_id, local_email_id)
               )")
             (sqlite-execute database "DROP TABLE old_email_preview"))))))
    (dolist (case cases)
      (let* ((failure
              (chidu-store-sqlite-schema-test--drift (cdr case)))
             (drift (plist-get failure :schema-drift)))
        (should (plist-get failure :rebuild-required))
        (should drift)
        (pcase (car case)
          ('columns
           (should
            (equal "jmap_email_preview" (plist-get drift :table)))
           (should (memq (plist-get drift :component) '(:sql :columns))))
          ('indexes
           (should (eq 'indexes (plist-get drift :component))))
          ('foreign-keys
           (should
            (equal "jmap_email_preview" (plist-get drift :table)))
           (should
            (memq (plist-get drift :component)
                  '(:sql :foreign-keys :indexes)))))))))

(provide 'chidu-store-sqlite-schema-test)

;;; chidu-store-sqlite-schema-test.el ends here
