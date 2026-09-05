;;; chidu-store-sqlite-mutation-support.el --- Shared SQLite mutation support -*- lexical-binding: t; -*-

;;; Commentary:

;; Target validation, one-Account Mailbox mutation lane lookup, and
;; Search ordinal compaction shared by ordinary move and Trash.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'chidu-sql)
(require 'chidu-store)

(defun chidu-store-sqlite--validate-local-email-ids (value)
  "Return nonempty unique local Email id vector VALUE, or signal."
  (chidu-store-validate-string-vector value "Mailbox move local Email ids")
  (unless (and (> (length value) 0)
               (cl-loop for id across value
                        always (chidu-store-local-id-p id)))
    (signal 'chidu-invariant-error
            '("Mailbox move requires canonical local Email ids")))
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for id across value
             do
             (when (gethash id seen)
               (signal 'chidu-invariant-error
                       '("Mailbox move contains a duplicate Email")))
             do (puthash id t seen)))
  value)

(defun chidu-store-sqlite--mailbox-mutation-operation-id
    (database account-id)
  "Return DATABASE's unresolved Mailbox mutation id for ACCOUNT-ID."
  (or
   (caar
    (chidu-sql-select database
      [:select [operation-id]
               :from jmap-mailbox-move
               :where [:= account-id [:bind account-id]]]))
   (caar
    (chidu-sql-select database
      [:select [operation-id]
               :from jmap-trash-operation
               :where [:= account-id [:bind account-id]]]))))

(defun chidu-store-sqlite--mailbox-mutation-operation-owner
    (database operation-id)
  "Return DATABASE Account owning mailbox mutation OPERATION-ID."
  (or
   (caar
    (chidu-sql-select database
      [:select [account-id]
               :from jmap-mailbox-move
               :where [:= operation-id [:bind operation-id]]]))
   (caar
    (chidu-sql-select database
      [:select [account-id]
               :from jmap-trash-operation
               :where [:= operation-id [:bind operation-id]]]))))

(defun chidu-store-sqlite--compact-search-ordinals
    (database account-id query-key)
  "Compact search ordinals for QUERY-KEY below ACCOUNT-ID in DATABASE."
  (let ((ids
         (mapcar
          #'car
          (chidu-sql-select database
            [:select [local-email-id]
                     :from jmap-search-projection-row
                     :where [:and
                             [:= account-id [:bind account-id]]
                             [:= query-key [:bind query-key]]]
                     :order-by [[ordinal :asc]]]))))
    (chidu-sql-execute database
      [:update jmap-search-projection-row
               :set [[ordinal [:+ ordinal 1000000000]]]
               :where [:and
                       [:= account-id [:bind account-id]]
                       [:= query-key [:bind query-key]]]])
    (cl-loop
     for local-id in ids
     for ordinal from 0
     do
     (chidu-sql-execute database
       [:update jmap-search-projection-row
                :set [[ordinal [:bind ordinal]]]
                :where [:and
                        [:= account-id [:bind account-id]]
                        [:= query-key [:bind query-key]]
                        [:= local-email-id [:bind local-id]]]]))))

(provide 'chidu-store-sqlite-mutation-support)

;;; chidu-store-sqlite-mutation-support.el ends here
