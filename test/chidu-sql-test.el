;;; chidu-sql-test.el --- Tests for Chidu's SQLite DSL -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'chidu-sql)

(ert-deftest chidu-sql-lexical-holes-bind-native-values-once ()
  (let ((database (sqlite-open))
        (events nil)
        (account-id "account")
        (local-email-id "email"))
    (unwind-protect
        (progn
          (sqlite-execute
           database
           "CREATE TABLE email (
              account_id TEXT NOT NULL,
              local_email_id TEXT NOT NULL,
              subject TEXT NOT NULL,
              PRIMARY KEY (account_id, local_email_id)
            )")
          (chidu-sql-execute
              (progn (setq events (append events '(database))) database)
            [:insert :into email
             :row
             [[account-id
               [:bind
                (progn (setq events (append events '(account))) account-id)]]
              [local-email-id
               [:bind
                (progn (setq events (append events '(email))) local-email-id)]]
              [subject
               [:bind
                (progn (setq events (append events '(subject))) "Hello")]]]])
          (should (equal '(database account email subject) events))
          (should
           (equal
            '(("Hello"))
            (chidu-sql-select database
              [:select [subject]
               :from email
               :where [:and
                       [:= account-id [:bind account-id]]
                       [:= local-email-id [:bind local-email-id]]]])))
          ;; Options may appear out of SQL order, but lexical holes are
          ;; evaluated once in the final placeholder order.
          (setq events nil)
          (should
           (equal
            '(("Hello"))
            (chidu-sql-select database
              [:select [subject]
               :limit
               [:bind (progn (setq events (append events '(limit))) 1)]
               :where
               [:= account-id
                   [:bind
                    (progn (setq events (append events '(where))) account-id)]]
               :from email])))
          (should (equal '(where limit) events))
          (let ((statement
                 (chidu-sql
                  [:select [account-id local-email-id]
                   :from email
                   :order-by [[account-id :asc]]])))
            (should (chidu-sql-statement-p statement))
            (should
             (equal
              '(("account" "email"))
              (chidu-sql-select database statement)))))
      (sqlite-close database))))

(ert-deftest chidu-sql-select-distinct-removes-duplicate-rows ()
  (let ((database (sqlite-open)))
    (unwind-protect
        (progn
          (sqlite-execute database "CREATE TABLE sample (value TEXT NOT NULL)")
          (sqlite-execute database "INSERT INTO sample VALUES ('a'), ('a'), ('b')")
          (should
           (equal
            '(("a") ("b"))
            (chidu-sql-select database
              [:select [value]
               :distinct t
               :from sample
               :order-by [[value :asc]]]))))
      (sqlite-close database))))

(ert-deftest chidu-sql-select-supports-aliased-subquery-relations ()
  (let ((database (sqlite-open))
        (account-id "account")
        (limit 2))
    (unwind-protect
        (progn
          (sqlite-execute
           database
           "CREATE TABLE email (
              account_id TEXT NOT NULL,
              item_id TEXT NOT NULL,
              received_at TEXT NOT NULL
            )")
          (sqlite-execute
           database
           "INSERT INTO email VALUES
              ('account', 'old', '2026-01-01T00:00:00Z'),
              ('account', 'new', '2026-01-02T00:00:00Z'),
              ('other', 'hidden', '2026-01-03T00:00:00Z')")
          (should
           (equal
            '(("new") ("old"))
            (chidu-sql-select database
              [:select [recent:item-id]
               :from
               [:as
                [:select [item-id received-at]
                 :from email
                 :where [:= account-id [:bind account-id]]
                 :order-by [[received-at :desc]]
                 :limit [:bind limit]]
                recent]
               :order-by [[recent:received-at :desc]]]))))
      (sqlite-close database))))

(ert-deftest chidu-sql-map-lowers-joins-case-and-result-bindings ()
  (let ((database (sqlite-open))
        (account-id "account"))
    (unwind-protect
        (progn
          (sqlite-execute
           database
           "CREATE TABLE sample (
              account_id TEXT NOT NULL,
              item_id TEXT NOT NULL,
              is_unread INTEGER NOT NULL
            )")
          (sqlite-execute
           database
           "CREATE TABLE intent (
              account_id TEXT NOT NULL,
              item_id TEXT NOT NULL,
              desired_seen INTEGER
            )")
          (sqlite-execute
           database
           "CREATE TABLE blocked (
              account_id TEXT NOT NULL,
              item_id TEXT NOT NULL
            )")
          (sqlite-execute
           database
           "INSERT INTO sample VALUES
              ('account', 'a', 1),
              ('account', 'b', 1),
              ('account', 'c', 0)")
          (sqlite-execute
           database
           "INSERT INTO intent VALUES ('account', 'a', 1)")
          (sqlite-execute
           database
           "INSERT INTO blocked VALUES ('account', 'c')")
          (should
           (equal
            '(("a" 0) ("b" 1))
            (chidu-sql-map database
                [:select
                 [[item-id sample:item-id]
                  [effective-unread
                   [:call coalesce
                          [:case intent:desired-seen [1 0] [0 1]]
                          sample:is-unread]]]
                 :from [:as sample sample]
                 :joins
                 [[:left [:as intent intent]
                   :on [:and
                        [:= intent:account-id sample:account-id]
                        [:= intent:item-id sample:item-id]]]]
                 :where
                 [:and
                  [:= sample:account-id [:bind account-id]]
                  [:not-exists
                   [:select [1]
                    :from [:as blocked blocked]
                    :where [:and
                            [:= blocked:account-id sample:account-id]
                            [:= blocked:item-id sample:item-id]]]]]
                 :order-by [[sample:item-id :asc]]]
              (list item-id effective-unread))))
          (should
           (equal
            '(("c"))
            (chidu-sql-select database
              [:select [sample:item-id]
               :from [:as sample sample]
               :where
               [:in sample:item-id
                    [:select [blocked:item-id]
                     :from [:as blocked blocked]
                     :where [:= blocked:account-id [:bind account-id]]]]])))
          (should
           (equal
            '("a" 1)
            (chidu-sql-one database
                [:select [[item-id item-id] [unread is-unread]]
                 :from sample
                 :where [:= item-id [:literal "a"]]]
              (list item-id unread))))
          (should-not
           (chidu-sql-one database
               [:select [[item-id item-id]]
                :from sample
                :where [:= item-id [:literal "missing"]]]
             item-id)))
      (sqlite-close database))))

(ert-deftest chidu-sql-expression-tree-preserves-sql-semantics ()
  (let ((database (sqlite-open)))
    (unwind-protect
        (progn
          (sqlite-execute
           database
           "CREATE TABLE sample (a INTEGER, b INTEGER, c INTEGER, d INTEGER)")
          (sqlite-execute database "INSERT INTO sample VALUES (1, 5, 2, 3)")
          (should
           (equal
            "SELECT ((10 - 3) * 2) FROM sample"
            (chidu-sql-statement-sql
             (chidu-sql-compile
              '[:select [[:* [:- 10 3] 2]] :from sample]))))
          (should
           (equal
            "SELECT (a - (b - c)) FROM sample"
            (chidu-sql-statement-sql
             (chidu-sql-compile
              '[:select [[:- a [:- b c]]] :from sample]))))
          (should
           (equal
            '((14 7 4 1 0 7))
            (chidu-sql-select database
              [:select
               [[:* [:- 10 3] 2]
                [:- 10 [:- 4 1]]
                [:/ 20 [:+ 2 3]]
                [:= [:+ a b] [:* c d]]
                [:not [:or [:= a 1] [:= b 2]]]
                [:call abs [:- 3 10]]]
               :from sample]))))
      (sqlite-close database))))

(ert-deftest chidu-sql-annotated-upsert-binds-in-generated-order ()
  (let ((database (sqlite-open))
        (events nil)
        (calls 0))
    (unwind-protect
        (progn
          (sqlite-execute
           database
           "CREATE TABLE mailbox (
              mailbox_id TEXT PRIMARY KEY,
              account_id TEXT NOT NULL,
              remote_mailbox_id TEXT NOT NULL,
              name TEXT NOT NULL,
              is_available INTEGER NOT NULL,
              revision INTEGER NOT NULL,
              UNIQUE (account_id, remote_mailbox_id)
            )")
          (cl-labels
              ((observe
                 (event value)
                 (cl-incf calls)
                 (setq events (append events (list event)))
                 value)
               (save
                 (mailbox-id name increment)
                 (chidu-sql-execute database
                   [:insert :into mailbox
                    :row
                    [[mailbox-id [:bind (observe 'insert-id mailbox-id)]]
                     [account-id [:literal "account"]]
                     [remote-mailbox-id [:literal "remote"]]
                     [name
                      [:bind (observe 'insert-name name)]
                      [:update [:bind (observe 'update-name name)]]]
                     [is-available 1 :update]
                     [revision
                      1
                      [:update
                       [:+ revision
                           [:bind (observe 'update-revision increment)]]]]]
                    :on-conflict [account-id remote-mailbox-id]])))
            (save "stable" "Inbox" 2)
            (should (= 4 calls))
            (should
             (equal '(insert-id insert-name update-name update-revision)
                    events))
            (should
             (equal
              '(("stable" "Inbox" 1 1))
              (sqlite-select
               database
               "SELECT mailbox_id, name, is_available, revision FROM mailbox")))
            (setq events nil
                  calls 0)
            (save "ignored" "Renamed" 3)
            (should (= 4 calls))
            (should
             (equal '(insert-id insert-name update-name update-revision)
                    events))
            (should
             (equal
              '(("stable" "Renamed" 1 4))
              (sqlite-select
               database
               "SELECT mailbox_id, name, is_available, revision FROM mailbox")))))
      (sqlite-close database))))

(ert-deftest chidu-sql-rejects-ambiguous-or-unsafe-syntax ()
  (should-error (chidu-sql-compile '[:select [id] :from "table"]))
  (should-error (chidu-sql-compile '[:select [id] :from bad/name]))
  (should-error (chidu-sql-identifier (intern "foo::bar")))
  (should-error (chidu-sql-identifier (intern "foo:")))
  (should-error (chidu-sql-compile '[:select [id] :from :table]))
  (should-error
   (chidu-sql-compile
    '[:select [id] :from table :where [:= value "runtime"]]))
  (should-error
   (chidu-sql-compile '[:select [id] :from table :where [:in id []]]))
  (should-error
   (chidu-sql-compile '[:select [] :from table]))
  (should-error
   (chidu-sql-compile '[:select [id] :from table :joins []]))
  (should-error
   (chidu-sql-compile '[:select [id] :from table :order-by []]))
  (should-error
   (chidu-sql-compile '[:select [id] :from table :offset 1]))
  (should-error
   (chidu-sql-compile '[:select [id] :from [:as [:+ a b] computed]]))
  (should-error
   (chidu-sql-compile
    '[:select [id] :from table
      :joins [[:outer other :on [:= other:id table:id]]]]))
  (should-error
   (chidu-sql-compile
    '[:select [[:case value [:else 0] [1 2]]] :from table]))
  (should-error
   (chidu-sql-compile
    '[:select [id] :from table :where [:and [:= a 1]]]))
  (should-error
   (macroexpand
    '(chidu-sql-map database
         [:select [[value id] [value name]] :from table]
       value)))
  (should-error
   (macroexpand
    '(chidu-sql-map database
         [:select [[value id]] :from table])))
  (should-error
   (chidu-sql-compile '[:update table :set [[value [:bind]]]]))
  (should-error
   (chidu-sql-compile
    '[:insert :into table
      :row [[id [:bind id]] [id [:bind duplicate]]]]))
  (should-error
   (chidu-sql-compile
    '[:insert :into table :row [[id [:bind id] :update]]]))
  (should-error
   (chidu-sql-compile
    '[:insert :into table
      :row [[id [:bind id]] [name [:bind name]]]
      :on-conflict [id]]))
  (should-error
   (chidu-sql-compile
    '[:insert :into table
      :columns [id name] :values [[1 2]]
      :on-conflict [id]]))
  (should-error
   (chidu-sql-compile
    '[:insert :into table
      :row [[id [:bind id] :invent]]
      :on-conflict [id]]))
  (should-error
   (chidu-sql-compile
    '[:select [id] :from table :where [:= id [:bind id]]]))
  (should-error
   (macroexpand
    '(chidu-sql
      [:select [id] :from table :where [:= id [:bind id]]])))
  (should-error
   (chidu-sql-compile-create-table
    '[:table bad [:column id :integer [:default [:bind value]]]]))
  (should-error
   (chidu-sql-compile-create-table
    '[:table bad
      [:column parent-id :text
               [:references parent [id] :initially :deferred]]]))
  (should
   (equal "SELECT id FROM table WHERE (value IS NULL)"
          (chidu-sql-statement-sql
           (chidu-sql-compile
            '[:select [id] :from table :where [:is value nil]]))))
  (should
   (equal "SELECT id FROM table WHERE (value = TRUE)"
          (chidu-sql-statement-sql
           (chidu-sql-compile
            '[:select [id] :from table :where [:= value t]])))))

(ert-deftest chidu-sql-schema-compiles-current-style-constraints ()
  (let* ((schema
          [[:table parent
                   [:column id :text :primary-key
                            [:check [:> [:call length id] 0]]]]
           [:table child
                   [:column account-id :text :not-null]
                   [:column parent-id :text :not-null
                            [:references parent [id] :on-delete :cascade]]
                   [:column phase :text :not-null
                            [:check [:in phase [[:literal "pending"] [:literal "unknown"]]]]]
                   [:column is-stale :integer :not-null [:default 0]
                            [:check [:in is-stale [0 1]]]]
                   [:primary-key [account-id parent-id]]
                   [:unique [parent-id account-id]]]
           [:index child-phase :on child :columns [account-id phase] :unique t
            :where [:= is-stale 0]]])
         (statements (chidu-sql-schema-statements schema))
         (database (sqlite-open)))
    (unwind-protect
        (progn
          (dolist (statement statements)
            (chidu-sql-execute database statement))
          (should
           (equal '("parent" "child")
                  (chidu-sql-schema-table-names schema)))
          (should
           (equal '("child_phase")
                  (chidu-sql-schema-index-names schema)))
          (should
           (equal
            '("account_id" "parent_id" "phase" "is_stale")
            (mapcar #'cadr (sqlite-select database "PRAGMA table_xinfo(child)"))))
          (sqlite-execute
           database
           "INSERT INTO parent (id) VALUES ('p')")
          (sqlite-execute
           database
           "INSERT INTO child (account_id, parent_id, phase) VALUES ('a', 'p', 'pending')")
          (should
           (equal 0
                  (caar
                   (sqlite-select
                    database
                    "SELECT is_stale FROM child LIMIT 1"))))
          (let ((sql
                 (caar
                  (sqlite-select
                   database
                   "SELECT sql FROM sqlite_master WHERE name = 'child_phase'"))))
            (should (member "where" (chidu-sql-normalize sql)))
            (should (member "is_stale" (chidu-sql-normalize sql)))))
      (sqlite-close database))))

(ert-deftest chidu-sql-normalization-is-token-based ()
  (should
   (equal
    (chidu-sql-normalize
     "CREATE TABLE x (value TEXT CHECK (value IN ('A', 'b'))) ;")
    (chidu-sql-normalize
     "create   table x(value text check(value in('A','b')))")))
  (should-not
   (equal
    (chidu-sql-normalize "CHECK (value = 'A')")
    (chidu-sql-normalize "CHECK (value = 'a')")))
  (should
   (equal
    (chidu-sql-normalize
     "CREATE TABLE jmap_endpoint (account_id TEXT)")
    (chidu-sql-normalize
     "CREATE TABLE \"jmap_endpoint\" ([account_id] TEXT)"))))

(provide 'chidu-sql-test)

;;; chidu-sql-test.el ends here
