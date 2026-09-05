;;; chidu-sql.el --- Small SQLite syntax compiler -*- lexical-binding: t; -*-

;;; Commentary:

;; Chidu keeps SQL as an explicit SQLite-backend implementation detail.  This
;; module compiles a deliberately small vector DSL into native SQLite SQL.
;; Runtime values are lexical holes:
;;
;;   [:bind ELISP-FORM]
;;
;; `chidu-sql-select' and `chidu-sql-execute' are macros.  They compile a
;; literal query at macro-expansion time and lower each hole directly to one
;; native `?' parameter.  ELISP-FORM remains beside the SQL column/expression
;; that consumes it; no positional argument list or parallel named-binding
;; plist exists at runtime.  Holes are evaluated once in final SQL placeholder
;; order, which may differ from the source order of statement options.
;;
;; Bound templates exist only as a macro-expansion intermediate.  Runtime
;; `chidu-sql-statement' values are parameter-free SQL used by static queries
;; and schema DDL.  The compiler does not
;; own a connection, transaction, row decoder, operation registry, or backend
;; abstraction.  Complex SQLite-specific statements may remain raw SQL when a
;; DSL form would obscure rather than clarify them.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'seq)
(require 'sqlite)
(require 'subr-x)

(define-error 'chidu-sql-error "Chidu SQL error")

(cl-defstruct (chidu-sql-statement
               (:constructor chidu-sql-statement-create))
  "One parameter-free compiled SQL statement."
  sql)

(cl-defstruct (chidu-sql--template
               (:constructor chidu-sql--template-create))
  "Macro-expansion intermediate containing SQL and lexical HOLES."
  sql
  (holes (vector)))

(defvar chidu-sql--hole-forms nil
  "Reversed lexical binding forms while compiling one statement.")

(defun chidu-sql--error (format-string &rest arguments)
  "Signal a DSL error using FORMAT-STRING and ARGUMENTS."
  (signal 'chidu-sql-error
          (list (concat "Chidu SQL: "
                        (apply #'format format-string arguments)))))

(defun chidu-sql--identifier-part (value)
  "Return one safe SQL identifier part for VALUE."
  (unless (and (symbolp value) (not (keywordp value)))
    (chidu-sql--error "Identifier must be a non-keyword symbol, got %S" value))
  (let ((name (string-replace "-" "_" (symbol-name value))))
    (unless (string-match-p
             (rx string-start
                 (or (in "A-Z" "a-z") "_")
                 (* (or alnum "_"))
                 string-end)
             name)
      (chidu-sql--error "Unsafe identifier %S" value))
    name))

(defun chidu-sql-identifier (value)
  "Return validated project-controlled SQL identifier VALUE.

A symbol containing colons denotes qualified components, for example
`email:account-id' becomes `email.account_id'.  Empty components are rejected.
This prevents SQL injection characters but does not classify SQLite keywords."
  (unless (and (symbolp value) (not (keywordp value)))
    (chidu-sql--error "Identifier must be a non-keyword symbol, got %S" value))
  (let ((parts (split-string (symbol-name value) ":" nil)))
    (when (seq-some #'string-empty-p parts)
      (chidu-sql--error "Qualified identifier has an empty component: %S"
                        value))
    (mapconcat
     (lambda (part) (chidu-sql--identifier-part (intern part)))
     parts
     ".")))

(defun chidu-sql--quote-string (value)
  "Return SQL string literal for VALUE."
  (format "'%s'" (string-replace "'" "''" value)))

(defun chidu-sql--literal (value)
  "Return trusted SQL literal VALUE."
  (cond
   ((null value) "NULL")
   ((eq value t) "TRUE")
   ((numberp value) (number-to-string value))
   ((stringp value) (chidu-sql--quote-string value))
   (t (chidu-sql--error "Unsupported literal %S" value))))

(defun chidu-sql--raw (arguments)
  "Compile trusted raw SQL ARGUMENTS."
  (pcase arguments
    (`(,value)
     (unless (stringp value)
       (chidu-sql--error ":raw requires a constant string, got %S" value))
     value)
    (_ (chidu-sql--error ":raw expects exactly one string"))))

(defun chidu-sql--bind (arguments)
  "Compile one lexical SQLite binding from ARGUMENTS."
  (pcase arguments
    (`(,form)
     (push form chidu-sql--hole-forms)
     "?")
    (_ (chidu-sql--error ":bind expects exactly one Elisp form"))))

(defun chidu-sql--compile-sequence (values separator)
  "Compile VALUES and join them with SEPARATOR."
  (mapconcat #'chidu-sql--compile-expression values separator))

(defun chidu-sql--compile-call (arguments)
  "Compile SQL function call ARGUMENTS."
  (pcase arguments
    (`(,name . ,values)
     (format "%s(%s)"
             (chidu-sql-identifier name)
             (chidu-sql--compile-sequence values ", ")))
    (_ (chidu-sql--error ":call requires a function name"))))

(defun chidu-sql--compile-in (arguments)
  "Compile an IN expression from ARGUMENTS."
  (pcase arguments
    (`(,left ,right)
     (unless (vectorp right)
       (chidu-sql--error
        ":in requires a value vector or SELECT on the right, got %S" right))
     (when (zerop (length right))
       (chidu-sql--error ":in does not accept an empty vector"))
     (format "(%s IN (%s))"
             (chidu-sql--compile-expression left)
             (if (eq (aref right 0) :select)
                 (chidu-sql--compile-select right)
               (chidu-sql--compile-sequence (append right nil) ", "))))
    (_
     (chidu-sql--error
      ":in expects left and a value vector or SELECT right operand"))))

(defun chidu-sql--compile-nary (operator arguments)
  "Compile infix OPERATOR over at least two ARGUMENTS."
  (unless (>= (length arguments) 2)
    (chidu-sql--error "%s requires at least two operands" operator))
  (format "(%s)"
          (chidu-sql--compile-sequence arguments
                                       (format " %s " operator))))

(defun chidu-sql--compile-binary (operator arguments)
  "Compile binary OPERATOR over ARGUMENTS."
  (pcase arguments
    (`(,left ,right)
     (format "(%s %s %s)"
             (chidu-sql--compile-expression left)
             operator
             (chidu-sql--compile-expression right)))
    (_ (chidu-sql--error "%s expects two operands" operator))))

(defun chidu-sql--compile-unary (operator arguments)
  "Compile unary OPERATOR over ARGUMENTS."
  (pcase arguments
    (`(,value)
     (format "(%s %s)" operator (chidu-sql--compile-expression value)))
    (_ (chidu-sql--error "%s expects one operand" operator))))

(defun chidu-sql--compile-as (arguments)
  "Compile an AS expression from ARGUMENTS."
  (pcase arguments
    (`(,value ,alias)
     (format "%s AS %s"
             (chidu-sql--compile-expression value)
             (chidu-sql-identifier alias)))
    (_ (chidu-sql--error ":as expects value and alias"))))

(defun chidu-sql--compile-case (arguments)
  "Compile simple SQL CASE ARGUMENTS.

The first argument is the compared expression.  Following `[WHEN THEN]'
clauses are lowered in order; an optional final `[:else VALUE]' supplies
the fallback."
  (unless (>= (length arguments) 2)
    (chidu-sql--error ":case requires a value and at least one clause"))
  (let ((value (car arguments))
        (clauses (cdr arguments))
        else
        seen-else
        compiled)
    (dolist (clause clauses)
      (unless (and (vectorp clause) (= (length clause) 2))
        (chidu-sql--error
         ":case clause must be [when then] or [:else value], got %S"
         clause))
      (if (eq (aref clause 0) :else)
          (progn
            (when seen-else
              (chidu-sql--error ":case repeats :else"))
            (setq seen-else t
                  else (aref clause 1)))
        (when seen-else
          (chidu-sql--error ":case :else must be the final clause"))
        (push
         (format "WHEN %s THEN %s"
                 (chidu-sql--compile-expression (aref clause 0))
                 (chidu-sql--compile-expression (aref clause 1)))
         compiled)))
    (unless compiled
      (chidu-sql--error ":case requires at least one [when then] clause"))
    (format "(CASE %s %s%s END)"
            (chidu-sql--compile-expression value)
            (string-join (nreverse compiled) " ")
            (if seen-else
                (format " ELSE %s" (chidu-sql--compile-expression else))
              ""))))

(defun chidu-sql--compile-exists (operator arguments)
  "Compile EXISTS OPERATOR around one SELECT in ARGUMENTS."
  (pcase arguments
    (`(,query)
     (unless (and (vectorp query) (> (length query) 0)
                  (eq (aref query 0) :select))
       (chidu-sql--error "%s requires one SELECT form" operator))
     (format "(%s (%s))" operator (chidu-sql--compile-select query)))
    (_ (chidu-sql--error "%s requires one SELECT form" operator))))

(defun chidu-sql--compile-expression (form)
  "Compile SQL expression FORM."
  (cond
   ((or (null form) (eq form t) (numberp form))
    (chidu-sql--literal form))
   ((stringp form)
    (chidu-sql--error
     "String values require explicit [:literal ...] or [:bind ...], got %S"
     form))
   ((symbolp form) (chidu-sql-identifier form))
   ((vectorp form)
    (let ((operator (and (> (length form) 0) (aref form 0)))
          (arguments (append (seq-subseq form 1) nil)))
      (pcase operator
        (:bind (chidu-sql--bind arguments))
        (:raw (chidu-sql--raw arguments))
        (:literal
         (pcase arguments
           (`(,value) (chidu-sql--literal value))
           (_ (chidu-sql--error ":literal expects one value"))))
        (:ident
         (pcase arguments
           (`(,value) (chidu-sql-identifier value))
           (_ (chidu-sql--error ":ident expects one symbol"))))
        (:call (chidu-sql--compile-call arguments))
        (:as (chidu-sql--compile-as arguments))
        (:case (chidu-sql--compile-case arguments))
        (:in (chidu-sql--compile-in arguments))
        (:exists (chidu-sql--compile-exists "EXISTS" arguments))
        (:not-exists (chidu-sql--compile-exists "NOT EXISTS" arguments))
        (:and (chidu-sql--compile-nary "AND" arguments))
        (:or (chidu-sql--compile-nary "OR" arguments))
        (:not (chidu-sql--compile-unary "NOT" arguments))
        (:= (chidu-sql--compile-binary "=" arguments))
        (:!= (chidu-sql--compile-binary "!=" arguments))
        (:< (chidu-sql--compile-binary "<" arguments))
        (:<= (chidu-sql--compile-binary "<=" arguments))
        (:> (chidu-sql--compile-binary ">" arguments))
        (:>= (chidu-sql--compile-binary ">=" arguments))
        (:is (chidu-sql--compile-binary "IS" arguments))
        (:is-not (chidu-sql--compile-binary "IS NOT" arguments))
        (:+ (chidu-sql--compile-nary "+" arguments))
        (:- (chidu-sql--compile-binary "-" arguments))
        (:* (chidu-sql--compile-nary "*" arguments))
        (:/ (chidu-sql--compile-binary "/" arguments))
        (_ (chidu-sql--error "Unknown expression operator %S" operator)))))
   (t (chidu-sql--error "Unsupported expression %S" form))))

(defun chidu-sql--validate-options (arguments allowed context)
  "Validate plist ARGUMENTS against ALLOWED keys for CONTEXT."
  (unless (zerop (% (length arguments) 2))
    (chidu-sql--error "%s options must be key/value pairs" context))
  (let ((seen nil))
    (cl-loop
     for (key _value) on arguments by #'cddr
     do
     (unless (memq key allowed)
       (chidu-sql--error "%s has unknown option %S" context key))
     (when (memq key seen)
       (chidu-sql--error "%s repeats option %S" context key))
     (push key seen)))
  arguments)

(defun chidu-sql--plist-value (arguments key)
  "Return KEY value from statement ARGUMENTS, or nil."
  (plist-get arguments key))

(defun chidu-sql--compile-relation (value)
  "Compile table, alias, or aliased SELECT relation VALUE."
  (cond
   ((symbolp value) (chidu-sql-identifier value))
   ((and (vectorp value)
         (= (length value) 3)
         (eq (aref value 0) :as)
         (symbolp (aref value 2)))
    (let ((source (aref value 1)))
      (format
       "%s AS %s"
       (cond
        ((symbolp source) (chidu-sql-identifier source))
        ((and (vectorp source) (> (length source) 0)
              (eq (aref source 0) :select))
         (format "(%s)" (chidu-sql--compile-select source)))
        (t
         (chidu-sql--error
          "Aliased relation source must be a table or SELECT, got %S"
          source)))
       (chidu-sql-identifier (aref value 2)))))
   (t
    (chidu-sql--error
     "Relation must be a table or [:as SOURCE alias], got %S" value))))

(defun chidu-sql--compile-join (form)
  "Compile one strict JOIN FORM.

FORM is `[:inner RELATION :on EXPR]' or `[:left RELATION :on EXPR]'."
  (unless (and (vectorp form)
               (= (length form) 4)
               (memq (aref form 0) '(:inner :left))
               (eq (aref form 2) :on))
    (chidu-sql--error
     "JOIN must be [:inner RELATION :on EXPR] or [:left RELATION :on EXPR], got %S"
     form))
  (format "%s %s ON %s"
          (if (eq (aref form 0) :left) "LEFT JOIN" "JOIN")
          (chidu-sql--compile-relation (aref form 1))
          (chidu-sql--compile-expression (aref form 3))))

(defun chidu-sql--compile-columns (value)
  "Compile SELECT or INSERT column VALUE."
  (cond
   ((eq value '*) "*")
   ((vectorp value)
    (chidu-sql--compile-sequence (append value nil) ", "))
   (t (chidu-sql--compile-expression value))))

(defun chidu-sql--compile-order-item (item)
  "Compile one ORDER BY ITEM."
  (if (and (vectorp item) (= (length item) 2)
           (memq (aref item 1) '(:asc :desc)))
      (format "%s %s"
              (chidu-sql--compile-expression (aref item 0))
              (upcase (substring (symbol-name (aref item 1)) 1)))
    (chidu-sql--compile-expression item)))

(defun chidu-sql--compile-select (form)
  "Compile SELECT statement FORM."
  (unless (>= (length form) 4)
    (chidu-sql--error ":select requires columns and :from"))
  (let* ((columns (aref form 1))
         (arguments (append (seq-subseq form 2) nil))
         (distinct (chidu-sql--plist-value arguments :distinct))
         (from (chidu-sql--plist-value arguments :from))
         (joins (chidu-sql--plist-value arguments :joins))
         (where (chidu-sql--plist-value arguments :where))
         (order-by (chidu-sql--plist-value arguments :order-by))
         (limit (chidu-sql--plist-value arguments :limit))
         (offset (chidu-sql--plist-value arguments :offset)))
    (chidu-sql--validate-options
     arguments '(:distinct :from :joins :where :order-by :limit :offset)
     ":select")
    (when (and (vectorp columns) (zerop (length columns)))
      (chidu-sql--error ":select columns must be nonempty"))
    (unless (memq distinct '(nil t))
      (chidu-sql--error ":select :distinct must be Boolean"))
    (unless from (chidu-sql--error ":select requires :from"))
    (when joins
      (unless (and (vectorp joins) (> (length joins) 0))
        (chidu-sql--error ":select :joins requires a nonempty vector")))
    (when order-by
      (unless (and (vectorp order-by) (> (length order-by) 0))
        (chidu-sql--error ":select :order-by requires a nonempty vector")))
    (when (and offset (null limit))
      (chidu-sql--error ":select :offset requires :limit"))
    (string-join
     (delq
      nil
      (list
       (format "SELECT %s%s"
               (if distinct "DISTINCT " "")
               (chidu-sql--compile-columns columns))
       (format "FROM %s" (chidu-sql--compile-relation from))
       (when joins
         (mapconcat #'chidu-sql--compile-join
                    (append joins nil) " "))
       (when where
         (format "WHERE %s" (chidu-sql--compile-expression where)))
       (when order-by
         (format "ORDER BY %s"
                 (mapconcat #'chidu-sql--compile-order-item
                            (append order-by nil) ", ")))
       (when limit
         (format "LIMIT %s" (chidu-sql--compile-expression limit)))
       (when offset
         (format "OFFSET %s" (chidu-sql--compile-expression offset)))))
     " ")))

(defun chidu-sql--compile-assignment (item)
  "Compile one SET assignment ITEM."
  (unless (and (vectorp item) (= (length item) 2))
    (chidu-sql--error "SET item must be [column value], got %S" item))
  (format "%s = %s"
          (chidu-sql--compile-expression (aref item 0))
          (chidu-sql--compile-expression (aref item 1))))

(defun chidu-sql--compile-update (form)
  "Compile UPDATE statement FORM."
  (unless (>= (length form) 4)
    (chidu-sql--error ":update requires table and :set"))
  (let* ((table (aref form 1))
         (arguments (append (seq-subseq form 2) nil))
         (assignments (chidu-sql--plist-value arguments :set))
         (where (chidu-sql--plist-value arguments :where)))
    (chidu-sql--validate-options arguments '(:set :where) ":update")
    (unless (and (vectorp assignments) (> (length assignments) 0))
      (chidu-sql--error ":update requires nonempty :set vector"))
    (concat
     (format "UPDATE %s SET %s"
             (chidu-sql-identifier table)
             (mapconcat #'chidu-sql--compile-assignment
                        (append assignments nil) ", "))
     (if where
         (format " WHERE %s" (chidu-sql--compile-expression where))
       ""))))

(defun chidu-sql--compile-delete (form)
  "Compile DELETE statement FORM."
  (let* ((arguments (append (seq-subseq form 1) nil))
         (table (chidu-sql--plist-value arguments :from))
         (where (chidu-sql--plist-value arguments :where)))
    (chidu-sql--validate-options arguments '(:from :where) ":delete")
    (unless table (chidu-sql--error ":delete requires :from"))
    (concat
     (format "DELETE FROM %s" (chidu-sql-identifier table))
     (if where
         (format " WHERE %s" (chidu-sql--compile-expression where))
       ""))))

(defun chidu-sql--compile-values-row (row &optional expected-arity)
  "Compile one INSERT values ROW and optionally require EXPECTED-ARITY."
  (unless (vectorp row)
    (chidu-sql--error "INSERT values row must be a vector, got %S" row))
  (when (and expected-arity (/= (length row) expected-arity))
    (chidu-sql--error
     "INSERT row has %d values for %d columns"
     (length row) expected-arity))
  (format "(%s)"
          (chidu-sql--compile-sequence (append row nil) ", ")))

(defun chidu-sql--insert-row (row)
  "Parse annotated INSERT ROW.

Return (COLUMNS VALUES UPDATES).  Each row item is `[COLUMN INSERT-VALUE]',
`[COLUMN INSERT-VALUE :update]' to assign `excluded.COLUMN' on conflict, or
`[COLUMN INSERT-VALUE [:update EXPRESSION]]' for an explicit update value."
  (unless (and (vectorp row) (> (length row) 0))
    (chidu-sql--error ":insert :row must be a nonempty vector"))
  (let ((seen (make-hash-table :test #'eq))
        columns values updates)
    (cl-loop
     for item across row
     do
     (unless (and (vectorp item) (memq (length item) '(2 3)))
       (chidu-sql--error
        ":insert :row item must be [column value &optional update], got %S"
        item))
     (let ((column (aref item 0))
           (update (and (= (length item) 3) (aref item 2))))
       (chidu-sql-identifier column)
       (when (gethash column seen)
         (chidu-sql--error ":insert :row repeats column %S" column))
       (unless (or (null update)
                   (eq update :update)
                   (and (vectorp update)
                        (= (length update) 2)
                        (eq (aref update 0) :update)))
         (chidu-sql--error "Invalid update policy for column %S: %S"
                           column update))
       (puthash column t seen)
       (push column columns)
       (push (aref item 1) values)
       (when update (push (cons column update) updates))))
    (list (vconcat (nreverse columns))
          (vconcat (nreverse values))
          (nreverse updates))))

(defun chidu-sql--insert-columns (columns context)
  "Return validated nonempty INSERT COLUMNS for CONTEXT."
  (unless (and (vectorp columns) (> (length columns) 0))
    (chidu-sql--error "%s requires a nonempty column vector" context))
  (let ((seen (make-hash-table :test #'eq)))
    (cl-loop
     for column across columns
     do
     (chidu-sql-identifier column)
     (when (gethash column seen)
       (chidu-sql--error "%s repeats column %S" context column))
     (puthash column t seen)))
  columns)

(defun chidu-sql--compile-row-update (entry)
  "Compile annotated upsert update ENTRY."
  (let ((column (car entry))
        (policy (cdr entry)))
    (if (eq policy :update)
        (format "%s = excluded.%s"
                (chidu-sql-identifier column)
                (chidu-sql-identifier column))
      (format "%s = %s"
              (chidu-sql-identifier column)
              (chidu-sql--compile-expression (aref policy 1))))))

(defun chidu-sql--compile-insert (form)
  "Compile INSERT or annotated UPSERT statement FORM."
  (let* ((arguments (append (seq-subseq form 1) nil))
         (table (chidu-sql--plist-value arguments :into))
         (columns (chidu-sql--plist-value arguments :columns))
         (values (chidu-sql--plist-value arguments :values))
         (row (chidu-sql--plist-value arguments :row))
         (or-action (chidu-sql--plist-value arguments :or))
         (conflict (chidu-sql--plist-value arguments :on-conflict))
         updates)
    (chidu-sql--validate-options
     arguments '(:into :columns :values :row :or :on-conflict) ":insert")
    (when (and or-action
               (not (memq or-action '(:replace :rollback :abort :fail :ignore))))
      (chidu-sql--error ":insert has invalid conflict action %S" or-action))
    (when (and or-action conflict)
      (chidu-sql--error ":insert cannot combine :or and :on-conflict"))
    (unless table (chidu-sql--error ":insert requires :into"))
    (when (and row (or columns values))
      (chidu-sql--error
       ":insert :row is mutually exclusive with :columns/:values"))
    (if row
        (pcase-let ((`(,row-columns ,row-values ,row-updates)
                     (chidu-sql--insert-row row)))
          (setq columns row-columns
                values (vector row-values)
                updates row-updates))
      (unless (and columns values)
        (chidu-sql--error
         ":insert requires :row or both :columns and :values")))
    (setq columns (chidu-sql--insert-columns columns ":insert :columns"))
    (unless (and (vectorp values) (> (length values) 0))
      (chidu-sql--error ":insert :values must be a nonempty vector"))
    (cl-loop for value-row across values
             unless (and (vectorp value-row)
                         (= (length value-row) (length columns)))
             do (chidu-sql--error
                 "INSERT values row must contain exactly %d values"
                 (length columns)))
    (when updates
      (unless conflict
        (chidu-sql--error "Annotated :update columns require :on-conflict")))
    (when conflict
      (unless row
        (chidu-sql--error ":on-conflict requires annotated :row syntax"))
      (unless updates
        (chidu-sql--error ":on-conflict requires at least one :update column"))
      (setq conflict
            (chidu-sql--insert-columns conflict ":insert :on-conflict"))
      (let ((insert-set (make-hash-table :test #'eq)))
        (cl-loop for column across columns do (puthash column t insert-set))
        (cl-loop for column across conflict
                 unless (gethash column insert-set)
                 do (chidu-sql--error
                     ":on-conflict column %S is absent from the inserted row"
                     column))))
    (concat
     (format "INSERT%s INTO %s (%s) VALUES %s"
             (if or-action
                 (format " OR %s"
                         (upcase (substring (symbol-name or-action) 1)))
               "")
             (chidu-sql-identifier table)
             (mapconcat #'chidu-sql-identifier (append columns nil) ", ")
             (mapconcat
              (lambda (value-row)
                (chidu-sql--compile-values-row value-row (length columns)))
              (append values nil) ", "))
     (when conflict
       (format " ON CONFLICT (%s) DO UPDATE SET %s"
               (mapconcat #'chidu-sql-identifier
                          (append conflict nil) ", ")
               (mapconcat #'chidu-sql--compile-row-update updates ", "))))))

(defun chidu-sql--compile-template (form)
  "Compile vector SQL FORM into a macro-expansion template."
  (unless (and (vectorp form) (> (length form) 0))
    (chidu-sql--error "Statement must be a nonempty vector, got %S" form))
  (let ((chidu-sql--hole-forms nil)
        sql)
    (setq sql
          (pcase (aref form 0)
            (:select (chidu-sql--compile-select form))
            (:update (chidu-sql--compile-update form))
            (:delete (chidu-sql--compile-delete form))
            (:insert (chidu-sql--compile-insert form))
            (_ (chidu-sql--error "Unknown statement operator %S"
                                 (aref form 0)))))
    (chidu-sql--template-create
     :sql sql :holes (vconcat (nreverse chidu-sql--hole-forms)))))

(defun chidu-sql-compile (form)
  "Compile parameter-free vector SQL FORM into a runtime statement."
  (let ((template (chidu-sql--compile-template form)))
    (unless (zerop (length (chidu-sql--template-holes template)))
      (chidu-sql--error
       "Bound SQL is macro-expansion-only; execute the literal form inline"))
    (chidu-sql-statement-create :sql (chidu-sql--template-sql template))))

(defmacro chidu-sql (form)
  "Compile static vector SQL FORM at macro expansion time."
  (declare (debug t))
  (let ((statement (chidu-sql-compile form)))
    `(chidu-sql-statement-create
      :sql ,(chidu-sql-statement-sql statement))))

(defun chidu-sql--static-sql (statement)
  "Return SQL from parameter-free compiled STATEMENT."
  (unless (chidu-sql-statement-p statement)
    (signal 'wrong-type-argument (list 'chidu-sql-statement-p statement)))
  (chidu-sql-statement-sql statement))

(defun chidu-sql--expand-execution (database form executor static-executor)
  "Expand DATABASE and SQL FORM for EXECUTOR or STATIC-EXECUTOR."
  (if (vectorp form)
      (let* ((template (chidu-sql--compile-template form))
             (holes (append (chidu-sql--template-holes template) nil))
             (database-symbol (make-symbol "chidu-sql-database"))
             (value-symbols
              (cl-loop for index below (length holes)
                       collect
                       (make-symbol (format "chidu-sql-value-%d" index)))))
        `(let* ((,database-symbol ,database)
                ,@(cl-mapcar (lambda (symbol value) `(,symbol ,value))
                             value-symbols holes))
           (,executor ,database-symbol
                      ,(chidu-sql--template-sql template)
                      (list ,@value-symbols))))
    `(,static-executor ,database ,form)))

(defun chidu-sql--select-static (database statement)
  "Execute parameter-free SELECT STATEMENT on DATABASE."
  (sqlite-select database (chidu-sql--static-sql statement)))

(defun chidu-sql--execute-static (database statement)
  "Execute parameter-free non-query STATEMENT on DATABASE."
  (sqlite-execute database (chidu-sql--static-sql statement)))

(defmacro chidu-sql-select (database form)
  "Execute literal SQL FORM on DATABASE with lexical `[:bind ...]' holes."
  (declare (indent 1) (debug t))
  (chidu-sql--expand-execution
   database form 'sqlite-select 'chidu-sql--select-static))

(defmacro chidu-sql-execute (database form)
  "Execute literal SQL FORM on DATABASE with lexical `[:bind ...]' holes."
  (declare (indent 1) (debug t))
  (chidu-sql--expand-execution
   database form 'sqlite-execute 'chidu-sql--execute-static))

(defun chidu-sql--map-query (form)
  "Return (PLAIN-FORM BINDINGS) for lexical-result SELECT FORM."
  (unless (and (vectorp form) (> (length form) 1)
               (eq (aref form 0) :select)
               (vectorp (aref form 1))
               (> (length (aref form 1)) 0))
    (chidu-sql--error
     "`chidu-sql-map' requires a literal SELECT with annotated columns"))
  (let ((seen nil)
        bindings
        expressions)
    (cl-loop
     for item across (aref form 1)
     do
     (unless (and (vectorp item) (= (length item) 2))
       (chidu-sql--error
        "Mapped SELECT column must be [binding expression], got %S" item))
     (let ((binding (aref item 0)))
       (unless (and (symbolp binding)
                    (not (keywordp binding))
                    (not (memq binding '(nil t))))
         (chidu-sql--error "Invalid mapped SELECT binding %S" binding))
       (when (memq binding seen)
         (chidu-sql--error "Mapped SELECT repeats binding %S" binding))
       (push binding seen)
       (push binding bindings)
       (push (aref item 1) expressions)))
    (let ((plain-form (copy-sequence form)))
      (aset plain-form 1 (vconcat (nreverse expressions)))
      (list plain-form (nreverse bindings)))))

(defun chidu-sql--expand-mapped-select (database form body one-p)
  "Expand mapped SELECT DATABASE, FORM, and BODY.

When ONE-P is non-nil, evaluate BODY for only the first row and return nil
when no row exists.  Otherwise return BODY results for every row."
  (unless body
    (chidu-sql--error
     "`%s' requires a result body"
     (if one-p "chidu-sql-one" "chidu-sql-map")))
  (pcase-let* ((`(,plain-form ,bindings) (chidu-sql--map-query form))
               (template (chidu-sql--compile-template plain-form))
               (holes (append (chidu-sql--template-holes template) nil))
               (database-symbol (make-symbol "chidu-sql-database"))
               (row-symbol (make-symbol "chidu-sql-row"))
               (value-symbols
                (cl-loop for index below (length holes)
                         collect
                         (make-symbol
                          (format "chidu-sql-value-%d" index))))
               (select-form
                `(sqlite-select ,database-symbol
                                ,(chidu-sql--template-sql template)
                                (list ,@value-symbols))))
    `(let* ((,database-symbol ,database)
            ,@(cl-mapcar (lambda (symbol value) `(,symbol ,value))
                         value-symbols holes))
       ,(if one-p
            `(let ((,row-symbol (car ,select-form)))
               (when ,row-symbol
                 (cl-destructuring-bind ,bindings ,row-symbol
                   ,@body)))
          `(mapcar
            (lambda (,row-symbol)
              (cl-destructuring-bind ,bindings ,row-symbol
                ,@body))
            ,select-form)))))

(defmacro chidu-sql-map (database form &rest body)
  "Execute literal SELECT FORM on DATABASE and evaluate BODY for each result row.

Each selected column must be `[BINDING SQL-EXPRESSION]'.  Runtime input holes
retain `[:bind ...]' semantics; result BINDING symbols are lexical only."
  (declare (indent 2) (debug t))
  (chidu-sql--expand-mapped-select database form body nil))

(defmacro chidu-sql-one (database form &rest body)
  "Execute literal SELECT FORM on DATABASE and evaluate BODY with its first row.

Return nil when the query returns no row.  Column annotations and lexical input
holes have the same semantics as `chidu-sql-map'."
  (declare (indent 2) (debug t))
  (chidu-sql--expand-mapped-select database form body t))

;;; Schema DSL

(defun chidu-sql--schema-type (value)
  "Compile schema type VALUE."
  (unless (keywordp value)
    (chidu-sql--error "Column type must be a keyword, got %S" value))
  (pcase value
    (:integer "INTEGER")
    (:real "REAL")
    (:text "TEXT")
    (:blob "BLOB")
    (:numeric "NUMERIC")
    (_ (chidu-sql--error "Unsupported column type %S" value))))

(defun chidu-sql--schema-action (value allowed context)
  "Compile schema action VALUE from ALLOWED for CONTEXT."
  (unless (memq value allowed)
    (chidu-sql--error "%s has invalid value %S" context value))
  (upcase (string-replace "-" " " (substring (symbol-name value) 1))))

(defun chidu-sql--schema-reference (form)
  "Compile column or table reference FORM."
  (unless (and (vectorp form) (>= (length form) 3)
               (eq (aref form 0) :references))
    (chidu-sql--error "Invalid :references form %S" form))
  (let ((table (aref form 1))
        (columns (aref form 2))
        (arguments (append (seq-subseq form 3) nil)))
    (unless (and (symbolp table) (vectorp columns) (> (length columns) 0))
      (chidu-sql--error "Invalid :references form %S" form))
    (chidu-sql--validate-options
     arguments '(:on-delete :on-update :match :deferrable :initially)
     ":references")
    (let ((deferrable (plist-get arguments :deferrable))
          (initially (plist-get arguments :initially)))
      (unless (memq deferrable '(nil t))
        (chidu-sql--error ":references :deferrable must be Boolean"))
      (when (and initially (not deferrable))
        (chidu-sql--error
         ":references :initially requires :deferrable t")))
    (concat
     (format "REFERENCES %s(%s)"
             (chidu-sql-identifier table)
             (mapconcat #'chidu-sql-identifier
                        (append columns nil) ", "))
     (when-let* ((action (plist-get arguments :on-delete)))
       (format " ON DELETE %s" (chidu-sql--schema-action
                                action
                                '(:cascade :restrict :set-null :set-default :no-action)
                                ":references :on-delete")))
     (when-let* ((action (plist-get arguments :on-update)))
       (format " ON UPDATE %s" (chidu-sql--schema-action
                                action
                                '(:cascade :restrict :set-null :set-default :no-action)
                                ":references :on-update")))
     (when-let* ((match (plist-get arguments :match)))
       (format " MATCH %s" (chidu-sql--schema-action
                            match '(:simple :partial :full) ":references :match")))
     (when (plist-get arguments :deferrable)
       " DEFERRABLE")
     (when-let* ((initially (plist-get arguments :initially)))
       (format " INITIALLY %s" (chidu-sql--schema-action
                                initially '(:deferred :immediate)
                                ":references :initially"))))))

(defun chidu-sql--schema-check (form)
  "Compile schema CHECK FORM."
  (unless (= (length form) 2)
    (chidu-sql--error ":check expects one expression"))
  (format "CHECK (%s)" (chidu-sql--compile-expression (aref form 1))))

(defun chidu-sql--schema-default (form)
  "Compile schema DEFAULT FORM."
  (unless (= (length form) 2)
    (chidu-sql--error ":default expects one value"))
  (format "DEFAULT %s" (chidu-sql--compile-expression (aref form 1))))

(defun chidu-sql--schema-column (form)
  "Compile schema column FORM."
  (unless (and (>= (length form) 3)
               (eq (aref form 0) :column))
    (chidu-sql--error "Invalid column form %S" form))
  (let ((parts
         (list (chidu-sql-identifier (aref form 1))
               (chidu-sql--schema-type (aref form 2)))))
    (cl-loop
     for modifier across (seq-subseq form 3)
     do
     (setq
      parts
      (append
       parts
       (list
        (pcase modifier
          (:primary-key "PRIMARY KEY")
          (:not-null "NOT NULL")
          (:unique "UNIQUE")
          ((pred vectorp)
           (pcase (aref modifier 0)
             (:check (chidu-sql--schema-check modifier))
             (:default (chidu-sql--schema-default modifier))
             (:references (chidu-sql--schema-reference modifier))
             (_ (chidu-sql--error "Unknown column modifier %S" modifier))))
          (_ (chidu-sql--error "Unknown column modifier %S" modifier)))))))
    (string-join parts " ")))

(defun chidu-sql--schema-column-list (value context)
  "Compile column vector VALUE for CONTEXT."
  (unless (and (vectorp value) (> (length value) 0))
    (chidu-sql--error "%s requires a nonempty column vector" context))
  (mapconcat #'chidu-sql-identifier (append value nil) ", "))

(defun chidu-sql--schema-table-constraint (form)
  "Compile table constraint FORM."
  (pcase (aref form 0)
    (:primary-key
     (format "PRIMARY KEY (%s)"
             (chidu-sql--schema-column-list (aref form 1) ":primary-key")))
    (:unique
     (format "UNIQUE (%s)"
             (chidu-sql--schema-column-list (aref form 1) ":unique")))
    (:check (chidu-sql--schema-check form))
    (:foreign-key
     (unless (and (>= (length form) 5)
                  (eq (aref form 2) :references))
       (chidu-sql--error "Invalid :foreign-key form %S" form))
     (let* ((local-columns (aref form 1))
            (reference
             (vconcat
              (vector :references (aref form 3) (aref form 4))
              (seq-subseq form 5))))
       (format "FOREIGN KEY (%s) %s"
               (chidu-sql--schema-column-list
                local-columns ":foreign-key")
               (chidu-sql--schema-reference reference))))
    (_ (chidu-sql--error "Unknown table constraint %S" form))))

(defun chidu-sql--schema-statement (sql)
  "Return parameter-free schema statement for SQL."
  (unless (null chidu-sql--hole-forms)
    (chidu-sql--error "Schema forms cannot contain [:bind]"))
  (chidu-sql-statement-create :sql sql))

(defun chidu-sql-compile-create-table (form)
  "Compile `[:table ...]' schema FORM into a statement."
  (unless (and (vectorp form) (> (length form) 2)
               (eq (aref form 0) :table))
    (chidu-sql--error "Invalid table form %S" form))
  (let ((chidu-sql--hole-forms nil)
        (name (aref form 1))
        (elements (append (seq-subseq form 2) nil)))
    (chidu-sql--schema-statement
     (format "CREATE TABLE IF NOT EXISTS %s (\n  %s\n)"
             (chidu-sql-identifier name)
             (mapconcat
              (lambda (element)
                (unless (and (vectorp element) (> (length element) 0))
                  (chidu-sql--error "Invalid table element %S" element))
                (if (eq (aref element 0) :column)
                    (chidu-sql--schema-column element)
                  (chidu-sql--schema-table-constraint element)))
              elements
              ",\n  ")))))

(defun chidu-sql-compile-create-index (form)
  "Compile `[:index ...]' schema FORM into a statement."
  (unless (and (vectorp form) (> (length form) 5)
               (eq (aref form 0) :index))
    (chidu-sql--error "Invalid index form %S" form))
  (let* ((chidu-sql--hole-forms nil)
         (name (aref form 1))
         (arguments (append (seq-subseq form 2) nil))
         (table (plist-get arguments :on))
         (columns (plist-get arguments :columns))
         (unique (plist-get arguments :unique))
         (where (plist-get arguments :where)))
    (chidu-sql--validate-options
     arguments '(:on :columns :unique :where) ":index")
    (unless (memq unique '(nil t))
      (chidu-sql--error ":index :unique must be Boolean"))
    (unless (and (symbolp table) (vectorp columns) (> (length columns) 0))
      (chidu-sql--error
       "Index requires :on TABLE :columns COLUMNS, got %S" form))
    (chidu-sql--schema-statement
     (concat
      (format "CREATE %sINDEX IF NOT EXISTS %s ON %s(%s)"
              (if unique "UNIQUE " "")
              (chidu-sql-identifier name)
              (chidu-sql-identifier table)
              (mapconcat #'chidu-sql-identifier
                         (append columns nil) ", "))
      (when where
        (format " WHERE %s" (chidu-sql--compile-expression where)))))))

(defun chidu-sql-schema-statements (schema)
  "Compile SCHEMA vector into CREATE statements."
  (unless (vectorp schema)
    (signal 'wrong-type-argument (list 'vectorp schema)))
  (cl-loop
   for form across schema
   collect
   (pcase (and (vectorp form) (> (length form) 0) (aref form 0))
     (:table (chidu-sql-compile-create-table form))
     (:index (chidu-sql-compile-create-index form))
     (_ (chidu-sql--error "Unknown schema form %S" form)))))

(defun chidu-sql-schema-table-names (schema)
  "Return table names declared by SCHEMA."
  (cl-loop for form across schema
           when (eq (aref form 0) :table)
           collect (chidu-sql-identifier (aref form 1))))

(defun chidu-sql-schema-index-names (schema)
  "Return explicit index names declared by SCHEMA."
  (cl-loop for form across schema
           when (eq (aref form 0) :index)
           collect (chidu-sql-identifier (aref form 1))))

(defun chidu-sql-normalize (sql)
  "Return whitespace- and case-normalized token list for SQL.

Quoted string contents remain case-sensitive.  This is intended for comparing
SQLite schema SQL, not for executing or validating arbitrary user input."
  (unless (stringp sql)
    (signal 'wrong-type-argument (list 'stringp sql)))
  (let ((length (length sql))
        (index 0)
        tokens)
    (cl-labels
        ((peek (&optional offset)
           (let ((position (+ index (or offset 0))))
             (and (< position length) (aref sql position))))
         (take-while (predicate)
           (let ((start index))
             (while (and (< index length)
                         (funcall predicate (aref sql index)))
               (cl-incf index))
             (substring sql start index))))
      (while (< index length)
        (let ((character (peek)))
          (cond
           ((memq character '(?\s ?\t ?\r ?\n))
            (cl-incf index))
           ((eq character ?\')
            (let ((start index)
                  done)
              (cl-incf index)
              (while (and (< index length) (not done))
                (if (eq (peek) ?\')
                    (if (eq (peek 1) ?\')
                        (cl-incf index 2)
                      (cl-incf index)
                      (setq done t))
                  (cl-incf index)))
              (unless done
                (chidu-sql--error "Unterminated SQL string literal"))
              (push (substring sql start index) tokens)))
           ((memq character '(?\" ?` ?\[))
            (let* ((closing (if (eq character ?\[) ?\] character))
                   (pieces nil)
                   done)
              (cl-incf index)
              (while (and (< index length) (not done))
                (let ((value (peek)))
                  (if (eq value closing)
                      (if (eq (peek 1) closing)
                          (progn
                            (push closing pieces)
                            (cl-incf index 2))
                        (cl-incf index)
                        (setq done t))
                    (push value pieces)
                    (cl-incf index))))
              (unless done
                (chidu-sql--error "Unterminated SQL identifier"))
              (push (downcase (apply #'string (nreverse pieces))) tokens)))
           ((or (and (>= character ?a) (<= character ?z))
                (and (>= character ?A) (<= character ?Z))
                (memq character '(?_ ?$)))
            (push
             (downcase
              (take-while
               (lambda (value)
                 (or (and (>= value ?a) (<= value ?z))
                     (and (>= value ?A) (<= value ?Z))
                     (and (>= value ?0) (<= value ?9))
                     (memq value '(?_ ?$))))))
             tokens))
           ((and (>= character ?0) (<= character ?9))
            (push
             (take-while
              (lambda (value)
                (or (and (>= value ?0) (<= value ?9))
                    (memq value '(?. ?e ?E ?+ ?-)))))
             tokens))
           ((and (memq character '(?< ?> ?! ?=))
                 (eq (peek 1) ?=))
            (push (substring sql index (+ index 2)) tokens)
            (cl-incf index 2))
           ((and (eq character ?<) (eq (peek 1) ?>))
            (push "<>" tokens)
            (cl-incf index 2))
           ((eq character ?\;)
            (cl-incf index))
           (t
            (push (char-to-string character) tokens)
            (cl-incf index)))))
      (nreverse tokens))))

(provide 'chidu-sql)

;;; chidu-sql.el ends here
