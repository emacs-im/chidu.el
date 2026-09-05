;;; chidu-record.el --- Closed records for Chidu -*- lexical-binding: t; -*-

;;; Commentary:

;; Small immutable-ish record helper used by Chidu domain code.  It only
;; removes constructor/copy boilerplate; state-machine control flow remains
;; explicit in ordinary functions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(define-error 'chidu-invariant-error
              "Chidu invariant violation")

(define-error 'chidu-overloaded
              "Chidu bounded resource is full")

(defmacro chidu-define-record (name docstring &rest slots)
  "Define immutable-ish record NAME with DOCSTRING and SLOTS.

Each element of SLOTS is either a symbol or (SLOT DEFAULT).  All slots are
`:read-only' through their generated accessors.  In addition to the usual
predicate and accessors, define NAME-create and NAME-with.  NAME-with returns
a new record with the keyword changes supplied in its argument plist.

This macro protects the normal update path; nested lists, vectors, hash tables,
and other referenced objects must still be treated as immutable by policy."
  (declare (indent 2) (debug (symbolp stringp &rest sexp)))
  (let* ((slot-names (mapcar (lambda (slot) (if (consp slot) (car slot) slot))
                             slots))
         (slot-forms
          (mapcar (lambda (slot)
                    (let ((slot-name (if (consp slot) (car slot) slot))
                          (default (and (consp slot) (cadr slot))))
                      `(,slot-name ,default :read-only t)))
                  slots))
         (constructor (intern (format "%s-create" name)))
         (with-function (intern (format "%s-with" name)))
         (predicate (intern (format "%s-p" name)))
         (allowed-keys (mapcar (lambda (slot) (intern (format ":%s" slot)))
                               slot-names)))
    `(progn
       (cl-defstruct (,name (:constructor ,constructor))
         ,docstring
         ,@slot-forms)
       (defun ,with-function (object &rest changes)
         "Return a copy of OBJECT with keyword CHANGES."
         (unless (,predicate object)
           (signal 'wrong-type-argument (list ',predicate object)))
         (unless (and (proper-list-p changes) (cl-evenp (length changes)))
           (signal 'chidu-invariant-error
                   (list "Record changes must be a proper plist" ',name changes)))
         (let ((tail changes)
               (seen nil))
           (while tail
             (let ((key (pop tail)))
               (pop tail)
               (unless (memq key ',allowed-keys)
                 (signal 'chidu-invariant-error
                         (list "Unknown record slot" ',name key)))
               (when (memq key seen)
                 (signal 'chidu-invariant-error
                         (list "Duplicate record slot" ',name key)))
               (push key seen))))
         (,constructor
          ,@(cl-loop
             for slot in slot-names
             for key = (intern (format ":%s" slot))
             for accessor = (intern (format "%s-%s" name slot))
             append
             (list key `(if (plist-member changes ,key)
                            (plist-get changes ,key)
                          (,accessor object)))))))))

(provide 'chidu-record)

;;; chidu-record.el ends here
