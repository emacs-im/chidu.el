;;; chidu-surface-operation.el --- Surface-owned runtime operations -*- lexical-binding: t; -*-

;;; Commentary:

;; Nest opaque Chidu runtime operations under keyed Appkit Surface effects.

;;; Code:

(require 'appkit-core)
(require 'appkit-surface)
(require 'chidu-runtime)

(defvar chidu--transition-context nil
  "Bound while a Chidu reducer collects closed post-commit commands.")

(defvar chidu--transition-commands nil
  "Commands emitted by domain work in the current Chidu transition.")

(defun chidu-surface-runtime (surface)
  "Return the live Chidu runtime owning SURFACE."
  (or (and (appkit-surface-live-p surface)
           (chidu-app-runtime (appkit-surface-app surface)))
      (error "Chidu reader has no live runtime")))

(defun chidu-surface-operation-start
    (surface key start-function success-function error-function)
  "Request keyed runtime work in the exact owning SURFACE."
  (chidu-post-surface-message surface
                              (list 'chidu-operation 'start key
                                    start-function success-function
                                    error-function)))

(defun chidu-surface-operation-cancel (surface key)
  "Cancel SURFACE's runtime operation KEY after committing its intent."
  (chidu-post-surface-message surface
                              (list 'chidu-operation 'cancel key)))

(defun chidu-surface-update (context model message)
  "Dispatch common reader operations and media intent under CONTEXT."
  (let
      ((chidu--transition-context context)
       (chidu--transition-commands nil))
    (let
        ((next
          (progn
            (pcase message
              (`(chidu-reader replace ,replacement)
               (appkit-next :model replacement :render t))
              (`(chidu-refresh) (appkit-next :model model :render t))
              (`(chidu-operation start ,key ,start ,success ,failure)
               (let
                   ((runtime
                     (chidu-surface-runtime (appkit-current-surface))))
                 (appkit-next :model model :render appkit-render-none
                              :commands
                              (list
                               (appkit-command-start-effect
                                (appkit-effect-create :key
                                                      (list
                                                       'chidu-operation
                                                       key)
                                                      :start
                                                      (lambda
                                                        (_context
                                                         _input
                                                         _observe
                                                         resolve
                                                         reject)
                                                        (let
                                                            ((operation
                                                              (funcall
                                                               start
                                                               runtime
                                                               (lambda
                                                                 (&rest
                                                                  values)
                                                                 (funcall
                                                                  resolve
                                                                  values))
                                                               (lambda
                                                                 (&rest
                                                                  values)
                                                                 (funcall
                                                                  reject
                                                                  values)))))
                                                          (appkit-cancellation-create
                                                           :kind
                                                           'logical
                                                           :cancel
                                                           (lambda ()
                                                             (chidu-runtime-cancel-operation
                                                              runtime
                                                              operation)))))
                                                      :success
                                                      (lambda (_input values)
                                                        (list
                                                         'chidu-operation
                                                         'settled
                                                         success
                                                         values))
                                                      :failure
                                                      (lambda (_input values)
                                                        (list
                                                         'chidu-operation
                                                         'settled
                                                         failure
                                                         values))))))))
              (`(chidu-operation cancel ,key)
               (appkit-next :model model :render appkit-render-none
                            :commands
                            (list
                             (appkit-command-cancel-effect
                              (list 'chidu-operation key)))))
              (`(chidu-operation settled ,function ,values)
               (apply function values)
               (appkit-next :model model :render t))
              (`(chidu-attachment \, _)
               (chidu-attachment-surface-update context model message))
              (_
               (appkit-next-reject
                (list 'unknown-chidu-message message)))))))
      (when (appkit-next-p next)
        (setf (appkit-next-commands next)
              (append (appkit-next-commands next)
                      (nreverse chidu--transition-commands))))
      next)))

(defun chidu-surface-refresh (surface)
  "Schedule presentation of SURFACE's committed domain model."
  (when (appkit-surface-live-p surface)
    (chidu-post-surface-message surface '(chidu-refresh))))

(defun chidu-post-surface-message (surface message)
  "Schedule MESSAGE for exact SURFACE, respecting active transition boundaries."
  (if chidu--transition-context
      (push (appkit-command-post-message
             :target (appkit-routing--address (appkit-surface-loop surface))
             :message message :delivery 'report)
            chidu--transition-commands)
    (appkit-surface-post surface message)))

(defun chidu-post-app-message (app message)
  "Schedule MESSAGE for APP, respecting active transition boundaries."
  (if chidu--transition-context
      (push
       (appkit-command-post-message :target
                                    (appkit-routing--address
                                     (appkit-app-loop app))
                                    :message message :delivery 'report)
       chidu--transition-commands)
    (appkit-app-send app message)))

(provide 'chidu-surface-operation)

;;; chidu-surface-operation.el ends here
