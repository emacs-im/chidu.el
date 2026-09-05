;;; chidu-transient-test.el --- Tests for Chidu command menus -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'seq)
(require 'transient)
(require 'chidu)
(require 'chidu-conversation)
(require 'chidu-message)
(require 'chidu-search)
(require 'chidu-transient)

(defun chidu-transient-test--prefix-has-command-p (prefix command)
  "Return non-nil when PREFIX contains COMMAND."
  (seq-find
   (lambda (suffix) (eq (oref suffix command) command))
   (transient-suffixes prefix)))

(defun chidu-transient-test--active-suffix (command)
  "Return active Transient suffix object for COMMAND."
  (seq-find
   (lambda (suffix) (eq (oref suffix command) command))
   transient--suffixes))

(defun chidu-transient-test--invoke-active-suffix (command)
  "Invoke active suffix COMMAND through Transient's real keymap."
  (let ((suffix
         (or (chidu-transient-test--active-suffix command)
             (ert-fail (format "No active suffix for %S" command)))))
    (execute-kbd-macro (kbd (oref suffix key)))))

(ert-deftest chidu-transient-prefixes-are-contextual-commands ()
  (dolist (command '(chidu-dispatch
                     chidu-home-transient
                     chidu-list-transient
                     chidu-conversation-transient
                     chidu-message-transient))
    (should (commandp command)))
  (dolist (case '((chidu-home-mode . chidu-home-transient)
                  (chidu-summary-mode . chidu-list-transient)
                  (chidu-search-mode . chidu-list-transient)
                  (chidu-conversation-mode . chidu-conversation-transient)
                  (chidu-message-mode . chidu-message-transient)))
    (with-temp-buffer
      (setq major-mode (car case))
      (let (called)
        (cl-letf (((symbol-function 'call-interactively)
                   (lambda (command &optional _record _keys)
                     (setq called command))))
          (chidu-dispatch))
        (should (eq called (cdr case)))))))

(ert-deftest chidu-message-transients-share-attachment-suffixes ()
  (dolist (prefix '(chidu-conversation-transient chidu-message-transient))
    (should
     (chidu-transient-test--prefix-has-command-p
      prefix #'chidu-transient-attachment-toggle-inline))))

(ert-deftest chidu-list-transient-preserves-active-region-plan ()
  (with-temp-buffer
    (let ((start (point)))
      (insert "One\n")
      (add-text-properties start (point) '(chidu-test-email-id "one")))
    (let ((start (point)))
      (insert "Two\n")
      (add-text-properties start (point) '(chidu-test-email-id "two")))
    (let ((start (point)))
      (insert "Three\n")
      (add-text-properties start (point) '(chidu-test-email-id "three")))
    (setq major-mode 'chidu-summary-mode)
    (chidu-selection-setup 'chidu-test-email-id #'ignore)
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 2)
    (setq transient-mark-mode t
          mark-active t)
    (let ((scope (chidu-transient--capture-list-scope))
          selected)
      ;; Transient or Evil may deactivate the live region after the prefix is
      ;; invoked.  Suffixes still receive the stable ids captured above.
      (setq mark-active nil)
      (cl-letf (((symbol-function 'transient-scope)
                 (lambda (&rest _arguments) scope))
                ((symbol-function 'chidu-mark-read)
                 (lambda ()
                   (interactive)
                   (setq selected (chidu-selection-selected-ids)))))
        (chidu-list-mark-read))
      (should (equal '("one" "two") selected)))))

(ert-deftest chidu-list-transient-captures-region-for-marker-edits-with-marks ()
  (with-temp-buffer
    (dolist (row '(("one" . "One") ("two" . "Two") ("three" . "Three")))
      (let ((start (point)))
        (insert (cdr row) "\n")
        (add-text-properties
         start (point) (list 'chidu-test-email-id (car row)))))
    (setq major-mode 'chidu-summary-mode)
    (chidu-selection-setup 'chidu-test-email-id #'ignore)
    ;; An existing ordinary mark remains the mail-operation target.
    (puthash "three" t chidu-selection--marked-ids)
    ;; The explicit region remains the marker-edit target.
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 2)
    (setq transient-mark-mode t
          mark-active t)
    (let ((scope (chidu-transient--capture-list-scope)))
      (should-not (chidu-transient-list-scope-region-ids scope))
      (should (equal '("one" "two")
                     (chidu-transient-list-scope-edit-region-ids scope)))
      (setq mark-active nil)
      (cl-letf (((symbol-function 'transient-scope)
                 (lambda (&rest _arguments) scope))
                ((symbol-function 'chidu-summary-flag-trash)
                 (lambda ()
                   (interactive)
                   (chidu-selection-flag-trash))))
        (chidu-list-flag-trash))
      (should (equal '("one" "two")
                     (chidu-selection-trash-flagged-ids)))
      (should (equal '("three") (chidu-selection-marked-ids))))))

(ert-deftest chidu-list-transient-refreshes-suffix-aptness ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *chidu transient refresh test*")))
      (unwind-protect
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (setq major-mode 'chidu-search-mode)
            (chidu-selection-setup 'chidu-test-email-id #'ignore)
            (let ((start (point)))
              (insert "One\n")
              (add-text-properties
               start (point) '(chidu-test-email-id "one")))
            (goto-char (point-min))
            (call-interactively #'chidu-list-transient)
            (should (transient-active-prefix 'chidu-list-transient))
            (should
             (chidu-transient-list-scope-p
              (transient-scope 'chidu-list-transient)))
            (should
             (oref
              (chidu-transient-test--active-suffix
               'chidu-list-execute-trash-flags)
              inapt))
            ;; `d' stays transient.  `:refresh-suffixes' must rebuild the
            ;; suffix objects so `x' becomes callable immediately.
            (chidu-transient-test--invoke-active-suffix
             'chidu-list-flag-trash)
            (should (transient-active-prefix 'chidu-list-transient))
            (should (equal '("one")
                           (chidu-selection-trash-flagged-ids)))
            (should-not
             (oref
              (chidu-transient-test--active-suffix
               'chidu-list-execute-trash-flags)
              inapt))
            (should
             (equal
              "Move 1 flagged Email to Trash"
              (funcall
               (oref
                (chidu-transient-test--active-suffix
                 'chidu-list-execute-trash-flags)
                description))))
            ;; Clearing markers also stays transient and re-disables `x'.
            (chidu-transient-test--invoke-active-suffix
             'chidu-list-clear-markers)
            (should (transient-active-prefix 'chidu-list-transient))
            (should (zerop (chidu-selection-marker-count)))
            (should
             (oref
              (chidu-transient-test--active-suffix
               'chidu-list-execute-trash-flags)
              inapt))
            (transient-quit-all))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chidu-list-transient-never-falls-back-from-dead-scope ()
  (let* ((buffer (generate-new-buffer " *chidu dead transient source*"))
         (scope
          (chidu-transient-list-scope-create :buffer buffer)))
    (kill-buffer buffer)
    (cl-letf (((symbol-function 'transient-scope)
               (lambda (&rest _arguments) scope)))
      (should-error
       (call-interactively #'chidu-list-mark-read)
       :type 'user-error))))

(ert-deftest chidu-list-transient-disables-trash-flagging-in-trash-summary ()
  (with-temp-buffer
    (setq major-mode 'chidu-summary-mode)
    (chidu-selection-setup 'chidu-test-email-id #'ignore)
    (let ((start (point)))
      (insert "One\n")
      (add-text-properties start (point) '(chidu-test-email-id "one")))
    (let ((scope
           (chidu-transient-list-scope-create
            :buffer (current-buffer))))
      (cl-letf (((symbol-function 'transient-scope)
                 (lambda (&rest _arguments) scope))
                ((symbol-function
                  'chidu-summary-trash-staging-available-p)
                 (lambda (&optional _view) nil)))
        (should (chidu-transient--flag-trash-inapt-p))))))

(provide 'chidu-transient-test)

;;; chidu-transient-test.el ends here
