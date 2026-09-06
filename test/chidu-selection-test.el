;;; chidu-selection-test.el --- Tests for Chidu process selection -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'ert)
(require 'chidu-test-support)
(require 'chidu)
(require 'chidu-search)
(require 'chidu-selection)
(require 'chidu-summary)

(defun chidu-selection-test--insert-row (id label)
  "Insert one selectable ID row containing LABEL."
  (let ((start (point)))
    (insert label "\n")
    (add-text-properties
     start (point) (list 'chidu-test-email-id id))))

(defun chidu-selection-test--next-row ()
  "Move to the next synthetic selectable row when one exists."
  (let ((start (point)))
    (forward-line 1)
    (unless (chidu-selection-row-id-at-point)
      (goto-char start))))

(defun chidu-selection-test--previous-row ()
  "Move to the previous synthetic selectable row when one exists."
  (let ((start (point)))
    (forward-line -1)
    (unless (chidu-selection-row-id-at-point)
      (goto-char start))))

(defmacro chidu-selection-test--with-buffer (&rest body)
  "Evaluate BODY in a configured three-row selection buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let (refresh-count)
       (chidu-selection-test--insert-row "one" "One")
       (chidu-selection-test--insert-row "two" "Two")
       (chidu-selection-test--insert-row "three" "Three")
       (chidu-selection-setup
        'chidu-test-email-id
        (lambda (_ids) (setq refresh-count (1+ (or refresh-count 0))))
        #'chidu-selection-test--next-row
        #'chidu-selection-test--previous-row)
       (goto-char (point-min))
       ,@body)))

(ert-deftest chidu-selection-resolves-marks-region-then-point ()
  (chidu-selection-test--with-buffer
    (forward-line 1)
    (should (equal '("two") (chidu-selection-selected-ids)))
    (should (eq 'point (chidu-selection-target-source)))
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 2)
    (setq transient-mark-mode t
          mark-active t)
    (should (equal '("one" "two")
                   (chidu-selection-selected-ids)))
    (should (eq 'region (chidu-selection-target-source)))
    (should (equal "2 Emails in region"
                   (chidu-selection-description)))
    (puthash "three" t chidu-selection--marked-ids)
    (should (equal '("three")
                   (chidu-selection-selected-ids)))
    (should (eq 'marks (chidu-selection-target-source)))
    (should (equal "1 marked Email"
                   (chidu-selection-description)))
    (should (equal '("one" "two")
                   (chidu-selection-selected-ids 'marks)))
    (should (equal '("three")
                   (chidu-selection-marked-ids)))))

(ert-deftest chidu-selection-list-marking-advances-and-manages-visible-set ()
  (chidu-selection-test--with-buffer
    (chidu-selection-mark)
    (should (equal "two" (chidu-selection-row-id-at-point)))
    (chidu-selection-mark)
    (should (equal "three" (chidu-selection-row-id-at-point)))
    (should (equal '("one" "two") (chidu-selection-marked-ids)))

    (goto-char (point-min))
    (chidu-selection-unmark)
    (should (equal "two" (chidu-selection-row-id-at-point)))
    (should (equal '("two") (chidu-selection-marked-ids)))

    (goto-char (point-min))
    (forward-line 2)
    (chidu-selection-toggle)
    (should (equal '("two" "three") (chidu-selection-marked-ids)))
    (goto-char (point-min))
    (chidu-selection-next-marked)
    (should (equal "two" (chidu-selection-row-id-at-point)))
    (chidu-selection-next-marked)
    (should (equal "three" (chidu-selection-row-id-at-point)))
    (chidu-selection-previous-marked)
    (should (equal "two" (chidu-selection-row-id-at-point)))

    (chidu-selection-mark-all)
    (should (equal '("one" "two" "three")
                   (chidu-selection-marked-ids)))
    (chidu-selection-toggle-all)
    (should-not (chidu-selection-marked-ids))
    (chidu-selection-toggle-all)
    (should (= 3 (chidu-selection-count)))
    (chidu-selection-clear)
    (should (zerop (chidu-selection-count)))

    (puthash "three" t chidu-selection--marked-ids)
    (should (= 1 (chidu-selection-prune-to-ids '("one" "two"))))
    (should (zerop (chidu-selection-count)))
    (should (> refresh-count 0))))

(ert-deftest chidu-selection-region-marking-does-not-advance-point ()
  (chidu-selection-test--with-buffer
    (set-mark (point))
    (forward-line 2)
    (setq transient-mark-mode t
          mark-active t)
    (let ((position (point)))
      (chidu-selection-mark)
      (should (= position (point))))
    (should (equal '("one" "two")
                   (chidu-selection-marked-ids)))))

(ert-deftest chidu-selection-trash-flags-follow-dired-marker-semantics ()
  (chidu-selection-test--with-buffer
    ;; `d' stages a distinct D flag and advances without creating an ordinary
    ;; process mark.
    (chidu-selection-flag-trash)
    (should (equal "two" (chidu-selection-row-id-at-point)))
    (should (equal '("one") (chidu-selection-trash-flagged-ids)))
    (should-not (chidu-selection-marked-ids))
    (should (= 1 (chidu-selection-marker-count)))

    ;; Dired's toggle operation leaves D flags untouched.
    (goto-char (point-min))
    (chidu-selection-toggle)
    (should (equal '("one") (chidu-selection-trash-flagged-ids)))
    (should-not (chidu-selection-marked-ids))

    ;; `m' overwrites D with the ordinary `*' marker.
    (goto-char (point-min))
    (chidu-selection-mark)
    (should-not (chidu-selection-trash-flagged-ids))
    (should (equal '("one") (chidu-selection-marked-ids)))
    (goto-char (point-max))
    (funcall (chidu-selection-icon-inserter "one"))
    (should (string-suffix-p "*" (buffer-string)))

    ;; `d' overwrites the ordinary mark, and `u' clears either kind.
    (goto-char (point-min))
    (chidu-selection-flag-trash)
    (should-not (chidu-selection-marked-ids))
    (should (equal '("one") (chidu-selection-trash-flagged-ids)))
    (goto-char (point-min))
    (chidu-selection-unmark)
    (should-not (chidu-selection-marked-ids))
    (should-not (chidu-selection-trash-flagged-ids))

    ;; Toggle-all ignores D rows; mark-all deliberately overwrites them.
    (goto-char (point-min))
    (chidu-selection-flag-trash)
    (chidu-selection-toggle-all)
    (should (equal '("one") (chidu-selection-trash-flagged-ids)))
    (should (equal '("two" "three") (chidu-selection-marked-ids)))
    (chidu-selection-mark-all)
    (should-not (chidu-selection-trash-flagged-ids))
    (should (equal '("one" "two" "three")
                   (chidu-selection-marked-ids)))
    (goto-char (point-min))
    (chidu-selection-flag-trash)
    (chidu-selection-clear)
    (should (zerop (chidu-selection-marker-count)))))

(ert-deftest chidu-selection-captured-region-plan-does-not-advance ()
  (chidu-selection-test--with-buffer
    (goto-char (point-min))
    (let ((chidu-selection-command-ids '("one" "two")))
      (chidu-selection-flag-trash))
    (should (equal "one" (chidu-selection-row-id-at-point)))
    (should (equal '("one" "two")
                   (chidu-selection-trash-flagged-ids)))))

(defun chidu-selection-test--marker-surface-data
    (surface account mailbox summary-row)
  "Return projection setup data for SURFACE using one SUMMARY-ROW."
  (pcase surface
    ('summary
     (list
      :mode 'chidu-summary-mode
      :state
      (chidu-summary-state-create
       :account account
       :mailbox mailbox
       :context
       (chidu-store-mailbox-summary-context-create
        :account account :mailbox mailbox :rows (vector summary-row)))
      :update #'chidu-summary--update
      :renderer #'chidu-summary--renderer

      :anchor 'chidu-summary-email-id))
    ('search
     (let ((spec
            (chidu-search-query-compile "One" (vector mailbox))))
       (list
        :mode 'chidu-search-mode
        :state
        (chidu-search-state-create
         :account account
         :mailboxes (vector mailbox)
         :spec spec
         :phase 'idle
         :context
         (chidu-store-search-context-create
          :account account
          :query-key "query"
          :query-text "One"
          :filter-json "{}"
          :rows
          (vector
           (chidu-store-search-row-create
            :summary-row summary-row
            :remote-mailbox-ids
            (vector
             (chidu-store-mailbox-remote-mailbox-id mailbox))))))
        :update #'chidu-search--update
        :renderer #'chidu-search--renderer

        :anchor 'chidu-search-email-id)))))

(ert-deftest chidu-list-markers-redraw-retained-summary-and-search-rows ()
  (dolist (surface '(summary search))
    (let* ((local-id "11111111-1111-4111-8111-111111111111")
           (app
            (chidu-test-app-create nil))
           (account
            (chidu-store-account-create
             :account-id "account-local"
             :remote-account-id "account-remote"
             :name "Mail"
             :available-p t
             :read-only-p nil))
           (mailbox
            (chidu-store-mailbox-create
             :mailbox-id "mailbox-local"
             :remote-mailbox-id "inbox"
             :name "Inbox"
             :role "inbox"
             :available-p t))
           (summary-row
            (chidu-store-email-summary-row-create
             :local-email-id local-id
             :remote-email-id "remote-one"
             :remote-thread-id "thread-one"
             :received-at "2026-08-25T00:00:00Z"
             :subject "One"
             :preview ""
             :unread-p t))
           (data
            (chidu-selection-test--marker-surface-data
             surface account mailbox summary-row))
           (view
            (appkit-open-generated-surface (appkit-surface-type-create :name surface :mode (plist-get data :mode) :init (lambda (_context input) (appkit-next :model input :render (appkit-projection-change-create :full-p t :frame-p t))) :update (plist-get data :update) :renderer-factory (plist-get data :renderer)) :app app :identity (list 'marker-render surface) :input (plist-get data :state))))
      (unwind-protect
          (with-current-buffer (appkit-surface-buffer view)
            (goto-char
             (or
              (text-property-not-all
               (point-min) (point-max) (plist-get data :anchor) nil)
              (ert-fail "projected Email row is missing")))
            (chidu-selection-mark)
            (chidu-test-drain view)
            (goto-char
             (text-property-not-all
              (point-min) (point-max) (plist-get data :anchor) nil))
            (should (get-text-property
                     (point) 'chidu-selection-marked-p))
            (should
             (string-match-p
              "\\*"
              (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
            (chidu-selection-flag-trash)
            (chidu-test-drain view)
            (goto-char
             (text-property-not-all
              (point-min) (point-max) (plist-get data :anchor) nil))
            (should-not
             (get-text-property (point) 'chidu-selection-marked-p))
            (should
             (get-text-property
              (point) 'chidu-selection-trash-flagged-p))
            (should
             (string-match-p
              "D"
              (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)))))
        (when (appkit-app-live-p app)
          (appkit-app-close app))
        (when (buffer-live-p (appkit-surface-buffer view)) (kill-buffer (appkit-surface-buffer view)))))))

(ert-deftest chidu-summary-trash-stages-before-explicit-execution ()
  (let* ((id-one "11111111-1111-4111-8111-111111111111")
         (id-two "22222222-2222-4222-8222-222222222222")
         (app (chidu-test-app-create nil))
         (account
          (chidu-store-account-create
           :account-id "account-local" :remote-account-id "account-remote"
           :name "Mail" :available-p t :read-only-p nil))
         (mailbox
          (chidu-store-mailbox-create
           :mailbox-id "mailbox-local" :remote-mailbox-id "inbox"
           :name "Inbox" :role "inbox" :available-p t))
         (rows
          (vector
           (chidu-store-email-summary-row-create
            :local-email-id id-one :remote-email-id "remote-one"
            :remote-thread-id "thread-one" :received-at "2026-08-25T00:00:00Z"
            :subject "One" :preview "" :unread-p t)
           (chidu-store-email-summary-row-create
            :local-email-id id-two :remote-email-id "remote-two"
            :remote-thread-id "thread-two" :received-at "2026-08-25T00:00:01Z"
            :subject "Two" :preview "" :unread-p t)))
         (state
          (chidu-summary-state-create
           :account account :mailbox mailbox
           :context
           (chidu-store-mailbox-summary-context-create
            :account account :mailbox mailbox :rows rows)))
         executed)
    (unwind-protect
        (with-temp-buffer
          (appkit-open-generated-surface (appkit-surface-type-create :name 'selection :mode #'chidu-summary-mode :init (lambda (_context input) (appkit-next :model input :render (appkit-projection-change-create :full-p t :frame-p t))) :update #'chidu-summary--update :renderer-factory #'chidu-summary--renderer) :app app :identity 'selection :input state :buffer (current-buffer))
          (goto-char (text-property-not-all (point-min) (point-max) 'chidu-summary-email-id nil))
          (cl-letf (((symbol-function 'chidu-summary--trash-ids)
                     (lambda (_view _state ids) (setq executed ids))))
            (chidu-summary-flag-trash)
            (should-not executed)
            (should (equal (list id-one)
                           (chidu-selection-trash-flagged-ids)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (chidu-summary-execute-trash-flags))
            (should (equal (vector id-one) executed))))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

(ert-deftest chidu-summary-process-marks-drive-bulk-read-state ()
  (let* ((id-one "11111111-1111-4111-8111-111111111111")
         (id-two "22222222-2222-4222-8222-222222222222")
         (id-three "33333333-3333-4333-8333-333333333333")
         (app (chidu-test-app-create nil))
         (account
          (chidu-store-account-create
           :account-id "account-local" :remote-account-id "account-remote"
           :name "Mail" :available-p t :read-only-p nil))
         (mailbox
          (chidu-store-mailbox-create
           :mailbox-id "mailbox-local" :remote-mailbox-id "inbox"
           :name "Inbox" :available-p t))
         (rows
          (vector
           (chidu-store-email-summary-row-create
            :local-email-id id-one :remote-email-id "remote-one"
            :remote-thread-id "thread-one" :received-at "2026-08-25T00:00:00Z"
            :subject "One" :preview "" :unread-p t)
           (chidu-store-email-summary-row-create
            :local-email-id id-two :remote-email-id "remote-two"
            :remote-thread-id "thread-two" :received-at "2026-08-25T00:00:01Z"
            :subject "Two" :preview "" :unread-p t)
           (chidu-store-email-summary-row-create
            :local-email-id id-three :remote-email-id "remote-three"
            :remote-thread-id "thread-three" :received-at "2026-08-25T00:00:02Z"
            :subject "Three" :preview "" :unread-p t)))
         (state
          (chidu-summary-state-create
           :account account :mailbox mailbox
           :context
           (chidu-store-mailbox-summary-context-create
            :account account :mailbox mailbox :rows rows)))
         called)
    (unwind-protect
        (with-temp-buffer
          (appkit-open-generated-surface (appkit-surface-type-create :name 'selection :mode #'chidu-summary-mode :init (lambda (_context input) (appkit-next :model input :render (appkit-projection-change-create :full-p t :frame-p t))) :update #'chidu-summary--update :renderer-factory #'chidu-summary--renderer) :app app :identity 'selection :input state :buffer (current-buffer))
          (puthash id-one t chidu-selection--marked-ids)
          (puthash id-three t chidu-selection--marked-ids)
          (cl-letf (((symbol-function 'chidu-set-seen)
                     (lambda (target desired &optional quiet)
                       (push
                        (list (chidu-seen-target-local-email-id target)
                              desired quiet)
                        called))))
            (chidu-mark-read))
          (should
           (equal
            (list (list id-one t t) (list id-three t t))
            (nreverse called))))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

(provide 'chidu-selection-test)

;;; chidu-selection-test.el ends here
