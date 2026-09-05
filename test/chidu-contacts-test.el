;;; chidu-contacts-test.el --- Contacts view tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'ert)
(require 'chidu-test-support)
(require 'appkit-core)
(require 'chidu)
(require 'chidu-address-books)
(require 'chidu-contact-view)
(require 'chidu-contacts)
(require 'chidu-message)
(require 'chidu-compose)

(defun chidu-contacts-test--endpoint ()
  "Return one synthetic Contacts-capable Endpoint."
  (chidu-store-endpoint-create
   :endpoint-id "endpoint"
   :session-url "https://mail.example.test/.well-known/jmap"
   :login "me@example.test"
   :authentication 'basic
   :api-url "https://mail.example.test/jmap/api"
   :primary-contacts-remote-account-id "contacts"
   :capabilities (vector chidu-jmap-core-capability
                         chidu-jmap-contacts-capability)))

(defun chidu-contacts-test--book ()
  "Return one readable synthetic AddressBook."
  (chidu-address-book-create
   :remote-id "book" :name "People" :description "Personal contacts"
   :sort-order 0 :default-p t :subscribed-p t
   :rights
   (chidu-contact-rights-create
    :may-read-p t :may-write-p t :may-share-p nil :may-delete-p t)))

(defun chidu-contacts-test--card (&optional complete-p)
  "Return one synthetic ContactCard, optionally COMPLETE-P."
  (chidu-contact-card-create
   :remote-id "contact" :uid "uid-contact" :kind "individual"
   :name "Alice Example" :address-book-ids ["book"]
   :emails
   (vector
    (chidu-contact-value-create
     :item-id "mail" :value "alice@example.test"
     :contexts ["work"] :pref 1))
   :phones
   (vector
    (chidu-contact-value-create
     :item-id "mobile" :value "+1 555 0100"
     :qualifiers ["mobile"]))
   :organizations
   (vector
    (chidu-contact-value-create
     :item-id "org" :value "Example Corp"))
   :titles
   (vector
    (chidu-contact-value-create
     :item-id "title" :value "Engineer"))
   :addresses
   (if complete-p
       (vector
        (chidu-contact-value-create
         :item-id "home" :value "1 Example Street"))
     (vector))
   :notes
   (if complete-p
       (vector
        (chidu-contact-value-create
         :item-id "note" :value "Met at the JMAP workshop."))
     (vector))
   :created (and complete-p "2026-08-01T00:00:00Z")
   :updated "2026-08-27T00:00:00Z"
   :complete-p complete-p))

(ert-deftest chidu-contact-page-append-requires-one-stable-query ()
  (let* ((first-card (chidu-contacts-test--card))
         (second-card
          (chidu-contact-card-with
           first-card :remote-id "contact-2" :uid "uid-contact-2"
           :name "Bob Example"))
         (first
          (chidu-contact-page-create
           :query-state "q1" :total 2 :position 0 :next-position 1
           :query "" :cards (vector first-card) :maybe-more-p t
           :anchor-id "contact"))
         (second
          (chidu-contact-page-create
           :query-state "q1" :total 2 :position 1 :next-position 2
           :query "" :cards (vector second-card) :maybe-more-p nil
           :anchor-id "contact-2"))
         (combined (chidu-contacts--append-page first second)))
    (should
     (equal '("contact" "contact-2")
            (mapcar #'chidu-contact-card-remote-id
                    (append (chidu-contact-page-cards combined) nil))))
    (should-not (chidu-contact-page-maybe-more-p combined))
    (should-error
     (chidu-contacts--append-page
      first (chidu-contact-page-with second :query-state "q2"))
     :type 'user-error)))

(ert-deftest chidu-contact-page-append-rejects-total-drift ()
  (let* ((card (chidu-contacts-test--card))
         (first
          (chidu-contact-page-create
           :query-state "q1" :total 2 :position 0 :next-position 1
           :query "" :cards (vector card) :maybe-more-p t
           :anchor-id "contact-1"))
         (second
          (chidu-contact-page-create
           :query-state "q1" :total 3 :position 1 :next-position 2
           :query "" :cards (vector) :maybe-more-p t
           :anchor-id "contact-2")))
    (should-error
     (chidu-contacts--append-page first second)
     :type 'user-error)))

(ert-deftest chidu-contact-page-append-uses-query-progress-not-card-count ()
  (let* ((card (chidu-contacts-test--card))
         (next-card
          (chidu-contact-card-with
           card :remote-id "contact-3" :uid "uid-contact-3"))
         (first
          (chidu-contact-page-create
           :query-state "q1" :total 3 :position 0 :next-position 2
           :query "" :cards (vector card) :maybe-more-p t
           :anchor-id "contact-2"))
         (second
          (chidu-contact-page-create
           :query-state "q1" :total 3 :position 2 :next-position 3
           :query "" :cards (vector next-card) :maybe-more-p nil
           :anchor-id "contact-3"))
         (combined (chidu-contacts--append-page first second)))
    (should (= 3 (chidu-contact-page-next-position combined)))
    (should (= 2 (length (chidu-contact-page-cards combined))))))

(ert-deftest chidu-contacts-pages-form-one-read-only-vertical-slice ()
  (let* ((app (chidu-test-app-create nil))
         (endpoint (chidu-contacts-test--endpoint))
         (book (chidu-contacts-test--book))
         (directory
          (chidu-address-book-directory-create
           :state "a1" :address-books (vector book)))
         (summary (chidu-contacts-test--card))
         (detail (chidu-contacts-test--card t))
         address-buffer list-buffer detail-buffer queries)
    (unwind-protect
        (progn
          (appkit-app-send app (list :runtime 'test-runtime))
          (cl-letf
              (((symbol-function 'chidu-contact-list-address-books)
                (lambda (_runtime _endpoint success _error)
                  (funcall success directory)
                  nil))
               ((symbol-function 'chidu-contact-query-page)
                (lambda (_runtime _endpoint _book query _limit _anchor
                                  success _error)
                  (setq queries (append queries (list query)))
                  (funcall
                   success
                   (chidu-contact-page-create
                    :query-state "q1" :total 1 :position 0 :next-position 1
                    :query query :cards (vector summary) :maybe-more-p nil
                    :anchor-id "contact"))
                  nil))
               ((symbol-function 'chidu-contact-get-detail)
                (lambda (_runtime _endpoint _remote-id success _error)
                  (funcall success detail)
                  nil)))
            (setq address-buffer
                  (chidu-address-books-open app endpoint nil))
            (with-current-buffer address-buffer
              (chidu-test-drain (appkit-current-surface))
              (should (eq major-mode 'chidu-address-books-mode))
              (should (string-match-p "People" (buffer-string))))
            (setq list-buffer
                  (chidu-contacts-open-list
                   app endpoint directory book nil))
            (with-current-buffer list-buffer
              (chidu-test-drain (appkit-current-surface))
              (should (eq major-mode 'chidu-contacts-mode))
              (should (string-match-p "Alice Example" (buffer-string)))
              (should (string-match-p "alice@example\\.test"
                                      (buffer-string)))
              (goto-char (point-min))
              (should (text-property-not-all
                       (point-min) (point-max)
                       'chidu-contact-card-id nil))
              (setf (chidu-contacts-state-query
                     (appkit-surface-model (appkit-current-surface)))
                    "alice"))
            (should
             (eq list-buffer
                 (chidu-contacts-open-list
                  app endpoint directory book nil)))
            (with-current-buffer list-buffer
              (chidu-test-drain (appkit-current-surface))
              (should
               (equal "alice"
                      (chidu-contacts-state-query
                       (appkit-surface-model (appkit-current-surface))))))
            (should (equal '("" "alice") queries))
            (setq detail-buffer
                  (chidu-contact-view-open
                   app endpoint directory summary nil))
            (with-current-buffer detail-buffer
              (chidu-test-drain (appkit-current-surface))
              (should (eq major-mode 'chidu-contact-view-mode))
              (should (string-match-p "Alice Example" (buffer-string)))
              (should (string-match-p "Example Corp" (buffer-string)))
              (should (string-match-p "Met at the JMAP workshop"
                                      (buffer-string))))))
      (when (appkit-app-live-p app) (appkit-app-close app)
            )
      (dolist (buffer (list address-buffer list-buffer detail-buffer)) (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest chidu-text-property-row-navigation-skips-current-row ()
  (with-temp-buffer
    (insert "first\n\nsecond\n")
    (put-text-property 1 6 'row-id 'first)
    (put-text-property 8 14 'row-id 'second)
    (goto-char 1)
    (chidu-text-next-property-row 'row-id "missing")
    (should (= 8 (point)))
    (chidu-text-previous-property-row 'row-id "missing")
    (should (= 1 (point)))))

(ert-deftest chidu-contact-page-limit-respects-session-capability ()
  (let* ((endpoint
          (chidu-store-endpoint-with
           (chidu-contacts-test--endpoint)
           :max-objects-in-get 17))
         (state
          (chidu-contacts-state-create
           :endpoint endpoint
           :address-book (chidu-contacts-test--book)))
         (chidu-contacts-page-size 64))
    (should (= 17 (chidu-contacts--page-limit state)))))

(ert-deftest chidu-contact-compose-uses-preferred-address-and-matching-account ()
  (let* ((identity
          (chidu-store-identity-create
           :identity-id "identity" :remote-identity-id "remote-identity"
           :name "Me" :email "me@example.test" :available-p t))
         (account
          (chidu-store-account-create
           :account-id "account" :remote-account-id "contacts"
           :name "Mail" :available-p t :read-only-p nil
           :capabilities (vector chidu-jmap-submission-capability)
           :identities (vector identity)))
         (endpoint
          (chidu-store-endpoint-with
           (chidu-contacts-test--endpoint)
           :accounts (vector account)))
         (card (chidu-contacts-test--card))
         (app (chidu-test-app-create nil))
         captured)
    (unwind-protect
        (let ((chidu--app app))
          (cl-letf
              (((symbol-function 'chidu-compose--read-identity)
                (lambda (selected)
                  (should (eq selected account))
                  identity))
               ((symbol-function 'chidu-compose--create-workspace)
                (lambda (actual-app actual-account actual-identity kind document)
                  (setq captured
                        (list actual-app actual-account actual-identity
                              kind document)))))
            (chidu-compose-to-contact endpoint card))
          (pcase-let ((`(,actual-app ,actual-account ,actual-identity
                                     ,kind ,document)
                       captured))
            (should (eq actual-app app))
            (should (eq actual-account account))
            (should (eq actual-identity identity))
            (should (eq kind 'new))
            (should
             (equal '("Alice Example" "alice@example.test")
                    (mail-extract-address-components
                     (chidu-store-compose-document-to document))))))
      (when (appkit-app-live-p app) (appkit-app-close app)))))

(provide 'chidu-contacts-test)

;;; chidu-contacts-test.el ends here
