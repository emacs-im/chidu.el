;;; chidu-contact-test.el --- JMAP Contacts completion tests -*- lexical-binding: t; -*-

;;; Code:

(let ((test-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-directory)
  (add-to-list 'load-path (expand-file-name ".." test-directory)))

(require 'ert)
(require 'chidu-contact)
(require 'chidu-jmap-contact)
(require 'chidu-jmap-discovery)
(require 'chidu-test-support)

(defun chidu-contact-test--session ()
  "Return Session bytes with one Mail/Submission/Contacts Account."
  (let ((core (make-hash-table :test #'equal))
        (capabilities (make-hash-table :test #'equal))
        (account-capabilities (make-hash-table :test #'equal))
        (accounts (make-hash-table :test #'equal))
        (primary (make-hash-table :test #'equal))
        (account (make-hash-table :test #'equal)))
    (dolist (entry
             '(("maxSizeUpload" . 1048576)
               ("maxConcurrentUpload" . 1)
               ("maxSizeRequest" . 1048576)
               ("maxConcurrentRequests" . 1)
               ("maxCallsInRequest" . 16)
               ("maxObjectsInGet" . 128)
               ("maxObjectsInSet" . 128)))
      (puthash (car entry) (cdr entry) core))
    (puthash "collationAlgorithms" (vector) core)
    (puthash chidu-jmap-core-capability core capabilities)
    (dolist (capability
             (list chidu-jmap-mail-capability
                   chidu-jmap-submission-capability
                   chidu-jmap-contacts-capability))
      (puthash capability (make-hash-table :test #'equal) capabilities)
      (puthash capability
               (make-hash-table :test #'equal) account-capabilities)
      (puthash capability "account" primary))
    (puthash
     "maxSizeAttachmentsPerEmail" 2097152
     (gethash chidu-jmap-mail-capability account-capabilities))
    (puthash "name" "Mail" account)
    (puthash "isPersonal" t account)
    (puthash "isReadOnly" :json-false account)
    (puthash "accountCapabilities" account-capabilities account)
    (puthash "account" account accounts)
    (chidu-store-test--payload
     `(:capabilities ,capabilities
       :accounts ,accounts
       :primaryAccounts ,primary
       :username "me@example.test"
       :apiUrl "https://mail.example.test/jmap/api"
       :downloadUrl
       "https://mail.example.test/jmap/download/{accountId}/{blobId}/{name}?type={type}"
       :uploadUrl "https://mail.example.test/jmap/upload/{accountId}"
       :eventSourceUrl
       "https://mail.example.test/jmap/eventsource/?types={types}"
       :state "session-state"))))

(ert-deftest chidu-session-preserves-primary-contacts-account ()
  (let ((session
         (chidu-jmap--validate-session
          (chidu-contact-test--session)
          "https://mail.example.test/.well-known/jmap")))
    (should
     (equal "account"
            (chidu-store-session-observation-primary-contacts-remote-account-id
             session)))
    (should
     (seq-contains-p
      (chidu-store-session-observation-capabilities session)
      chidu-jmap-contacts-capability #'equal))))

(ert-deftest chidu-contact-request-is-native-query-plus-result-reference ()
  (let* ((request
           (chidu-jmap-contact-search-request "account" "alice" 24))
         (using (plist-get request :using))
         (calls (plist-get request :methodCalls))
         (addressbook-call (aref calls 0))
         (query-call (aref calls 1))
         (get-call (aref calls 2))
         (query (aref query-call 1))
         (get (aref get-call 1)))
    (should
     (equal
      (vector chidu-jmap-core-capability chidu-jmap-contacts-capability)
      using))
    (should (equal "AddressBook/get" (aref addressbook-call 0)))
    (should (equal "ContactCard/query" (aref query-call 0)))
    (should (equal "alice" (plist-get (plist-get query :filter) :text)))
    (should (= 24 (plist-get query :limit)))
    (should (equal "ContactCard/get" (aref get-call 0)))
    (should
     (equal
      '(:resultOf "contact-query" :name "ContactCard/query" :path "/ids")
      (plist-get get (intern ":#ids"))))))

(ert-deftest chidu-contact-empty-query-uses-a-bounded-native-window ()
  (let* ((request (chidu-jmap-contact-search-request "account" "" 12))
         (query (aref (aref (plist-get request :methodCalls) 1) 1)))
    (should-not (plist-member query :filter))
    (should (= 12 (plist-get query :limit)))
    (should (equal "updated"
                   (plist-get (aref (plist-get query :sort) 0) :property)))))

(ert-deftest chidu-contact-decoder-respects-address-books-name-email-and-pref ()
  (let ((alice-emails (make-hash-table :test #'equal))
        (bob-emails (make-hash-table :test #'equal))
        (group-emails (make-hash-table :test #'equal))
        (hidden-emails (make-hash-table :test #'equal)))
    (puthash "work"
             '(:address "alice@work.example" :pref 2
               :contexts (:work t))
             alice-emails)
    (puthash "home"
             '(:address "alice@home.example" :pref 1
               :contexts (:private t))
             alice-emails)
    (puthash "main" '(:address "bob@example.test") bob-emails)
    (puthash "list" '(:address "team@example.test") group-emails)
    (puthash "hidden" '(:address "hidden@example.test") hidden-emails)
    (let* ((bytes
            (chidu-store-test--payload
             `(:sessionState "session"
               :methodResponses
               [["AddressBook/get"
                 (:accountId "account" :state "a1"
                  :list
                  [(:id "book-visible" :isSubscribed t
                    :myRights (:mayRead t :mayWrite t
                               :mayShare :json-false :mayDelete :json-false))
                   (:id "book-hidden" :isSubscribed :json-false
                    :myRights (:mayRead t :mayWrite :json-false
                               :mayShare :json-false :mayDelete :json-false))]
                  :notFound [])
                 "addressbook-get"]
                ["ContactCard/query"
                 (:accountId "account" :queryState "q1"
                  :canCalculateChanges t :position 0
                  :ids ["c1" "c2" "c3" "c4"] :total 4)
                 "contact-query"]
                ["ContactCard/get"
                 (:accountId "account" :state "c1"
                  :list
                  [(:id "c1" :uid "uid-c1" :kind "individual"
                    :addressBookIds (:book-visible t)
                    :name (:full "Alice Example")
                    :emails ,alice-emails)
                   (:id "c2" :uid "uid-c2" :kind "individual"
                    :addressBookIds (:book-visible t)
                    :name
                    (:components
                     [(:kind "given" :value "Bob")
                      (:kind "surname" :value "Builder")]
                     :isOrdered t)
                    :emails ,bob-emails)
                   (:id "c3" :uid "uid-c3" :kind "group"
                    :addressBookIds (:book-visible t)
                    :name (:full "Team")
                    :emails ,group-emails)
                   (:id "c4" :uid "uid-c4" :kind "individual"
                    :addressBookIds (:book-hidden t)
                    :name (:full "Hidden")
                    :emails ,hidden-emails)]
                  :notFound [])
                 "contact-get"]])))
           (candidates
            (chidu-jmap-contact-validate-search-response
             bytes "account" 4 4)))
      (should (= 3 (length candidates)))
      (should
       (equal
        '("alice@home.example" "alice@work.example" "bob@example.test")
        (cl-loop
         for candidate across candidates
         collect (chidu-store-email-address-email candidate))))
      (should
       (equal '("Alice Example" "Alice Example" "Bob Builder")
              (cl-loop
               for candidate across candidates
               collect (chidu-store-email-address-name candidate))))
      (should (seq-every-p #'chidu-store-email-address-p candidates)))))

(ert-deftest chidu-contact-address-books-preserve-rights-and-sort-order ()
  (let* ((bytes
          (chidu-store-test--method-response
           "AddressBook/get" "addressbook-get"
           '(:accountId "account" :state "a1"
             :list
             [(:id "later" :name "Later" :description :json-null
               :sortOrder 20 :isDefault :json-false :isSubscribed t
               :myRights (:mayRead t :mayWrite :json-false
                          :mayShare :json-false :mayDelete :json-false))
              (:id "first" :name "First" :description "Personal"
               :sortOrder 0 :isDefault t :isSubscribed t
               :myRights (:mayRead t :mayWrite t
                          :mayShare t :mayDelete t))]
             :notFound :json-null)))
         (directory
          (chidu-jmap-contact-validate-address-books-response
           bytes "account"))
         (books (chidu-address-book-directory-address-books directory))
         (first (aref books 0))
         (later (aref books 1)))
    (should (equal "a1" (chidu-address-book-directory-state directory)))
    (should (equal '("first" "later")
                   (mapcar #'chidu-address-book-remote-id
                           (append books nil))))
    (should (chidu-address-book-default-p first))
    (should (equal "Personal" (chidu-address-book-description first)))
    (should (chidu-contact-rights-may-write-p
             (chidu-address-book-rights first)))
    (should-not (chidu-contact-rights-may-write-p
                 (chidu-address-book-rights later)))))

(ert-deftest chidu-contact-page-request-is-address-book-scoped-and-anchored ()
  (let* ((request
           (chidu-jmap-contact-page-request
            "account" "book" "alice" 32 "contact-1"))
         (calls (plist-get request :methodCalls))
         (query (aref (aref calls 0) 1))
         (get (aref (aref calls 1) 1)))
    (should (equal '(:inAddressBook "book" :text "alice")
                   (plist-get query :filter)))
    (should (equal "contact-1" (plist-get query :anchor)))
    (should (= 1 (plist-get query :anchorOffset)))
    (should-not (plist-member query :position))
    (should (equal "updated"
                   (plist-get (aref (plist-get query :sort) 0) :property)))
    (should
     (equal
      '(:resultOf "contact-query" :name "ContactCard/query" :path "/ids")
      (plist-get get (intern ":#ids"))))))

(ert-deftest chidu-contact-page-decoder-preserves-query-order ()
  (let* ((bytes
          (chidu-store-test--payload
           '(:sessionState "session"
             :methodResponses
             [["ContactCard/query"
               (:accountId "account" :queryState "q1"
                :canCalculateChanges t :position 1
                :ids ["c2" "c1"] :total 5)
               "contact-query"]
              ["ContactCard/get"
               (:accountId "account" :state "c1"
                :list
                [(:id "c1" :uid "uid-c1" :kind "individual"
                  :addressBookIds (:book t)
                  :name (:full "Alice"))
                 (:id "c2" :uid "uid-c2" :kind "individual"
                  :addressBookIds (:book t)
                  :name (:full "Bob"))]
                :notFound [])
               "contact-get"]])))
         (page
          (chidu-jmap-contact-validate-page-response
           bytes "account" "" 2)))
    (should (equal "q1" (chidu-contact-page-query-state page)))
    (should (= 1 (chidu-contact-page-position page)))
    (should (= 3 (chidu-contact-page-next-position page)))
    (should (= 5 (chidu-contact-page-total page)))
    (should (chidu-contact-page-maybe-more-p page))
    (should (equal "c1" (chidu-contact-page-anchor-id page)))
    (should
     (equal '("c2" "c1")
            (mapcar #'chidu-contact-card-remote-id
                    (append (chidu-contact-page-cards page) nil))))))

(ert-deftest chidu-contact-page-progress-survives-query-get-race ()
  (let* ((bytes
          (chidu-store-test--payload
           '(:sessionState "session"
             :methodResponses
             [["ContactCard/query"
               (:accountId "account" :queryState "q1"
                :canCalculateChanges t :position 0
                :ids ["gone" "present"] :total 3)
               "contact-query"]
              ["ContactCard/get"
               (:accountId "account" :state "c1"
                :list
                [(:id "present" :uid "uid-present" :kind "individual"
                  :addressBookIds (:book t)
                  :name (:full "Present"))]
                :notFound ["gone"])
               "contact-get"]])))
         (page
          (chidu-jmap-contact-validate-page-response
           bytes "account" "" 2)))
    (should (= 0 (chidu-contact-page-position page)))
    (should (= 2 (chidu-contact-page-next-position page)))
    (should (= 1 (length (chidu-contact-page-cards page))))
    (should (chidu-contact-page-maybe-more-p page))
    (should (equal "present" (chidu-contact-page-anchor-id page)))))

(ert-deftest chidu-contact-detail-decodes-selected-jscontact-fields ()
  (let ((emails (make-hash-table :test #'equal))
        (phones (make-hash-table :test #'equal))
        (organizations (make-hash-table :test #'equal))
        (titles (make-hash-table :test #'equal))
        (addresses (make-hash-table :test #'equal))
        (services (make-hash-table :test #'equal))
        (notes (make-hash-table :test #'equal)))
    (puthash "mail" '(:address "alice@example.test" :pref 1
                      :contexts (:work t)) emails)
    (puthash "mobile" '(:number "+1 555 0100"
                        :features (:mobile t)) phones)
    (puthash "org" '(:name "Example Corp"
                     :units [(:name "Engineering")]) organizations)
    (puthash "title" '(:name "Engineer" :kind "title"
                       :organizationId "org") titles)
    (puthash "home" '(:full "1 Example Street" :countryCode "US")
             addresses)
    (puthash "chat" '(:service "Matrix" :user "@alice:example.test"
                      :uri "https://matrix.to/#/@alice:example.test")
             services)
    (puthash "note" '(:note "Met at the JMAP workshop.") notes)
    (let* ((bytes
            (chidu-store-test--method-response
             "ContactCard/get" "contact-detail"
             `(:accountId "account" :state "c1"
               :list
               [(:id "c1" :uid "uid-c1" :kind "individual"
                 :addressBookIds (:book t)
                 :name (:full "Alice Example")
                 :emails ,emails :phones ,phones
                 :organizations ,organizations :titles ,titles
                 :addresses ,addresses :onlineServices ,services
                 :notes ,notes :members (:uid-c2 t)
                 :created "2026-08-01T00:00:00Z"
                 :updated "2026-08-27T00:00:00Z")]
               :notFound [])))
           (card
            (chidu-jmap-contact-validate-detail-response
             bytes "account" "c1")))
      (should (chidu-contact-card-complete-p card))
      (should (equal "Alice Example" (chidu-contact-card-name card)))
      (should
       (equal "alice@example.test"
              (chidu-contact-value-value
               (chidu-contact-card-primary-email card))))
      (should
       (equal "Example Corp · Engineering"
              (chidu-contact-value-value
               (aref (chidu-contact-card-organizations card) 0))))
      (should (equal ["uid-c2"] (chidu-contact-card-members card)))
      (should (equal "2026-08-27T00:00:00Z"
                     (chidu-contact-card-updated card))))))

(provide 'chidu-contact-test)

;;; chidu-contact-test.el ends here
