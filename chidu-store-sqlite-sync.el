;;; chidu-store-sqlite-sync.el --- SQLite sync and query projections -*- lexical-binding: t; -*-

;;; Commentary:

;; Email bootstrap plus query-bound Search projection operations.
;; Canonical Mailbox Summary reads live in `chidu-store-sqlite-generation'.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'subr-x)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)

(defun chidu-store-sqlite--email-context (state account-id)
  "Return ACCOUNT-ID Email synchronization context from SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((null (chidu-store-endpoint-api-url (car location)))
      (chidu-result-failure-create
       :kind 'endpoint-not-connected :data (list :account-id account-id)
       :retryable-p nil))
     (t
      (chidu-result-ok-create
       :value
       (or
        (chidu-sql-one database
            [:select
             [[phase phase]
              [generation-id generation-id]
              [profile-version profile-version]
              [state-token state]
              [query-state query-state]
              [can-calculate-changes can-calculate-changes]
              [committed-count committed-count]
              [anchor anchor-remote-email-id]
              [hydration-after hydration-after-local-email-id]
              [revision revision]]
             :from jmap-email-checkpoint
             :where [:= account-id [:bind account-id]]]
          (chidu-store-email-sync-context-create
           :endpoint (car location)
           :account (cdr location)
           :phase (intern phase)
           :generation-id generation-id
           :profile-version profile-version
           :state state-token
           :query-state query-state
           :can-calculate-changes-p
           (and (integerp can-calculate-changes)
                (chidu-store-sqlite--bool can-calculate-changes))
           :committed-count committed-count
           :anchor-remote-email-id anchor
           :hydration-after-local-email-id hydration-after
           :revision revision))
        (chidu-store-email-sync-context-create
         :endpoint (car location)
         :account (cdr location))))))))

(defun chidu-store-sqlite--search-rows (database account-id query-key)
  "Return visible search rows from DATABASE for ACCOUNT-ID and QUERY-KEY."
  (vconcat
   (chidu-sql-map database
       [:select
        [[local-id row:local-email-id]
         [remote-id email:remote-email-id]
         [thread-id row:remote-thread-id]
         [received-at row:received-at]
         [from-name row:from-name]
         [from-email row:from-email]
         [subject row:subject]
         [preview row:preview]
         [unread
          [:call coalesce
                 [:case intent:desired-seen [1 0] [0 1]]
                 row:is-unread]]
         [flagged row:is-flagged]
         [attachment row:has-attachment]
         [mailboxes-json row:remote-mailbox-ids-json]
         [snippet-subject row:snippet-subject]
         [snippet-preview row:snippet-preview]]
        :from [:as jmap-search-projection-row row]
        :joins
        [[:inner [:as jmap-email-record email]
          :on [:and
               [:= email:account-id row:account-id]
               [:= email:local-email-id row:local-email-id]]]
         [:left [:as jmap-seen-intent intent]
          :on [:and
               [:= intent:account-id row:account-id]
               [:= intent:local-email-id row:local-email-id]]]]
        :where
        [:and
         [:= row:account-id [:bind account-id]]
         [:= row:query-key [:bind query-key]]
         [:not-exists
          [:select [1]
           :from [:as jmap-mailbox-move-target move-target]
           :where [:and
                   [:= move-target:account-id row:account-id]
                   [:= move-target:local-email-id row:local-email-id]]]]
         [:not-exists
          [:select [1]
           :from [:as jmap-trash-target trash-target]
           :where [:and
                   [:= trash-target:account-id row:account-id]
                   [:= trash-target:local-email-id row:local-email-id]]]]]
        :order-by [[row:ordinal :asc]]]
     (chidu-store-search-row-create
      :summary-row
      (chidu-store-email-summary-row-create
       :local-email-id local-id
       :remote-email-id remote-id
       :remote-thread-id thread-id
       :received-at received-at
       :from-name from-name
       :from-email from-email
       :subject subject
       :preview preview
       :unread-p (chidu-store-sqlite--bool unread)
       :flagged-p (chidu-store-sqlite--bool flagged)
       :has-attachment-p (chidu-store-sqlite--bool attachment))
      :remote-mailbox-ids
      (chidu-store-sqlite--string-vector-from-json
       mailboxes-json "Search remote Mailbox ids")
      :snippet
      (when (or snippet-subject snippet-preview)
        (chidu-store-search-snippet-create
         :subject snippet-subject :preview snippet-preview))))))

(defun chidu-store-sqlite--invalidate-search-projections
    (database account-id change-seq)
  "Invalidate DATABASE Search projections below ACCOUNT-ID at CHANGE-SEQ."
  (chidu-sql-execute database
    [:update jmap-search-projection
     :set [[is-stale 1]
           [maybe-more 0]
           [revision [:+ revision 1]]
           [observed-change-seq [:bind change-seq]]]
     :where [:= account-id [:bind account-id]]]))

(defun chidu-store-sqlite--search-context (state account-id query-key)
  "Return ACCOUNT-ID and QUERY-KEY search context from SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((not (and (stringp query-key) (not (string-empty-p query-key))))
      (chidu-result-failure-create
       :kind 'invalid-search-query :data (list :query-key query-key)
       :retryable-p nil))
     (t
      (chidu-result-ok-create
       :value
       (or
        (chidu-sql-one database
            [:select
             [[query-text query-text]
              [filter-json filter-json]
              [query-state query-state]
              [email-state email-state]
              [cursor-remote-email-id cursor-remote-email-id]
              [revision revision]
              [maybe-more maybe-more]
              [is-stale is-stale]]
             :from jmap-search-projection
             :where [:and
                     [:= account-id [:bind account-id]]
                     [:= query-key [:bind query-key]]]]
          (chidu-store-search-context-create
           :endpoint (car location)
           :account (cdr location)
           :query-key query-key
           :query-text query-text
           :filter-json filter-json
           :query-state query-state
           :email-state email-state
           :cursor-remote-email-id cursor-remote-email-id
           :revision revision
           :maybe-more-p (chidu-store-sqlite--bool maybe-more)
           :stale-p (chidu-store-sqlite--bool is-stale)
           :rows
           (chidu-store-sqlite--search-rows
            database account-id query-key)))
        (chidu-store-search-context-create
         :endpoint (car location)
         :account (cdr location)
         :query-key query-key)))))))

(defun chidu-store-sqlite--begin-email-bootstrap (state operation)
  "Apply Email bootstrap begin OPERATION to SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-begin-email-bootstrap-account-id operation))
         (expected-revision
          (chidu-store-op-begin-email-bootstrap-expected-revision operation))
         (context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-revision
              (chidu-store-email-sync-context-revision context)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((not (eq 'uninitialized
                   (chidu-store-email-sync-context-phase context)))
          (chidu-result-failure-create
           :kind 'email-bootstrap-already-started
           :data (list :account-id account-id
                       :phase (chidu-store-email-sync-context-phase context))
           :retryable-p nil))
         (t
          (let* ((state-token
                  (chidu-store-validate-nonempty-string
                   (chidu-store-op-begin-email-bootstrap-state operation)
                   "Email object state"))
                 (profile-version
                  (chidu-store-validate-nonempty-string
                   (chidu-store-op-begin-email-bootstrap-profile-version operation)
                   "Email profile version"))
                 (generation-id (chidu-store-new-local-id))
                 (revision (1+ actual-revision)))
            (with-sqlite-transaction database
              (let ((change-seq
                     (chidu-store-sqlite--increment-change-seq database)))
                (chidu-sql-execute database
                  [:insert :into jmap-email-generation
                   :row
                   [[generation-id [:bind generation-id]]
                    [account-id [:bind account-id]]
                    [lifecycle [:literal "building"]]
                    [profile-version [:bind profile-version]]
                    [created-checkpoint-revision [:bind revision]]]])
                (chidu-sql-execute database
                  [:insert :into jmap-email-checkpoint
                   :row
                   [[account-id [:bind account-id]]
                    [phase [:literal "enumerating"]]
                    [generation-id [:bind generation-id]]
                    [profile-version [:bind profile-version]]
                    [state [:bind state-token]]
                    [query-state nil]
                    [can-calculate-changes nil]
                    [committed-count 0]
                    [anchor-remote-email-id nil]
                    [revision [:bind revision]]
                    [observed-change-seq [:bind change-seq]]]])))
            (chidu-store-sqlite--email-context state account-id))))))))

(defun chidu-store-sqlite--restart-email-bootstrap (state operation)
  "Replace OPERATION's unprovable Email generation in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-restart-email-bootstrap-account-id operation))
         (generation-id
          (chidu-store-op-restart-email-bootstrap-generation-id operation))
         (expected-revision
          (chidu-store-op-restart-email-bootstrap-expected-revision operation))
         (context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (phase (chidu-store-email-sync-context-phase context))
             (actual-revision
              (chidu-store-email-sync-context-revision context)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((not (equal generation-id
                      (chidu-store-email-sync-context-generation-id context)))
          (chidu-result-failure-create
           :kind 'generation-conflict
           :data (list :account-id account-id)
           :retryable-p t))
         ((not (memq phase
                     '(enumerating membership-catchup metadata-catchup live)))
          (chidu-result-failure-create
           :kind 'email-bootstrap-phase-conflict
           :data (list :account-id account-id :phase phase)
           :retryable-p t))
         (t
          (let* ((state-token
                  (chidu-store-validate-nonempty-string
                   (chidu-store-op-restart-email-bootstrap-state operation)
                   "Email object state"))
                 (profile-version
                  (chidu-store-validate-nonempty-string
                   (chidu-store-op-restart-email-bootstrap-profile-version operation)
                   "Email profile version"))
                 (live-p (eq phase 'live))
                 (next-generation-id (chidu-store-new-local-id))
                 (next-revision (1+ actual-revision)))
            (with-sqlite-transaction database
              (let ((change-seq
                     (chidu-store-sqlite--increment-change-seq database)))
                ;; A live gap starts a replacement building generation while the
                ;; old active generation remains queryable.  An unpublished
                ;; generation can be retired and removed immediately.
                (unless live-p
                  (chidu-sql-execute database
                    [:update jmap-email-generation
                     :set [[lifecycle [:literal "retired"]]]
                     :where [:and
                             [:= generation-id [:bind generation-id]]
                             [:= account-id [:bind account-id]]
                             [:= lifecycle [:literal "building"]]]]))
                (chidu-sql-execute database
                  [:insert :into jmap-email-generation
                   :row
                   [[generation-id [:bind next-generation-id]]
                    [account-id [:bind account-id]]
                    [lifecycle [:literal "building"]]
                    [profile-version [:bind profile-version]]
                    [created-checkpoint-revision [:bind next-revision]]]])
                (chidu-sql-execute database
                  [:update jmap-email-checkpoint
                   :set [[phase [:literal "enumerating"]]
                         [generation-id [:bind next-generation-id]]
                         [profile-version [:bind profile-version]]
                         [state [:bind state-token]]
                         [query-state nil]
                         [can-calculate-changes nil]
                         [committed-count 0]
                         [anchor-remote-email-id nil]
                         [revision [:bind next-revision]]
                         [observed-change-seq [:bind change-seq]]]
                   :where [:= account-id [:bind account-id]]])
                (unless live-p
                  (chidu-sql-execute database
                    [:delete
                     :from jmap-email-generation
                     :where [:and
                             [:= generation-id [:bind generation-id]]
                             [:= account-id [:bind account-id]]
                             [:= lifecycle [:literal "retired"]]]]))))
            (chidu-store-sqlite--email-context state account-id))))))))

(defun chidu-store-sqlite--append-email-query-chunk (state operation)
  "Append validated Email/query prefix chunk OPERATION to SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-append-email-query-chunk-account-id operation))
         (expected-revision
          (chidu-store-op-append-email-query-chunk-expected-revision operation))
         (generation-id
          (chidu-store-op-append-email-query-chunk-generation-id operation))
         (observation
          (chidu-store-op-append-email-query-chunk-observation operation))
         (context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-revision
              (chidu-store-email-sync-context-revision context))
             (expected-generation
              (chidu-store-email-sync-context-generation-id context))
             (query-state
              (chidu-store-email-query-page-observation-query-state observation))
             (ids
              (chidu-store-email-query-page-observation-remote-email-ids
               observation))
             (count (chidu-store-email-sync-context-committed-count context)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((not (equal generation-id expected-generation))
          (chidu-result-failure-create
           :kind 'generation-conflict
           :data (list :account-id account-id
                       :expected expected-generation :actual generation-id)
           :retryable-p t))
         ((not (eq 'enumerating
                   (chidu-store-email-sync-context-phase context)))
          (chidu-result-failure-create
           :kind 'email-bootstrap-phase-conflict
           :data (list :account-id account-id
                       :phase (chidu-store-email-sync-context-phase context))
           :retryable-p t))
         ((and (chidu-store-email-sync-context-query-state context)
               (not (equal query-state
                           (chidu-store-email-sync-context-query-state context))))
          (chidu-result-failure-create
           :kind 'query-state-changed
           :data (list :account-id account-id)
           :retryable-p t))
         ((and (chidu-store-email-sync-context-query-state context)
               (not (eq
                     (chidu-store-email-query-page-observation-can-calculate-changes-p
                      observation)
                     (chidu-store-email-sync-context-can-calculate-changes-p
                      context))))
          (chidu-result-failure-create
           :kind 'query-contract-changed
           :data (list :account-id account-id)
           :retryable-p t))
         ((and (> (length ids) 0)
               (/= count
                   (chidu-store-email-query-page-observation-position
                    observation)))
          (chidu-result-failure-create
           :kind 'query-position-mismatch
           :data (list :account-id account-id :expected count
                       :actual
                       (chidu-store-email-query-page-observation-position
                        observation))
           :retryable-p t))
         ((not
           (car
            (chidu-sql-select database
              [:select [1]
               :from jmap-email-generation
               :where [:and
                       [:= generation-id [:bind generation-id]]
                       [:= account-id [:bind account-id]]
                       [:= lifecycle [:literal "building"]]]])))
          (signal 'chidu-invariant-error
                  (list "Missing owned building Email generation"
                        generation-id)))
         (t
          (let (conflict)
            (cl-loop
             for remote-id across ids
             when
             (car
              (chidu-sql-select database
                [:select [1]
                 :from [:as jmap-email-generation-member member]
                 :joins
                 [[:inner [:as jmap-email-record email]
                   :on [:and
                        [:= email:local-email-id member:local-email-id]
                        [:= email:account-id member:account-id]]]]
                 :where [:and
                         [:= member:generation-id [:bind generation-id]]
                         [:= member:account-id [:bind account-id]]
                         [:= email:remote-email-id [:bind remote-id]]]]))
             do (setq conflict remote-id))
            (if conflict
                (chidu-result-failure-create
                 :kind 'query-prefix-conflict
                 :data (list :account-id account-id :remote-email-id conflict)
                 :retryable-p t)
              (let* ((nonempty (> (length ids) 0))
                     (next-count (+ count (length ids)))
                     (next-anchor
                      (if nonempty
                          (aref ids (1- (length ids)))
                        (chidu-store-email-sync-context-anchor-remote-email-id
                         context)))
                     (next-revision (1+ actual-revision)))
                (with-sqlite-transaction database
                  (let ((change-seq
                         (chidu-store-sqlite--increment-change-seq database)))
                    (cl-loop
                     for remote-id across ids
                     for ordinal from count
                     for local-id =
                     (chidu-store-sqlite--email-record-id
                      database account-id remote-id change-seq)
                     do
                     (chidu-sql-execute database
                       [:insert :into jmap-email-generation-member
                        :row
                        [[account-id [:bind account-id]]
                         [generation-id [:bind generation-id]]
                         [local-email-id [:bind local-id]]
                         [ordinal [:bind ordinal]]]]))
                    (chidu-sql-execute database
                      [:update jmap-email-checkpoint
                       :set
                       [[phase
                         [:bind
                          (if nonempty
                              "enumerating"
                            "membership-catchup")]]
                        [query-state [:bind query-state]]
                        [can-calculate-changes
                         [:bind
                          (chidu-store-sqlite--integer-bool
                           (chidu-store-email-query-page-observation-can-calculate-changes-p
                            observation))]]
                        [committed-count [:bind next-count]]
                        [anchor-remote-email-id [:bind next-anchor]]
                        [revision [:bind next-revision]]
                        [observed-change-seq [:bind change-seq]]]
                       :where [:= account-id [:bind account-id]]])))
                (chidu-store-sqlite--email-context state account-id))))))))))

(defun chidu-store-sqlite--generation-member-local-id
    (database account-id generation-id remote-email-id)
  "Return DATABASE member id for REMOTE-EMAIL-ID, or nil.

ACCOUNT-ID and GENERATION-ID scope the lookup."
  (caar
   (chidu-sql-select database
     [:select [member:local-email-id]
      :from [:as jmap-email-generation-member member]
      :joins
      [[:inner [:as jmap-email-record email]
        :on [:and
             [:= email:account-id member:account-id]
             [:= email:local-email-id member:local-email-id]]]]
      :where [:and
              [:= member:account-id [:bind account-id]]
              [:= member:generation-id [:bind generation-id]]
              [:= email:remote-email-id [:bind remote-email-id]]]
      :limit 1])))

(defun chidu-store-sqlite--generation-next-ordinal
    (database account-id generation-id)
  "Return DATABASE's next free ordinal for GENERATION-ID.

ACCOUNT-ID scopes the generation."
  (1+
   (or
    (caar
     (chidu-sql-select database
       [:select [[:call max ordinal]]
        :from jmap-email-generation-member
        :where [:and
                [:= account-id [:bind account-id]]
                [:= generation-id [:bind generation-id]]]]))
    -1)))

(defun chidu-store-sqlite--apply-email-membership-changes (state operation)
  "CAS-apply canonical membership changes OPERATION to SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-apply-email-membership-changes-account-id operation))
         (generation-id
          (chidu-store-op-apply-email-membership-changes-generation-id operation))
         (expected-revision
          (chidu-store-op-apply-email-membership-changes-expected-revision
           operation))
         (expected-state
          (chidu-store-op-apply-email-membership-changes-expected-state
           operation))
         (observation
          (chidu-store-op-apply-email-membership-changes-observation operation))
         (context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-revision
              (chidu-store-email-sync-context-revision context))
             (actual-state (chidu-store-email-sync-context-state context)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((not (equal generation-id
                      (chidu-store-email-sync-context-generation-id context)))
          (chidu-result-failure-create
           :kind 'generation-conflict
           :data (list :account-id account-id)
           :retryable-p t))
         ((not (eq 'membership-catchup
                   (chidu-store-email-sync-context-phase context)))
          (chidu-result-failure-create
           :kind 'email-bootstrap-phase-conflict
           :data (list :account-id account-id
                       :phase (chidu-store-email-sync-context-phase context))
           :retryable-p t))
         ((not (chidu-store-email-changes-observation-p observation))
          (signal 'wrong-type-argument
                  (list 'chidu-store-email-changes-observation-p observation)))
         ((or (not (equal expected-state actual-state))
              (not
               (equal
                actual-state
                (chidu-store-email-changes-observation-old-state observation))))
          (chidu-result-failure-create
           :kind 'state-mismatch
           :data (list :account-id account-id
                       :expected expected-state :actual actual-state)
           :retryable-p t))
         ((not
           (car
            (chidu-sql-select database
              [:select [1]
               :from jmap-email-generation
               :where [:and
                       [:= generation-id [:bind generation-id]]
                       [:= account-id [:bind account-id]]
                       [:= lifecycle [:literal "building"]]]])))
          (signal 'chidu-invariant-error
                  (list "Missing owned building Email generation"
                        generation-id)))
         (t
          (with-sqlite-transaction database
            (let* ((change-seq
                    (chidu-store-sqlite--increment-change-seq database))
                   (next-ordinal
                    (chidu-store-sqlite--generation-next-ordinal
                     database account-id generation-id)))
              (cl-loop
               for remote-id across
               (chidu-store-email-changes-observation-destroyed observation)
               for local-id =
               (chidu-store-sqlite--generation-member-local-id
                database account-id generation-id remote-id)
               when local-id
               do
               (chidu-sql-execute database
                 [:delete :from jmap-email-generation-member
                  :where [:and
                          [:= account-id [:bind account-id]]
                          [:= generation-id [:bind generation-id]]
                          [:= local-email-id [:bind local-id]]]]))
              (cl-loop
               for remote-id across
               (chidu-store-email-changes-observation-created observation)
               unless
               (chidu-store-sqlite--generation-member-local-id
                database account-id generation-id remote-id)
               do
               (let ((local-id
                      (chidu-store-sqlite--email-record-id
                       database account-id remote-id change-seq)))
                 (chidu-sql-execute database
                   [:insert :into jmap-email-generation-member
                    :row
                    [[account-id [:bind account-id]]
                     [generation-id [:bind generation-id]]
                     [local-email-id [:bind local-id]]
                     [ordinal [:bind next-ordinal]]]])
                 (cl-incf next-ordinal)))
              (chidu-sql-execute database
                [:update jmap-email-checkpoint
                 :set
                 [[phase
                   [:bind
                    (if
                        (chidu-store-email-changes-observation-has-more-changes-p
                         observation)
                        "membership-catchup"
                      "hydrating")]]
                  [state
                   [:bind
                    (chidu-store-email-changes-observation-new-state
                     observation)]]
                  [revision [:bind (1+ actual-revision)]]
                  [observed-change-seq [:bind change-seq]]]
                 :where [:= account-id [:bind account-id]]])))
          (chidu-store-sqlite--email-context state account-id)))))))

(defun chidu-store-sqlite--insert-search-rows
    (database account-id query-key start-ordinal rows change-seq)
  "Insert ROWS into DATABASE for ACCOUNT-ID search QUERY-KEY.

START-ORDINAL sets the first row ordinal; CHANGE-SEQ records observation time."
  (cl-loop
   for item across rows
   for ordinal from start-ordinal
   for summary = (chidu-store-search-observation-row-summary-row item)
   for local-id =
   (chidu-store-sqlite--email-record-id
    database account-id
    (chidu-store-email-summary-observation-row-remote-email-id summary)
    change-seq)
   for snippet = (chidu-store-search-observation-row-snippet item)
   do
   (chidu-sql-execute database
     [:insert :into jmap-search-projection-row
      :row
      [[account-id [:bind account-id]]
       [query-key [:bind query-key]]
       [ordinal [:bind ordinal]]
       [local-email-id [:bind local-id]]
       [remote-thread-id
        [:bind
         (chidu-store-email-summary-observation-row-remote-thread-id summary)]]
       [received-at
        [:bind
         (chidu-store-email-summary-observation-row-received-at summary)]]
       [from-name
        [:bind (chidu-store-email-summary-observation-row-from-name summary)]]
       [from-email
        [:bind (chidu-store-email-summary-observation-row-from-email summary)]]
       [subject
        [:bind (chidu-store-email-summary-observation-row-subject summary)]]
       [preview
        [:bind (chidu-store-email-summary-observation-row-preview summary)]]
       [is-unread
        [:bind
         (chidu-store-sqlite--integer-bool
          (chidu-store-email-summary-observation-row-unread-p summary))]]
       [is-flagged
        [:bind
         (chidu-store-sqlite--integer-bool
          (chidu-store-email-summary-observation-row-flagged-p summary))]]
       [has-attachment
        [:bind
         (chidu-store-sqlite--integer-bool
          (chidu-store-email-summary-observation-row-has-attachment-p
           summary))]]
       [remote-mailbox-ids-json
        [:bind
         (chidu-store-sqlite--string-vector-json
          (chidu-store-search-observation-row-remote-mailbox-ids item)
          "Search remote Mailbox ids")]]
       [snippet-subject
        [:bind (and snippet (chidu-store-search-snippet-subject snippet))]]
       [snippet-preview
        [:bind (and snippet (chidu-store-search-snippet-preview snippet))]]]])))

(defun chidu-store-sqlite--pagination-overlap-id (current-remote-ids rows key)
  "Return first duplicate remote id between CURRENT-REMOTE-IDS and ROWS.

KEY extracts one summary observation row from each element of ROWS."
  (let ((seen (make-hash-table :test #'equal))
        duplicate)
    (mapc (lambda (id) (puthash id t seen)) current-remote-ids)
    (cl-loop
     for item across rows
     for summary = (funcall key item)
     for remote-id =
     (chidu-store-email-summary-observation-row-remote-email-id summary)
     do
     (if (gethash remote-id seen)
         (unless duplicate (setq duplicate remote-id))
       (puthash remote-id t seen)))
    duplicate))

(defun chidu-store-sqlite--replace-search (state operation)
  "CAS-replace Email search OPERATION in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-replace-search-account-id operation))
         (query-key
          (chidu-store-op-replace-search-query-key operation))
         (expected-revision
          (chidu-store-op-replace-search-expected-revision operation))
         (observation
          (chidu-store-op-replace-search-observation operation))
         (context-result
          (chidu-store-sqlite--search-context state account-id query-key)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-revision
              (chidu-store-search-context-revision context)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id :query-key query-key
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((not
           (equal query-key
                  (chidu-store-search-observation-query-key observation)))
          (chidu-result-failure-create
           :kind 'search-query-mismatch
           :data (list :account-id account-id :query-key query-key)
           :retryable-p nil))
         (t
          (let ((next-revision (1+ actual-revision)))
            (with-sqlite-transaction database
              (let ((change-seq
                     (chidu-store-sqlite--increment-change-seq database)))
                (chidu-sql-execute database
                  [:insert :into jmap-search-projection
                   :row
                   [[account-id [:bind account-id]]
                    [query-key [:bind query-key]]
                    [query-text
                     [:bind
                      (chidu-store-search-observation-query-text observation)]
                     :update]
                    [filter-json
                     [:bind
                      (chidu-store-search-observation-filter-json observation)]
                     :update]
                    [query-state
                     [:bind
                      (chidu-store-search-observation-query-state observation)]
                     :update]
                    [email-state
                     [:bind
                      (chidu-store-search-observation-email-state observation)]
                     :update]
                    [cursor-remote-email-id
                     [:bind
                      (chidu-store-search-observation-cursor-remote-email-id
                       observation)]
                     :update]
                    [revision [:bind next-revision] :update]
                    [maybe-more
                     [:bind
                      (chidu-store-sqlite--integer-bool
                       (chidu-store-search-observation-maybe-more-p
                        observation))]
                     :update]
                    [is-stale 0 :update]
                    [observed-change-seq [:bind change-seq] :update]]
                   :on-conflict [account-id query-key]])
                (chidu-sql-execute database
                  [:delete
                   :from jmap-search-projection-row
                   :where [:and
                           [:= account-id [:bind account-id]]
                           [:= query-key [:bind query-key]]]])
                (chidu-store-sqlite--insert-search-rows
                 database account-id query-key 0
                 (chidu-store-search-observation-rows observation)
                 change-seq)))
            (chidu-store-sqlite--search-context
             state account-id query-key))))))))

(defun chidu-store-sqlite--append-search (state operation)
  "CAS-append Email search page OPERATION in SQLite Store STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id (chidu-store-op-append-search-account-id operation))
         (query-key (chidu-store-op-append-search-query-key operation))
         (expected-revision
          (chidu-store-op-append-search-expected-revision operation))
         (expected-query-state
          (chidu-store-op-append-search-expected-query-state operation))
         (expected-cursor
          (chidu-store-op-append-search-expected-cursor-remote-email-id
           operation))
         (observation (chidu-store-op-append-search-observation operation))
         (context-result
          (chidu-store-sqlite--search-context state account-id query-key)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (actual-revision (chidu-store-search-context-revision context))
             (actual-query-state
              (chidu-store-search-context-query-state context))
             (actual-cursor
              (chidu-store-search-context-cursor-remote-email-id context))
             (rows (chidu-store-search-observation-rows observation))
             (next-cursor
              (chidu-store-search-observation-cursor-remote-email-id
               observation))
             (current-remote-ids
              (cl-loop
               for row across (chidu-store-search-context-rows context)
               collect
               (chidu-store-email-summary-row-remote-email-id
                (chidu-store-search-row-summary-row row))))
             (overlap
              (chidu-store-sqlite--pagination-overlap-id
               current-remote-ids rows
               #'chidu-store-search-observation-row-summary-row)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision actual-revision)))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id :query-key query-key
                       :expected expected-revision :actual actual-revision)
           :retryable-p t))
         ((chidu-store-search-context-stale-p context)
          (chidu-result-failure-create
           :kind 'projection-stale
           :data (list :account-id account-id :query-key query-key)
           :retryable-p t))
         ((not (chidu-store-search-context-maybe-more-p context))
          (chidu-result-failure-create
           :kind 'pagination-exhausted
           :data (list :account-id account-id :query-key query-key)
           :retryable-p nil))
         ((not (and (stringp expected-query-state)
                    (equal expected-query-state actual-query-state)
                    (equal expected-query-state
                           (chidu-store-search-observation-query-state
                            observation))))
          (chidu-result-failure-create
           :kind 'query-state-changed
           :data (list :account-id account-id :query-key query-key)
           :retryable-p t))
         ((not (and (stringp expected-cursor)
                    (equal expected-cursor actual-cursor)))
          (chidu-result-failure-create
           :kind 'pagination-cursor-conflict
           :data (list :account-id account-id :query-key query-key)
           :retryable-p t))
         ((or (not (equal query-key
                          (chidu-store-search-observation-query-key
                           observation)))
              (not (equal (chidu-store-search-context-query-text context)
                          (chidu-store-search-observation-query-text
                           observation)))
              (not (equal (chidu-store-search-context-filter-json context)
                          (chidu-store-search-observation-filter-json
                           observation))))
          (chidu-result-failure-create
           :kind 'search-query-mismatch
           :data (list :account-id account-id :query-key query-key)
           :retryable-p nil))
         ((not (and (stringp next-cursor) (not (string-empty-p next-cursor))))
          (chidu-result-failure-create
           :kind 'pagination-cursor-missing
           :data (list :account-id account-id :query-key query-key)
           :retryable-p t))
         (overlap
          (chidu-result-failure-create
           :kind 'query-page-overlap
           :data (list :account-id account-id :query-key query-key
                       :remote-email-id overlap)
           :retryable-p t))
         (t
          (let ((next-revision (1+ actual-revision))
                (start-ordinal
                 (length (chidu-store-search-context-rows context))))
            (with-sqlite-transaction database
              (let ((change-seq
                     (chidu-store-sqlite--increment-change-seq database)))
                (chidu-store-sqlite--insert-search-rows
                 database account-id query-key start-ordinal rows change-seq)
                (chidu-sql-execute database
                  [:update jmap-search-projection
                   :set
                   [[email-state
                     [:bind
                      (chidu-store-search-observation-email-state observation)]]
                    [cursor-remote-email-id [:bind next-cursor]]
                    [revision [:bind next-revision]]
                    [maybe-more
                     [:bind
                      (chidu-store-sqlite--integer-bool
                       (chidu-store-search-observation-maybe-more-p
                        observation))]]
                    [observed-change-seq [:bind change-seq]]]
                   :where [:and
                           [:= account-id [:bind account-id]]
                           [:= query-key [:bind query-key]]]])))
            (chidu-store-sqlite--search-context
             state account-id query-key))))))))

(provide 'chidu-store-sqlite-sync)

;;; chidu-store-sqlite-sync.el ends here
