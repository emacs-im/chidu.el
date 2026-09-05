;;; chidu-store-sqlite-catchup.el --- SQLite Email catch-up and activation -*- lexical-binding: t; -*-

;;; Commentary:

;; Apply one normalized Email/changes round plus its exact full/mutable
;; Email/get settlements to a building or active generation.  Building closure
;; enters `activating'; active rounds advance the same durable checkpoint in
;; place.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-sync)
(require 'chidu-store-sqlite-hydration)

(defun chidu-store-sqlite--email-local-id
    (database account-id remote-email-id)
  "Return DATABASE local id for REMOTE-EMAIL-ID in ACCOUNT-ID, or nil."
  (caar
   (chidu-sql-select database
     [:select [local-email-id]
              :from jmap-email-record
              :where [:and
                      [:= account-id [:bind account-id]]
                      [:= remote-email-id [:bind remote-email-id]]]
              :limit 1])))

(defun chidu-store-sqlite--email-metadata
    (database account-id local-email-id)
  "Return DATABASE metadata entry for LOCAL-EMAIL-ID in ACCOUNT-ID.

The value is (PROFILE-VERSION . METADATA), or nil when coverage is absent."
  (chidu-sql-one database
      [:select
       [[profile profile-version]
        [remote-blob-id remote-blob-id]
        [remote-thread-id remote-thread-id]
        [size size]
        [received-at received-at]
        [sent-at sent-at]
        [sender-json sender-json]
        [from-json from-json]
        [to-json to-json]
        [cc-json cc-json]
        [bcc-json bcc-json]
        [reply-to-json reply-to-json]
        [subject subject]
        [message-ids-json message-ids-json]
        [in-reply-to-json in-reply-to-json]
        [references-json references-json]
        [has-attachment has-attachment]]
       :from jmap-email-metadata
       :where [:and
               [:= account-id [:bind account-id]]
               [:= local-email-id [:bind local-email-id]]]]
    (cons
     profile
     (chidu-store-email-metadata-create
      :remote-blob-id remote-blob-id
      :remote-thread-id remote-thread-id
      :size size
      :received-at received-at
      :sent-at sent-at
      :sender
      (chidu-store-sqlite--email-address-vector-from-json
       sender-json "Email metadata sender")
      :from
      (chidu-store-sqlite--email-address-vector-from-json
       from-json "Email metadata from")
      :to
      (chidu-store-sqlite--email-address-vector-from-json
       to-json "Email metadata to")
      :cc
      (chidu-store-sqlite--email-address-vector-from-json
       cc-json "Email metadata cc")
      :bcc
      (chidu-store-sqlite--email-address-vector-from-json
       bcc-json "Email metadata bcc")
      :reply-to
      (chidu-store-sqlite--email-address-vector-from-json
       reply-to-json "Email metadata replyTo")
      :subject subject
      :message-ids
      (chidu-store-sqlite--string-vector-from-json
       message-ids-json "Email metadata messageId")
      :in-reply-to
      (chidu-store-sqlite--string-vector-from-json
       in-reply-to-json "Email metadata inReplyTo")
      :references
      (chidu-store-sqlite--string-vector-from-json
       references-json "Email metadata references")
      :has-attachment-p
      (chidu-store-sqlite--bool has-attachment)))))

(defun chidu-store-sqlite--hydration-covers-ids-p
    (hydration kind remote-email-ids)
  "Return non-nil when HYDRATION of KIND exactly covers REMOTE-EMAIL-IDS."
  (if (zerop (length remote-email-ids))
      (null hydration)
    (and
     (chidu-store-email-hydration-observation-p hydration)
     (eq kind (chidu-store-email-hydration-observation-kind hydration))
     (stringp (chidu-store-email-hydration-observation-state hydration))
     (let ((results
            (chidu-store-email-hydration-observation-results hydration)))
       (and
        (= (length remote-email-ids) (length results))
        (cl-loop
         for remote-id across remote-email-ids
         for result across results
         always
         (and
          (chidu-store-email-hydration-result-p result)
          (equal
           remote-id
           (chidu-store-email-hydration-result-remote-email-id result)))))))))

(defun chidu-store-sqlite--catchup-observation-valid-p (observation)
  "Return non-nil when catch-up OBSERVATION has exact profile coverage."
  (when (chidu-store-email-catchup-observation-p observation)
    (let* ((changes
            (chidu-store-email-catchup-observation-changes observation))
           (created
            (and (chidu-store-email-changes-observation-p changes)
                 (chidu-store-email-changes-observation-created changes)))
           (updated
            (and (chidu-store-email-changes-observation-p changes)
                 (chidu-store-email-changes-observation-updated changes))))
      (and
       created updated
       (chidu-store-sqlite--hydration-covers-ids-p
        (chidu-store-email-catchup-observation-full observation)
        'full created)
       (chidu-store-sqlite--hydration-covers-ids-p
        (chidu-store-email-catchup-observation-mutable observation)
        'mutable updated)))))

(defun chidu-store-sqlite--catchup-closed-p (observation)
  "Return non-nil when OBSERVATION proves state-matched round closure."
  (let* ((changes
          (chidu-store-email-catchup-observation-changes observation))
         (new-state
          (chidu-store-email-changes-observation-new-state changes))
         (full (chidu-store-email-catchup-observation-full observation))
         (mutable
          (chidu-store-email-catchup-observation-mutable observation)))
    (and
     (not
      (chidu-store-email-changes-observation-has-more-changes-p changes))
     (or (null full)
         (equal new-state
                (chidu-store-email-hydration-observation-state full)))
     (or (null mutable)
         (equal new-state
                (chidu-store-email-hydration-observation-state mutable))))))

(defun chidu-store-sqlite--metadata-conflict
    (database account-id profile hydration)
  "Return DATABASE metadata conflict for ACCOUNT-ID PROFILE HYDRATION, or nil."
  (when hydration
    (cl-loop
     for result across
     (chidu-store-email-hydration-observation-results hydration)
     when (chidu-store-email-hydration-result-found-p result)
     for remote-id =
     (chidu-store-email-hydration-result-remote-email-id result)
     for local-id =
     (chidu-store-sqlite--email-local-id
      database account-id remote-id)
     for stored =
     (and local-id
          (chidu-store-sqlite--email-metadata
           database account-id local-id))
     thereis
     (cond
      ((and stored (not (equal profile (car stored))))
       (chidu-result-failure-create
        :kind 'email-profile-mismatch
        :data (list :account-id account-id :remote-email-id remote-id
                    :expected profile :actual (car stored))
        :retryable-p nil))
      ((and stored
            (not
             (equal
              (cdr stored)
              (chidu-store-email-hydration-result-metadata result))))
       (chidu-result-failure-create
        :kind 'email-immutable-conflict
        :data (list :account-id account-id :remote-email-id remote-id)
        :retryable-p nil))))))

(defun chidu-store-sqlite--mutable-coverage-conflict
    (database account-id generation-id profile hydration)
  "Return DATABASE conflict for ACCOUNT-ID GENERATION-ID PROFILE HYDRATION."
  (when hydration
    (cl-loop
     for result across
     (chidu-store-email-hydration-observation-results hydration)
     when (chidu-store-email-hydration-result-found-p result)
     for remote-id =
     (chidu-store-email-hydration-result-remote-email-id result)
     for local-id =
     (chidu-store-sqlite--generation-member-local-id
      database account-id generation-id remote-id)
     for stored =
     (and local-id
          (chidu-store-sqlite--email-metadata
           database account-id local-id))
     thereis
     (cond
      ((null local-id)
       (chidu-result-failure-create
        :kind 'email-catchup-missing-member
        :data (list :account-id account-id :remote-email-id remote-id)
        :retryable-p nil))
      ((or (null stored) (not (equal profile (car stored))))
       (chidu-result-failure-create
        :kind 'email-catchup-missing-metadata
        :data (list :account-id account-id :remote-email-id remote-id)
        :retryable-p nil))))))

(defun chidu-store-sqlite--ensure-generation-member
    (database account-id generation-id remote-email-id change-seq next-ordinal)
  "Ensure REMOTE-EMAIL-ID in DATABASE ACCOUNT-ID GENERATION-ID.

Return (LOCAL-ID NEXT-ORDINAL INSERTED-P).  CHANGE-SEQ records a new stable
Email identity."
  (let ((local-id
         (chidu-store-sqlite--generation-member-local-id
          database account-id generation-id remote-email-id)))
    (if local-id
        (list local-id next-ordinal nil)
      (setq local-id
            (chidu-store-sqlite--email-record-id
             database account-id remote-email-id change-seq))
      (chidu-sql-execute database
        [:insert :into jmap-email-generation-member
                 :row
                 [[account-id [:bind account-id]]
                  [generation-id [:bind generation-id]]
                  [local-email-id [:bind local-id]]
                  [ordinal [:bind next-ordinal]]]])
      (list local-id (1+ next-ordinal) t))))

(defun chidu-store-sqlite--remove-generation-remote-id
    (database account-id generation-id remote-email-id)
  "Remove REMOTE-EMAIL-ID from DATABASE ACCOUNT-ID GENERATION-ID when present."
  (when-let* ((local-id
               (chidu-store-sqlite--generation-member-local-id
                database account-id generation-id remote-email-id)))
    (chidu-store-sqlite--remove-generation-member
     database account-id generation-id local-id)))

(defun chidu-store-sqlite--apply-full-catchup-results
    (database account-id generation-id profile hydration change-seq
              next-ordinal collect-new-p)
  "Apply full HYDRATION to DATABASE ACCOUNT-ID GENERATION-ID.

Use PROFILE and CHANGE-SEQ for new metadata.  NEXT-ORDINAL is the next member
ordinal.  When COLLECT-NEW-P is non-nil, return ids newly inserted into the
active generation.  The value is (NEXT-ORDINAL NEW-LOCAL-EMAIL-IDS)."
  (let (new-local-ids)
    (when hydration
      (cl-loop
       for result across
       (chidu-store-email-hydration-observation-results hydration)
       for remote-id =
       (chidu-store-email-hydration-result-remote-email-id result)
       do
       (if (chidu-store-email-hydration-result-found-p result)
           (pcase-let*
               ((`(,local-id ,next ,inserted-p)
                 (chidu-store-sqlite--ensure-generation-member
                  database account-id generation-id remote-id
                  change-seq next-ordinal))
                (stored
                 (chidu-store-sqlite--email-metadata
                  database account-id local-id)))
             (setq next-ordinal next)
             (if stored
                 (chidu-store-sqlite--upsert-email-preview
                  database account-id local-id
                  (chidu-store-email-hydration-result-preview result)
                  change-seq)
               (chidu-store-sqlite--insert-email-metadata
                database account-id local-id profile
                (chidu-store-email-hydration-result-metadata result)
                (chidu-store-email-hydration-result-preview result)
                change-seq))
             (chidu-store-sqlite--replace-generation-mutable
              database account-id generation-id local-id
              (chidu-store-email-hydration-result-remote-mailbox-ids result)
              (chidu-store-email-hydration-result-keywords result))
             (when (and collect-new-p inserted-p)
               (push local-id new-local-ids)))
         (chidu-store-sqlite--remove-generation-remote-id
          database account-id generation-id remote-id))))
    (list next-ordinal (nreverse new-local-ids))))

(defun chidu-store-sqlite--apply-mutable-catchup-results
    (database account-id generation-id hydration)
  "Apply mutable HYDRATION to DATABASE ACCOUNT-ID GENERATION-ID."
  (when hydration
    (cl-loop
     for result across
     (chidu-store-email-hydration-observation-results hydration)
     for remote-id =
     (chidu-store-email-hydration-result-remote-email-id result)
     for local-id =
     (chidu-store-sqlite--generation-member-local-id
      database account-id generation-id remote-id)
     do
     (if (chidu-store-email-hydration-result-found-p result)
         (chidu-store-sqlite--replace-generation-mutable
          database account-id generation-id local-id
          (chidu-store-email-hydration-result-remote-mailbox-ids result)
          (chidu-store-email-hydration-result-keywords result))
       (when local-id
         (chidu-store-sqlite--remove-generation-member
          database account-id generation-id local-id))))))

(defun chidu-store-sqlite--commit-email-catchup-round
    (state context expected-revision observation)
  "Commit catch-up OBSERVATION to STATE under CONTEXT and EXPECTED-REVISION."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-account-account-id
           (chidu-store-email-sync-context-account context)))
         (generation-id
          (chidu-store-email-sync-context-generation-id context))
         (profile (chidu-store-email-sync-context-profile-version context))
         (phase (chidu-store-email-sync-context-phase context))
         (live-p (eq phase 'live))
         (changes
          (chidu-store-email-catchup-observation-changes observation))
         (full (chidu-store-email-catchup-observation-full observation))
         (mutable
          (chidu-store-email-catchup-observation-mutable observation))
         (closed-p (chidu-store-sqlite--catchup-closed-p observation))
         (changed-p
          (> (+ (length (chidu-store-email-changes-observation-created changes))
                (length (chidu-store-email-changes-observation-updated changes))
                (length (chidu-store-email-changes-observation-destroyed changes)))
             0))
         (conflict
          (or
           (chidu-store-sqlite--metadata-conflict
            database account-id profile full)
           (chidu-store-sqlite--mutable-coverage-conflict
            database account-id generation-id profile mutable)))
         new-local-ids)
    (if conflict
        conflict
      (with-sqlite-transaction database
        (let* ((change-seq
                (chidu-store-sqlite--increment-change-seq database))
               (next-ordinal
                (chidu-store-sqlite--generation-next-ordinal
                 database account-id generation-id)))
          (cl-loop
           for remote-id across
           (chidu-store-email-changes-observation-destroyed changes)
           do
           (chidu-store-sqlite--remove-generation-remote-id
            database account-id generation-id remote-id))
          (pcase-let
              ((`(,next ,rows)
                (chidu-store-sqlite--apply-full-catchup-results
                 database account-id generation-id profile full
                 change-seq next-ordinal live-p)))
            (setq next-ordinal next
                  new-local-ids rows))
          (chidu-store-sqlite--apply-mutable-catchup-results
           database account-id generation-id mutable)
          (when (and live-p changed-p)
            (chidu-store-sqlite--invalidate-search-projections
             database account-id change-seq))
          (if live-p
              (chidu-sql-execute database
                [:update jmap-email-checkpoint
                         :set
                         [[phase [:literal "live"]]
                          [state
                           [:bind
                            (chidu-store-email-changes-observation-new-state changes)]]
                          [committed-count
                           [:bind
                            (chidu-store-sqlite--generation-member-count
                             database account-id generation-id)]]
                          [revision [:bind (1+ expected-revision)]]
                          [observed-change-seq [:bind change-seq]]]
                         :where [:= account-id [:bind account-id]]])
            (chidu-sql-execute database
              [:update jmap-email-checkpoint
                       :set
                       [[phase [:bind (if closed-p "activating" "metadata-catchup")]]
                        [state
                         [:bind
                          (chidu-store-email-changes-observation-new-state changes)]]
                        [revision [:bind (1+ expected-revision)]]
                        [observed-change-seq [:bind change-seq]]]
                       :where [:= account-id [:bind account-id]]]))))
      (let ((context-result
             (chidu-store-sqlite--email-context state account-id)))
        (if (chidu-result-failure-p context-result)
            context-result
          (chidu-result-ok-create
           :value
           (chidu-store-email-round-result-create
            :context (chidu-result-ok-value context-result)
            :new-local-email-ids (vconcat new-local-ids)
            :closed-p closed-p
            :changed-p changed-p)))))))

(defun chidu-store-sqlite--apply-email-catchup-round (state operation)
  "CAS-apply one canonical Email catch-up OPERATION to SQLite STATE."
  (let* ((account-id
          (chidu-store-op-apply-email-catchup-round-account-id operation))
         (generation-id
          (chidu-store-op-apply-email-catchup-round-generation-id operation))
         (expected-revision
          (chidu-store-op-apply-email-catchup-round-expected-revision operation))
         (expected-state
          (chidu-store-op-apply-email-catchup-round-expected-state operation))
         (observation
          (chidu-store-op-apply-email-catchup-round-observation operation))
         (context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (changes
              (and (chidu-store-email-catchup-observation-p observation)
                   (chidu-store-email-catchup-observation-changes observation))))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision
                       (chidu-store-email-sync-context-revision context))))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id :expected expected-revision
                       :actual
                       (chidu-store-email-sync-context-revision context))
           :retryable-p t))
         ((not (equal generation-id
                      (chidu-store-email-sync-context-generation-id context)))
          (chidu-result-failure-create
           :kind 'generation-conflict
           :data (list :account-id account-id)
           :retryable-p t))
         ((not (memq (chidu-store-email-sync-context-phase context)
                     '(metadata-catchup live)))
          (chidu-result-failure-create
           :kind 'email-bootstrap-phase-conflict
           :data (list :account-id account-id
                       :phase
                       (chidu-store-email-sync-context-phase context))
           :retryable-p t))
         ((not (chidu-store-sqlite--catchup-observation-valid-p observation))
          (chidu-result-failure-create
           :kind 'email-catchup-observation-mismatch
           :data (list :account-id account-id)
           :retryable-p nil))
         ((or (not (equal expected-state
                          (chidu-store-email-sync-context-state context)))
              (not (equal expected-state
                          (chidu-store-email-changes-observation-old-state
                           changes))))
          (chidu-result-failure-create
           :kind 'state-mismatch
           :data (list :account-id account-id :expected expected-state
                       :actual
                       (chidu-store-email-sync-context-state context))
           :retryable-p t))
         (t
          (chidu-store-sqlite--commit-email-catchup-round
           state context expected-revision observation)))))))

(defun chidu-store-sqlite--generation-member-count
    (database account-id generation-id)
  "Return member count for DATABASE generation GENERATION-ID in ACCOUNT-ID."
  (or
   (caar
    (chidu-sql-select database
      [:select [[:call count 1]]
               :from jmap-email-generation-member
               :where [:and
                       [:= account-id [:bind account-id]]
                       [:= generation-id [:bind generation-id]]]]))
   0))

(defun chidu-store-sqlite--generation-incomplete-metadata-count
    (database account-id generation-id profile)
  "Return DATABASE ACCOUNT-ID GENERATION-ID count lacking PROFILE metadata."
  (or
   (caar
    (chidu-sql-select database
      [:select [[:call count 1]]
               :from [:as jmap-email-generation-member member]
               :joins
               [[:left [:as jmap-email-metadata metadata]
                       :on [:and
                            [:= metadata:account-id member:account-id]
                            [:= metadata:local-email-id member:local-email-id]]]]
               :where
               [:and
                [:= member:account-id [:bind account-id]]
                [:= member:generation-id [:bind generation-id]]
                [:or
                 [:is metadata:local-email-id nil]
                 [:!= metadata:profile-version [:bind profile]]]]]))
   0))

(defun chidu-store-sqlite--generation-missing-mailbox-count
    (database account-id generation-id)
  "Return DATABASE ACCOUNT-ID GENERATION-ID members without Mailbox membership."
  (or
   (caar
    (chidu-sql-select database
      [:select [[:call count 1]]
               :from [:as jmap-email-generation-member member]
               :where
               [:and
                [:= member:account-id [:bind account-id]]
                [:= member:generation-id [:bind generation-id]]
                [:not-exists
                 [:select [1]
                          :from [:as jmap-email-generation-mailbox membership]
                          :where [:and
                                  [:= membership:account-id member:account-id]
                                  [:= membership:generation-id member:generation-id]
                                  [:= membership:local-email-id member:local-email-id]]]]]]))
   0))

(defun chidu-store-sqlite--generation-unavailable-mailbox-count
    (database account-id generation-id)
  "Return DATABASE ACCOUNT-ID GENERATION-ID Mailbox ids absent from snapshot."
  (or
   (caar
    (chidu-sql-select database
      [:select [[:call count 1]]
               :from [:as jmap-email-generation-mailbox membership]
               :where
               [:and
                [:= membership:account-id [:bind account-id]]
                [:= membership:generation-id [:bind generation-id]]
                [:not-exists
                 [:select [1]
                          :from [:as jmap-mailbox mailbox]
                          :where [:and
                                  [:= mailbox:account-id membership:account-id]
                                  [:= mailbox:remote-mailbox-id
                                      membership:remote-mailbox-id]
                                  [:= mailbox:is-available 1]
                                  [:= mailbox:may-read-items 1]]]]]]))
   0))

(defun chidu-store-sqlite--building-generation-p
    (database account-id generation-id profile)
  "Return non-nil for DATABASE building GENERATION-ID with PROFILE in ACCOUNT-ID."
  (and
   (car
    (chidu-sql-select database
      [:select [1]
               :from jmap-email-generation
               :where [:and
                       [:= account-id [:bind account-id]]
                       [:= generation-id [:bind generation-id]]
                       [:= lifecycle [:literal "building"]]
                       [:= profile-version [:bind profile]]]
               :limit 1]))
   t))

(defun chidu-store-sqlite--activation-readiness-failure
    (database account-id generation-id profile)
  "Return DATABASE activation failure for ACCOUNT-ID GENERATION-ID PROFILE."
  (let ((incomplete
         (chidu-store-sqlite--generation-incomplete-metadata-count
          database account-id generation-id profile)))
    (cond
     ((> incomplete 0)
      (chidu-result-failure-create
       :kind 'email-generation-incomplete-metadata
       :data (list :account-id account-id :count incomplete)
       :retryable-p nil))
     ((let ((missing
             (chidu-store-sqlite--generation-missing-mailbox-count
              database account-id generation-id)))
        (when (> missing 0)
          (chidu-result-failure-create
           :kind 'email-generation-missing-mailbox
           :data (list :account-id account-id :count missing)
           :retryable-p nil))))
     ((let ((unavailable
             (chidu-store-sqlite--generation-unavailable-mailbox-count
              database account-id generation-id)))
        (when (> unavailable 0)
          (chidu-result-failure-create
           :kind 'email-generation-mailbox-unavailable
           :data (list :account-id account-id :count unavailable)
           :retryable-p t)))))))

(defun chidu-store-sqlite--publish-email-generation
    (state database account-id generation-id expected-revision)
  "Publish DATABASE GENERATION-ID for ACCOUNT-ID in STATE.

EXPECTED-REVISION fences the checkpoint transition."
  (let ((member-count
         (chidu-store-sqlite--generation-member-count
          database account-id generation-id)))
    (with-sqlite-transaction database
      (let ((change-seq
             (chidu-store-sqlite--increment-change-seq database)))
        (chidu-sql-execute database
          [:update jmap-email-generation
                   :set [[lifecycle [:literal "retired"]]]
                   :where [:and
                           [:= account-id [:bind account-id]]
                           [:= lifecycle [:literal "active"]]]])
        (chidu-sql-execute database
          [:update jmap-email-generation
                   :set [[lifecycle [:literal "active"]]]
                   :where [:and
                           [:= account-id [:bind account-id]]
                           [:= generation-id [:bind generation-id]]
                           [:= lifecycle [:literal "building"]]]])
        (chidu-sql-execute database
          [:update jmap-email-checkpoint
                   :set
                   [[phase [:literal "live"]]
                    [query-state nil]
                    [can-calculate-changes nil]
                    [committed-count [:bind member-count]]
                    [anchor-remote-email-id nil]
                    [hydration-after-local-email-id nil]
                    [revision [:bind (1+ expected-revision)]]
                    [observed-change-seq [:bind change-seq]]]
                   :where [:= account-id [:bind account-id]]])))
    (chidu-store-sqlite--email-context state account-id)))

(defun chidu-store-sqlite--activate-email-generation (state operation)
  "CAS-activate building Email generation OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-op-activate-email-generation-account-id operation))
         (generation-id
          (chidu-store-op-activate-email-generation-generation-id operation))
         (expected-revision
          (chidu-store-op-activate-email-generation-expected-revision operation))
         (expected-state
          (chidu-store-op-activate-email-generation-expected-state operation))
         (context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (profile (chidu-store-email-sync-context-profile-version context)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision
                       (chidu-store-email-sync-context-revision context))))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data (list :account-id account-id :expected expected-revision
                       :actual
                       (chidu-store-email-sync-context-revision context))
           :retryable-p t))
         ((not (equal generation-id
                      (chidu-store-email-sync-context-generation-id context)))
          (chidu-result-failure-create
           :kind 'generation-conflict
           :data (list :account-id account-id)
           :retryable-p t))
         ((not (eq 'activating
                   (chidu-store-email-sync-context-phase context)))
          (chidu-result-failure-create
           :kind 'email-bootstrap-phase-conflict
           :data (list :account-id account-id
                       :phase
                       (chidu-store-email-sync-context-phase context))
           :retryable-p t))
         ((not (equal expected-state
                      (chidu-store-email-sync-context-state context)))
          (chidu-result-failure-create
           :kind 'state-mismatch
           :data (list :account-id account-id :expected expected-state
                       :actual
                       (chidu-store-email-sync-context-state context))
           :retryable-p t))
         ((not
           (chidu-store-sqlite--building-generation-p
            database account-id generation-id profile))
          (chidu-result-failure-create
           :kind 'generation-conflict
           :data (list :account-id account-id)
           :retryable-p t))
         ((chidu-store-sqlite--activation-readiness-failure
           database account-id generation-id profile))
         (t
          (chidu-store-sqlite--publish-email-generation
           state database account-id generation-id expected-revision)))))))

(provide 'chidu-store-sqlite-catchup)

;;; chidu-store-sqlite-catchup.el ends here
