;;; chidu-store-sqlite-hydration.el --- SQLite Email hydration -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage-aware metadata hydration for a building Email generation.  Plans
;; are homogeneous, and each closed commit preserves the Store-plan order.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-sync)

(defun chidu-store-sqlite--email-hydration-candidates
    (database account-id generation-id after limit)
  "Return bounded hydration candidate rows from DATABASE after AFTER.

ACCOUNT-ID and GENERATION-ID select the building generation; LIMIT bounds the
result."
  (chidu-sql-select database
    [:select
     [member:local-email-id email:remote-email-id metadata:profile-version]
     :from [:as jmap-email-generation-member member]
     :joins
     [[:inner [:as jmap-email-record email]
       :on [:and
            [:= email:account-id member:account-id]
            [:= email:local-email-id member:local-email-id]]]
      [:left [:as jmap-email-metadata metadata]
       :on [:and
            [:= metadata:account-id member:account-id]
            [:= metadata:local-email-id member:local-email-id]]]]
     :where
     [:and
      [:= member:account-id [:bind account-id]]
      [:= member:generation-id [:bind generation-id]]
      [:> member:local-email-id
          [:call coalesce [:bind after] [:literal ""]]]]
     :order-by [[member:local-email-id :asc]]
     :limit [:bind limit]]))

(defun chidu-store-sqlite--email-hydration-kind (stored-profile profile)
  "Return hydration kind for STORED-PROFILE under required PROFILE."
  (cond
   ((null stored-profile) 'full)
   ((equal stored-profile profile) 'mutable)
   (t nil)))

(defun chidu-store-sqlite--next-email-hydration-plan
    (database context limit)
  "Return DATABASE and CONTEXT's next homogeneous hydration plan.

LIMIT bounds candidate rows.  Return a typed failure on profile drift."
  (let* ((account-id
          (chidu-store-account-account-id
           (chidu-store-email-sync-context-account context)))
         (generation-id
          (chidu-store-email-sync-context-generation-id context))
         (profile (chidu-store-email-sync-context-profile-version context))
         (rows
          (chidu-store-sqlite--email-hydration-candidates
           database account-id generation-id
           (chidu-store-email-sync-context-hydration-after-local-email-id
            context)
           limit)))
    (if (null rows)
        (chidu-store-email-hydration-plan-create :targets (vector))
      (let* ((first (car rows))
             (kind
              (chidu-store-sqlite--email-hydration-kind
               (nth 2 first) profile)))
        (if (null kind)
            (chidu-result-failure-create
             :kind 'email-profile-mismatch
             :data
             (list :account-id account-id
                   :local-email-id (nth 0 first)
                   :expected profile
                   :actual (nth 2 first))
             :retryable-p nil)
          (chidu-store-email-hydration-plan-create
           :kind kind
           :targets
           (vconcat
            (cl-loop
             for (local-id remote-id stored-profile) in rows
             while
             (eq kind
                 (chidu-store-sqlite--email-hydration-kind
                  stored-profile profile))
             collect
             (chidu-store-email-hydration-target-create
              :local-email-id local-id
              :remote-email-id remote-id)))))))))

(defun chidu-store-sqlite--email-hydration-plan (state account-id limit)
  "Return ACCOUNT-ID's next metadata hydration plan from SQLite STATE."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'positive-integer-p limit)))
  (let ((context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let ((context (chidu-result-ok-value context-result)))
        (if (not (eq 'hydrating
                     (chidu-store-email-sync-context-phase context)))
            (chidu-result-failure-create
             :kind 'email-bootstrap-phase-conflict
             :data (list :account-id account-id
                         :phase
                         (chidu-store-email-sync-context-phase context))
             :retryable-p t)
          (let ((plan
                 (chidu-store-sqlite--next-email-hydration-plan
                  (chidu-store-sqlite--assert-open state) context limit)))
            (if (chidu-result-failure-p plan)
                plan
              (chidu-result-ok-create :value plan))))))))

(defun chidu-store-sqlite--upsert-email-preview
    (database account-id local-email-id preview change-seq)
  "Upsert refreshable PREVIEW for LOCAL-EMAIL-ID in DATABASE.

ACCOUNT-ID scopes the Email and CHANGE-SEQ records observation time."
  (chidu-sql-execute database
    [:insert :into jmap-email-preview
     :row
     [[account-id [:bind account-id]]
      [local-email-id [:bind local-email-id]]
      [value [:bind preview] :update]
      [observed-change-seq [:bind change-seq] :update]]
     :on-conflict [account-id local-email-id]]))

(defun chidu-store-sqlite--insert-email-metadata
    (database account-id local-email-id profile metadata preview change-seq)
  "Insert immutable METADATA and PREVIEW into DATABASE.

ACCOUNT-ID, LOCAL-EMAIL-ID, PROFILE, and CHANGE-SEQ identify the observation."
  (chidu-sql-execute database
    [:insert :into jmap-email-metadata
     :row
     [[account-id [:bind account-id]]
      [local-email-id [:bind local-email-id]]
      [profile-version [:bind profile]]
      [remote-blob-id
       [:bind (chidu-store-email-metadata-remote-blob-id metadata)]]
      [remote-thread-id
       [:bind (chidu-store-email-metadata-remote-thread-id metadata)]]
      [size [:bind (chidu-store-email-metadata-size metadata)]]
      [received-at [:bind (chidu-store-email-metadata-received-at metadata)]]
      [sent-at [:bind (chidu-store-email-metadata-sent-at metadata)]]
      [sender-json
       [:bind
        (chidu-store-sqlite--email-address-vector-json
         (chidu-store-email-metadata-sender metadata))]]
      [from-json
       [:bind
        (chidu-store-sqlite--email-address-vector-json
         (chidu-store-email-metadata-from metadata))]]
      [to-json
       [:bind
        (chidu-store-sqlite--email-address-vector-json
         (chidu-store-email-metadata-to metadata))]]
      [cc-json
       [:bind
        (chidu-store-sqlite--email-address-vector-json
         (chidu-store-email-metadata-cc metadata))]]
      [bcc-json
       [:bind
        (chidu-store-sqlite--email-address-vector-json
         (chidu-store-email-metadata-bcc metadata))]]
      [reply-to-json
       [:bind
        (chidu-store-sqlite--email-address-vector-json
         (chidu-store-email-metadata-reply-to metadata))]]
      [subject [:bind (chidu-store-email-metadata-subject metadata)]]
      [message-ids-json
       [:bind
        (chidu-store-sqlite--string-vector-json
         (chidu-store-email-metadata-message-ids metadata)
         "Email metadata messageId")]]
      [in-reply-to-json
       [:bind
        (chidu-store-sqlite--string-vector-json
         (chidu-store-email-metadata-in-reply-to metadata)
         "Email metadata inReplyTo")]]
      [references-json
       [:bind
        (chidu-store-sqlite--string-vector-json
         (chidu-store-email-metadata-references metadata)
         "Email metadata references")]]
      [has-attachment
       [:bind
        (chidu-store-sqlite--integer-bool
         (chidu-store-email-metadata-has-attachment-p metadata))]]
      [observed-change-seq [:bind change-seq]]]])
  (chidu-store-sqlite--upsert-email-preview
   database account-id local-email-id preview change-seq))

(defun chidu-store-sqlite--replace-generation-mutable
    (database account-id generation-id local-email-id mailbox-ids keywords)
  "Replace one DATABASE generation member's MAILBOX-IDS and KEYWORDS.

ACCOUNT-ID, GENERATION-ID, and LOCAL-EMAIL-ID identify the member."
  (chidu-sql-execute database
    [:delete :from jmap-email-generation-mailbox
     :where [:and
             [:= account-id [:bind account-id]]
             [:= generation-id [:bind generation-id]]
             [:= local-email-id [:bind local-email-id]]]])
  (chidu-sql-execute database
    [:delete :from jmap-email-generation-keyword
     :where [:and
             [:= account-id [:bind account-id]]
             [:= generation-id [:bind generation-id]]
             [:= local-email-id [:bind local-email-id]]]])
  (cl-loop
   for remote-mailbox-id across mailbox-ids
   do
   (chidu-sql-execute database
     [:insert :into jmap-email-generation-mailbox
      :row
      [[account-id [:bind account-id]]
       [generation-id [:bind generation-id]]
       [local-email-id [:bind local-email-id]]
       [remote-mailbox-id [:bind remote-mailbox-id]]]]))
  (cl-loop
   for keyword across keywords
   do
   (chidu-sql-execute database
     [:insert :into jmap-email-generation-keyword
      :row
      [[account-id [:bind account-id]]
       [generation-id [:bind generation-id]]
       [local-email-id [:bind local-email-id]]
       [keyword [:bind keyword]]]])))

(defun chidu-store-sqlite--remove-generation-member
    (database account-id generation-id local-email-id)
  "Remove LOCAL-EMAIL-ID from DATABASE generation GENERATION-ID.

ACCOUNT-ID scopes the generation."
  (chidu-sql-execute database
    [:delete :from jmap-email-generation-member
     :where [:and
             [:= account-id [:bind account-id]]
             [:= generation-id [:bind generation-id]]
             [:= local-email-id [:bind local-email-id]]]]))

(defun chidu-store-sqlite--hydration-plan-matches-p (plan observation)
  "Return non-nil when OBSERVATION exactly settles PLAN in order."
  (let ((targets (chidu-store-email-hydration-plan-targets plan))
        (results (chidu-store-email-hydration-observation-results observation)))
    (and (eq (chidu-store-email-hydration-plan-kind plan)
             (chidu-store-email-hydration-observation-kind observation))
         (= (length targets) (length results))
         (cl-loop
          for target across targets
          for result across results
          always
          (equal
           (chidu-store-email-hydration-target-remote-email-id target)
           (chidu-store-email-hydration-result-remote-email-id result))))))

(defun chidu-store-sqlite--commit-email-hydration
    (state context expected-revision observation)
  "Commit ordered OBSERVATION to STATE under CONTEXT.

EXPECTED-REVISION fences the closed transition."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (account-id
          (chidu-store-account-account-id
           (chidu-store-email-sync-context-account context)))
         (generation-id
          (chidu-store-email-sync-context-generation-id context))
         (profile (chidu-store-email-sync-context-profile-version context))
         (results (chidu-store-email-hydration-observation-results observation))
         (plan
          (chidu-store-sqlite--next-email-hydration-plan
           database context (length results))))
    (cond
     ((chidu-result-failure-p plan) plan)
     ((or (zerop (length results))
          (not (chidu-store-sqlite--hydration-plan-matches-p
                plan observation)))
      (chidu-result-failure-create
       :kind 'email-hydration-plan-changed
       :data (list :account-id account-id)
       :retryable-p t))
     (t
      (let* ((targets (chidu-store-email-hydration-plan-targets plan))
             (kind (chidu-store-email-hydration-plan-kind plan))
             (last-local-id
              (chidu-store-email-hydration-target-local-email-id
               (aref targets (1- (length targets))))))
        (with-sqlite-transaction database
          (let ((change-seq
                 (chidu-store-sqlite--increment-change-seq database)))
            (cl-loop
             for target across targets
             for result across results
             for local-id =
             (chidu-store-email-hydration-target-local-email-id target)
             do
             (if (chidu-store-email-hydration-result-found-p result)
                 (progn
                   (when (eq kind 'full)
                     (chidu-store-sqlite--insert-email-metadata
                      database account-id local-id profile
                      (chidu-store-email-hydration-result-metadata result)
                      (chidu-store-email-hydration-result-preview result)
                      change-seq))
                   (chidu-store-sqlite--replace-generation-mutable
                    database account-id generation-id local-id
                    (chidu-store-email-hydration-result-remote-mailbox-ids
                     result)
                    (chidu-store-email-hydration-result-keywords result)))
               (chidu-store-sqlite--remove-generation-member
                database account-id generation-id local-id)))
            (chidu-sql-execute database
              [:update jmap-email-checkpoint
               :set
               [[hydration-after-local-email-id [:bind last-local-id]]
                [revision [:bind (1+ expected-revision)]]
                [observed-change-seq [:bind change-seq]]]
               :where [:= account-id [:bind account-id]]])))
        (chidu-store-sqlite--email-context state account-id))))))

(defun chidu-store-sqlite--apply-email-hydration (state operation)
  "CAS-apply exact metadata hydration OPERATION to SQLite STATE."
  (let* ((account-id
          (chidu-store-op-apply-email-hydration-account-id operation))
         (expected-revision
          (chidu-store-op-apply-email-hydration-expected-revision operation))
         (context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let ((context (chidu-result-ok-value context-result)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision
                       (chidu-store-email-sync-context-revision context))))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data
           (list :account-id account-id :expected expected-revision
                 :actual
                 (chidu-store-email-sync-context-revision context))
           :retryable-p t))
         ((not (equal
                (chidu-store-op-apply-email-hydration-generation-id operation)
                (chidu-store-email-sync-context-generation-id context)))
          (chidu-result-failure-create
           :kind 'generation-conflict
           :data (list :account-id account-id)
           :retryable-p t))
         ((not (eq 'hydrating
                   (chidu-store-email-sync-context-phase context)))
          (chidu-result-failure-create
           :kind 'email-bootstrap-phase-conflict
           :data (list :account-id account-id
                       :phase
                       (chidu-store-email-sync-context-phase context))
           :retryable-p t))
         ((not
           (chidu-store-email-hydration-observation-p
            (chidu-store-op-apply-email-hydration-observation operation)))
          (signal 'wrong-type-argument
                  (list
                   'chidu-store-email-hydration-observation-p
                   (chidu-store-op-apply-email-hydration-observation
                    operation))))
         (t
          (chidu-store-sqlite--commit-email-hydration
           state context expected-revision
           (chidu-store-op-apply-email-hydration-observation operation))))))))

(defun chidu-store-sqlite--finish-email-hydration (state operation)
  "CAS-finish exhausted metadata hydration OPERATION in SQLite STATE."
  (let* ((account-id
          (chidu-store-op-finish-email-hydration-account-id operation))
         (expected-revision
          (chidu-store-op-finish-email-hydration-expected-revision operation))
         (context-result (chidu-store-sqlite--email-context state account-id)))
    (if (chidu-result-failure-p context-result)
        context-result
      (let* ((context (chidu-result-ok-value context-result))
             (database (chidu-store-sqlite--assert-open state)))
        (cond
         ((not (and (integerp expected-revision)
                    (= expected-revision
                       (chidu-store-email-sync-context-revision context))))
          (chidu-result-failure-create
           :kind 'revision-conflict
           :data
           (list :account-id account-id :expected expected-revision
                 :actual
                 (chidu-store-email-sync-context-revision context))
           :retryable-p t))
         ((not (equal
                (chidu-store-op-finish-email-hydration-generation-id operation)
                (chidu-store-email-sync-context-generation-id context)))
          (chidu-result-failure-create
           :kind 'generation-conflict
           :data (list :account-id account-id)
           :retryable-p t))
         ((not (eq 'hydrating
                   (chidu-store-email-sync-context-phase context)))
          (chidu-result-failure-create
           :kind 'email-bootstrap-phase-conflict
           :data (list :account-id account-id
                       :phase
                       (chidu-store-email-sync-context-phase context))
           :retryable-p t))
         (t
          (let ((plan
                 (chidu-store-sqlite--next-email-hydration-plan
                  database context 1)))
            (cond
             ((chidu-result-failure-p plan) plan)
             ((> (length
                  (chidu-store-email-hydration-plan-targets plan))
                 0)
              (chidu-result-failure-create
               :kind 'email-hydration-not-exhausted
               :data (list :account-id account-id)
               :retryable-p t))
             (t
              (with-sqlite-transaction database
                (let ((change-seq
                       (chidu-store-sqlite--increment-change-seq database)))
                  (chidu-sql-execute database
                    [:update jmap-email-checkpoint
                     :set
                     [[phase [:literal "metadata-catchup"]]
                      [hydration-after-local-email-id nil]
                      [revision [:bind (1+ expected-revision)]]
                      [observed-change-seq [:bind change-seq]]]
                     :where [:= account-id [:bind account-id]]])))
              (chidu-store-sqlite--email-context state account-id))))))))))

(provide 'chidu-store-sqlite-hydration)

;;; chidu-store-sqlite-hydration.el ends here
