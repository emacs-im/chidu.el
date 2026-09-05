;;; chidu-store-sqlite-generation.el --- Active Email generation views -*- lexical-binding: t; -*-

;;; Commentary:

;; The active Email generation is Chidu's canonical source for Mailbox
;; membership and keywords.  Mailbox Summary is a bounded read model derived
;; directly from it; successful local mutations update the same generation.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'seq)
(require 'subr-x)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)

(defun chidu-store-sqlite--active-email-generation-id (database account-id)
  "Return DATABASE's active Email generation id for ACCOUNT-ID, or nil."
  (caar
   (chidu-sql-select database
     [:select [generation-id]
              :from jmap-email-generation
              :where [:and
                      [:= account-id [:bind account-id]]
                      [:= lifecycle [:literal "active"]]]
              :limit 1])))

(defun chidu-store-sqlite--active-generation-for-email
    (database account-id local-email-id)
  "Return active generation containing LOCAL-EMAIL-ID in DATABASE ACCOUNT-ID."
  (caar
   (chidu-sql-select database
     [:select [generation:generation-id]
              :from [:as jmap-email-generation generation]
              :joins
              [[:inner [:as jmap-email-generation-member member]
                       :on [:and
                            [:= member:account-id generation:account-id]
                            [:= member:generation-id generation:generation-id]]]]
              :where [:and
                      [:= generation:account-id [:bind account-id]]
                      [:= generation:lifecycle [:literal "active"]]
                      [:= member:local-email-id [:bind local-email-id]]]
              :limit 1])))

(defun chidu-store-sqlite--set-active-email-keyword
    (database account-id local-email-id keyword present-p)
  "Set KEYWORD presence for one active Email in DATABASE.

ACCOUNT-ID and LOCAL-EMAIL-ID identify the Email.  Do nothing when the Email is
not a member of the active generation."
  (when-let* ((generation-id
               (chidu-store-sqlite--active-generation-for-email
                database account-id local-email-id)))
    (if present-p
        (chidu-sql-execute database
          [:insert :or :ignore :into jmap-email-generation-keyword
                   :row
                   [[account-id [:bind account-id]]
                    [generation-id [:bind generation-id]]
                    [local-email-id [:bind local-email-id]]
                    [keyword [:bind keyword]]]])
      (chidu-sql-execute database
        [:delete :from jmap-email-generation-keyword
                 :where [:and
                         [:= account-id [:bind account-id]]
                         [:= generation-id [:bind generation-id]]
                         [:= local-email-id [:bind local-email-id]]
                         [:= keyword [:bind keyword]]]]))))

(defun chidu-store-sqlite--move-active-email
    (database account-id local-email-id source-remote-id destination-remote-id)
  "Move one active Email between remote Mailboxes in DATABASE.

ACCOUNT-ID and LOCAL-EMAIL-ID identify the Email.  Remove SOURCE-REMOTE-ID and
add DESTINATION-REMOTE-ID while preserving every other membership."
  (when-let* ((generation-id
               (chidu-store-sqlite--active-generation-for-email
                database account-id local-email-id)))
    (chidu-sql-execute database
      [:delete :from jmap-email-generation-mailbox
               :where [:and
                       [:= account-id [:bind account-id]]
                       [:= generation-id [:bind generation-id]]
                       [:= local-email-id [:bind local-email-id]]
                       [:= remote-mailbox-id [:bind source-remote-id]]]])
    (chidu-sql-execute database
      [:insert :or :ignore :into jmap-email-generation-mailbox
               :row
               [[account-id [:bind account-id]]
                [generation-id [:bind generation-id]]
                [local-email-id [:bind local-email-id]]
                [remote-mailbox-id [:bind destination-remote-id]]]])))

(defun chidu-store-sqlite--replace-active-email-mailboxes
    (database account-id local-email-id remote-mailbox-ids)
  "Replace one active Email's Mailboxes in DATABASE.

ACCOUNT-ID and LOCAL-EMAIL-ID identify the Email; REMOTE-MAILBOX-IDS is the
complete nonempty replacement set."
  (unless (and (vectorp remote-mailbox-ids)
               (> (length remote-mailbox-ids) 0))
    (signal 'chidu-invariant-error
            '("Active Email requires at least one Mailbox")))
  (when-let* ((generation-id
               (chidu-store-sqlite--active-generation-for-email
                database account-id local-email-id)))
    (chidu-sql-execute database
      [:delete :from jmap-email-generation-mailbox
               :where [:and
                       [:= account-id [:bind account-id]]
                       [:= generation-id [:bind generation-id]]
                       [:= local-email-id [:bind local-email-id]]]])
    (cl-loop
     for remote-id across remote-mailbox-ids
     do
     (chidu-sql-execute database
       [:insert :into jmap-email-generation-mailbox
                :row
                [[account-id [:bind account-id]]
                 [generation-id [:bind generation-id]]
                 [local-email-id [:bind local-email-id]]
                 [remote-mailbox-id [:bind remote-id]]]]))))

(defconst chidu-store-sqlite--summary-sparse-mailbox-limit 4096
  "Maximum Mailbox size for the membership-first Summary query plan.

Small Mailboxes should not scan the Account's entire recent-metadata index;
large Mailboxes should not sort their complete membership to return one page.")

(defun chidu-store-sqlite--canonical-summary-row
    (local-id remote-id thread-id received-at from-json subject preview
              seen flagged desired-seen has-attachment)
  "Build one canonical Summary row from typed SQLite fields.

LOCAL-ID, REMOTE-ID, THREAD-ID, RECEIVED-AT, FROM-JSON, SUBJECT, PREVIEW,
SEEN, FLAGGED, DESIRED-SEEN, and HAS-ATTACHMENT are one decoded query row."
  (let* ((addresses
          (chidu-store-sqlite--email-address-vector-from-json
           from-json "Canonical Email from"))
         (from (and (> (length addresses) 0) (aref addresses 0))))
    (chidu-store-email-summary-row-create
     :local-email-id local-id
     :remote-email-id remote-id
     :remote-thread-id thread-id
     :received-at received-at
     :from-name (and from (chidu-store-email-address-name from))
     :from-email (and from (chidu-store-email-address-email from))
     :subject subject
     :preview preview
     :unread-p
     (if (integerp desired-seen)
         (not (chidu-store-sqlite--bool desired-seen))
       (null seen))
     :flagged-p (and flagged t)
     :has-attachment-p (chidu-store-sqlite--bool has-attachment))))

(defmacro chidu-store-sqlite--map-canonical-summary
    (database recent-query generation-id)
  "Map DATABASE Summary rows over bounded RECENT-QUERY.

GENERATION-ID selects canonical keyword state.  RECENT-QUERY must return the
canonical metadata columns consumed below in newest-first order."
  (declare (indent 1) (debug t))
  `(chidu-sql-map ,database
       [:select
        [[local-id recent:local-email-id]
         [remote-id email:remote-email-id]
         [thread-id recent:remote-thread-id]
         [received-at recent:received-at]
         [from-json recent:from-json]
         [subject recent:subject]
         [preview [:call coalesce preview:value [:literal ""]]]
         [seen seen:local-email-id]
         [flagged flagged:local-email-id]
         [desired-seen intent:desired-seen]
         [has-attachment recent:has-attachment]]
        :from [:as ,recent-query recent]
        :joins
        [[:inner [:as jmap-email-record email]
                 :on [:and
                      [:= email:account-id recent:account-id]
                      [:= email:local-email-id recent:local-email-id]]]
         [:left [:as jmap-email-preview preview]
                :on [:and
                     [:= preview:account-id recent:account-id]
                     [:= preview:local-email-id recent:local-email-id]]]
         [:left [:as jmap-email-generation-keyword seen]
                :on [:and
                     [:= seen:account-id recent:account-id]
                     [:= seen:generation-id [:bind ,generation-id]]
                     [:= seen:local-email-id recent:local-email-id]
                     [:= seen:keyword [:literal "$seen"]]]]
         [:left [:as jmap-email-generation-keyword flagged]
                :on [:and
                     [:= flagged:account-id recent:account-id]
                     [:= flagged:generation-id [:bind ,generation-id]]
                     [:= flagged:local-email-id recent:local-email-id]
                     [:= flagged:keyword [:literal "$flagged"]]]]
         [:left [:as jmap-seen-intent intent]
                :on [:and
                     [:= intent:account-id recent:account-id]
                     [:= intent:local-email-id recent:local-email-id]]]]
        :order-by [[recent:received-at :desc] [recent:local-email-id :desc]]]
     (chidu-store-sqlite--canonical-summary-row
      local-id remote-id thread-id received-at from-json subject preview
      seen flagged desired-seen has-attachment)))

(defun chidu-store-sqlite--recent-first-summary-records
    (database account-id generation-id remote-mailbox-id mailbox-id probe-limit)
  "Return recent-first canonical Summary records from DATABASE.

ACCOUNT-ID, GENERATION-ID, REMOTE-MAILBOX-ID, and MAILBOX-ID scope the view;
PROBE-LIMIT bounds returned rows."
  (chidu-store-sqlite--map-canonical-summary database
    [:select
     [metadata:account-id metadata:local-email-id
                          metadata:remote-thread-id metadata:received-at metadata:from-json
                          metadata:subject metadata:has-attachment]
     :from [:as jmap-email-metadata metadata]
     :where
     [:and
      [:= metadata:account-id [:bind account-id]]
      [:exists
       [:select [1]
                :from [:as jmap-email-generation-mailbox membership]
                :where [:and
                        [:= membership:account-id metadata:account-id]
                        [:= membership:generation-id [:bind generation-id]]
                        [:= membership:local-email-id metadata:local-email-id]
                        [:= membership:remote-mailbox-id
                            [:bind remote-mailbox-id]]]]]
      [:not-exists
       [:select [1]
                :from [:as jmap-mailbox-move-target move-target]
                :joins
                [[:inner [:as jmap-mailbox-move move]
                         :on [:and
                              [:= move:operation-id move-target:operation-id]
                              [:= move:account-id move-target:account-id]]]]
                :where [:and
                        [:= move-target:account-id metadata:account-id]
                        [:= move-target:local-email-id metadata:local-email-id]
                        [:= move:source-mailbox-id [:bind mailbox-id]]]]]
      [:not-exists
       [:select [1]
                :from [:as jmap-trash-target trash-target]
                :joins
                [[:inner [:as jmap-trash-operation trash]
                         :on [:and
                              [:= trash:operation-id trash-target:operation-id]
                              [:= trash:account-id trash-target:account-id]]]]
                :where [:and
                        [:= trash-target:account-id metadata:account-id]
                        [:= trash-target:local-email-id metadata:local-email-id]
                        [:!= trash:trash-mailbox-id [:bind mailbox-id]]]]]]
     :order-by [[metadata:received-at :desc]
                [metadata:local-email-id :desc]]
     :limit [:bind probe-limit]]
    generation-id))

(defun chidu-store-sqlite--membership-first-summary-records
    (database account-id generation-id remote-mailbox-id mailbox-id probe-limit)
  "Return membership-first canonical Summary records from DATABASE.

ACCOUNT-ID, GENERATION-ID, REMOTE-MAILBOX-ID, and MAILBOX-ID scope the view;
PROBE-LIMIT bounds returned rows."
  (chidu-store-sqlite--map-canonical-summary database
    [:select
     [membership:account-id membership:local-email-id
                            metadata:remote-thread-id metadata:received-at metadata:from-json
                            metadata:subject metadata:has-attachment]
     :from [:as jmap-email-generation-mailbox membership]
     :joins
     [[:inner [:as jmap-email-metadata metadata]
              :on [:and
                   [:= metadata:account-id membership:account-id]
                   [:= metadata:local-email-id membership:local-email-id]]]]
     :where
     [:and
      [:= membership:account-id [:bind account-id]]
      [:= membership:generation-id [:bind generation-id]]
      [:= membership:remote-mailbox-id [:bind remote-mailbox-id]]
      [:not-exists
       [:select [1]
                :from [:as jmap-mailbox-move-target move-target]
                :joins
                [[:inner [:as jmap-mailbox-move move]
                         :on [:and
                              [:= move:operation-id move-target:operation-id]
                              [:= move:account-id move-target:account-id]]]]
                :where [:and
                        [:= move-target:account-id membership:account-id]
                        [:= move-target:local-email-id membership:local-email-id]
                        [:= move:source-mailbox-id [:bind mailbox-id]]]]]
      [:not-exists
       [:select [1]
                :from [:as jmap-trash-target trash-target]
                :joins
                [[:inner [:as jmap-trash-operation trash]
                         :on [:and
                              [:= trash:operation-id trash-target:operation-id]
                              [:= trash:account-id trash-target:account-id]]]]
                :where [:and
                        [:= trash-target:account-id membership:account-id]
                        [:= trash-target:local-email-id membership:local-email-id]
                        [:!= trash:trash-mailbox-id [:bind mailbox-id]]]]]]
     :order-by [[metadata:received-at :desc]
                [metadata:local-email-id :desc]]
     :limit [:bind probe-limit]]
    generation-id))

(defun chidu-store-sqlite--active-email-summary-row
    (database account-id generation-id local-email-id)
  "Return one active Summary row from DATABASE, or nil.

ACCOUNT-ID, GENERATION-ID, and LOCAL-EMAIL-ID identify the canonical Email."
  (car
   (chidu-store-sqlite--map-canonical-summary database
     [:select
      [metadata:account-id metadata:local-email-id
                           metadata:remote-thread-id metadata:received-at metadata:from-json
                           metadata:subject metadata:has-attachment]
      :from [:as jmap-email-metadata metadata]
      :where
      [:and
       [:= metadata:account-id [:bind account-id]]
       [:= metadata:local-email-id [:bind local-email-id]]
       [:exists
        [:select [1]
                 :from [:as jmap-email-generation-member member]
                 :where [:and
                         [:= member:account-id metadata:account-id]
                         [:= member:generation-id [:bind generation-id]]
                         [:= member:local-email-id metadata:local-email-id]]]]]
      :limit 1]
     generation-id)))

(defun chidu-store-sqlite--active-email-mailboxes
    (database account-id generation-id local-email-id)
  "Return visible Mailbox ids from DATABASE.

ACCOUNT-ID, GENERATION-ID, and LOCAL-EMAIL-ID identify the active Email."
  (let* ((mailboxes
          (mapcar
           #'car
           (chidu-sql-select database
             [:select [remote-mailbox-id]
                      :from jmap-email-generation-mailbox
                      :where [:and
                              [:= account-id [:bind account-id]]
                              [:= generation-id [:bind generation-id]]
                              [:= local-email-id [:bind local-email-id]]]
                      :order-by [[remote-mailbox-id :asc]]])))
         (trash-p
          (chidu-sql-one database
              [:select [[present 1]]
                       :from jmap-trash-target
                       :where [:and
                               [:= account-id [:bind account-id]]
                               [:= local-email-id [:bind local-email-id]]]
                       :limit 1]
            present))
         (move-source
          (chidu-sql-one database
              [:select [[source mailbox:remote-mailbox-id]]
                       :from [:as jmap-mailbox-move-target target]
                       :joins
                       [[:inner [:as jmap-mailbox-move move]
                                :on [:and
                                     [:= move:operation-id target:operation-id]
                                     [:= move:account-id target:account-id]]]
                        [:inner [:as jmap-mailbox mailbox]
                                :on [:and
                                     [:= mailbox:account-id move:account-id]
                                     [:= mailbox:mailbox-id move:source-mailbox-id]]]]
                       :where [:and
                               [:= target:account-id [:bind account-id]]
                               [:= target:local-email-id [:bind local-email-id]]]
                       :limit 1]
            source)))
    (cond
     (trash-p (vector))
     (move-source
      (vconcat
       (seq-remove (lambda (remote-id) (equal remote-id move-source))
                   mailboxes)))
     (t (vconcat mailboxes)))))

(defun chidu-store-sqlite--active-email-row
    (database account-id generation-id local-email-id)
  "Return one current active Email row from DATABASE, or nil.

ACCOUNT-ID, GENERATION-ID, and LOCAL-EMAIL-ID identify the Email."
  (when-let* ((summary
               (chidu-store-sqlite--active-email-summary-row
                database account-id generation-id local-email-id)))
    (chidu-store-new-email-row-create
     :summary-row summary
     :remote-mailbox-ids
     (chidu-store-sqlite--active-email-mailboxes
      database account-id generation-id local-email-id))))

(defun chidu-store-sqlite--active-email-rows
    (state account-id local-email-ids)
  "Return ACCOUNT-ID active rows for LOCAL-EMAIL-IDS in SQLite STATE."
  (chidu-store-validate-string-vector
   local-email-ids "Active Email local ids")
  (unless (cl-loop for local-id across local-email-ids
                   always (chidu-store-local-id-p local-id))
    (signal 'chidu-invariant-error
            '("Active Email rows require canonical local ids")))
  (when (> (length local-email-ids) chidu-store-active-email-row-limit)
    (signal 'chidu-invariant-error
            (list "Too many active Email rows requested"
                  (length local-email-ids))))
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id)))
         (generation-id
          (and location
               (chidu-store-sqlite--active-email-generation-id
                database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((null generation-id)
      (chidu-result-failure-create
       :kind 'email-index-unavailable
       :data (list :account-id account-id)
       :retryable-p nil))
     (t
      (chidu-result-ok-create
       :value
       (vconcat
        (cl-loop
         for local-id across local-email-ids
         for row =
         (chidu-store-sqlite--active-email-row
          database account-id generation-id local-id)
         when row collect row)))))))

(defun chidu-store-sqlite--canonical-mailbox-member-count
    (database account-id generation-id remote-mailbox-id)
  "Return exact canonical Mailbox member count from DATABASE.

ACCOUNT-ID, GENERATION-ID, and REMOTE-MAILBOX-ID identify the membership set."
  (or
   (caar
    (chidu-sql-select database
      [:select [[:call count 1]]
               :from jmap-email-generation-mailbox
               :where [:and
                       [:= account-id [:bind account-id]]
                       [:= generation-id [:bind generation-id]]
                       [:= remote-mailbox-id [:bind remote-mailbox-id]]]]))
   0))

(defun chidu-store-sqlite--canonical-summary-records
    (database account-id generation-id mailbox limit)
  "Return LIMIT plus one canonical Summary records from DATABASE.

ACCOUNT-ID and GENERATION-ID select canonical state; MAILBOX identifies the
membership set.  An exact indexed member count chooses only the query plan."
  (let* ((probe-limit (1+ limit))
         (remote-mailbox-id
          (chidu-store-mailbox-remote-mailbox-id mailbox))
         (mailbox-id (chidu-store-mailbox-mailbox-id mailbox))
         (member-count
          (chidu-store-sqlite--canonical-mailbox-member-count
           database account-id generation-id remote-mailbox-id)))
    (funcall
     (if (<= member-count
             chidu-store-sqlite--summary-sparse-mailbox-limit)
         #'chidu-store-sqlite--membership-first-summary-records
       #'chidu-store-sqlite--recent-first-summary-records)
     database account-id generation-id remote-mailbox-id mailbox-id
     probe-limit)))

(defun chidu-store-sqlite--canonical-mailbox-summary
    (database location mailbox generation-id limit)
  "Return bounded canonical Summary from DATABASE.

LOCATION and MAILBOX identify its owner; GENERATION-ID is active and LIMIT
bounds retained rows."
  (let* ((account-id
          (chidu-store-account-account-id (cdr location)))
         (records
          (chidu-store-sqlite--canonical-summary-records
           database account-id generation-id mailbox limit))
         (count (length records)))
    (chidu-store-mailbox-summary-context-create
     :endpoint (car location)
     :account (cdr location)
     :mailbox mailbox
     :revision (chidu-store-sqlite--change-seq database)
     :maybe-more-p (> count limit)
     :rows (vconcat (cl-subseq records 0 (min count limit))))))

(defun chidu-store-sqlite--mailbox-summary-context
    (state account-id mailbox-id limit)
  "Return ACCOUNT-ID MAILBOX-ID canonical Summary from SQLite STATE.

LIMIT is the maximum number of visible rows."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'positive-integer-p limit)))
  (let* ((database (chidu-store-sqlite--assert-open state))
         (location
          (and (stringp account-id)
               (chidu-store-sqlite--account-location database account-id)))
         (mailbox
          (and location (stringp mailbox-id)
               (chidu-store-sqlite--mailbox-by-id
                database account-id mailbox-id)))
         (generation-id
          (and location
               (chidu-store-sqlite--active-email-generation-id
                database account-id))))
    (cond
     ((null location)
      (chidu-result-failure-create
       :kind 'unknown-account :data (list :account-id account-id)
       :retryable-p nil))
     ((not (chidu-store-account-available-p (cdr location)))
      (chidu-result-failure-create
       :kind 'account-unavailable :data (list :account-id account-id)
       :retryable-p nil))
     ((null mailbox)
      (chidu-result-failure-create
       :kind 'unknown-mailbox
       :data (list :account-id account-id :mailbox-id mailbox-id)
       :retryable-p nil))
     ((not (chidu-store-mailbox-available-p mailbox))
      (chidu-result-failure-create
       :kind 'mailbox-unavailable
       :data (list :account-id account-id :mailbox-id mailbox-id)
       :retryable-p nil))
     ((null generation-id)
      (chidu-result-failure-create
       :kind 'email-index-unavailable
       :data (list :account-id account-id)
       :retryable-p nil))
     (t
      (chidu-result-ok-create
       :value
       (chidu-store-sqlite--canonical-mailbox-summary
        database location mailbox generation-id limit))))))

(provide 'chidu-store-sqlite-generation)

;;; chidu-store-sqlite-generation.el ends here
