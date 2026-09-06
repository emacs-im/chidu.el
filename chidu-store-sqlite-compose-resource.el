;;; chidu-store-sqlite-compose-resource.el --- Stable Compose resources -*- lexical-binding: t; -*-

;;; Commentary:

;; A Compose resource is immutable attachment metadata plus either exact local
;; bytes identified by SHA-256, a confirmed JMAP Blob id, or both.  Blob upload
;; itself is safely repeatable and therefore has no durable attempt state.
;; Dedicated add/remove Store operations own document membership atomically;
;; ordinary Compose checkpoints cannot smuggle resource membership changes.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'seq)
(require 'subr-x)
(require 'chidu-result)
(require 'chidu-sql)
(require 'chidu-store)
(require 'chidu-store-sqlite-compose)
(require 'chidu-store-sqlite-core)
(require 'chidu-store-sqlite-directory)

(declare-function chidu-store-sqlite--draft-publish-attempt
                  "chidu-store-sqlite-draft" (database workspace-id))

(defun chidu-store-sqlite--compose-resource-from-row (row)
  "Decode one persisted Compose resource ROW."
  (pcase-let
      ((`(,resource-id ,workspace-id ,name ,media-type ,size ,digest
          ,remote-blob-id ,charset ,disposition ,cid
          ,language-json ,location)
        row))
    (chidu-store-compose-resource-create
     :resource-id resource-id
     :workspace-id workspace-id
     :name name
     :media-type media-type
     :size size
     :digest digest
     :remote-blob-id remote-blob-id
     :charset charset
     :disposition disposition
     :cid cid
     :language
     (chidu-store-sqlite--string-vector-from-json
      language-json "Compose resource language")
     :location location)))

(defun chidu-store-sqlite--compose-resource-by-id
    (database workspace-id resource-id)
  "Return WORKSPACE-ID RESOURCE-ID from DATABASE, or nil."
  (when-let* ((row
               (car
                (chidu-sql-select database
                  [:select
                   [resource-id workspace-id name media-type size digest
                                remote-blob-id charset disposition cid language-json
                                location]
                   :from chidu-compose-resource
                   :where [:and
                           [:= workspace-id [:bind workspace-id]]
                           [:= resource-id [:bind resource-id]]]
                   :limit 1]))))
    (chidu-store-sqlite--compose-resource-from-row row)))

(defun chidu-store-sqlite--compose-resource-text-p
    (value &optional nonempty-p token-p)
  "Return non-nil when VALUE is safe resource text.

NONEMPTY-P requires at least one character.  TOKEN-P additionally rejects
whitespace and semicolons."
  (and (stringp value)
       (or (not nonempty-p) (not (string-empty-p value)))
       (not (string-match-p "[\0\r\n]" value))
       (or (not token-p)
           (not (string-match-p "[[:space:];]" value)))))

(defun chidu-store-sqlite--compose-resource-media-type-p (value)
  "Return non-nil when VALUE is a lowercase parameter-free MIME type."
  (and (chidu-store-sqlite--compose-resource-text-p value t)
       (string-match-p
        "\\`[^[:space:]/;]+/[^[:space:]/;]+\\'" value)
       (equal value (downcase value))))

(defun chidu-store-sqlite--compose-resource-digest-p (value)
  "Return non-nil when VALUE is nil or a lowercase SHA-256 digest."
  (or (null value)
      (and (stringp value)
           (= 64 (length value))
           (string-match-p "\\`[0-9a-f]+\\'" value))))

(defun chidu-store-sqlite--compose-resource-observation-p (resource)
  "Return non-nil when RESOURCE has a valid immutable persisted shape."
  (and
   (chidu-store-compose-resource-observation-p resource)
   (chidu-store-local-id-p
    (chidu-store-compose-resource-observation-resource-id resource))
   (let ((name
          (chidu-store-compose-resource-observation-name resource)))
     (or (null name)
         (chidu-store-sqlite--compose-resource-text-p name t)))
   (chidu-store-sqlite--compose-resource-media-type-p
    (chidu-store-compose-resource-observation-media-type resource))
   (let ((size (chidu-store-compose-resource-observation-size resource)))
     (and (integerp size) (>= size 0)))
   (chidu-store-sqlite--compose-resource-digest-p
    (chidu-store-compose-resource-observation-digest resource))
   (let ((blob
          (chidu-store-compose-resource-observation-remote-blob-id resource)))
     (or (null blob)
         (chidu-store-sqlite--compose-resource-text-p blob t)))
   (or (chidu-store-compose-resource-observation-digest resource)
       (chidu-store-compose-resource-observation-remote-blob-id resource))
   (cl-loop
    for value in
    (list
     (chidu-store-compose-resource-observation-charset resource)
     (chidu-store-compose-resource-observation-cid resource)
     (chidu-store-compose-resource-observation-location resource))
    always
    (or (null value)
        (chidu-store-sqlite--compose-resource-text-p value)))
   (let ((disposition
          (chidu-store-compose-resource-observation-disposition resource)))
     (or (null disposition)
         (chidu-store-sqlite--compose-resource-text-p
          disposition t t)))
   (condition-case nil
       (progn
         (chidu-store-validate-string-vector
          (chidu-store-compose-resource-observation-language resource)
          "Compose resource language")
         (cl-loop
          for language across
          (chidu-store-compose-resource-observation-language resource)
          always
          (chidu-store-sqlite--compose-resource-text-p language t)))
     (error nil))))

(defun chidu-store-sqlite--compose-resource-observations-match-p
    (resources document &optional require-remote-p require-local-p)
  "Return non-nil when RESOURCES exactly back DOCUMENT resource ids.

When REQUIRE-REMOTE-P is non-nil, every resource must carry a JMAP Blob id.
When REQUIRE-LOCAL-P is non-nil, every resource must carry a SHA-256 digest."
  (and
   (vectorp resources)
   (chidu-store-sqlite--compose-document-p document)
   (= (length resources)
      (length (chidu-store-compose-document-resource-ids document)))
   (cl-loop
    for resource across resources
    for resource-id across
    (chidu-store-compose-document-resource-ids document)
    always
    (and
     (chidu-store-sqlite--compose-resource-observation-p resource)
     (equal resource-id
            (chidu-store-compose-resource-observation-resource-id resource))
     (or (not require-remote-p)
         (chidu-store-compose-resource-observation-remote-blob-id resource))
     (or (not require-local-p)
         (chidu-store-compose-resource-observation-digest resource))))))

(defun chidu-store-sqlite--compose-document-resources-result
    (database workspace-id document &optional require-remote-p)
  "Resolve DOCUMENT resources below WORKSPACE-ID in DATABASE.

The returned vector follows DOCUMENT order.  When REQUIRE-REMOTE-P is non-nil,
return a typed failure unless every resource has a confirmed JMAP Blob id."
  (if (not (chidu-store-sqlite--compose-document-p document))
      (chidu-result-failure-create
       :kind 'invalid-compose-document :data (list :workspace-id workspace-id)
       :retryable-p nil)
    (let (resources failure)
      (cl-loop
       for resource-id across
       (chidu-store-compose-document-resource-ids document)
       while (null failure)
       for resource =
       (chidu-store-sqlite--compose-resource-by-id
        database workspace-id resource-id)
       do
       (cond
        ((null resource)
         (setq failure
               (chidu-result-failure-create
                :kind 'unknown-compose-resource
                :data (list :workspace-id workspace-id
                            :resource-id resource-id)
                :retryable-p nil)))
        ((and require-remote-p
              (null (chidu-store-compose-resource-remote-blob-id resource)))
         (setq failure
               (chidu-result-failure-create
                :kind 'compose-resource-not-uploaded
                :data (list :workspace-id workspace-id
                            :resource-id resource-id)
                :retryable-p t)))
        (t (push resource resources))))
      (or failure
          (chidu-result-ok-create :value (vconcat (nreverse resources)))))))

(defun chidu-store-sqlite--compose-resources
    (database workspace-id document)
  "Return exact ordered DOCUMENT resources below WORKSPACE-ID in DATABASE."
  (let ((result
         (chidu-store-sqlite--compose-document-resources-result
          database workspace-id document)))
    (if (chidu-result-ok-p result)
        (chidu-result-ok-value result)
      (signal 'chidu-invariant-error
              (list "Compose document references missing resources"
                    :workspace-id workspace-id
                    :failure result)))))

(defun chidu-store-sqlite--insert-compose-resource-observation
    (database workspace-id resource change-seq)
  "Insert RESOURCE below WORKSPACE-ID in DATABASE at CHANGE-SEQ."
  (chidu-sql-execute database
    [:insert :into chidu-compose-resource
     :row
     [[resource-id
       [:bind
        (chidu-store-compose-resource-observation-resource-id resource)]]
      [workspace-id [:bind workspace-id]]
      [name
       [:bind (chidu-store-compose-resource-observation-name resource)]]
      [media-type
       [:bind
        (chidu-store-compose-resource-observation-media-type resource)]]
      [size [:bind (chidu-store-compose-resource-observation-size resource)]]
      [digest
       [:bind (chidu-store-compose-resource-observation-digest resource)]]
      [remote-blob-id
       [:bind
        (chidu-store-compose-resource-observation-remote-blob-id resource)]]
      [charset
       [:bind (chidu-store-compose-resource-observation-charset resource)]]
      [disposition
       [:bind
        (chidu-store-compose-resource-observation-disposition resource)]]
      [cid [:bind (chidu-store-compose-resource-observation-cid resource)]]
      [language-json
       [:bind
        (chidu-store-sqlite--string-vector-json
         (chidu-store-compose-resource-observation-language resource)
         "Compose resource language")]]
      [location
       [:bind (chidu-store-compose-resource-observation-location resource)]]
      [created-change-seq [:bind change-seq]]
      [updated-change-seq [:bind change-seq]]]]))

(defun chidu-store-sqlite--compose-resource-size-failure
    (database row resource)
  "Return DATABASE size failure for RESOURCE below workspace ROW, or nil."
  (let* ((location
          (chidu-store-sqlite--account-location
           database (chidu-store-sqlite--compose-row-account-id row)))
         (endpoint (car location))
         (maximum (and endpoint
                       (chidu-store-endpoint-max-size-upload endpoint)))
         (size (chidu-store-compose-resource-observation-size resource)))
    (when (and maximum (> size maximum))
      (chidu-result-failure-create
       :kind 'compose-resource-too-large
       :data (list :actual-bytes size :max-size-upload maximum)
       :retryable-p nil))))

(defun chidu-store-sqlite--compose-attachments-size-failure
    (database row resources)
  "Return DATABASE aggregate attachment-size failure for ROW RESOURCES."
  (let* ((location
          (chidu-store-sqlite--account-location
           database (chidu-store-sqlite--compose-row-account-id row)))
         (account (cdr location))
         (maximum
          (and account
               (chidu-store-account-max-size-attachments-per-email account)))
         (total
          (cl-loop for resource across resources
                   sum (chidu-store-compose-resource-size resource))))
    (when (and maximum (> total maximum))
      (chidu-result-failure-create
       :kind 'compose-attachments-too-large
       :data (list :actual-bytes total
                   :max-size-attachments-per-email maximum)
       :retryable-p nil))))

(defun chidu-store-sqlite--compose-resource-membership-p
    (old-ids new-ids resource-id add-p)
  "Return non-nil when NEW-IDS is exact OLD-IDS membership edit.

RESOURCE-ID is appended when ADD-P is non-nil and removed otherwise."
  (let ((expected
         (if add-p
             (vconcat old-ids (vector resource-id))
           (vconcat
            (seq-remove (lambda (value) (equal value resource-id)) old-ids)))))
    (and
     (if add-p
         (not (seq-contains-p old-ids resource-id #'equal))
       (seq-contains-p old-ids resource-id #'equal))
     (equal expected new-ids))))

(defun chidu-store-sqlite--compose-resource-owner-failure
    (database row identity-id expected revision document)
  "Return common DATABASE resource-edit failure for ROW.

IDENTITY-ID, EXPECTED, REVISION, and DOCUMENT are the captured edit state."
  (let* ((workspace-id
          (chidu-store-sqlite--compose-row-workspace-id row))
         (actual (chidu-store-sqlite--compose-row-revision row)))
    (cond
     ((not (chidu-store-sqlite--compose-document-p document))
      (chidu-result-failure-create
       :kind 'invalid-compose-document
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((not (and (integerp expected) (>= expected 0)
                (integerp revision) (> revision expected)))
      (chidu-result-failure-create
       :kind 'invalid-compose-revision
       :data (list :workspace-id workspace-id
                   :expected expected :revision revision)
       :retryable-p nil))
     ((/= expected actual)
      (chidu-store-sqlite--compose-revision-conflict
       workspace-id expected actual))
     ((chidu-store-sqlite--draft-publish-attempt database workspace-id)
      (chidu-result-failure-create
       :kind 'draft-publish-active
       :data (list :workspace-id workspace-id) :retryable-p t))
     ((chidu-store-sqlite--compose-owner-failure
       database
       (chidu-store-sqlite--compose-row-account-id row)
       identity-id t)))))

(defun chidu-store-sqlite--update-compose-document
    (database row identity-id revision document change-seq)
  "Update DATABASE ROW for IDENTITY-ID to REVISION and DOCUMENT at CHANGE-SEQ."
  (chidu-sql-execute database
    [:update chidu-compose-workspace
     :set
     [[identity-id [:bind identity-id]]
      [to-value [:bind (chidu-store-compose-document-to document)]]
      [cc-value [:bind (chidu-store-compose-document-cc document)]]
      [bcc-value [:bind (chidu-store-compose-document-bcc document)]]
      [reply-to-value
       [:bind (chidu-store-compose-document-reply-to document)]]
      [subject [:bind (chidu-store-compose-document-subject document)]]
      [body [:bind (chidu-store-compose-document-body document)]]
      [resource-ids-json
       [:bind
        (chidu-store-sqlite--string-vector-json
         (chidu-store-compose-document-resource-ids document)
         "Compose document resource ids")]]
      [revision [:bind revision]]
      [updated-change-seq [:bind change-seq]]]
     :where
     [:and
      [:= workspace-id
          [:bind (chidu-store-sqlite--compose-row-workspace-id row)]]
      [:= revision [:bind (chidu-store-sqlite--compose-row-revision row)]]]]))

(defun chidu-store-sqlite--add-compose-resource (state operation)
  "Atomically add Compose resource OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (workspace-id
          (chidu-store-op-add-compose-resource-workspace-id operation))
         (identity-id
          (chidu-store-op-add-compose-resource-identity-id operation))
         (expected
          (chidu-store-op-add-compose-resource-expected-revision operation))
         (revision
          (chidu-store-op-add-compose-resource-revision operation))
         (document
          (chidu-store-op-add-compose-resource-document operation))
         (resource
          (chidu-store-op-add-compose-resource-resource operation))
         (row
          (and (chidu-store-local-id-p workspace-id)
               (chidu-store-sqlite--select-compose-row
                database workspace-id))))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-compose-workspace
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((chidu-store-sqlite--compose-resource-owner-failure
       database row identity-id expected revision document))
     ((not (chidu-store-sqlite--compose-resource-observation-p resource))
      (chidu-result-failure-create
       :kind 'invalid-compose-resource
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((or
       (null (chidu-store-compose-resource-observation-digest resource))
       (chidu-store-compose-resource-observation-remote-blob-id resource))
      (chidu-result-failure-create
       :kind 'invalid-local-compose-resource
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((not
       (chidu-store-sqlite--compose-resource-membership-p
        (chidu-store-compose-document-resource-ids
         (chidu-store-sqlite--compose-row-document row))
        (chidu-store-compose-document-resource-ids document)
        (chidu-store-compose-resource-observation-resource-id resource)
        t))
      (chidu-result-failure-create
       :kind 'compose-resource-membership-conflict
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((chidu-store-sqlite--compose-resource-by-id
       database workspace-id
       (chidu-store-compose-resource-observation-resource-id resource))
      (chidu-result-failure-create
       :kind 'compose-resource-conflict
       :data
       (list :workspace-id workspace-id
             :resource-id
             (chidu-store-compose-resource-observation-resource-id resource))
       :retryable-p nil))
     ((chidu-store-sqlite--compose-resource-size-failure
       database row resource))
     (t
      (let* ((existing
              (chidu-store-sqlite--compose-resources
               database workspace-id
               (chidu-store-sqlite--compose-row-document row)))
             (candidate
              (vconcat
               existing
               (vector
                (chidu-store-compose-resource-create
                 :resource-id
                 (chidu-store-compose-resource-observation-resource-id resource)
                 :workspace-id workspace-id
                 :name (chidu-store-compose-resource-observation-name resource)
                 :media-type
                 (chidu-store-compose-resource-observation-media-type resource)
                 :size (chidu-store-compose-resource-observation-size resource)
                 :digest (chidu-store-compose-resource-observation-digest resource)
                 :remote-blob-id nil
                 :charset (chidu-store-compose-resource-observation-charset resource)
                 :disposition
                 (chidu-store-compose-resource-observation-disposition resource)
                 :cid (chidu-store-compose-resource-observation-cid resource)
                 :language
                 (chidu-store-compose-resource-observation-language resource)
                 :location
                 (chidu-store-compose-resource-observation-location resource))))))
        (or
         (chidu-store-sqlite--compose-attachments-size-failure
          database row candidate)
         (progn
           (with-sqlite-transaction database
             (let ((change-seq
                    (chidu-store-sqlite--increment-change-seq database)))
               (chidu-store-sqlite--insert-compose-resource-observation
                database workspace-id resource change-seq)
               (chidu-store-sqlite--update-compose-document
                database row identity-id revision document change-seq)))
           (chidu-store-sqlite--compose-context state workspace-id))))))))

(defun chidu-store-sqlite--remove-compose-resource (state operation)
  "Atomically remove Compose resource OPERATION in SQLite STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (workspace-id
          (chidu-store-op-remove-compose-resource-workspace-id operation))
         (resource-id
          (chidu-store-op-remove-compose-resource-resource-id operation))
         (identity-id
          (chidu-store-op-remove-compose-resource-identity-id operation))
         (expected
          (chidu-store-op-remove-compose-resource-expected-revision operation))
         (revision
          (chidu-store-op-remove-compose-resource-revision operation))
         (document
          (chidu-store-op-remove-compose-resource-document operation))
         (row
          (and (chidu-store-local-id-p workspace-id)
               (chidu-store-sqlite--select-compose-row
                database workspace-id))))
    (cond
     ((null row)
      (chidu-result-failure-create
       :kind 'unknown-compose-workspace
       :data (list :workspace-id workspace-id) :retryable-p nil))
     ((chidu-store-sqlite--compose-resource-owner-failure
       database row identity-id expected revision document))
     ((not
       (chidu-store-sqlite--compose-resource-membership-p
        (chidu-store-compose-document-resource-ids
         (chidu-store-sqlite--compose-row-document row))
        (chidu-store-compose-document-resource-ids document)
        resource-id nil))
      (chidu-result-failure-create
       :kind 'compose-resource-membership-conflict
       :data (list :workspace-id workspace-id :resource-id resource-id)
       :retryable-p nil))
     ((null (chidu-store-sqlite--compose-resource-by-id
             database workspace-id resource-id))
      (chidu-result-failure-create
       :kind 'unknown-compose-resource
       :data (list :workspace-id workspace-id :resource-id resource-id)
       :retryable-p nil))
     (t
      (with-sqlite-transaction database
        (let ((change-seq
               (chidu-store-sqlite--increment-change-seq database)))
          (chidu-store-sqlite--update-compose-document
           database row identity-id revision document change-seq)
          (chidu-sql-execute database
            [:delete :from chidu-compose-resource
             :where [:and
                     [:= workspace-id [:bind workspace-id]]
                     [:= resource-id [:bind resource-id]]]])))
      (chidu-store-sqlite--compose-context state workspace-id)))))

(defun chidu-store-sqlite--set-compose-resource-blob (state operation)
  "Set or clear confirmed Blob evidence from resource OPERATION in STATE."
  (let* ((database (chidu-store-sqlite--assert-open state))
         (workspace-id
          (chidu-store-op-set-compose-resource-blob-workspace-id operation))
         (resource-id
          (chidu-store-op-set-compose-resource-blob-resource-id operation))
         (remote-blob-id
          (chidu-store-op-set-compose-resource-blob-remote-blob-id operation))
         (resource
          (and (chidu-store-local-id-p workspace-id)
               (chidu-store-sqlite--compose-resource-by-id
                database workspace-id resource-id))))
    (cond
     ((null resource)
      (chidu-result-failure-create
       :kind 'unknown-compose-resource
       :data (list :workspace-id workspace-id :resource-id resource-id)
       :retryable-p nil))
     ((and remote-blob-id
           (not (chidu-store-sqlite--compose-resource-text-p
                 remote-blob-id t)))
      (chidu-result-failure-create
       :kind 'invalid-remote-blob-id :data nil :retryable-p nil))
     ((and (null remote-blob-id)
           (null (chidu-store-compose-resource-digest resource)))
      (chidu-result-failure-create
       :kind 'compose-resource-bytes-unavailable
       :data (list :workspace-id workspace-id :resource-id resource-id)
       :retryable-p nil))
     (t
      (with-sqlite-transaction database
        (let ((change-seq
               (chidu-store-sqlite--increment-change-seq database)))
          (chidu-sql-execute database
            [:update chidu-compose-resource
             :set [[remote-blob-id [:bind remote-blob-id]]
                   [updated-change-seq [:bind change-seq]]]
             :where [:and
                     [:= workspace-id [:bind workspace-id]]
                     [:= resource-id [:bind resource-id]]]])))
      (chidu-store-sqlite--compose-context state workspace-id)))))

(provide 'chidu-store-sqlite-compose-resource)

;;; chidu-store-sqlite-compose-resource.el ends here
