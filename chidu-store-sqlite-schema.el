;;; chidu-store-sqlite-schema.el --- Current SQLite schema contract -*- lexical-binding: t; -*-

;;; Commentary:

;; Chidu's greenfield on-disk format has one executable manifest.  The same
;; schema AST creates a fresh database and builds the reference snapshot used to
;; reject drift in an existing database before any Store operation runs.
;; Runtime migrations are intentionally absent.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'seq)
(require 'sqlite)
(require 'chidu-sql)
(require 'chidu-store)

(defconst chidu-store-sqlite--schema
  [[:table jmap-account
           [:column account-id :text :primary-key]
           [:column endpoint-id :text :not-null [:references jmap-endpoint [endpoint-id] :on-delete :cascade]]
           [:column remote-account-id :text :not-null]
           [:column name :text :not-null]
           [:column is-personal :integer :not-null [:check [:in is-personal [0 1]]]]
           [:column is-read-only :integer :not-null [:check [:in is-read-only [0 1]]]]
           [:column is-primary-mail :integer :not-null [:check [:in is-primary-mail [0 1]]]]
           [:column is-primary-submission :integer :not-null [:check [:in is-primary-submission [0 1]]]]
           [:column is-available :integer :not-null [:check [:in is-available [0 1]]]]
           [:column capabilities-json :text :not-null]
           [:column extra-properties-json :text :not-null]
           [:column identity-state :text]
           [:column max-size-attachments-per-email :integer
                    [:check
                     [:or [:is max-size-attachments-per-email nil]
                          [:>= max-size-attachments-per-email 0]]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:unique [endpoint-id remote-account-id]]]
   [:table jmap-conversation
           [:column account-id :text :not-null]
           [:column remote-thread-id :text :not-null [:check [:> [:call length remote-thread-id] 0]]]
           [:column thread-state :text :not-null [:check [:> [:call length thread-state] 0]]]
           [:column email-state :text :not-null [:check [:> [:call length email-state] 0]]]
           [:column revision :integer :not-null [:check [:>= revision 0]]]
           [:column is-complete :integer :not-null [:check [:in is-complete [0 1]]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:primary-key [account-id remote-thread-id]]
           [:foreign-key [account-id] :references jmap-account [account-id] :on-delete :cascade]]
   [:table jmap-conversation-row
           [:column account-id :text :not-null]
           [:column remote-thread-id :text :not-null]
           [:column ordinal :integer :not-null [:check [:>= ordinal 0]]]
           [:column local-email-id :text :not-null]
           [:column parent-local-email-id :text]
           [:column depth :integer :not-null [:check [:>= depth 0]]]
           [:column received-at :text :not-null [:check [:> [:call length received-at] 0]]]
           [:column sent-at :text]
           [:column from-name :text]
           [:column from-email :text]
           [:column subject :text :not-null]
           [:column preview :text :not-null]
           [:column is-unread :integer :not-null [:check [:in is-unread [0 1]]]]
           [:column is-flagged :integer :not-null [:check [:in is-flagged [0 1]]]]
           [:column has-attachment :integer :not-null [:check [:in has-attachment [0 1]]]]
           [:column message-ids-json :text :not-null]
           [:column in-reply-to-json :text :not-null]
           [:column references-json :text :not-null]
           [:primary-key [account-id remote-thread-id ordinal]]
           [:unique [account-id remote-thread-id local-email-id]]
           [:foreign-key [account-id remote-thread-id] :references jmap-conversation [account-id remote-thread-id] :on-delete :cascade]
           [:foreign-key [account-id local-email-id] :references jmap-email-record [account-id local-email-id] :on-delete :cascade]
           [:foreign-key [parent-local-email-id] :references jmap-email-record [local-email-id] :on-delete :set-null]]
   [:table jmap-email-body
           [:column account-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column email-state :text :not-null [:check [:> [:call length email-state] 0]]]
           [:column text-content :text :not-null]
           [:column html-content :text :not-null]
           [:column revision :integer :not-null [:check [:>= revision 0]]]
           [:column is-truncated :integer :not-null [:check [:in is-truncated [0 1]]]]
           [:column encoding-problem :integer :not-null [:check [:in encoding-problem [0 1]]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:primary-key [account-id local-email-id]]
           [:foreign-key [account-id local-email-id] :references jmap-email-record [account-id local-email-id] :on-delete :cascade]]
   [:table jmap-email-attachment
           [:column account-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column ordinal :integer :not-null [:check [:>= ordinal 0]]]
           [:column part-id :text :not-null [:check [:> [:call length part-id] 0]]]
           [:column blob-id :text :not-null [:check [:> [:call length blob-id] 0]]]
           [:column size :integer :not-null [:check [:>= size 0]]]
           [:column name :text]
           [:column media-type :text :not-null [:check [:> [:call length media-type] 0]]]
           [:column charset :text]
           [:column disposition :text]
           [:column cid :text]
           [:column language-json :text :not-null]
           [:column location :text]
           [:primary-key [account-id local-email-id ordinal]]
           [:unique [account-id local-email-id part-id]]
           [:foreign-key [account-id local-email-id]
                         :references jmap-email-body [account-id local-email-id]
                         :on-delete :cascade]]
   [:table jmap-email-checkpoint
           [:column account-id :text :primary-key [:references jmap-account [account-id] :on-delete :cascade]]
           [:column phase :text :not-null [:check [:in phase [[:literal "enumerating"] [:literal "membership-catchup"] [:literal "hydrating"] [:literal "metadata-catchup"] [:literal "activating"] [:literal "live"]]]]]
           [:column generation-id :text :not-null]
           [:column profile-version :text :not-null [:check [:> [:call length profile-version] 0]]]
           [:column state :text :not-null [:check [:> [:call length state] 0]]]
           [:column query-state :text
                    [:check [:or [:is query-state nil]
                                 [:> [:call length query-state] 0]]]]
           [:column can-calculate-changes :integer
                    [:check [:or [:is can-calculate-changes nil]
                                 [:in can-calculate-changes [0 1]]]]]
           [:column committed-count :integer :not-null [:check [:>= committed-count 0]]]
           [:column anchor-remote-email-id :text
                    [:check [:or [:is anchor-remote-email-id nil]
                                 [:> [:call length anchor-remote-email-id] 0]]]]
           [:column hydration-after-local-email-id :text
                    [:check [:or [:is hydration-after-local-email-id nil]
                                 [:> [:call length hydration-after-local-email-id] 0]]]]
           [:column revision :integer :not-null [:check [:>= revision 0]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:foreign-key [account-id generation-id] :references jmap-email-generation [account-id generation-id]]
           [:check
            [:or
             [:is-not query-state nil]
             [:and
              [:is can-calculate-changes nil]
              [:is anchor-remote-email-id nil]
              [:or [:= phase [:literal "live"]]
                   [:= committed-count 0]]]]]
           [:check
            [:or [:in phase [[:literal "enumerating"] [:literal "live"]]]
                 [:is-not query-state nil]]]
           [:check
            [:or [:= phase [:literal "live"]]
                 [:= committed-count 0]
                 [:is-not anchor-remote-email-id nil]]]
           [:check
            [:or [:= phase [:literal "hydrating"]]
                 [:is hydration-after-local-email-id nil]]]
           [:check
            [:or [:!= phase [:literal "live"]]
                 [:and [:is query-state nil]
                       [:is can-calculate-changes nil]
                       [:is anchor-remote-email-id nil]
                       [:is hydration-after-local-email-id nil]]]]]
   [:table jmap-email-generation
           [:column generation-id :text :primary-key [:check [:> [:call length generation-id] 0]]]
           [:column account-id :text :not-null [:references jmap-account [account-id] :on-delete :cascade]]
           [:column lifecycle :text :not-null [:check [:in lifecycle [[:literal "building"] [:literal "active"] [:literal "retired"]]]]]
           [:column profile-version :text :not-null [:check [:> [:call length profile-version] 0]]]
           [:column created-checkpoint-revision :integer :not-null [:check [:>= created-checkpoint-revision 0]]]
           [:unique [account-id generation-id]]]
   [:table jmap-email-generation-member
           [:column account-id :text :not-null]
           [:column generation-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column ordinal :integer :not-null [:check [:>= ordinal 0]]]
           [:primary-key [generation-id local-email-id]]
           [:unique [account-id generation-id local-email-id]]
           [:unique [generation-id ordinal]]
           [:foreign-key [account-id generation-id] :references jmap-email-generation [account-id generation-id] :on-delete :cascade]
           [:foreign-key [account-id local-email-id] :references jmap-email-record [account-id local-email-id] :on-delete :cascade]]
   [:table jmap-email-generation-mailbox
           [:column account-id :text :not-null]
           [:column generation-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column remote-mailbox-id :text :not-null [:check [:> [:call length remote-mailbox-id] 0]]]
           [:primary-key [generation-id local-email-id remote-mailbox-id]]
           [:foreign-key [account-id generation-id local-email-id]
                         :references jmap-email-generation-member [account-id generation-id local-email-id]
                         :on-delete :cascade]]
   [:table jmap-email-generation-keyword
           [:column account-id :text :not-null]
           [:column generation-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column keyword :text :not-null [:check [:> [:call length keyword] 0]]]
           [:primary-key [generation-id local-email-id keyword]]
           [:foreign-key [account-id generation-id local-email-id]
                         :references jmap-email-generation-member [account-id generation-id local-email-id]
                         :on-delete :cascade]]
   [:table jmap-email-record
           [:column local-email-id :text :primary-key [:check [:> [:call length local-email-id] 0]]]
           [:column account-id :text :not-null [:references jmap-account [account-id] :on-delete :cascade]]
           [:column remote-email-id :text :not-null [:check [:> [:call length remote-email-id] 0]]]
           [:column created-change-seq :integer :not-null [:check [:>= created-change-seq 0]]]
           [:unique [account-id remote-email-id]]
           [:unique [account-id local-email-id]]]
   [:table jmap-email-metadata
           [:column account-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column profile-version :text :not-null [:check [:> [:call length profile-version] 0]]]
           [:column remote-blob-id :text :not-null [:check [:> [:call length remote-blob-id] 0]]]
           [:column remote-thread-id :text :not-null [:check [:> [:call length remote-thread-id] 0]]]
           [:column size :integer :not-null [:check [:>= size 0]]]
           [:column received-at :text :not-null [:check [:> [:call length received-at] 0]]]
           [:column sent-at :text]
           [:column sender-json :text :not-null]
           [:column from-json :text :not-null]
           [:column to-json :text :not-null]
           [:column cc-json :text :not-null]
           [:column bcc-json :text :not-null]
           [:column reply-to-json :text :not-null]
           [:column subject :text :not-null]
           [:column message-ids-json :text :not-null]
           [:column in-reply-to-json :text :not-null]
           [:column references-json :text :not-null]
           [:column has-attachment :integer :not-null [:check [:in has-attachment [0 1]]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:primary-key [account-id local-email-id]]
           [:foreign-key [account-id local-email-id]
                         :references jmap-email-record [account-id local-email-id]
                         :on-delete :cascade]]
   [:table jmap-email-preview
           [:column account-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column value :text :not-null]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:primary-key [account-id local-email-id]]
           [:foreign-key [account-id local-email-id]
                         :references jmap-email-record [account-id local-email-id]
                         :on-delete :cascade]]
   [:table jmap-endpoint
           [:column endpoint-id :text :primary-key]
           [:column session-url :text :not-null]
           [:column login :text :not-null]
           [:column authentication :text :not-null [:check [:in authentication [[:literal "basic"] [:literal "bearer"]]]]]
           [:column session-username :text]
           [:column session-state :text]
           [:column api-url :text]
           [:column download-url :text]
           [:column upload-url :text]
           [:column event-source-url :text]
           [:column max-size-request :integer
                    [:check [:or [:is max-size-request nil] [:> max-size-request 0]]]]
           [:column max-size-upload :integer
                    [:check [:or [:is max-size-upload nil] [:> max-size-upload 0]]]]
           [:column max-objects-in-get :integer
                    [:check [:or [:is max-objects-in-get nil] [:> max-objects-in-get 0]]]]
           [:column max-objects-in-set :integer
                    [:check [:or [:is max-objects-in-set nil] [:> max-objects-in-set 0]]]]
           [:column primary-contacts-remote-account-id :text
                    [:check
                     [:or [:is primary-contacts-remote-account-id nil]
                          [:> [:call length primary-contacts-remote-account-id] 0]]]]
           [:column capabilities-json :text]
           [:column extra-properties-json :text]
           [:column observed-change-seq :integer
                    [:check [:or [:is observed-change-seq nil]
                                 [:>= observed-change-seq 0]]]]
           [:unique [session-url login]]]
   [:table jmap-identity
           [:column identity-id :text :primary-key]
           [:column account-id :text :not-null [:references jmap-account [account-id] :on-delete :cascade]]
           [:column remote-identity-id :text :not-null]
           [:column name :text :not-null]
           [:column email :text :not-null]
           [:column reply-to-json :text]
           [:column bcc-json :text]
           [:column text-signature :text :not-null]
           [:column html-signature :text :not-null]
           [:column may-delete :integer :not-null [:check [:in may-delete [0 1]]]]
           [:column is-available :integer :not-null [:check [:in is-available [0 1]]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:unique [account-id remote-identity-id]]]
   [:table chidu-compose-workspace
           [:column workspace-id :text :primary-key
                    [:check [:> [:call length workspace-id] 0]]]
           [:column account-id :text :not-null]
           [:column identity-id :text :not-null]
           [:column kind :text :not-null
                    [:check
                     [:in kind
                          [[:literal "new"]
                           [:literal "reply-sender"]
                           [:literal "reply-all"]
                           [:literal "reply-list"]
                           [:literal "forward"]
                           [:literal "draft"]]]]]
           [:column to-value :text :not-null]
           [:column cc-value :text :not-null]
           [:column bcc-value :text :not-null]
           [:column reply-to-value :text :not-null]
           [:column subject :text :not-null]
           [:column body :text :not-null]
           [:column resource-ids-json :text :not-null]
           [:column base-remote-email-id :text
                    [:check [:or [:is base-remote-email-id nil]
                                 [:> [:call length base-remote-email-id] 0]]]]
           [:column base-remote-blob-id :text
                    [:check [:or [:is base-remote-blob-id nil]
                                 [:> [:call length base-remote-blob-id] 0]]]]
           [:column published-revision :integer
                    [:check [:or [:is published-revision nil]
                                 [:>= published-revision 0]]]]
           [:column revision :integer :not-null [:check [:>= revision 0]]]
           [:column created-change-seq :integer :not-null
                    [:check [:>= created-change-seq 0]]]
           [:column updated-change-seq :integer :not-null
                    [:check [:>= updated-change-seq 0]]]
           [:check [:or [:is published-revision nil]
                        [:<= published-revision revision]]]
           [:check
            [:or
             [:and [:is base-remote-email-id nil]
                   [:is base-remote-blob-id nil]]
             [:and [:is-not base-remote-email-id nil]
                   [:is-not base-remote-blob-id nil]]]]
           [:foreign-key [account-id]
                         :references jmap-account [account-id] :on-delete :cascade]
           [:foreign-key [account-id identity-id]
                         :references jmap-identity [account-id identity-id]]]
   [:table chidu-compose-resource
           [:column resource-id :text :primary-key
                    [:check [:> [:call length resource-id] 0]]]
           [:column workspace-id :text :not-null
                    [:references chidu-compose-workspace [workspace-id]
                                 :on-delete :cascade]]
           [:column name :text
                    [:check [:or [:is name nil] [:> [:call length name] 0]]]]
           [:column media-type :text :not-null
                    [:check [:> [:call length media-type] 0]]]
           [:column size :integer :not-null [:check [:>= size 0]]]
           [:column digest :text
                    [:check
                     [:or [:is digest nil]
                          [:and [:= [:call length digest] 64]
                                [:= digest [:call lower digest]]]]]]
           [:column remote-blob-id :text
                    [:check [:or [:is remote-blob-id nil]
                                 [:> [:call length remote-blob-id] 0]]]]
           [:column charset :text]
           [:column disposition :text]
           [:column cid :text]
           [:column language-json :text :not-null]
           [:column location :text]
           [:column created-change-seq :integer :not-null
                    [:check [:>= created-change-seq 0]]]
           [:column updated-change-seq :integer :not-null
                    [:check [:>= updated-change-seq 0]]]
           [:check [:or [:is-not digest nil]
                        [:is-not remote-blob-id nil]]]]
   [:table chidu-draft-publish-attempt
           [:column attempt-id :text :primary-key
                    [:check [:> [:call length attempt-id] 0]]]
           [:column workspace-id :text :not-null
                    [:references chidu-compose-workspace [workspace-id]
                                 :on-delete :cascade]]
           [:column account-id :text :not-null]
           [:column identity-id :text :not-null]
           [:column drafts-mailbox-id :text :not-null]
           [:column revision :integer :not-null [:check [:>= revision 0]]]
           [:column message-id :text :not-null
                    [:check [:> [:call length message-id] 0]]]
           [:column predecessor-remote-email-id :text
                    [:check [:or [:is predecessor-remote-email-id nil]
                                 [:> [:call length predecessor-remote-email-id] 0]]]]
           [:column predecessor-remote-blob-id :text
                    [:check [:or [:is predecessor-remote-blob-id nil]
                                 [:> [:call length
                                            predecessor-remote-blob-id] 0]]]]
           [:column phase :text :not-null
                    [:check
                     [:in phase
                          [[:literal "pending"]
                           [:literal "unknown"]
                           [:literal "cleanup-pending"]]]]]
           [:column error-kind :text]
           [:column created-change-seq :integer :not-null
                    [:check [:>= created-change-seq 0]]]
           [:column updated-change-seq :integer :not-null
                    [:check [:>= updated-change-seq 0]]]
           [:check
            [:or
             [:and [:is predecessor-remote-email-id nil]
                   [:is predecessor-remote-blob-id nil]]
             [:and [:is-not predecessor-remote-email-id nil]
                   [:is-not predecessor-remote-blob-id nil]]]]
           [:foreign-key [account-id identity-id]
                         :references jmap-identity [account-id identity-id]]
           [:foreign-key [account-id drafts-mailbox-id]
                         :references jmap-mailbox [account-id mailbox-id]]]
   [:table jmap-mailbox
           [:column mailbox-id :text :primary-key [:check [:> [:call length mailbox-id] 0]]]
           [:column account-id :text :not-null [:references jmap-account [account-id] :on-delete :cascade]]
           [:column remote-mailbox-id :text :not-null [:check [:> [:call length remote-mailbox-id] 0]]]
           [:column name :text :not-null [:check [:> [:call length name] 0]]]
           [:column parent-mailbox-id :text]
           [:column parent-remote-mailbox-id :text]
           [:column role :text
                    [:check
                     [:or
                      [:is role nil]
                      [:and [:> [:call length role] 0]
                            [:= role [:call lower role]]]]]]
           [:column sort-order :integer :not-null
                    [:check [:and [:>= sort-order 0] [:< sort-order 2147483648]]]]
           [:column total-emails :integer :not-null [:check [:>= total-emails 0]]]
           [:column unread-emails :integer :not-null [:check [:>= unread-emails 0]]]
           [:column total-threads :integer :not-null [:check [:>= total-threads 0]]]
           [:column unread-threads :integer :not-null [:check [:>= unread-threads 0]]]
           [:column may-read-items :integer :not-null [:check [:in may-read-items [0 1]]]]
           [:column may-add-items :integer :not-null [:check [:in may-add-items [0 1]]]]
           [:column may-remove-items :integer :not-null [:check [:in may-remove-items [0 1]]]]
           [:column may-set-seen :integer :not-null [:check [:in may-set-seen [0 1]]]]
           [:column may-set-keywords :integer :not-null [:check [:in may-set-keywords [0 1]]]]
           [:column may-create-child :integer :not-null [:check [:in may-create-child [0 1]]]]
           [:column may-rename :integer :not-null [:check [:in may-rename [0 1]]]]
           [:column may-delete :integer :not-null [:check [:in may-delete [0 1]]]]
           [:column may-submit :integer :not-null [:check [:in may-submit [0 1]]]]
           [:column is-subscribed :integer :not-null [:check [:in is-subscribed [0 1]]]]
           [:column is-available :integer :not-null [:check [:in is-available [0 1]]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:unique [account-id remote-mailbox-id]]
           [:check [:<= unread-emails total-emails]]
           [:check [:<= total-threads total-emails]]
           [:check [:<= unread-threads total-threads]]
           [:foreign-key [parent-mailbox-id] :references jmap-mailbox [mailbox-id] :deferrable t :initially :deferred]]
   [:table jmap-mailbox-move
           [:column operation-id :text :primary-key [:check [:> [:call length operation-id] 0]]]
           [:column account-id :text :not-null :unique]
           [:column source-mailbox-id :text :not-null]
           [:column destination-mailbox-id :text :not-null]
           [:column accepted-change-seq :integer :not-null [:check [:>= accepted-change-seq 0]]]
           [:column updated-change-seq :integer :not-null [:check [:>= updated-change-seq 0]]]
           [:unique [operation-id account-id]]
           [:check [:!= source-mailbox-id destination-mailbox-id]]
           [:foreign-key [account-id] :references jmap-account [account-id] :on-delete :cascade]
           [:foreign-key [account-id source-mailbox-id] :references jmap-mailbox [account-id mailbox-id]]
           [:foreign-key [account-id destination-mailbox-id] :references jmap-mailbox [account-id mailbox-id]]]
   [:table jmap-mailbox-move-target
           [:column operation-id :text :not-null]
           [:column account-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column remote-email-id :text :not-null [:check [:> [:call length remote-email-id] 0]]]
           [:column phase :text :not-null [:check [:in phase [[:literal "pending"] [:literal "unknown"]]]]]
           [:column error-kind :text
                    [:check [:or [:is error-kind nil]
                                 [:> [:call length error-kind] 0]]]]
           [:column accepted-change-seq :integer :not-null [:check [:>= accepted-change-seq 0]]]
           [:column updated-change-seq :integer :not-null [:check [:>= updated-change-seq 0]]]
           [:primary-key [operation-id local-email-id]]
           [:unique [account-id local-email-id]]
           [:foreign-key [operation-id account-id] :references jmap-mailbox-move [operation-id account-id] :on-delete :cascade]
           [:foreign-key [account-id local-email-id] :references jmap-email-record [account-id local-email-id] :on-delete :cascade]]
   [:table jmap-parsed-blob
           [:column account-id :text :not-null]
           [:column blob-id :text :not-null [:check [:> [:call length blob-id] 0]]]
           [:column profile-version :text :not-null [:check [:> [:call length profile-version] 0]]]
           [:column revision :integer :not-null [:check [:>= revision 0]]]
           [:column message-ids-json :text :not-null]
           [:column in-reply-to-json :text :not-null]
           [:column references-json :text :not-null]
           [:column sender-json :text :not-null]
           [:column from-json :text :not-null]
           [:column to-json :text :not-null]
           [:column cc-json :text :not-null]
           [:column bcc-json :text :not-null]
           [:column reply-to-json :text :not-null]
           [:column subject :text :not-null]
           [:column sent-at :text]
           [:column preview :text :not-null]
           [:column text-content :text :not-null]
           [:column html-content :text :not-null]
           [:column is-truncated :integer :not-null [:check [:in is-truncated [0 1]]]]
           [:column encoding-problem :integer :not-null [:check [:in encoding-problem [0 1]]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:primary-key [account-id blob-id profile-version]]
           [:foreign-key [account-id] :references jmap-account [account-id] :on-delete :cascade]]
   [:table jmap-parsed-attachment
           [:column account-id :text :not-null]
           [:column source-blob-id :text :not-null]
           [:column profile-version :text :not-null]
           [:column ordinal :integer :not-null [:check [:>= ordinal 0]]]
           [:column part-id :text :not-null [:check [:> [:call length part-id] 0]]]
           [:column blob-id :text :not-null [:check [:> [:call length blob-id] 0]]]
           [:column size :integer :not-null [:check [:>= size 0]]]
           [:column name :text]
           [:column media-type :text :not-null [:check [:> [:call length media-type] 0]]]
           [:column charset :text]
           [:column disposition :text]
           [:column cid :text]
           [:column language-json :text :not-null]
           [:column location :text]
           [:primary-key [account-id source-blob-id profile-version ordinal]]
           [:unique [account-id source-blob-id profile-version part-id]]
           [:foreign-key [account-id source-blob-id profile-version]
                         :references jmap-parsed-blob [account-id blob-id profile-version]
                         :on-delete :cascade]]
   [:table jmap-search-projection
           [:column account-id :text :not-null [:references jmap-account [account-id] :on-delete :cascade]]
           [:column query-key :text :not-null [:check [:> [:call length query-key] 0]]]
           [:column query-text :text :not-null [:check [:> [:call length query-text] 0]]]
           [:column filter-json :text :not-null [:check [:> [:call length filter-json] 0]]]
           [:column query-state :text :not-null [:check [:> [:call length query-state] 0]]]
           [:column email-state :text :not-null [:check [:> [:call length email-state] 0]]]
           [:column cursor-remote-email-id :text
                    [:check [:or [:is cursor-remote-email-id nil]
                                 [:> [:call length cursor-remote-email-id] 0]]]]
           [:column revision :integer :not-null [:check [:>= revision 0]]]
           [:column maybe-more :integer :not-null [:check [:in maybe-more [0 1]]]]
           [:column is-stale :integer :not-null [:default 0] [:check [:in is-stale [0 1]]]]
           [:column observed-change-seq :integer :not-null [:check [:>= observed-change-seq 0]]]
           [:primary-key [account-id query-key]]]
   [:table jmap-search-projection-row
           [:column account-id :text :not-null]
           [:column query-key :text :not-null]
           [:column ordinal :integer :not-null [:check [:>= ordinal 0]]]
           [:column local-email-id :text :not-null]
           [:column remote-thread-id :text :not-null [:check [:> [:call length remote-thread-id] 0]]]
           [:column received-at :text :not-null [:check [:> [:call length received-at] 0]]]
           [:column from-name :text]
           [:column from-email :text]
           [:column subject :text :not-null]
           [:column preview :text :not-null]
           [:column is-unread :integer :not-null [:check [:in is-unread [0 1]]]]
           [:column is-flagged :integer :not-null [:check [:in is-flagged [0 1]]]]
           [:column has-attachment :integer :not-null [:check [:in has-attachment [0 1]]]]
           [:column remote-mailbox-ids-json :text :not-null]
           [:column snippet-subject :text]
           [:column snippet-preview :text]
           [:primary-key [account-id query-key ordinal]]
           [:unique [account-id query-key local-email-id]]
           [:foreign-key [account-id query-key] :references jmap-search-projection [account-id query-key] :on-delete :cascade]
           [:foreign-key [account-id local-email-id] :references jmap-email-record [account-id local-email-id] :on-delete :cascade]]
   [:table jmap-seen-intent
           [:column account-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column remote-email-id :text :not-null [:check [:> [:call length remote-email-id] 0]]]
           [:column operation-id :text :not-null :unique [:check [:> [:call length operation-id] 0]]]
           [:column desired-seen :integer :not-null [:check [:in desired-seen [0 1]]]]
           [:column base-unread :integer :not-null [:check [:in base-unread [0 1]]]]
           [:column phase :text :not-null [:check [:in phase [[:literal "pending"] [:literal "unknown"]]]]]
           [:column error-kind :text
                    [:check [:or [:is error-kind nil]
                                 [:> [:call length error-kind] 0]]]]
           [:column accepted-change-seq :integer :not-null [:check [:>= accepted-change-seq 0]]]
           [:column updated-change-seq :integer :not-null [:check [:>= updated-change-seq 0]]]
           [:primary-key [account-id local-email-id]]
           [:foreign-key [account-id local-email-id] :references jmap-email-record [account-id local-email-id] :on-delete :cascade]]
   [:table jmap-trash-operation
           [:column operation-id :text :primary-key [:check [:> [:call length operation-id] 0]]]
           [:column account-id :text :not-null :unique]
           [:column trash-mailbox-id :text :not-null]
           [:column accepted-change-seq :integer :not-null [:check [:>= accepted-change-seq 0]]]
           [:column updated-change-seq :integer :not-null [:check [:>= updated-change-seq 0]]]
           [:unique [operation-id account-id]]
           [:foreign-key [account-id] :references jmap-account [account-id] :on-delete :cascade]
           [:foreign-key [account-id trash-mailbox-id] :references jmap-mailbox [account-id mailbox-id]]]
   [:table jmap-trash-target
           [:column operation-id :text :not-null]
           [:column account-id :text :not-null]
           [:column local-email-id :text :not-null]
           [:column remote-email-id :text :not-null [:check [:> [:call length remote-email-id] 0]]]
           [:column original-mailbox-ids-json :text]
           [:column phase :text :not-null [:check [:in phase [[:literal "pending"] [:literal "unknown"]]]]]
           [:column error-kind :text
                    [:check [:or [:is error-kind nil]
                                 [:> [:call length error-kind] 0]]]]
           [:column accepted-change-seq :integer :not-null [:check [:>= accepted-change-seq 0]]]
           [:column updated-change-seq :integer :not-null [:check [:>= updated-change-seq 0]]]
           [:primary-key [operation-id local-email-id]]
           [:unique [account-id local-email-id]]
           [:foreign-key [operation-id account-id] :references jmap-trash-operation [operation-id account-id] :on-delete :cascade]
           [:foreign-key [account-id local-email-id] :references jmap-email-record [account-id local-email-id] :on-delete :cascade]]
   [:table jmap-type-checkpoint
           [:column account-id :text :not-null [:references jmap-account [account-id] :on-delete :cascade]]
           [:column data-type :text :not-null [:check [:> [:call length data-type] 0]]]
           [:column state :text
                    [:check [:or [:is state nil] [:> [:call length state] 0]]]]
           [:column revision :integer :not-null [:check [:>= revision 0]]]
           [:column observed-change-seq :integer
                    [:check [:or [:is observed-change-seq nil]
                                 [:>= observed-change-seq 0]]]]
           [:primary-key [account-id data-type]]]
   [:table store-metadata
           [:column singleton :integer :primary-key [:check [:= singleton 1]]]
           [:column store-id :text :not-null [:check [:> [:call length store-id] 0]]]
           [:column change-seq :integer :not-null [:check [:>= change-seq 0]]]]
   [:index jmap-account-endpoint :on jmap-account :columns [endpoint-id is-available]]
   [:index jmap-email-one-active-generation :on jmap-email-generation
           :columns [account-id] :unique t
           :where [:= lifecycle [:literal "active"]]]
   [:index jmap-email-one-building-generation :on jmap-email-generation
           :columns [account-id] :unique t
           :where [:= lifecycle [:literal "building"]]]
   [:index jmap-email-generation-mailbox-by-mailbox
           :on jmap-email-generation-mailbox
           :columns [account-id generation-id remote-mailbox-id local-email-id]]
   [:index jmap-email-metadata-recent
           :on jmap-email-metadata
           :columns [account-id received-at local-email-id]]
   [:index jmap-identity-account :on jmap-identity :columns [account-id is-available]]
   [:index jmap-identity-account-local :on jmap-identity
           :columns [account-id identity-id] :unique t]
   [:index chidu-compose-workspace-updated :on chidu-compose-workspace
           :columns [updated-change-seq workspace-id]]
   [:index chidu-compose-workspace-remote-draft
           :on chidu-compose-workspace
           :columns [account-id base-remote-email-id]
           :unique t]
   [:index chidu-compose-resource-workspace
           :on chidu-compose-resource
           :columns [workspace-id resource-id]]
   [:index chidu-draft-publish-phase :on chidu-draft-publish-attempt
           :columns [phase updated-change-seq attempt-id]]
   [:index chidu-draft-publish-workspace :on chidu-draft-publish-attempt
           :columns [workspace-id phase created-change-seq attempt-id]]
   [:index jmap-mailbox-account :on jmap-mailbox :columns [account-id is-available]]
   [:index jmap-mailbox-account-local :on jmap-mailbox :columns [account-id mailbox-id] :unique t]
   [:index jmap-mailbox-move-target-account :on jmap-mailbox-move-target :columns [account-id phase accepted-change-seq]]
   [:index jmap-trash-target-account :on jmap-trash-target :columns [account-id phase accepted-change-seq]]]
  "Chidu current greenfield SQLite schema.")

(defconst chidu-store-sqlite--metadata-present-query
  (chidu-sql
   [:select [1]
            :from store-metadata
            :where [:= singleton 1]
            :limit 1])
  "Query proving that Store metadata has been initialized.")

(defvar chidu-store-sqlite--expected-schema-snapshot-cache nil
  "Lazily compiled snapshot of `chidu-store-sqlite--schema'.")

(defun chidu-store-sqlite--initialize-schema (database)
  "Create Chidu's current greenfield schema in fresh DATABASE."
  (dolist (statement
           (chidu-sql-schema-statements chidu-store-sqlite--schema))
    (chidu-sql-execute database statement)))

(defun chidu-store-sqlite--initialize-metadata (database)
  "Create the sole Store metadata row in fresh DATABASE."
  (unless (car
           (chidu-sql-select
               database chidu-store-sqlite--metadata-present-query))
    (let ((store-id (chidu-store-new-local-id)))
      (chidu-sql-execute database
        [:insert :into store-metadata
                 :row
                 [[singleton 1]
                  [store-id [:bind store-id]]
                  [change-seq 0]]]))))

(defun chidu-store-sqlite--pragma-identifier (value)
  "Return validated SQLite pragma identifier VALUE."
  (unless (and (stringp value)
               (string-match-p
                (rx string-start
                    (or (in "A-Z" "a-z") "_")
                    (* (or alnum "_"))
                    string-end)
                value))
    (signal 'chidu-invariant-error
            (list "SQLite schema contains an unsafe identifier" value)))
  value)

(defun chidu-store-sqlite--pragma (database pragma identifier)
  "Run PRAGMA for IDENTIFIER in DATABASE.

PRAGMA is a trusted constant supplied by this module."
  (sqlite-select
   database
   (format "PRAGMA %s(%s)"
           pragma
           (chidu-store-sqlite--pragma-identifier identifier))))

(defun chidu-store-sqlite--schema-object-sql (database type name)
  "Return normalized DATABASE schema SQL for object TYPE and NAME."
  (when-let* ((sql
               (caar
                (sqlite-select
                 database
                 "SELECT sql FROM sqlite_master
                    WHERE type = ? AND name = ? AND sql IS NOT NULL"
                 (list type name)))))
    (chidu-sql-normalize sql)))

(defun chidu-store-sqlite--schema-index-snapshot (database row)
  "Return deterministic DATABASE index snapshot for index-list ROW."
  (let* ((name (nth 1 row))
         (auto-p (string-prefix-p "sqlite_autoindex_" name)))
    (list
     :name (unless auto-p name)
     :unique (nth 2 row)
     :origin (nth 3 row)
     :partial (nth 4 row)
     :columns
     (chidu-store-sqlite--pragma database "index_xinfo" name)
     :sql
     (unless auto-p
       (chidu-store-sqlite--schema-object-sql database "index" name)))))

(defun chidu-store-sqlite--schema-table-snapshot (database name)
  "Return deterministic DATABASE schema snapshot for table NAME."
  (let ((indexes
         (mapcar
          (lambda (row)
            (chidu-store-sqlite--schema-index-snapshot database row))
          (chidu-store-sqlite--pragma database "index_list" name))))
    (list
     :sql (chidu-store-sqlite--schema-object-sql database "table" name)
     :columns (chidu-store-sqlite--pragma database "table_xinfo" name)
     :foreign-keys
     (sort
      (chidu-store-sqlite--pragma database "foreign_key_list" name)
      (lambda (left right)
        (string< (prin1-to-string left) (prin1-to-string right))))
     :indexes
     (sort indexes
           (lambda (left right)
             (string< (prin1-to-string left) (prin1-to-string right)))))))

(defun chidu-store-sqlite--schema-object-names (database type)
  "Return sorted non-internal DATABASE object names of TYPE."
  (sort
   (mapcar
    #'car
    (sqlite-select
     database
     "SELECT name FROM sqlite_master
        WHERE type = ? AND name NOT LIKE 'sqlite\\_%' ESCAPE '\\'
          AND (? != 'index' OR sql IS NOT NULL)
        ORDER BY name"
     (list type type)))
   #'string<))

(defun chidu-store-sqlite--schema-snapshot (database)
  "Return complete current-format schema snapshot for DATABASE."
  (let ((table-names
         (chidu-store-sqlite--schema-object-names database "table"))
        (index-names
         (chidu-store-sqlite--schema-object-names database "index")))
    (list
     :table-names table-names
     :index-names index-names
     :tables
     (mapcar
      (lambda (name)
        (cons name
              (chidu-store-sqlite--schema-table-snapshot database name)))
      table-names))))

(defun chidu-store-sqlite--expected-schema-snapshot ()
  "Return cached schema snapshot compiled from the current manifest."
  (or chidu-store-sqlite--expected-schema-snapshot-cache
      (setq
       chidu-store-sqlite--expected-schema-snapshot-cache
       (let ((database (sqlite-open)))
         (unwind-protect
             (progn
               (sqlite-execute database "PRAGMA foreign_keys = ON")
               (with-sqlite-transaction database
                 (chidu-store-sqlite--initialize-schema database))
               (chidu-store-sqlite--schema-snapshot database))
           (sqlite-close database))))))

(defun chidu-store-sqlite--sequence-drift (expected actual)
  "Return first concise difference between EXPECTED and ACTUAL sequences."
  (let ((expected-length (length expected))
        (actual-length (length actual))
        (index 0)
        difference)
    (while (and (< index expected-length)
                (< index actual-length)
                (null difference))
      (unless (equal (elt expected index) (elt actual index))
        (setq difference
              (list :index index
                    :expected (elt expected index)
                    :actual (elt actual index))))
      (cl-incf index))
    (append
     difference
     (unless (= expected-length actual-length)
       (list :expected-length expected-length
             :actual-length actual-length)))))

(defun chidu-store-sqlite--name-set-drift (component expected actual)
  "Return COMPONENT diagnostic for EXPECTED and ACTUAL name sets."
  (list :component component
        :missing (seq-difference expected actual #'equal)
        :extra (seq-difference actual expected #'equal)))

(defun chidu-store-sqlite--schema-drift (expected actual)
  "Return first diagnostic difference between EXPECTED and ACTUAL schema."
  (cond
   ((not (equal (plist-get expected :table-names)
                (plist-get actual :table-names)))
    (chidu-store-sqlite--name-set-drift
     'tables
     (plist-get expected :table-names)
     (plist-get actual :table-names)))
   ((not (equal (plist-get expected :index-names)
                (plist-get actual :index-names)))
    (chidu-store-sqlite--name-set-drift
     'indexes
     (plist-get expected :index-names)
     (plist-get actual :index-names)))
   (t
    (cl-loop
     for (name . expected-table) in (plist-get expected :tables)
     for actual-table = (cdr (assoc name (plist-get actual :tables)))
     thereis
     (cl-loop
      for component in '(:sql :columns :foreign-keys :indexes)
      for expected-value = (plist-get expected-table component)
      for actual-value = (plist-get actual-table component)
      unless (equal expected-value actual-value)
      return
      (append
       (list :component component :table name)
       (chidu-store-sqlite--sequence-drift
        expected-value actual-value)))))))

(defun chidu-store-sqlite--assert-current-schema (database)
  "Require DATABASE to match Chidu's complete current greenfield schema."
  (let* ((expected (chidu-store-sqlite--expected-schema-snapshot))
         (actual (chidu-store-sqlite--schema-snapshot database))
         (drift (chidu-store-sqlite--schema-drift expected actual)))
    (when drift
      (signal
       'chidu-invariant-error
       (list "SQLite Store does not match the current greenfield format"
             :schema-drift drift
             :rebuild-required t)))))

(provide 'chidu-store-sqlite-schema)

;;; chidu-store-sqlite-schema.el ends here
