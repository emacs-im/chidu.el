;;; chidu-store.el --- Closed Store contract for Chidu -*- lexical-binding: t; -*-

;;; Commentary:

;; Backend-independent records and closed Store operations.  Domain and runtime
;; code may construct these operations, but never receives a database handle,
;; SQL string, transaction callback, or arbitrary keyspace accessor.
;;
;; Untrusted JSON and configuration are validated where they enter Chidu.
;; Store operations consume those typed records and enforce transition-level
;; facts such as identity, revision, state, and ownership.  SQLite constraints
;; guard persisted shape.  The Store deliberately does not deep-copy and
;; revalidate every scalar field at every internal handoff.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'chidu-record)
(require 'chidu-result)

(defvar chidu-store--local-id-counter 0
  "Process-local entropy counter for opaque local identifiers.")

(defconst chidu-store-active-email-row-limit 256
  "Maximum local Email ids accepted by one targeted active-row read.")

(defun chidu-store-new-local-id ()
  "Return a lowercase RFC 4122 variant UUID-shaped local identifier."
  (let* ((hex
          (secure-hash
           'sha256
           (format "%S:%d:%d:%d"
                   (current-time)
                   (emacs-pid)
                   (random)
                   (cl-incf chidu-store--local-id-counter))))
         (chars (string-to-vector (substring hex 0 32))))
    ;; Version 4 and RFC 4122 variant bits.  The remaining bits are opaque
    ;; process entropy; uniqueness is enforced again by Store constraints.
    (aset chars 12 ?4)
    (aset chars 16 ?8)
    (let ((text (concat chars)))
      (format "%s-%s-%s-%s-%s"
              (substring text 0 8)
              (substring text 8 12)
              (substring text 12 16)
              (substring text 16 20)
              (substring text 20 32)))))

(defun chidu-store-local-id-p (value)
  "Return non-nil when VALUE has the canonical local UUID shape."
  (and (stringp value)
       (= 36 (length value))
       (cl-loop for index below 36
                for character = (aref value index)
                always
                (cond
                 ((memq index '(8 13 18 23)) (= character ?-))
                 ((= index 14) (= character ?4))
                 ((= index 19) (memq character '(?8 ?9 ?a ?b)))
                 (t (or (and (<= ?0 character) (<= character ?9))
                        (and (<= ?a character) (<= character ?f))))))))

(chidu-define-record chidu-store-identity
    "Current viewer-scoped Identity projection."
  identity-id
  remote-identity-id
  name
  email
  available-p)

(chidu-define-record chidu-store-account
    "Current viewer-scoped Account projection."
  account-id
  remote-account-id
  name
  personal-p
  read-only-p
  primary-mail-p
  primary-submission-p
  available-p
  identity-state
  (max-size-attachments-per-email nil)
  (capabilities (vector))
  (identities (vector)))

(chidu-define-record chidu-store-compose-document
    "One structured, editable outbound mail document."
  (to "")
  (cc "")
  (bcc "")
  (reply-to "")
  (subject "")
  (body "")
  (resource-ids (vector)))

(chidu-define-record chidu-store-compose-workspace
    "One local checkout of an outbound mail document."
  workspace-id
  account-id
  identity-id
  kind
  document
  base-remote-email-id
  base-remote-blob-id
  published-revision
  (revision 0))

(chidu-define-record chidu-store-compose-resource
    "One stable attachment resource owned by a Compose workspace."
  resource-id
  workspace-id
  name
  media-type
  (size 0)
  digest
  remote-blob-id
  charset
  disposition
  cid
  (language (vector))
  location)

(chidu-define-record chidu-store-compose-resource-observation
    "One validated Compose resource before Store registration."
  resource-id
  name
  media-type
  (size 0)
  digest
  remote-blob-id
  charset
  disposition
  cid
  (language (vector))
  location)

(chidu-define-record chidu-store-draft-publish-attempt
    "One durable publication attempt for a Compose workspace revision."
  attempt-id
  workspace-id
  account-id
  identity-id
  drafts-mailbox-id
  revision
  message-id
  predecessor-remote-email-id
  predecessor-remote-blob-id
  phase
  error-kind)

(chidu-define-record chidu-store-compose-context
    "Durable local context for one outbound composition."
  endpoint
  account
  identity
  drafts-mailbox
  workspace
  (resources (vector))
  publish-attempt
  (cleanup-attempts (vector)))

(chidu-define-record chidu-store-mailbox-rights
    "Current viewer-scoped Mailbox rights."
  may-read-items-p
  may-add-items-p
  may-remove-items-p
  may-set-seen-p
  may-set-keywords-p
  may-create-child-p
  may-rename-p
  may-delete-p
  may-submit-p)

(chidu-define-record chidu-store-mailbox
    "Current viewer-scoped Mailbox projection."
  mailbox-id
  remote-mailbox-id
  name
  parent-mailbox-id
  parent-remote-mailbox-id
  role
  sort-order
  total-emails
  unread-emails
  total-threads
  unread-threads
  rights
  subscribed-p
  available-p)

(defun chidu-store--mailbox-sort-key (mailbox)
  "Return user-facing sort key for MAILBOX."
  (let ((role-rank
         (pcase (chidu-store-mailbox-role mailbox)
           ("inbox" 0) ("drafts" 1) ("sent" 2) ("archive" 3)
           ("all" 4) ("junk" 8) ("trash" 9) (_ 5))))
    (list role-rank
          (chidu-store-mailbox-sort-order mailbox)
          (downcase (chidu-store-mailbox-name mailbox)))))

(defun chidu-store-mailbox-less-p (left right)
  "Return non-nil when Mailbox LEFT sorts before RIGHT."
  (let ((a (chidu-store--mailbox-sort-key left))
        (b (chidu-store--mailbox-sort-key right)))
    (or (< (nth 0 a) (nth 0 b))
        (and (= (nth 0 a) (nth 0 b))
             (or (< (nth 1 a) (nth 1 b))
                 (and (= (nth 1 a) (nth 1 b))
                      (string< (nth 2 a) (nth 2 b))))))))

(chidu-define-record chidu-store-mailbox-sync-context
    "Durable Account context used by the Mailbox workflow."
  endpoint
  account
  state
  (revision 0)
  (mailboxes (vector)))

(chidu-define-record chidu-store-email-sync-context
    "Durable baseline state for one Account's Email synchronization."
  endpoint
  account
  (phase 'uninitialized)
  generation-id
  profile-version
  state
  query-state
  can-calculate-changes-p
  (committed-count 0)
  anchor-remote-email-id
  hydration-after-local-email-id
  (revision 0))

(chidu-define-record chidu-store-email-summary-row
    "One row derived from canonical Email state for list presentation."
  local-email-id
  remote-email-id
  remote-thread-id
  received-at
  from-name
  from-email
  subject
  preview
  unread-p
  flagged-p
  has-attachment-p)

(chidu-define-record chidu-store-mailbox-summary-context
    "One bounded Summary view derived from the active Email generation."
  endpoint
  account
  mailbox
  (revision 0)
  maybe-more-p
  (rows (vector)))

(chidu-define-record chidu-store-draft-row
    "One canonical server Draft with its optional local checkout overlay."
  summary-row
  (recipients (vector))
  workspace-id
  workspace-revision
  published-revision
  publish-phase)

(chidu-define-record chidu-store-drafts-context
    "One bounded Drafts view derived from canonical Email state."
  endpoint
  account
  mailbox
  (revision 0)
  maybe-more-p
  (rows (vector)))

(chidu-define-record chidu-store-search-snippet
    "Server-derived highlighted text for one Email search hit."
  subject
  preview)

(chidu-define-record chidu-store-search-row
    "One locally committed Email search hit."
  summary-row
  (remote-mailbox-ids (vector))
  snippet)

(chidu-define-record chidu-store-search-context
    "Locally committed bounded server-search projection."
  endpoint
  account
  query-key
  query-text
  filter-json
  query-state
  email-state
  cursor-remote-email-id
  (revision 0)
  maybe-more-p
  stale-p
  (rows (vector)))

(chidu-define-record chidu-store-seen-intent
    "One durable explicit $seen intent for an Email."
  operation-id
  local-email-id
  remote-email-id
  desired-seen-p
  base-unread-p
  phase
  error-kind)

(chidu-define-record chidu-store-seen-context
    "Current explicit $seen intents for one Account."
  endpoint
  account
  (intents (vector)))

(chidu-define-record chidu-store-seen-change
    "Effective Email read state after one intent transition."
  endpoint
  account
  operation-id
  local-email-id
  remote-email-id
  unread-p
  phase
  error-kind)

(chidu-define-record chidu-store-mailbox-move-intent
    "One unresolved target in a durable Mailbox move operation."
  local-email-id
  remote-email-id
  phase
  error-kind)

(chidu-define-record chidu-store-mailbox-move-context
    "Current durable Mailbox move operation for one Account."
  endpoint
  account
  operation-id
  source-mailbox
  destination-mailbox
  (intents (vector)))

(chidu-define-record chidu-store-mailbox-move-target-outcome
    "One remote outcome used to settle a Mailbox move target."
  local-email-id
  outcome
  error-kind)

(chidu-define-record chidu-store-mailbox-move-target-change
    "One effective target transition emitted by a Mailbox move."
  local-email-id
  phase
  error-kind)

(chidu-define-record chidu-store-mailbox-move-result
    "Mailbox move context and target changes after one Store transition."
  context
  (changes (vector)))

(chidu-define-record chidu-store-trash-intent
    "One unresolved target in a durable move-to-Trash operation."
  local-email-id
  remote-email-id
  original-remote-mailbox-ids
  phase
  error-kind)

(chidu-define-record chidu-store-trash-context
    "Current durable move-to-Trash operation for one Account."
  endpoint
  account
  operation-id
  trash-mailbox
  (intents (vector)))

(chidu-define-record chidu-store-trash-target-evidence
    "Authoritative mutable-state evidence for one Trash target."
  local-email-id
  remote-email-id
  found-p
  (remote-mailbox-ids (vector)))

(chidu-define-record chidu-store-trash-target-outcome
    "One remote outcome used to settle a Trash target."
  local-email-id
  outcome
  error-kind)

(chidu-define-record chidu-store-trash-target-change
    "One effective target transition emitted by move-to-Trash."
  local-email-id
  phase
  error-kind)

(chidu-define-record chidu-store-trash-result
    "Trash context and target changes after one Store transition."
  context
  (changes (vector)))

(chidu-define-record chidu-store-new-email-row
    "One Email newly inserted into the active generation."
  summary-row
  (remote-mailbox-ids (vector)))

(chidu-define-record chidu-store-email-address
    "One parsed RFC mailbox address from a JMAP Email shape."
  name
  email)

(chidu-define-record chidu-store-email-metadata
    "One immutable metadata-v1 fragment for a canonical Email."
  remote-blob-id
  remote-thread-id
  size
  received-at
  sent-at
  (sender (vector))
  (from (vector))
  (to (vector))
  (cc (vector))
  (bcc (vector))
  (reply-to (vector))
  subject
  (message-ids (vector))
  (in-reply-to (vector))
  (references (vector))
  has-attachment-p)

(chidu-define-record chidu-store-email-hydration-target
    "One stable Email identity selected for homogeneous metadata hydration."
  local-email-id
  remote-email-id)

(chidu-define-record chidu-store-email-hydration-plan
    "One bounded homogeneous metadata hydration plan."
  kind
  (targets (vector)))

(chidu-define-record chidu-store-email-hydration-result
    "One current Email/get settlement in requested target order."
  remote-email-id
  found-p
  metadata
  preview
  (remote-mailbox-ids (vector))
  (keywords (vector)))

(chidu-define-record chidu-store-email-attachment
    "One immutable JMAP Email attachment descriptor."
  part-id
  blob-id
  (size 0)
  name
  media-type
  charset
  disposition
  cid
  (language (vector))
  location)

(chidu-define-record chidu-store-email-body
    "Locally committed display body for one Email."
  email-state
  text-content
  html-content
  truncated-p
  encoding-problem-p
  (attachments (vector)))

(chidu-define-record chidu-store-email-body-context
    "Local body materialization context for one Email."
  endpoint
  account
  local-email-id
  remote-email-id
  (revision 0)
  body)

(chidu-define-record chidu-store-parsed-message
    "One structured read-only message returned by JMAP Email/parse."
  (message-ids (vector))
  (in-reply-to (vector))
  (references (vector))
  (sender (vector))
  (from (vector))
  (to (vector))
  (cc (vector))
  (bcc (vector))
  (reply-to (vector))
  subject
  sent-at
  preview
  body)

(chidu-define-record chidu-store-parsed-blob-context
    "Local materialization context for one account-scoped parsed Blob."
  endpoint
  account
  blob-id
  profile-version
  (revision 0)
  message)

(chidu-define-record chidu-store-conversation-row
    "One locally committed Email row in a reply tree."
  summary-row
  sent-at
  (message-ids (vector))
  (in-reply-to (vector))
  (references (vector))
  parent-local-email-id
  (depth 0))

(chidu-define-record chidu-store-conversation-context
    "Locally committed on-demand Conversation projection."
  endpoint
  account
  remote-thread-id
  thread-state
  email-state
  (revision 0)
  complete-p
  (rows (vector)))

(chidu-define-record chidu-store-endpoint
    "Current local Endpoint configuration and Session projection."
  endpoint-id
  session-url
  login
  authentication
  session-username
  session-state
  api-url
  download-url
  upload-url
  event-source-url
  max-size-request
  (max-size-upload nil)
  max-objects-in-get
  max-objects-in-set
  primary-contacts-remote-account-id
  (capabilities (vector))
  (accounts (vector)))

(chidu-define-record chidu-store-runtime
    "Bounded runtime metadata projection."
  store-id
  change-seq)

(chidu-define-record chidu-store-identity-observation
    "Validated remote Identity observation before Store commit."
  remote-identity-id
  name
  email)

(chidu-define-record chidu-store-account-observation
    "Validated remote Account observation before Store commit."
  remote-account-id
  name
  personal-p
  read-only-p
  primary-mail-p
  primary-submission-p
  identity-state
  (max-size-attachments-per-email nil)
  (capabilities (vector))
  (identities (vector)))

(chidu-define-record chidu-store-mailbox-observation
    "Validated remote Mailbox observation before Store commit."
  remote-mailbox-id
  name
  parent-remote-mailbox-id
  role
  sort-order
  total-emails
  unread-emails
  total-threads
  unread-threads
  rights
  subscribed-p)

(chidu-define-record chidu-store-mailbox-snapshot-observation
    "One complete validated Mailbox/get snapshot."
  state
  (mailboxes (vector)))

(chidu-define-record chidu-store-email-query-page-observation
    "One validated Email/query baseline page."
  query-state
  can-calculate-changes-p
  position
  server-limit
  (remote-email-ids (vector)))

(chidu-define-record chidu-store-email-changes-observation
    "One normalized canonical Email/changes page before Store commit."
  old-state
  new-state
  has-more-changes-p
  (created (vector))
  (updated (vector))
  (destroyed (vector)))

(chidu-define-record chidu-store-email-hydration-observation
    "One exact homogeneous Email/get hydration response."
  kind
  state
  (results (vector)))

(chidu-define-record chidu-store-email-catchup-observation
    "One normalized Email/changes round and its exact get settlements."
  changes
  full
  mutable)

(chidu-define-record chidu-store-email-round-result
    "Result of one canonical Email/changes Store transition."
  context
  (new-local-email-ids (vector))
  closed-p
  changed-p)

(chidu-define-record chidu-store-email-summary-observation-row
    "One validated remote Email row used by query-bound materializations."
  remote-email-id
  remote-thread-id
  received-at
  from-name
  from-email
  subject
  preview
  unread-p
  flagged-p
  has-attachment-p)

(chidu-define-record chidu-store-search-observation-row
    "One validated remote Email search hit before Store commit."
  summary-row
  (remote-mailbox-ids (vector))
  snippet)

(chidu-define-record chidu-store-search-observation
    "One validated bounded server-search observation."
  query-key
  query-text
  filter-json
  query-state
  email-state
  cursor-remote-email-id
  maybe-more-p
  (rows (vector)))

(chidu-define-record chidu-store-email-body-observation
    "One validated remote display body before Store commit."
  remote-email-id
  email-state
  text-content
  html-content
  truncated-p
  encoding-problem-p
  (attachments (vector)))

(chidu-define-record chidu-store-parsed-blob-observation
    "One validated Email/parse result before Store commit."
  blob-id
  profile-version
  message)

(chidu-define-record chidu-store-conversation-observation-row
    "One remote Email metadata row before reply-tree projection."
  summary-row
  sent-at
  (message-ids (vector))
  (in-reply-to (vector))
  (references (vector)))

(chidu-define-record chidu-store-conversation-observation
    "One validated Thread/get plus Email/get Conversation observation."
  remote-thread-id
  thread-state
  email-state
  complete-p
  (rows (vector)))

(chidu-define-record chidu-store-session-observation
    "Validated JMAP Session and Account observation."
  username
  state
  api-url
  download-url
  upload-url
  event-source-url
  max-size-request
  (max-size-upload nil)
  max-objects-in-get
  max-objects-in-set
  primary-contacts-remote-account-id
  (capabilities (vector))
  (accounts (vector)))

(chidu-define-record chidu-store-op-runtime
    "Read runtime metadata.")

(chidu-define-record chidu-store-op-list-endpoints
    "List Endpoint projections.")

(chidu-define-record chidu-store-op-list-compose-workspaces
    "List local outbound Compose workspaces.")

(chidu-define-record chidu-store-op-get-compose-workspace
    "Read one local outbound Compose workspace."
  workspace-id)

(chidu-define-record chidu-store-op-get-drafts
    "Read one bounded canonical Drafts projection."
  account-id
  mailbox-id
  limit)

(chidu-define-record chidu-store-op-checkout-draft
    "Create or recover one local checkout of a canonical server Draft."
  workspace-id
  account-id
  identity-id
  drafts-mailbox-id
  local-email-id
  remote-email-id
  remote-blob-id
  document
  (resources (vector)))

(chidu-define-record chidu-store-op-create-compose-workspace
    "Create one local outbound Compose workspace."
  workspace-id
  account-id
  identity-id
  kind
  document)

(chidu-define-record chidu-store-op-add-compose-resource
    "Atomically checkpoint and append one stable Compose resource."
  workspace-id
  identity-id
  expected-revision
  revision
  document
  resource)

(chidu-define-record chidu-store-op-remove-compose-resource
    "Atomically checkpoint and remove one Compose resource."
  workspace-id
  identity-id
  expected-revision
  revision
  document
  resource-id)

(chidu-define-record chidu-store-op-set-compose-resource-blob
    "Set or clear confirmed JMAP Blob evidence for one Compose resource."
  workspace-id
  resource-id
  remote-blob-id)

(chidu-define-record chidu-store-op-checkpoint-compose-workspace
    "CAS-checkpoint one local Compose document revision."
  workspace-id
  identity-id
  expected-revision
  revision
  document)

(chidu-define-record chidu-store-op-accept-draft-publish
    "Checkpoint and accept one durable server-Draft publication attempt."
  workspace-id
  identity-id
  expected-revision
  revision
  document
  attempt-id
  message-id)

(chidu-define-record chidu-store-op-mark-draft-publish-unknown
    "Fence a pending Draft create before the remote request may run."
  attempt-id)

(chidu-define-record chidu-store-op-retry-draft-publish-create
    "Return a reconciled absent Draft create to the safe pending phase."
  attempt-id)

(chidu-define-record chidu-store-op-settle-draft-publish-create
    "Settle one Draft create attempt from exact remote evidence."
  attempt-id
  outcome
  remote-email-id
  remote-blob-id
  error-kind)

(chidu-define-record chidu-store-op-settle-draft-publish-cleanup
    "Settle predecessor cleanup for one published server Draft."
  attempt-id
  outcome
  error-kind)

(chidu-define-record chidu-store-op-discard-compose-workspace
    "CAS-discard one local Compose workspace."
  workspace-id
  expected-revision)

(chidu-define-record chidu-store-op-configure-endpoint
    "Create or update one Endpoint configuration."
  session-url
  login
  authentication)

(chidu-define-record chidu-store-op-observe-session
    "Commit one validated Session observation for an Endpoint."
  endpoint-id
  observation)

(chidu-define-record chidu-store-op-get-mailbox-sync-context
    "Read one Account's Mailbox checkpoint and current projection."
  account-id)

(chidu-define-record chidu-store-op-observe-mailbox-snapshot
    "CAS-commit one complete Mailbox/get snapshot."
  account-id
  expected-revision
  observation)

(chidu-define-record chidu-store-op-list-mailboxes
    "Read one Account's Mailbox checkpoint and current projection."
  account-id)

(chidu-define-record chidu-store-op-get-email-sync-context
    "Read one Account's durable Email synchronization context."
  account-id)

(chidu-define-record chidu-store-op-begin-email-bootstrap
    "Create a building Email generation from an observed object state."
  account-id
  expected-revision
  state
  profile-version)

(chidu-define-record chidu-store-op-append-email-query-chunk
    "CAS-append one validated Email/query prefix chunk."
  account-id
  generation-id
  expected-revision
  observation)

(chidu-define-record chidu-store-op-restart-email-bootstrap
    "Replace a drifting building generation from a fresh Email object state."
  account-id
  generation-id
  expected-revision
  state
  profile-version)

(chidu-define-record chidu-store-op-apply-email-membership-changes
    "CAS-apply one canonical Email/changes page to building membership."
  account-id
  generation-id
  expected-revision
  expected-state
  observation)

(chidu-define-record chidu-store-op-get-email-hydration-plan
    "Read the next homogeneous metadata hydration batch."
  account-id
  limit)

(chidu-define-record chidu-store-op-apply-email-hydration
    "CAS-apply one exact metadata hydration batch."
  account-id
  generation-id
  expected-revision
  observation)

(chidu-define-record chidu-store-op-finish-email-hydration
    "CAS-close exhausted metadata hydration."
  account-id
  generation-id
  expected-revision)

(chidu-define-record chidu-store-op-apply-email-catchup-round
    "CAS-apply one state-bounded metadata catch-up round."
  account-id
  generation-id
  expected-revision
  expected-state
  observation)

(chidu-define-record chidu-store-op-activate-email-generation
    "CAS-publish one state-closed building Email generation."
  account-id
  generation-id
  expected-revision
  expected-state)

(chidu-define-record chidu-store-op-get-active-email-rows
    "Read current active-generation rows for bounded local Email ids."
  account-id
  (local-email-ids (vector)))

(chidu-define-record chidu-store-op-get-mailbox-summary
    "Read one bounded Summary view from the active Email generation."
  account-id
  mailbox-id
  limit)

(chidu-define-record chidu-store-op-get-search
    "Read one locally committed bounded server-search projection."
  account-id
  query-key)

(chidu-define-record chidu-store-op-replace-search
    "CAS-replace one bounded server-search projection."
  account-id
  query-key
  expected-revision
  observation)

(chidu-define-record chidu-store-op-append-search
    "CAS-append one stable-query server-search page."
  account-id
  query-key
  expected-revision
  expected-query-state
  expected-cursor-remote-email-id
  observation)

(chidu-define-record chidu-store-op-list-seen-intents
    "Read durable explicit $seen intents for one Account."
  account-id)

(chidu-define-record chidu-store-op-accept-seen-intent
    "Accept or supersede one explicit $seen intent."
  account-id
  local-email-id
  remote-email-id
  operation-id
  desired-seen-p
  current-unread-p)

(chidu-define-record chidu-store-op-settle-seen-intent
    "Settle one explicit $seen intent by operation identity."
  account-id
  local-email-id
  operation-id
  outcome
  error-kind)

(chidu-define-record chidu-store-op-get-mailbox-move-context
    "Read one Account's current durable Mailbox move operation."
  account-id)

(chidu-define-record chidu-store-op-accept-mailbox-move
    "Atomically accept one explicit multi-Email Mailbox move."
  account-id
  operation-id
  source-mailbox-id
  destination-mailbox-id
  (local-email-ids (vector)))

(chidu-define-record chidu-store-op-settle-mailbox-move
    "Settle a nonempty subset of one Mailbox move's targets."
  account-id
  operation-id
  (outcomes (vector)))

(chidu-define-record chidu-store-op-get-trash-context
    "Read one Account's current durable move-to-Trash operation."
  account-id)

(chidu-define-record chidu-store-op-accept-trash
    "Atomically accept one explicit multi-Email move-to-Trash operation."
  account-id
  operation-id
  trash-mailbox-id
  (local-email-ids (vector)))

(chidu-define-record chidu-store-op-record-trash-evidence
    "Record authoritative mailbox membership for Trash targets."
  account-id
  operation-id
  (evidence (vector)))

(chidu-define-record chidu-store-op-settle-trash
    "Settle a nonempty subset of one move-to-Trash operation."
  account-id
  operation-id
  (outcomes (vector)))

(chidu-define-record chidu-store-op-get-email-body
    "Read one Email's local body materialization context."
  account-id
  local-email-id
  remote-email-id)

(chidu-define-record chidu-store-op-replace-email-body
    "CAS-replace one Email's local display body."
  account-id
  local-email-id
  remote-email-id
  expected-revision
  observation)

(chidu-define-record chidu-store-op-get-parsed-blob
    "Read one account-scoped Email/parse materialization."
  account-id
  blob-id
  profile-version)

(chidu-define-record chidu-store-op-replace-parsed-blob
    "CAS-replace one account-scoped Email/parse materialization."
  account-id
  blob-id
  profile-version
  expected-revision
  observation)

(chidu-define-record chidu-store-op-get-conversation
    "Read one on-demand Conversation projection."
  account-id
  remote-thread-id)

(chidu-define-record chidu-store-op-replace-conversation
    "CAS-replace one on-demand Conversation projection."
  account-id
  remote-thread-id
  expected-revision
  observation)

(defun chidu-store-normalize-session-url (value)
  "Return normalized absolute HTTPS Session URL VALUE, or signal.

The URL must not contain userinfo or a fragment."
  (unless (and (stringp value) (not (string-empty-p value)))
    (signal 'chidu-invariant-error
            '("Session URL must be a nonempty string")))
  (let ((url (url-generic-parse-url value)))
    (unless (and (equal "https" (url-type url))
                 (stringp (url-host url))
                 (not (string-empty-p (url-host url)))
                 (null (url-user url))
                 (null (url-password url))
                 (null (url-target url)))
      (signal 'chidu-invariant-error
              (list "Session URL must be absolute HTTPS without userinfo or fragment"
                    value)))
    (url-recreate-url url)))

(defun chidu-store-validate-login (value)
  "Return nonempty Endpoint login VALUE."
  (unless (and (stringp value) (not (string-empty-p value)))
    (signal 'chidu-invariant-error
            '("Endpoint login must be a nonempty string")))
  value)

(defun chidu-store-validate-authentication (value)
  "Return closed authentication VALUE."
  (unless (memq value '(basic bearer))
    (signal 'chidu-invariant-error
            (list "Unknown authentication kind" value)))
  value)

(defun chidu-store-validate-id (value context)
  "Return nonempty identifier VALUE for CONTEXT."
  (unless (and (stringp value) (not (string-empty-p value)))
    (signal 'chidu-invariant-error
            (list (format "%s must be a nonempty string" context))))
  value)

(defun chidu-store-validate-string-vector (value context)
  "Return unique nonempty string vector VALUE for CONTEXT."
  (let ((seen (make-hash-table :test #'equal)))
    (unless
        (and
         (vectorp value)
         (cl-loop
          for item across value
          always
          (and (stringp item)
               (not (string-empty-p item))
               (not (gethash item seen))
               (puthash item t seen))))
      (signal 'chidu-invariant-error
              (list (format "%s must be unique nonempty strings" context)))))
  value)

(defun chidu-store-validate-positive-integer (value context)
  "Return safe positive integer VALUE for CONTEXT."
  (unless (and (integerp value)
               (> value 0)
               (<= value 9007199254740991))
    (signal 'chidu-invariant-error
            (list (format "%s must be a safe positive integer" context))))
  value)

(defun chidu-store-validate-nonempty-string (value context)
  "Return nonempty string VALUE for CONTEXT."
  (unless (and (stringp value) (not (string-empty-p value)))
    (signal 'chidu-invariant-error
            (list (format "%s must be a nonempty string" context))))
  value)

(cl-defstruct (chidu-store--conversation-node
               (:constructor chidu-store--conversation-node-create))
  "Private materialized node used while building a reply tree."
  ordinal
  remote-email-id
  local-email-id
  observation
  summary-row)

(defun chidu-store--materialize-summary-row (observation local-email-id)
  "Return local Summary row from remote OBSERVATION and LOCAL-EMAIL-ID."
  (chidu-store-email-summary-row-create
   :local-email-id local-email-id
   :remote-email-id
   (chidu-store-email-summary-observation-row-remote-email-id observation)
   :remote-thread-id
   (chidu-store-email-summary-observation-row-remote-thread-id observation)
   :received-at
   (chidu-store-email-summary-observation-row-received-at observation)
   :from-name
   (chidu-store-email-summary-observation-row-from-name observation)
   :from-email
   (chidu-store-email-summary-observation-row-from-email observation)
   :subject
   (chidu-store-email-summary-observation-row-subject observation)
   :preview
   (chidu-store-email-summary-observation-row-preview observation)
   :unread-p
   (chidu-store-email-summary-observation-row-unread-p observation)
   :flagged-p
   (chidu-store-email-summary-observation-row-flagged-p observation)
   :has-attachment-p
   (chidu-store-email-summary-observation-row-has-attachment-p observation)))

(defun chidu-store--conversation-break-cycles (nodes parent-by-remote by-remote)
  "Break malformed reply cycles in PARENT-BY-REMOTE over NODES.

For each cycle, remove the edge from its oldest Thread-order node."
  (let ((changed t))
    (while changed
      (setq changed nil)
      (catch 'cycle-broken
        (dolist (node nodes)
          (let ((seen (make-hash-table :test #'equal))
                (current
                 (chidu-store--conversation-node-remote-email-id node)))
            (while current
              (if (gethash current seen)
                  (let ((cycle (list current))
                        (next (gethash current parent-by-remote)))
                    (while (and next (not (equal next current)))
                      (push next cycle)
                      (setq next (gethash next parent-by-remote)))
                    (let ((oldest
                           (car
                            (sort
                             cycle
                             (lambda (left right)
                               (<
                                (chidu-store--conversation-node-ordinal
                                 (gethash left by-remote))
                                (chidu-store--conversation-node-ordinal
                                 (gethash right by-remote))))))))
                      (puthash oldest nil parent-by-remote)
                      (setq changed t)
                      (throw 'cycle-broken t)))
                (puthash current t seen)
                (setq current (gethash current parent-by-remote)))))))))
  parent-by-remote)

(defun chidu-store-materialize-conversation-rows
    (observation resolve-local-email-id)
  "Build a stable reply tree from validated OBSERVATION.

RESOLVE-LOCAL-EMAIL-ID receives each remote Email id and returns its stable
local id.  Membership comes exclusively from the JMAP Thread observation;
header relations only select display parents."
  (unless (functionp resolve-local-email-id)
    (signal 'wrong-type-argument
            (list 'functionp resolve-local-email-id)))
  (unless (chidu-store-conversation-observation-p observation)
    (signal 'wrong-type-argument
            (list 'chidu-store-conversation-observation-p observation)))
  (let* ((ambiguous (make-symbol "ambiguous-message-id"))
         (message-owner (make-hash-table :test #'equal))
         (by-remote (make-hash-table :test #'equal))
         (parent-by-remote (make-hash-table :test #'equal))
         nodes)
    (cl-loop
     for item across (chidu-store-conversation-observation-rows observation)
     for ordinal from 0
     for summary-observation =
     (chidu-store-conversation-observation-row-summary-row item)
     for remote-id =
     (chidu-store-email-summary-observation-row-remote-email-id
      summary-observation)
     for local-id = (funcall resolve-local-email-id remote-id)
     for node =
     (chidu-store--conversation-node-create
      :ordinal ordinal
      :remote-email-id remote-id
      :local-email-id local-id
      :observation item
      :summary-row
      (chidu-store--materialize-summary-row summary-observation local-id))
     do
     (puthash remote-id node by-remote)
     (push node nodes)
     (cl-loop
      for message-id across
      (chidu-store-conversation-observation-row-message-ids item)
      for current = (gethash message-id message-owner)
      do
      (cond
       ((null current) (puthash message-id remote-id message-owner))
       ((not (equal current remote-id))
        (puthash message-id ambiguous message-owner)))))
    (setq nodes (nreverse nodes))
    (dolist (node nodes)
      (let* ((item (chidu-store--conversation-node-observation node))
             (self (chidu-store--conversation-node-remote-email-id node))
             (in-reply-to
              (chidu-store-conversation-observation-row-in-reply-to item))
             (references
              (chidu-store-conversation-observation-row-references item)))
        (cl-labels
            ((candidate-parent
               (ids)
               (cl-loop
                for index downfrom (1- (length ids)) to 0
                for owner = (gethash (aref ids index) message-owner)
                when
                (and (stringp owner)
                     (not (equal owner self))
                     (gethash owner by-remote))
                return owner)))
          (puthash
           self
           (or (candidate-parent in-reply-to)
               (candidate-parent references))
           parent-by-remote))))
    (chidu-store--conversation-break-cycles
     nodes parent-by-remote by-remote)
    (let ((children (make-hash-table :test #'equal))
          roots
          result)
      (dolist (node nodes)
        (let* ((remote-id
                (chidu-store--conversation-node-remote-email-id node))
               (parent (gethash remote-id parent-by-remote)))
          (if parent
              (puthash parent (cons node (gethash parent children)) children)
            (push node roots))))
      (setq roots
            (sort roots
                  (lambda (left right)
                    (< (chidu-store--conversation-node-ordinal left)
                       (chidu-store--conversation-node-ordinal right)))))
      (maphash
       (lambda (parent values)
         (puthash
          parent
          (sort values
                (lambda (left right)
                  (< (chidu-store--conversation-node-ordinal left)
                     (chidu-store--conversation-node-ordinal right))))
          children))
       children)
      (cl-labels
          ((emit
             (node depth)
             (let* ((remote-id
                     (chidu-store--conversation-node-remote-email-id node))
                    (parent-remote-id
                     (gethash remote-id parent-by-remote))
                    (item (chidu-store--conversation-node-observation node)))
               (push
                (chidu-store-conversation-row-create
                 :summary-row
                 (chidu-store--conversation-node-summary-row node)
                 :sent-at
                 (chidu-store-conversation-observation-row-sent-at item)
                 :message-ids
                 (chidu-store-conversation-observation-row-message-ids item)
                 :in-reply-to
                 (chidu-store-conversation-observation-row-in-reply-to item)
                 :references
                 (chidu-store-conversation-observation-row-references item)
                 :parent-local-email-id
                 (and parent-remote-id
                      (chidu-store--conversation-node-local-email-id
                       (gethash parent-remote-id by-remote)))
                 :depth depth)
                result)
               (dolist (child (gethash remote-id children))
                 (emit child (1+ depth))))))
        (dolist (root roots) (emit root 0)))
      (vconcat (nreverse result)))))

(chidu-define-record chidu-store-capability
    "Opaque Store capability exposing closed domain operations."
  name
  invoke-function
  inspect-function
  (close-function nil))

(defun chidu-store-call (store operation deliver)
  "Invoke typed Store OPERATION through STORE and call DELIVER on completion."
  (unless (chidu-store-capability-p store)
    (signal 'wrong-type-argument (list 'chidu-store-capability-p store)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (funcall (chidu-store-capability-invoke-function store) operation deliver))

(defun chidu-store-inspect (store)
  "Return STORE's bounded capability description."
  (unless (chidu-store-capability-p store)
    (signal 'wrong-type-argument (list 'chidu-store-capability-p store)))
  (funcall (chidu-store-capability-inspect-function store)))

(defun chidu-store-close (store)
  "Close STORE exactly as its backend requires."
  (unless (chidu-store-capability-p store)
    (signal 'wrong-type-argument (list 'chidu-store-capability-p store)))
  (when-let* ((close (chidu-store-capability-close-function store)))
    (funcall close)))

(provide 'chidu-store)

;;; chidu-store.el ends here
