;;; chidu-jmap-email-changes.el --- Canonical JMAP Email changes -*- lexical-binding: t; -*-

;;; Commentary:

;; Decode one bounded Email/changes page for canonical Email synchronization.
;; RFC 8620 permits overlap between created, updated, and destroyed.  Chidu
;; normalizes each response to disjoint sets with destroyed > created > updated;
;; this is the semantic shape consumed by membership catch-up and, later, by
;; metadata/live reconciliation.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-api)
(require 'chidu-jmap-email)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-store)

(chidu-define-record chidu-jmap-email-changes-page
    "One normalized bounded Email/changes response."
  session-state
  old-state
  new-state
  has-more-changes-p
  (created (vector))
  (updated (vector))
  (destroyed (vector)))

(defun chidu-jmap-email-changes--request
    (remote-account-id since-state max-changes)
  "Return Email/changes request for REMOTE-ACCOUNT-ID from SINCE-STATE.

MAX-CHANGES bounds the total number of ids in the three wire change arrays."
  `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
    :methodCalls
    [["Email/changes"
      (:accountId ,remote-account-id
       :sinceState ,since-state
       :maxChanges ,max-changes)
      "email-changes"]]))

(defun chidu-jmap-email-changes--id-vector (value context)
  "Decode unique JMAP Id vector VALUE for CONTEXT."
  (let ((wire (chidu-jmap--vector value context))
        (seen (make-hash-table :test #'equal))
        result)
    (cl-loop
     for item across wire
     for id = (chidu-jmap--id item context)
     do
     (when (gethash id seen)
       (signal 'chidu-jmap-error
               (list (format "%s contains a duplicate id" context))))
     (puthash id t seen)
     (push id result))
    (vconcat (nreverse result))))

(defun chidu-jmap-email-changes--method-error-type (bytes)
  "Return Email/changes method error type from BYTES, or nil."
  (let* ((wire (chidu-jmap--parse-json-object bytes "JMAP API response"))
         (_session-state
          (chidu-jmap--string
           (chidu-jmap--required wire "sessionState" "JMAP API response")
           "API sessionState"))
         (responses
          (chidu-jmap--vector
           (chidu-jmap--required wire "methodResponses" "JMAP API response")
           "methodResponses")))
    (when (= 1 (length responses))
      (let ((item (aref responses 0)))
        (when (and (vectorp item) (= 3 (length item))
                   (equal "error" (aref item 0))
                   (equal "email-changes" (aref item 2)))
          (let ((arguments
                 (chidu-jmap--hash
                  (aref item 1) "Email/changes method error")))
            (chidu-jmap--string
             (chidu-jmap--required
              arguments "type" "Email/changes method error")
             "Email/changes method error type")))))))

(defun chidu-jmap-email-changes--without (ids excluded-a excluded-b)
  "Return IDS excluding keys present in EXCLUDED-A or EXCLUDED-B."
  (vconcat
   (cl-loop
    for id across ids
    unless (or (and excluded-a (gethash id excluded-a))
               (and excluded-b (gethash id excluded-b)))
    collect id)))

(defun chidu-jmap-email-changes--index (ids)
  "Return an equality hash set containing IDS."
  (let ((table (make-hash-table :test #'equal)))
    (cl-loop for id across ids do (puthash id t table))
    table))

(defun chidu-jmap-email-changes--decode
    (bytes remote-account-id since-state max-changes)
  "Decode Email/changes BYTES for REMOTE-ACCOUNT-ID from SINCE-STATE.

MAX-CHANGES is the exact request bound.  Return a typed failure for
`cannotCalculateChanges'; otherwise return a normalized changes page."
  (let ((method-error
         (chidu-jmap-email-changes--method-error-type bytes)))
    (if (equal method-error "cannotCalculateChanges")
        (chidu-result-failure-create
         :kind 'cannot-calculate-changes
         :data (list :method "Email/changes")
         :retryable-p nil)
      (let* ((response
              (chidu-jmap-parse-single-method-response
               bytes "Email/changes" "email-changes" remote-account-id))
             (arguments
              (chidu-jmap--hash
               (chidu-jmap-method-response-arguments response)
               "Email/changes"))
             (old-state
              (chidu-jmap--string
               (chidu-jmap--required arguments "oldState" "Email/changes")
               "Email/changes oldState"))
             (new-state
              (chidu-jmap--string
               (chidu-jmap--required arguments "newState" "Email/changes")
               "Email/changes newState"))
             (has-more
              (chidu-jmap--json-boolean
               (chidu-jmap--required
                arguments "hasMoreChanges" "Email/changes")
               "Email/changes hasMoreChanges"))
             (created-wire
              (chidu-jmap-email-changes--id-vector
               (chidu-jmap--required arguments "created" "Email/changes")
               "Email/changes created"))
             (updated-wire
              (chidu-jmap-email-changes--id-vector
               (chidu-jmap--required arguments "updated" "Email/changes")
               "Email/changes updated"))
             (destroyed
              (chidu-jmap-email-changes--id-vector
               (chidu-jmap--required arguments "destroyed" "Email/changes")
               "Email/changes destroyed"))
             (wire-count
              (+ (length created-wire)
                 (length updated-wire)
                 (length destroyed)))
             (destroyed-set
              (chidu-jmap-email-changes--index destroyed))
             (created-set
              (chidu-jmap-email-changes--index created-wire))
             (created
              (chidu-jmap-email-changes--without
               created-wire destroyed-set nil))
             (updated
              (chidu-jmap-email-changes--without
               updated-wire destroyed-set created-set)))
        (unless (equal old-state since-state)
          (signal 'chidu-jmap-error
                  '("Email/changes oldState does not echo sinceState")))
        (when (> wire-count max-changes)
          (signal 'chidu-jmap-error
                  '("Email/changes returned more ids than maxChanges")))
        (chidu-jmap-email-changes-page-create
         :session-state (chidu-jmap-method-response-session-state response)
         :old-state old-state
         :new-state new-state
         :has-more-changes-p has-more
         :created created
         :updated updated
         :destroyed destroyed)))))

(defun chidu-jmap-email-fetch-changes-page
    (context secret max-changes deliver)
  "Fetch one canonical Email/changes page for CONTEXT using SECRET.

MAX-CHANGES bounds changed ids.  DELIVER receives a typed result.  SECRET
remains owned by the enclosing Email synchronization workflow."
  (unless (chidu-store-email-sync-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-email-sync-context-p context)))
  (unless (and (integerp max-changes) (> max-changes 0))
    (signal 'wrong-type-argument (list 'positive-integer-p max-changes)))
  (let* ((endpoint (chidu-store-email-sync-context-endpoint context))
         (account (chidu-store-email-sync-context-account context))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (since-state (chidu-store-email-sync-context-state context)))
    (chidu-jmap-api-start
     endpoint secret
     (chidu-jmap-email-changes--request
      remote-account-id since-state max-changes)
     (lambda (bytes)
       (chidu-jmap-email-changes--decode
        bytes remote-account-id since-state max-changes))
     deliver)))

(provide 'chidu-jmap-email-changes)

;;; chidu-jmap-email-changes.el ends here
