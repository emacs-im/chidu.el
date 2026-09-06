;;; chidu-jmap-search.el --- Bounded JMAP Email search -*- lexical-binding: t; -*-

;;; Commentary:

;; One server-search request combines Email/query, Email/get, and
;; SearchSnippet/get with JMAP result references.  The adapter preserves query
;; order, settles query/get races, and returns one closed Store observation.

;;; Code:

(require 'cl-lib)
(require 'chidu-jmap-email)
(require 'chidu-jmap-http)
(require 'chidu-jmap-response)
(require 'chidu-jmap-types)
(require 'chidu-record)
(require 'chidu-result)
(require 'chidu-search-query)
(require 'chidu-store)

(defconst chidu-jmap-search-properties
  ["id" "threadId" "mailboxIds" "keywords" "receivedAt"
   "from" "subject" "preview" "hasAttachment"]
  "Email properties fetched for bounded search results.")

(cl-defstruct (chidu-jmap-search-fetch
               (:constructor chidu-jmap-search-fetch-create))
  "Mechanical state for one cancelable Email search fetch."
  context
  spec
  secret
  limit
  request-limit
  anchor
  expected-query-state
  deliver
  request
  completed-p
  canceled-p)

(defun chidu-jmap-search--request
    (remote-account-id spec limit &optional anchor)
  "Return one search request for REMOTE-ACCOUNT-ID, SPEC, and LIMIT.

With ANCHOR, start immediately after that Email; otherwise start at position
zero.  Email/get and SearchSnippet/get consume the exact query ids by result
reference."
  (let ((filter (chidu-search-spec-filter spec)))
    `(:using [,chidu-jmap-core-capability ,chidu-jmap-mail-capability]
      :methodCalls
      [["Email/query"
        (:accountId ,remote-account-id
         :filter ,filter
         :sort [(:property "receivedAt" :isAscending :json-false)]
         :collapseThreads :json-false
         :calculateTotal :json-false
         :limit ,limit
         ,@(if anchor
               `(:anchor ,anchor :anchorOffset 1)
             '(:position 0)))
        "search-query"]
       ["Email/get"
        (:accountId ,remote-account-id
         ,(intern ":#ids")
         (:resultOf "search-query" :name "Email/query" :path "/ids")
         :properties ,chidu-jmap-search-properties)
        "search-email"]
       ["SearchSnippet/get"
        (:accountId ,remote-account-id
         :filter ,filter
         ,(intern ":#emailIds")
         (:resultOf "search-query" :name "Email/query" :path "/ids"))
        "search-snippet"]])))

(defun chidu-jmap-search--nullable-snippet-text (value context)
  "Return nullable SearchSnippet text VALUE for CONTEXT."
  (if (eq value :json-null)
      nil
    (chidu-jmap--string value context t)))

(defun chidu-jmap-search--preview (value)
  "Decode nullable SearchSnippet preview VALUE and enforce its RFC bound."
  (let ((preview
         (chidu-jmap-search--nullable-snippet-text
          value "SearchSnippet preview")))
    (when (and preview
               (> (string-bytes (encode-coding-string preview 'utf-8-unix))
                  255))
      (signal 'chidu-jmap-error
              '("SearchSnippet preview exceeds 255 UTF-8 octets")))
    preview))

(defun chidu-jmap-search--decode-snippets (arguments remote-email-ids)
  "Decode SearchSnippet/get ARGUMENTS for REMOTE-EMAIL-IDS.

Return a hash table from Email id to `chidu-store-search-snippet'."
  (let* ((arguments (chidu-jmap--hash arguments "SearchSnippet/get"))
         (wire-list
          (chidu-jmap--vector
           (chidu-jmap--required arguments "list" "SearchSnippet/get")
           "SearchSnippet/get list"))
         (wire-not-found
          (chidu-jmap-email--nullable-id-vector
           (chidu-jmap--required
            arguments "notFound" "SearchSnippet/get")
           "SearchSnippet/get notFound"))
         (expected (make-hash-table :test #'equal))
         (settled (make-hash-table :test #'equal))
         (snippets (make-hash-table :test #'equal)))
    (cl-loop for id across remote-email-ids do (puthash id t expected))
    (cl-loop
     for wire across wire-list
     for snippet = (chidu-jmap--hash wire "SearchSnippet")
     for id =
     (chidu-jmap--id
      (chidu-jmap--required snippet "emailId" "SearchSnippet")
      "SearchSnippet emailId")
     do
     (unless (gethash id expected)
       (signal 'chidu-jmap-error
               '("SearchSnippet/get returned an unrequested Email id")))
     (when (gethash id settled)
       (signal 'chidu-jmap-error
               '("SearchSnippet/get settled one Email more than once")))
     (puthash id t settled)
     (puthash
      id
      (chidu-store-search-snippet-create
       :subject
       (chidu-jmap-search--nullable-snippet-text
        (chidu-jmap--required snippet "subject" "SearchSnippet")
        "SearchSnippet subject")
       :preview
       (chidu-jmap-search--preview
        (chidu-jmap--required snippet "preview" "SearchSnippet")))
      snippets))
    (cl-loop
     for id across wire-not-found
     do
     (unless (gethash id expected)
       (signal 'chidu-jmap-error
               '("SearchSnippet/get returned an unrequested notFound id")))
     (when (gethash id settled)
       (signal 'chidu-jmap-error
               '("SearchSnippet/get settled one Email more than once")))
     (puthash id t settled))
    (unless (= (hash-table-count expected) (hash-table-count settled))
      (signal 'chidu-jmap-error
              '("SearchSnippet/get did not settle every requested Email")))
    snippets))

(defun chidu-jmap-search--decode
    (bytes remote-account-id spec page-size
           &optional request-limit anchor expected-query-state)
  "Decode search BYTES for REMOTE-ACCOUNT-ID and SPEC.

PAGE-SIZE bounds rows retained by the Store.  REQUEST-LIMIT is the wire probe
bound.  ANCHOR and EXPECTED-QUERY-STATE identify an append request."
  (setq request-limit (or request-limit page-size))
  (let* ((responses
          (chidu-jmap-parse-method-responses
           bytes
           (list
            (list "Email/query" "search-query" remote-account-id)
            (list "Email/get" "search-email" remote-account-id)
            (list "SearchSnippet/get" "search-snippet" remote-account-id))))
         (query-response (aref responses 0))
         (email-response (aref responses 1))
         (snippet-response (aref responses 2))
         (page
          (chidu-jmap-email--decode-query-page-arguments
           (chidu-jmap-method-response-arguments query-response)
           request-limit))
         (query-state
          (chidu-store-email-query-page-observation-query-state page))
         (all-ids
          (chidu-store-email-query-page-observation-remote-email-ids page)))
    (when (and (null anchor)
               (not
                (zerop
                 (chidu-store-email-query-page-observation-position page))))
      (signal 'chidu-jmap-error
              '("Email search response did not start at position zero")))
    (when (and anchor (not (equal expected-query-state query-state)))
      (signal 'chidu-jmap-error
              '("Email search query state changed during pagination")))
    (let* ((slice
            (chidu-jmap-email-query-page-slice
             page page-size request-limit))
           (visible-ids (plist-get slice :ids))
           (email-decoded
            (chidu-jmap-email--decode-view-get
             (chidu-jmap-method-response-arguments email-response)
             all-ids "Email search object"))
           (email-state (car email-decoded))
           (rows-by-id (cdr email-decoded))
           (snippets
            (chidu-jmap-search--decode-snippets
             (chidu-jmap-method-response-arguments snippet-response) all-ids))
           rows)
      (cl-loop
       for id across visible-ids
       for decoded = (gethash id rows-by-id)
       when decoded
       do
       (push
        (chidu-store-search-observation-row-create
         :summary-row (car decoded)
         :remote-mailbox-ids (cdr decoded)
         :snippet (gethash id snippets))
        rows))
      (chidu-store-search-observation-create
       :query-key (chidu-search-spec-query-key spec)
       :query-text (chidu-search-spec-query-text spec)
       :filter-json (chidu-search-spec-filter-json spec)
       :query-state query-state
       :email-state email-state
       :cursor-remote-email-id
       (or (plist-get slice :cursor) anchor)
       :maybe-more-p (plist-get slice :maybe-more-p)
       :rows (vconcat (nreverse rows))))))

(defun chidu-jmap-search--finish (fetch result)
  "Complete FETCH exactly once with RESULT and clear its credential."
  (unless (chidu-jmap-search-fetch-completed-p fetch)
    (setf (chidu-jmap-search-fetch-completed-p fetch) t
          (chidu-jmap-search-fetch-request fetch) nil)
    (when-let* ((secret (chidu-jmap-search-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-search-fetch-secret fetch) nil))
    (unless (chidu-jmap-search-fetch-canceled-p fetch)
      (funcall (chidu-jmap-search-fetch-deliver fetch) result))))

(defun chidu-jmap-search--cancel (fetch)
  "Cancel FETCH and clear its credential."
  (unless (chidu-jmap-search-fetch-completed-p fetch)
    (setf (chidu-jmap-search-fetch-canceled-p fetch) t
          (chidu-jmap-search-fetch-completed-p fetch) t)
    (when-let* ((request (chidu-jmap-search-fetch-request fetch)))
      (chidu-jmap-http-cancel request)
      (setf (chidu-jmap-search-fetch-request fetch) nil))
    (when-let* ((secret (chidu-jmap-search-fetch-secret fetch)))
      (clear-string secret)
      (setf (chidu-jmap-search-fetch-secret fetch) nil))))

(defun chidu-jmap-search--start
    (context spec secret page-size request-limit
             anchor expected-query-state deliver)
  "Start one initial or anchored JMAP search page for CONTEXT and SPEC.

SECRET authenticates the request.  PAGE-SIZE and REQUEST-LIMIT bound it;
ANCHOR and EXPECTED-QUERY-STATE identify an append.  DELIVER receives a result."
  (unless (chidu-store-search-context-p context)
    (signal 'wrong-type-argument
            (list 'chidu-store-search-context-p context)))
  (unless (chidu-search-spec-p spec)
    (signal 'wrong-type-argument (list 'chidu-search-spec-p spec)))
  (unless (and (stringp secret) (not (string-empty-p secret)))
    (signal 'chidu-jmap-error '("credential is empty")))
  (unless (and (integerp page-size) (> page-size 0)
               (integerp request-limit) (>= request-limit page-size))
    (signal 'wrong-type-argument
            (list 'valid-search-page-bounds-p page-size request-limit)))
  (unless (functionp deliver)
    (signal 'wrong-type-argument (list 'functionp deliver)))
  (when anchor
    (unless (and (stringp anchor) (not (string-empty-p anchor))
                 (stringp expected-query-state)
                 (not (string-empty-p expected-query-state)))
      (signal 'chidu-invariant-error
              '("Search pagination requires cursor and query state"))))
  (let* ((endpoint (chidu-store-search-context-endpoint context))
         (account (chidu-store-search-context-account context))
         (remote-account-id
          (chidu-store-account-remote-account-id account))
         (fetch
          (chidu-jmap-search-fetch-create
           :context context :spec spec :secret secret :limit page-size
           :request-limit request-limit :anchor anchor
           :expected-query-state expected-query-state :deliver deliver)))
    (condition-case error-data
        (setf
         (chidu-jmap-search-fetch-request fetch)
         (chidu-jmap-http-request
          (chidu-store-endpoint-api-url endpoint)
          (chidu-store-endpoint-login endpoint)
          (chidu-store-endpoint-authentication endpoint)
          secret
          (lambda (result)
            (cond
             ((chidu-result-failure-p result)
              (chidu-jmap-search--finish fetch result))
             ((chidu-result-ok-p result)
              (condition-case validation-error
                  (let ((response (chidu-result-ok-value result)))
                    (if (= 200 (chidu-jmap-http-response-status response))
                        (chidu-jmap-search--finish
                         fetch
                         (chidu-result-ok-create
                          :value
                          (chidu-jmap-search--decode
                           (chidu-jmap-http-response-body response)
                           remote-account-id spec page-size request-limit
                           anchor expected-query-state)))
                      (chidu-jmap-search--finish
                       fetch
                       (chidu-result-failure-create
                        :kind 'unexpected-http-status
                        :data
                        (list :status
                              (chidu-jmap-http-response-status response))
                        :retryable-p nil))))
                (error
                 (chidu-jmap-search--finish
                  fetch
                  (chidu-result-failure-create
                   :kind 'invalid-jmap-response
                   :data
                   (list :message
                         (error-message-string validation-error))
                   :retryable-p nil)))))
             (t (chidu-jmap-search--finish fetch result))))
          :body
          (chidu-jmap-search--request
           remote-account-id spec request-limit anchor)
          :max-request-bytes
          (chidu-store-endpoint-max-size-request endpoint)
          :byte-cap chidu-jmap-api-byte-cap))
      (error
       (chidu-jmap-search--finish
        fetch
        (chidu-result-failure-create
         :kind 'jmap-request-failed
         :data (list :message (error-message-string error-data))
         :retryable-p nil))))
    (apply-partially #'chidu-jmap-search--cancel fetch)))

(defun chidu-jmap-fetch-search-page
    (context spec secret page-size request-limit deliver)
  "Fetch CONTEXT's first search SPEC page using owned SECRET.

PAGE-SIZE and REQUEST-LIMIT bound the query; DELIVER receives a typed result."
  (chidu-jmap-search--start
   context spec secret page-size request-limit nil nil deliver))

(defun chidu-jmap-fetch-more-search
    (context spec secret page-size request-limit deliver)
  "Fetch CONTEXT's next search SPEC page using owned SECRET.

PAGE-SIZE and REQUEST-LIMIT bound the query; DELIVER receives a typed result."
  (unless (and (chidu-store-search-context-maybe-more-p context)
               (chidu-store-search-context-cursor-remote-email-id context)
               (chidu-store-search-context-query-state context))
    (signal 'chidu-invariant-error
            '("Search context has no appendable page cursor")))
  (chidu-jmap-search--start
   context spec secret page-size request-limit
   (chidu-store-search-context-cursor-remote-email-id context)
   (chidu-store-search-context-query-state context)
   deliver))

(provide 'chidu-jmap-search)

;;; chidu-jmap-search.el ends here
