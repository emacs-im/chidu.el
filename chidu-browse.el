;;; chidu-browse.el --- Exact web targets in mail readers -*- lexical-binding: t; -*-

;;; Code:

(require 'browse-url)
(require 'url-parse)
(require 'thingatpt)
(require 'appkit-ui)

(declare-function chidu-conversation-focus "chidu-conversation" ())

(defun chidu-browse-web-url-p (url)
  "Return non-nil for an absolute HTTP(S) URL without credentials."
  (and (stringp url)
       (not (string-match-p "[[:space:][:cntrl:]]" url))
       (condition-case nil
           (let ((parsed (url-generic-parse-url url)))
             (and (member (url-type parsed) '("http" "https"))
                  (stringp (url-host parsed))
                  (> (length (url-host parsed)) 0)
                  (not (url-user parsed))
                  (not (url-password parsed))))
         (error nil))))

(defun chidu-browse-open (url)
  "Open explicit web target URL, never a non-web URI scheme."
  (unless (chidu-browse-web-url-p url)
    (user-error "No valid web target"))
  (browse-url url))

(defun chidu-browse-add-link (start end url)
  "Make source text START..END an Appkit web action for URL."
  (when (chidu-browse-web-url-p url)
    (add-text-properties start end
                         (list 'chidu-browse-url url
                               'rear-nonsticky '(chidu-browse-url)))
    (appkit-ui-add-action start end (apply-partially #'chidu-browse-open url)
                          :help-echo url :face 'link)))

(defun chidu-browse--source-in-region (start end)
  "Return a provider source URL annotated within START..END."
  (when-let* ((position (text-property-not-all
                        start end 'chidu-browse-source-url nil)))
    (get-text-property position 'chidu-browse-source-url)))

(defun chidu-browse--source-at-point ()
  "Return the current message's source URL, without crossing message rows."
  (or (get-text-property (point) 'chidu-browse-source-url)
      (cond
       ((derived-mode-p 'chidu-message-mode 'chidu-parsed-message-mode)
        (chidu-browse--source-in-region (point-min) (point-max)))
       ((and (derived-mode-p 'chidu-conversation-mode)
             (< (point) (point-max))
             (get-text-property (point) 'chidu-conversation-email-id))
        (chidu-browse--source-in-region
         (or (previous-single-property-change
              (1+ (point)) 'chidu-conversation-email-id)
             (point-min))
         (next-single-property-change
          (point) 'chidu-conversation-email-id nil (point-max)))))))

;;;###autoload
(defun chidu-activate-at-point ()
  "Activate a link or control at point; otherwise focus a Conversation row.
Explicit links and controls take priority over URL-looking label text.
This never falls back to the message's source URL."
  (interactive)
  (let ((url (or (get-text-property (point) 'chidu-browse-url)
                 (get-text-property (point) 'shr-url))))
    (cond
     (url (chidu-browse-open url))
     ((appkit-ui-activate-at (point)))
     ((setq url (thing-at-point 'url t)) (chidu-browse-open url))
     ((derived-mode-p 'chidu-conversation-mode) (chidu-conversation-focus))
     (t (user-error "No link or control at point")))))

;;;###autoload
(defun chidu-browse-at-point ()
  "Browse the current message's original provider page, not the link at point.
Resolve only within the current reader or Conversation Email row.  Never
open a neighboring message's target or fetch an uncached body."
  (interactive)
  (if-let* ((url (chidu-browse--source-at-point)))
      (chidu-browse-open url)
    (user-error "No original-post URL here; open the full message first")))

(provide 'chidu-browse)

;;; chidu-browse.el ends here
