(use-package notmuch
  :init
  (progn

    (defun my-notmuch-hello-filtered-query-unread (query filter)
      "Constructs a query to search all messages matching QUERY and
      FILTER. Query is augmented of with \"AND tag:unread\".

If FILTER is a string, it is directly used in the returned query.

If FILTER is a function, it is called with QUERY as a parameter and
the string it returns is used as the query. If nil is returned,
the entry is hidden.

Otherwise, FILTER is ignored.
"
      (cond
       ((functionp filter) (funcall filter query))
       ((stringp filter)
        (concat "(" query ") and (" filter ") and tag:unread"))
       (t (concat "(" query ") and tag:unread"))))

    (defun my-notmuch-hello-query-counts (query-alist &rest options)
      "Compute list of counts of matched messages from QUERY-ALIST.

QUERY-ALIST must be a list containing elements of the form (NAME . QUERY)
or (NAME QUERY COUNT-QUERY). If the latter form is used,
COUNT-QUERY specifies an alternate query to be used to generate
the count for the associated query.

The result is the list of elements of the form (NAME QUERY COUNT).

The values :show-empty-searches, :filter and :filter-count from
options will be handled as specified for
`notmuch-hello-insert-searches'."
      (with-temp-buffer
        (dolist (elem query-alist nil)
          (let ((count-query (if (consp (cdr elem))
                                 ;; do we have a different query for the message count?
                                 (third elem)
                               (cdr elem))))
            (insert
             (notmuch-hello-filtered-query count-query
                                           (or (plist-get options :filter-count)
                                               (plist-get options :filter)))
             "\n"
             (my-notmuch-hello-filtered-query-unread count-query
                                           (or (plist-get options :filter-count)
                                               (plist-get options
                                                          :filter)))
             "\n")))
        (call-process-region (point-min) (point-max) notmuch-command
                             t t nil "count" "--batch")
        (goto-char (point-min))

        (notmuch-remove-if-not
         #'identity
         (mapcar
          (lambda (elem)
            (let ((name (car elem))
                  (search-query (if (consp (cdr elem))
                                    ;; do we have a different query for the message count?
                                    (second elem)
                                  (cdr elem)))
                  (message-count (prog1 (read (current-buffer))
                                   (forward-line 1)))
                  (unread-count (prog1 (read (current-buffer))
                                  (forward-line 1))))
              (and (or (plist-get options :show-empty-searches) (> message-count 0))
                   (list name (notmuch-hello-filtered-query
                               search-query (plist-get options :filter))
                         message-count
                         unread-count))))
          query-alist))))

    (defun my-notmuch-hello-insert-buttons (searches)
      "Insert buttons for SEARCHES.

SEARCHES must be a list containing lists of the form (NAME QUERY COUNT), where
QUERY is the query to start when the button for the corresponding entry is
activated. COUNT should be the number of messages matching the query.
Such a list can be computed with `notmuch-hello-query-counts'."
      (let* ((widest (notmuch-hello-longest-label searches))
             (tags-and-width (notmuch-hello-tags-per-line widest))
             (tags-per-line (car tags-and-width))
             (column-width (cdr tags-and-width))
             (column-indent 0)
             (count 0)
             (reordered-list (notmuch-hello-reflect searches tags-per-line))
             ;; Hack the display of the buttons used.
             (widget-push-button-prefix "")
             (widget-push-button-suffix ""))
        ;; dme: It feels as though there should be a better way to
        ;; implement this loop than using an incrementing counter.
        (mapc (lambda (elem)
                ;; (not elem) indicates an empty slot in the matrix.
                (when elem
                  (if (> column-indent 0)
                      (widget-insert (make-string column-indent ? )))
                  (let* ((name (first elem))
                         (query (second elem))
                         (msg-count (third elem))
                         (msg-unread-count (fourth elem)))
                    (if (eq msg-unread-count 0)
                        (widget-insert (format "%8s "
                                               (notmuch-hello-nice-number
                                                msg-count)))
                      (widget-insert (format "%8s (%s) "
                                             (notmuch-hello-nice-number
                                              msg-count)
                                             msg-unread-count)))
                    (widget-create 'push-button
                                   :notify #'notmuch-hello-widget-search
                                   :notmuch-search-terms query
                                   name)
                    (setq column-indent
                          (1+ (max 0 (- column-width (length name)))))))
                (setq count (1+ count))
                (when (eq (% count tags-per-line) 0)
                  (setq column-indent 0)
                  (widget-insert "\n")))
              reordered-list)

        ;; If the last line was not full (and hence did not include a
        ;; carriage return), insert one now.
        (unless (eq (% count tags-per-line) 0)
          (widget-insert "\n"))))

    (defun my-notmuch-hello-mode-hook
      ()
      (define-key notmuch-hello-mode-map "="
        (lambda ()
          (interactive)
          (notmuch-hello-update t)
          (notmuch-jump-to-unread-or-inbox)))

      (define-key notmuch-hello-mode-map (kbd "+") '(lambda ()
						      (interactive)
						      (shell-command "offlineimap -o")
						      (notmuch-hello-update
						       nil))))

    (defun my-notmuch-mark-as-read
      ()
      (interactive)
      (notmuch-search-tag
       (if (member "unread" (notmuch-search-get-tags))
           "-unread" "+unread")))

    (defun my-notmuch-search-mode-hook
      ()
      (define-key notmuch-show-mode-map (kbd "M-m")
	(lambda ()
	  "toggle markos tag for message"
	  (interactive)
	  (notmuch-show-tag-message
	   (if (member "markos" (notmuch-show-get-tags))
	       "-markos" "+markos"))))

      (bind-key "r" 'my-notmuch-mark-as-read notmuch-search-mode-map)
      (bind-key "r" 'notmuch-show-reply notmuch-show-mode-map)
      (bind-key "R" 'notmuch-show-reply-sender notmuch-show-mode-map))

      (defun notmuch-jump-to-unread-or-inbox
        ()
        (goto-char 0)
        (re-search-forward "me" nil t)
        ;; (when (not (re-search-forward "unread" nil t))
        ;;   (re-search-forward "me" nil t))
        (backward-word))

      (defun my-notmuch-show-hook
        ()
        (require 'org-notmuch))

      (use-package notmuch-address)

      ;; (use-package gnus-art)

      (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                     notmuch-hello-insert-search
                                     notmuch-hello-insert-recent-searches
                                     notmuch-hello-insert-alltags
                                     ;; notmuch-hello-insert-footer
                                     ))

      (setq notmuch-address-command "nottoomuch-addresses.sh")
      (setq mail-user-agent 'message-user-agent)

      (setq user-mail-address "email@"
            user-full-name "name")

      (load "~/.work-config.el")

      (setq smtpmail-debug-info t)
      (setq message-send-mail-function 'message-send-mail-with-sendmail)
      (setq sendmail-program "msmtp")
      ;; (setq gnus-inhibit-images t)
      (setq notmuch-fcc-dirs "Sent")
      (setq notmuch-search-oldest-first nil)

      (add-hook 'notmuch-hello-mode-hook 'my-notmuch-hello-mode-hook)
      ;; (add-hook 'notmuch-search-mode-hook
      ;; 'my-notmuch-search-mode-hook)
      (add-hook 'notmuch-show-hook 'my-notmuch-show-hook)

      (defadvice notmuch-hello (after jump-to-registero-unread-or-inbox activate)
        (notmuch-jump-to-unread-or-inbox))

      (defadvice notmuch-hello (before replace-insert activate)
        (defun notmuch-hello-insert-saved-searches ()
          "Insert the saved-searches section."
          (let ((searches (my-notmuch-hello-query-counts
                           (if notmuch-saved-search-sort-function
                               (funcall notmuch-saved-search-sort-function
                                        notmuch-saved-searches)
                             notmuch-saved-searches)
                           :show-empty-searches notmuch-show-empty-saved-searches)))
            (when searches
              (widget-insert "Saved searches: ")
              (widget-create 'push-button
                             :notify (lambda (&rest ignore)
                                       (customize-variable 'notmuch-saved-searches))
                             "edit")
              (widget-insert "\n\n")
              (let ((start (point)))
                (my-notmuch-hello-insert-buttons searches)
                (indent-rigidly start (point) notmuch-hello-indent))))))

      (defadvice notmuch-search (after fake-search-mode-hook activate)
        (my-notmuch-search-mode-hook))

      (add-hook 'message-mode-hook
                (lambda ()
                  (notmuch-address-message-insinuate)
                  (flyspell-mode))))

  :bind (("C-c m n" . notmuch-mua-new-mail)
         ("C-c m m" . notmuch-hello)))

(provide 'init-notmuch)
