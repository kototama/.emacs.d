(define-minor-mode notmuch-kill-mode
  "Toggle Notmuch Kill Mode.
     Allows the killing of message threads. All new incoming
     messages contained in a killed thread will have their
     'unread' tag removed.
  "
  :group 'notmuch
  :lighter " notmuch-kill"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-k") 'notmuch-kill-add-thread-to-kill-list)
            map)
  :after-hook (notmuch-kill-init))

(require 'notmuch-query)

(defvar notmuch-kill-kill-list-path "~/.kill-list")

(setq notmuch-kill-kill-list nil)

(defun notmuch-kill-init
  ()
  "Initializes the mode."
  (when (not (file-exists-p notmuch-kill-kill-list-path))
    (notmuch-kill-write-kill-list))
  (setq notmuch-kill-kill-list (notmuch-kill-read-kill-list))
  (add-hook 'kill-buffer-hook 'notmuch-kill-kill-buffer-hook))

(defun notmuch-kill-kill-buffer-hook
  (&optional exit-data)
  "Called when a buffer is killed."
  (when (or (eq major-mode 'notmuch-search-mode)
            (eq major-mode 'notmuch-show-mode)
            (eq major-mode 'notmuch-hello-mode))
    (notmuch-kill-write-kill-list)))

(defun notmuch-kill-mark-as-read
  (messageid)
  "Removes the 'unread' tag of the message."
  (notmuch-tag (concat "id:" messageid) '("-unread")))

(defun notmuch-kill-refresh-view
  ()
  "Refresh the current view"
  (cond ((eq major-mode 'notmuch-show-mode)
         (notmuch-show-refresh-view))

        ((eq major-mode 'notmuch-search-mode)
         (notmuch-search-refresh-view))))

(defun notmuch-kill-get-id
  ()
  "Returns the message id of the mail being read or the message id of
  the thread."
  (cond ((eq major-mode 'notmuch-show-mode)
         (notmuch-show-get-message-id))

        ((eq major-mode 'notmuch-search-mode)
         (notmuch-search-find-thread-id))))

(defun notmuch-kill-mark-thread-as-read
  (idstring)
  "Marks the thread containing the message as read."
  (notmuch-query-map-threads
   (lambda (msg)
     (let ((id (plist-get msg :id)))
       (notmuch-kill-mark-as-read id)))
   (notmuch-query-get-threads (list idstring))))

(defun notmuch-kill-mark-thread-as-read-and-refresh
  (idstring)
  (interactive (list (notmuch-kill-get-id)))
  "Marks the thread containing the message as read and refreshed the view."
  (notmuch-kill-mark-thread-as-read idstring)
  (notmuch-kill-refresh-view))

(defun notmuch-kill-get-string-from-file
  (pathname)
  "Return file's content."
  (with-temp-buffer
    (insert-file-contents pathname)
    (buffer-string)))

(defun notmuch-kill-read-kill-list
  ()
  "Returns the content of the kill-list."
  (let ((content (notmuch-kill-get-string-from-file
                  notmuch-kill-kill-list-path)))
    (read content)))

(defun notmuch-kill-write-kill-list
  ()
  "Writes the content of the kill-list to the disk."
  (with-temp-file notmuch-kill-kill-list-path
    (insert-string (prin1-to-string notmuch-kill-kill-list))))

(defun notmuch-kill-add-to-kill-list
  (idstr)
  "Adds the thread containing the message's id to the kill-list."
  (setq notmuch-kill-kill-list (cons idstr notmuch-kill-kill-list)))

(defun notmuch-kill-add-thread-to-kill-list
  (idstr)
  "Adds the thread containing the message's id to the kill-list
and refreshes the view."
  (interactive (list (notmuch-kill-get-id)))
  (notmuch-kill-add-to-kill-list idstr)
  (notmuch-kill-mark-thread-as-read-and-refresh idstr))

(defun notmuch-kill-process-kill-list
  ()
  "Marks all thread of all messages contained in the kill-list as
read."
  (dolist (idstr notmuch-kill-kill-list)
    (notmuch-kill-mark-thread-as-read idstr))
  (notmuch-kill-refresh-view)
  (message "Kill-list processed."))

(provide 'notmuch-kill-mode)
