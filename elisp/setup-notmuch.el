(require 'notmuch)
(require 'org-notmuch)
(require 'offlineimap)

(setq mail-user-agent 'message-user-agent)

(setq user-mail-address "email@"
      user-full-name "name")

(load "~/.work-config.el")

(setq smtpmail-debug-info t)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
(setq gnus-inhibit-images t)
(setq notmuch-fcc-dirs "Sent")

(define-key notmuch-show-mode-map "r" 'notmuch-show-reply)
(define-key notmuch-show-mode-map "R" 'notmuch-show-reply-sender)

(define-key notmuch-search-mode-map "r" 'notmuch-search-reply-to-thread)
(define-key notmuch-search-mode-map "R" 'notmuch-search-reply-to-thread-sender)

(offlineimap)

(defun ktm-index-mails
  ()
  (shell-command "indexmails.sh"))

(define-key notmuch-hello-mode-map "=" '(lambda ()
                                          (interactive)
                                          (ktm-index-mails)
                                          (notmuch-hello-update t)))

(defun ktm-read-mails
  ()
  (interactive)
  (ktm-index-mails)
  (notmuch-hello))

(global-set-key (kbd "C-c m n") 'notmuch-mua-new-mail)
(global-set-key (kbd "C-c m m") 'ktm-read-mails)

(add-hook 'notmuch-hello-refresh-hook 'ktm-fetch-mail)

(provide 'setup-notmuch)
