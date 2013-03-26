(require 'notmuch)
;; (require 'notmuch-pick nil t)
(require 'notmuch-address)
(require 'org-notmuch)
(require 'offlineimap)
(require 'gnus-art)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; starts offlineimap process
(offlineimap)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq notmuch-address-command "nottoomuch-addresses.sh")
(notmuch-address-message-insinuate)

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

(define-key notmuch-show-mode-map "r" 'notmuch-show-reply)
(define-key notmuch-show-mode-map "R" 'notmuch-show-reply-sender)

(define-key notmuch-search-mode-map "r" 'notmuch-search-reply-to-thread)
(define-key notmuch-search-mode-map "R" 'notmuch-search-reply-to-thread-sender)

(define-key notmuch-hello-mode-map "=" '(lambda ()
                                          (interactive)
                                          (notmuch-hello-update t)))

(define-key notmuch-hello-mode-map (kbd "+") '(lambda ()
                                                  (interactive)
                                                  (shell-command "offlineimap -o")
                                                  (notmuch-hello-update nil)))

(add-hook 'message-mode-hook
          (lambda ()
            (flyspell-mode)))

(global-set-key (kbd "C-c m n") 'notmuch-mua-new-mail)
(global-set-key (kbd "C-c m m") 'notmuch-hello)

(provide 'setup-notmuch)
