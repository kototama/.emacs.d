(use-package notmuch
  :init
  (progn

    (use-package notmuch-address)
    (use-package org-notmuch)
    ;; (use-package gnus-art)

    (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                   notmuch-hello-insert-search
                                   notmuch-hello-insert-recent-searches
                                   notmuch-hello-insert-alltags
                                   notmuch-hello-insert-footer))
    
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
						       nil)))
      )

    (defun my-notmuch-search-mode-hook
      ()
      (define-key notmuch-show-mode-map (kbd "M-m")
	(lambda ()
	  "toggle markos tag for message"
	  (interactive)
	  (notmuch-show-tag-message
	   (if (member "markos" (notmuch-show-get-tags))
	       "-markos" "+markos"))))

      (define-key notmuch-search-mode-map (kbd "r") '(lambda ()
						       (interactive)
						       (notmuch-search-tag
							(if (member "unread" (notmuch-search-get-tags))
							    "-unread"
							  "+unread"))))

      (define-key notmuch-show-mode-map "r" 'notmuch-show-reply)
      (define-key notmuch-show-mode-map "R"
      'notmuch-search-reply-sender))

    (defun notmuch-jump-to-unread-or-inbox
      ()
      (goto-char 0)
      (when (not (re-search-forward "unread" nil t))
        (re-search-forward "inbox" nil t))
      (backward-word))

    (add-hook 'notmuch-hello-mode-hook 'my-notmuch-hello-mode-hook)
    (add-hook 'notmuch-search-mode-hook 'my-notmuch-search-mode-hook)

    (defadvice notmuch-hello (after jump-to-unread-or-inbox activate)
      (notmuch-jump-to-unread-or-inbox))
      

    (add-hook 'message-mode-hook
              (lambda ()
                (flyspell-mode))))

  :bind (("C-c m n" . notmuch-mua-new-mail)
         ("C-c m m" . notmuch-hello)))

;; ;; (autoload 'notmuch-mua-new-mail "notmuch" "notmuch-mode" t)
;; ;; (autoload 'notmuch-hello "notmuch" "notmuch-mode" t)

;; (global-set-key (kbd "C-c m n") 'notmuch-mua-new-mail)
;; (global-set-key (kbd "C-c m m") 'notmuch-hello)

(provide 'init-notmuch)
