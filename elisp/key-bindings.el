;; global key bindings
(global-set-key (kbd "<C-tab>") 'other-window)
;; (global-set-key (kbd "<S-iso-lefttab>") '(lambda ()
;;                                      (other-window -1)))
(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-k") 'eager-kill-line)
(global-set-key (kbd "C-p") 'backward-char)
(global-set-key (kbd "C-S-p") 'previous-line)
(global-set-key (kbd "M-p") '(lambda (arg)
                               (interactive "p")
                               (previous-line (+ arg 4) nil)))
(global-set-key (kbd "M-n") '(lambda (arg)
                               (interactive "p")
                               (next-line (+ arg 4) nil)))
(global-set-key (kbd "C-S-j") 'join-line)
;; (global-set-key (kbd "C-<prior>") 'tabbar-forward)
;; (global-set-key (kbd "C-<next>") 'tabbar-backward)
(global-set-key (kbd "C-M-s") 'igrep-find)
;; (global-set-key [C-kp-1] '(lambda ()
;;                             (ido-find-file "~/.emacs")))

(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning-of-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-o") 'my-anything)

(global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-x") 'execute-extended-command)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key [f1] 'multi-term)
(global-set-key [f2] 'multi-term-prev)
(global-set-key [f3] 'multi-term-next)
(global-set-key [(shift f3)] 'kmacro-start-macro-or-insert-counter)
(global-set-key [(shift f4)] 'kmacro-end-or-call-macro)
(global-set-key [f4] 'slime-connect)
(global-set-key [f6] 'next-error)
(global-set-key [f8] 'paredit-mode)
(global-set-key [f9] 'magit-status)
(global-set-key [f10] 'org-agenda)

;; (global-set-key [f10] 'toggle-fullscreen)
(global-set-key [f12] '(lambda ()
                         (interactive)
                         (kill-buffer nil)))
;; (global-set-key (kbd "²") '(lambda ()
;;                                  (interactive)
;;                                  (kill-buffer nil)))

(global-set-key (kbd "<C-return>")
                '(lambda ()
                   (interactive)
                   (switch-to-buffer nil)))
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<SPC>") 'er/expand-region)
(global-set-key (kbd "C-.") 'hippie-expand)
(define-key undo-tree-map (kbd "C-?") nil)

(global-set-key (kbd "C-x ,") 'ido-switch-buffer)
(global-set-key (kbd "C-M-i") 'indent-region)


;; keybindins specific to azerty
;; altgr-b
(global-set-key (kbd "”") 'backward-word)
;; altgr-f
(global-set-key (kbd "đ") 'forward-word)
;; altgr-x
(global-set-key (kbd "»") 'smex)
;; altgr-a
(global-set-key (kbd "æ") 'beginning-of-buffer)
;; altgr-e
(global-set-key (kbd "€") 'end-of-buffer)

(provide 'key-bindings)
