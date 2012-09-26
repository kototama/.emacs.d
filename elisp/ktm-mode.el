
;; Personal global mode so that keybindings applies everywhere
;; see http://www.reddit.com/r/emacs/comments/y76sl/proper_way_of_overriding_mode_keys/

(define-minor-mode ktm-mode
  "Ktm mode"
  :init-value nil
  :lighter " ktm"
  :keymap
  (let ((keymap (make-sparse-keymap)))
    ;; (define-key keymap (kbd "C-<backspace>") 'forward-char)
    (define-key keymap (kbd "M-o") 'my-helm)
    (define-key keymap (kbd "<C-tab>") 'other-window)
    (define-key keymap "\r" 'newline-and-indent)
    (define-key keymap (kbd "C-;") 'comment-region)
    (define-key keymap (kbd "C-k") 'eager-kill-line)
    (define-key keymap (kbd "C-p") 'backward-char)
    (define-key keymap (kbd "C-S-p") 'previous-line)
    (define-key keymap (kbd "M-p") '(lambda (arg)
                                   (interactive "p")
                                   (previous-line (+ arg 4) nil)))
    (define-key keymap (kbd "M-n") '(lambda (arg)
                                   (interactive "p")
                                   (next-line (+ arg 4) nil)))
    (define-key keymap (kbd "C-S-j") 'join-line)
    (define-key keymap (kbd "C-M-s") 'igrep-find)
    (define-key keymap (kbd "C-a") 'back-to-indentation-or-beginning-of-line)
    (define-key keymap (kbd "M-g") 'goto-line)
    (define-key keymap (kbd "M-o") 'my-helm)
    (define-key keymap (kbd "M-x") 'smex)
    (define-key keymap (kbd "M-X") 'smex-major-mode-commands)
    (define-key keymap (kbd "C-x SPC") 'ace-jump-mode)
    (define-key keymap (kbd "C-x p") 'pop-to-mark-command)
    (define-key keymap (kbd "C-c l") 'org-store-link)
    (define-key keymap (kbd "C-x b") 'helm-buffers-list)

    (define-key keymap [f1] 'multi-term)
    (define-key keymap [f2] 'multi-term-prev)
    (define-key keymap [f3] 'multi-term-next)
    (define-key keymap [(shift f3)] 'kmacro-start-macro-or-insert-counter)
    (define-key keymap [(shift f4)] 'kmacro-end-or-call-macro)
    (define-key keymap [f4] 'slime-connect)
    (define-key keymap [f6] 'next-error)
    (define-key keymap [f8] 'paredit-mode)
    (define-key keymap [f9] 'magit-status)
    (define-key keymap [f10] 'org-agenda)

    (define-key keymap [f12] '(lambda ()
                             (interactive)
                             (kill-buffer nil)))
    (define-key keymap (kbd "<C-return>")
                    '(lambda ()
                       (interactive)
                       (switch-to-buffer nil)))
    (define-key keymap (kbd "C-c t") 'multi-term-next)
    (define-key keymap (kbd "C-c T") 'multi-term)
    (define-key keymap (kbd "C-<") 'mark-previous-like-this)
    (define-key keymap (kbd "C->") 'mark-next-like-this)
    (define-key keymap (kbd "C-M-m") 'mc/mark-next-like-this)
    (define-key keymap (kbd "C-M-<SPC>") 'er/expand-region)
    (define-key keymap (kbd "C-.") 'hippie-expand)
    (define-key undo-tree-map (kbd "C-?") nil)

    (define-key keymap (kbd "C-x ,") 'ido-switch-buffer)
    (define-key keymap (kbd "C-M-i") 'indent-region)

    ;; keybindins specific to azerty
    ;; altgr-b
    (define-key keymap (kbd "”") 'backward-word)
    ;; altgr-f
    (define-key keymap (kbd "đ") 'forward-word)
    ;; altgr-x
    (define-key keymap (kbd "»") 'smex)
    ;; altgr-a
    (define-key keymap (kbd "æ") 'beginning-of-buffer)
    ;; altgr-e
    (define-key keymap (kbd "€") 'end-of-buffer)

    keymap)
  
  :gkeymaproup 'ktm-mode)

(define-globalized-minor-mode ktm-global-mode ktm-mode
  (lambda ()
    (if (not (minibufferp (current-buffer)))
        (ktm-mode 1))))

(add-to-list 'emulation-mode-map-alists '(ktm-global-mode ktm-mode-map))

(provide 'ktm-mode)
