(use-package ido-mode
  :init
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (setq ido-use-virtual-buffers t)
    (setq ido-ignore-extensions t)

    (ido-mode 1)

    (add-hook 'ido-setup-hook
              (lambda ()
                ;; Go straight home
                (define-key ido-file-completion-map
                  (kbd "~")
                  (lambda ()
                    (interactive)
                    ;; type ~~ to go the ~/.emacs.d
                    (cond ((looking-back "~/") (insert ".emacs.d/"))
                          ((looking-back "/") (insert "~/"))
                          (t (call-interactively 'self-insert-command)))))))
    )
  :bind (("C-S-o" . ido-switch-buffer)))

(provide 'init-ido-mode)
