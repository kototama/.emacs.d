(autoload 'magit "magit-mode" "A major mode for Git" t)

(add-hook 'magit-log-edit-mode-hook
          '(lambda ()
             (flyspell-mode t)))

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;; C programming
(setq-default c-basic-offset 4)

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (define-key flyspell-mode-map (kbd "C-;") nil)))

(provide 'setup-dev)
