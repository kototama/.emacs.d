(setq org-default-notes-file "~/Documents/Notes/todo.org")

(add-hook 'org-mode-hook
          '(lambda ()

             (define-key org-mode-map (kbd "<C-return>") nil)
             (define-key org-mode-map (kbd "<C-tab>") nil)
             (define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
             (define-key org-mode-map (kbd "<backtab>") nil)))

;; (setq org-todo-keywords
;;       '((sequence "[ ]" "|" "[x]")))

(global-set-key (kbd "C-c r") 'org-capture)

(provide 'setup-org)
