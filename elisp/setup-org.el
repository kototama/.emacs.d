(setq org-default-notes-file "~/Documents/Notes/todo.org")

(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-agenda-span 55)
             (define-key org-mode-map (kbd "<C-return>") nil)
             (define-key org-mode-map (kbd "<return>") 'newline)
             (define-key org-mode-map (kbd "<C-tab>") nil)
             (define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
             (define-key org-mode-map (kbd "<backtab>") nil)))

;; (setq org-todo-keywords
;;       '((sequence "[ ]" "|" "[x]")))

(global-set-key (kbd "C-c r") 'org-capture)

(setq org-agenda-files '("~/Documents/Notes/"))

(provide 'setup-org)
