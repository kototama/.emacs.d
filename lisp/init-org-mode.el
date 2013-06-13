(use-package org
  :init (progn

          (defun my-org-mode-hook
            ()
            (setq org-file-apps
                  (append '(("\\.pdf\\'" . "acroread %s")) org-file-apps))
            (setq org-agenda-span 128)
            ;; (setq org-agenda-include-diary nil)
            (define-key org-mode-map (kbd "<C-return>") nil)
            (define-key org-mode-map (kbd "<return>") 'newline)
            (define-key org-mode-map (kbd "<C-tab>") nil)
            (define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
            (define-key org-mode-map (kbd "<backtab>")
              nil)

            (use-package ox-reveal
              :init
              (progn
                (setq org-reveal-root "file:///home/pal/Documents/Presentations/reveal.js-master"))))

          (setq org-default-notes-file "~/Documents/Notes/todo.org")
          (setq org-agenda-files '("~/Documents/Notes/" "~/Documents/Markos/"))
          (setq org-agenda-diary-file "~/Documents/Notes/diary.org")
          (setq org-agenda-include-diary nil)

          (add-hook 'org-mode-hook 'my-org-mode-hook)
))

;; (setq org-todo-keywords
;;       '((sequence "[ ]" "|" "[x]")))

(bind-key "C-c r" 'org-capture)
;; (global-set-key (kbd "C-c r") 'org-capture)
