(require 'use-package)

(use-package org
  :init (progn

          (defun my-org-mode-mail-to-agenda-entry
            (date text)
            (interactive (list (calendar-read-date)
                               (read-string "Day entry:")))
            (org-notmuch-store-link)
            (let* ((link (concat "[["
                                 (plist-get org-store-link-plist :link)
                                 "]["
                                 (plist-get org-store-link-plist
                                            :description)
                                 "]]"))
                   (entry (concat text " " link)))
              (require 'org-agenda)
              (org-agenda-add-entry-to-org-agenda-diary-file 'day entry date)))

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
                (setq org-reveal-root
                      "file:///home/pal/Documents/Presentations/reveal.js-master"))))

          (use-package org-octopress
            :init
            (progn
              (setq org-octopress-directory-top       "~/documents/projects/blog/octopress/source")
              (setq org-octopress-directory-posts     "~/documents/projects/blog/octopress/source/_posts")
              (setq org-octopress-directory-org-top   "~/documents/projects/blog/octopress/source")
              (setq org-octopress-directory-org-posts "~/documents/projects/blog/octopress/source/_org_posts")
              (setq org-octopress-setup-file
                    "~/documents/projects/blog/setupfile.org")
              (setq org-jekyll-comments "true")))

          (setq org-default-notes-file "~/Documents/Notes/todo.org")
          (setq org-agenda-files '("~/Documents/Notes/" "~/Documents/Notes/markos/"))
          (setq org-agenda-diary-file "~/Documents/Notes/diary.org")
          (setq org-agenda-include-diary nil)

          (add-hook 'org-mode-hook 'my-org-mode-hook)
))

;; (setq org-todo-keywords
;;       '((sequence "[ ]" "|" "[x]")))

(bind-key "C-c r" 'org-capture)
;; (global-set-key (kbd "C-c r") 'org-capture)
