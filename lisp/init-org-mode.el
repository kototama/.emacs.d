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
              (setq org-octopress-directory-top       "~/Documents/Projects/Blog/octopress/source")
              (setq org-octopress-directory-posts     "~/Documents/Projects/Blog/octopress/source/_posts")
              (setq org-octopress-directory-org-top   "~/Documents/Projects/Blog/octopress/source")
              (setq org-octopress-directory-org-posts "~/Documents/Projects/Blog/octopress/source/_org_posts")
              (setq org-octopress-setup-file
                    "~/Documents/Projects/Blog/setupfile.org")
              (setq org-jekyll-comments "true")))

          (add-hook 'org-mode-hook 'my-org-mode-hook)
))

;; (setq org-todo-keywords
;;       '((sequence "[ ]" "|" "[x]")))

(bind-key "C-c r" 'org-capture)
;; (global-set-key (kbd "C-c r") 'org-capture)
