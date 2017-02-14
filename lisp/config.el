;;; * beginning of file

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package/")
(require 'use-package)

;;; * ace-jump
(use-package ace-jump-mode
  :bind (("M-SPC" . ace-jump-mode)))

;;; * cider-repl
(use-package cider-repl
  :config

  (defun my-cider-mode-hook
    ()
    (paredit-mode t)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-popup-stacktraces t)
    (setq cider-repl-popup-stacktraces t)
    ;; (bind-key "<S-return>" 'cider-repl-return cider-mode-map)
    )

  (add-hook 'cider-repl-mode-hook 'my-cider-mode-hook)

  :disabled t)

;;; * clojure
(use-package clojure-mode
  :config
  (progn
    (defun cider-insert-required-ns-in-repl
      ()
      (interactive)
      (save-excursion
        (goto-char 0)
        (if (re-search-forward ":require" nil t)
            (let ((pos-first-require (1+ (point))))
              (re-search-forward ")")
              (let* ((pos-last-require (1- (point)))
                     (requires (buffer-substring-no-properties pos-first-require pos-last-require))
                     (quoted-requires (replace-regexp-in-string "\n +\\(\\[\\)" "\n'[" requires))
                     (form (concat "(require '" quoted-requires ")")))
                (switch-to-buffer-other-window (cider-find-or-create-repl-buffer))
                (goto-char (max-char))
                (insert form)))
          (message ":require form not found"))))

    (defun clojure-show-defs
      ()
      (interactive)
      (occur "def"))

    (defun my-clojure-mode-hook
      ()
      (message "my-clojure-mode-hook")
      (paredit-mode t)
      (auto-indent-mode t)
      (clojure-test-mode t)
      ;; indents ring context function properly
      (put 'context 'clojure-indent-function 2))

    (add-hook 'clojure-mode-hook 'my-clojure-mode-hook)))

;;; * coffee
(use-package coffee-mode
  :config
  (progn
    (setq coffee-tab-width 2)))

;;; * color-theme
(use-package color-theme
             :config
             (require 'color-theme-kototama)
             (color-theme-kototama))

;;; * company
(use-package company
  :config
  (progn
    (setq company-idle-delay 0.5)
    ;; (global-company-mode)
    ))

;;; * css-mode
(use-package css-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
  :config
  (progn
    (add-hook 'css-mode-hook 'auto-indent-mode)))

;;; * dired
(use-package dired
  :config
  (progn
    (use-package dired-open
      :config
      (progn
       (defun my-dired-open-hook
         ()
         (local-set-key (kbd "<S-return>") 'dired-open-xdg))

       (add-hook 'dired-mode-hook 'my-dired-open-hook)))))

;;; * dired+
(use-package dired+)

;;; * elpy
(use-package elpy
  :config
  (progn

    (defun my-elpy-mode-hook ()
      (setq elpy-rpc-backend "rope")
      (setq elpy-rpc-timeout 3)
      (setq elpy-test-runner 'elpy-test-pytest-runner)
      (local-set-key (kbd "M-.") 'python-goto-definition))

    (add-hook 'elpy-mode-hook 'my-elpy-mode-hook)
    )
  )

;;; * flycheck
(use-package flycheck
  :config
  (progn
    (use-package flycheck-hdevtools
      :disabled t)))

;;; * grep

(use-package grep
  :config
  (progn
    (when (string-suffix-p "/fish" (getenv "SHELL"))
      ;; for grep-find
      (grep-apply-setting 'grep-find-command "find <D> -type f -exec grep -nH -e <R> \\{\\} +")
      ;; for rgrep
      (grep-apply-setting 'grep-find-template
                          "find . <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} \\\;")
      )))


;;; * haskell
(use-package haskell-mode
  :config
  (progn

    (setq haskell-process-type 'stack-ghci)


    (defun my-haskell-mode-hook
      ()
      (intero-mode))

    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
    ))

;;; * hl
(use-package hl-line
  :config
  (progn
    (set-face-background 'hl-line "dark slate blue")
    ))
;;; * ido
(use-package ido-mode
  :init
  (progn

    (use-package ido-ubiquitous
      :init
      (ido-ubiquitous))

    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (setq ido-use-virtual-buffers t)
    (setq ido-ignore-extensions t)
    (setq ido-default-buffer-method #'selected-window)
    (setq ido-max-directory-size 100000)

    (ido-mode 1)

    (defun my-ido-setup-hook
        ()
      ;; Go straight home
      (define-key ido-file-completion-map
        (kbd "~")
        (lambda ()
          (interactive)
          ;; type ~~ to go the ~/.emacs.d
          (cond ((looking-back "~/") (insert ".emacs.d/"))
                ((looking-back "/") (insert "~/"))
                (t (call-interactively 'self-insert-command))))))

    (add-hook 'ido-setup-hook 'my-ido-setup-hook))
  :bind (("C-S-o" . ido-switch-buffer)))

;;; * imenu
(use-package imenu
  :bind ("C-c i" . imenu))

;;; * javascript
(use-package js
  :config
  (progn

    (defun my-js-mode-hook
        ()
      (auto-indent-mode)
      (whitespace-mode)
      (flycheck-mode)
      (smartparens-mode))

    ;; for Jasmine
    (defun js-xit-tests
        ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\(^ +\\)it" nil t)
          (replace-match "\\1xit" nil nil))))

    (defun js-unxit-tests
        ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\(^ +\\)xit" nil t)
          (replace-match "\\1it" nil nil))))

    (add-hook 'js-mode-hook 'my-js-mode-hook)))

;;; * lisp
(use-package lisp-mode
  :config
  (progn

    (use-package auto-async-byte-compile
      :config
      (progn
        (setq auto-async-byte-compile-suppress-warnings t)
        (add-hook 'emacs-lisp-mode-hook
                  'enable-auto-async-byte-compile-mode)))

    (defun my-emacs-lisp-mode-hook
        ()
      (use-package paredit
        :config
        (paredit-mode t)
        (auto-indent-mode t))

      (use-package elisp-slime-nav
        :config
        (elisp-slime-nav-mode t))

      ;; (use-package auto-indent
      ;;   :config
      ;;   (auto-indent-mode t))
      )

    (defun my-minibuffer-mode-hook
      ()
      ;; (paredit-mode 1)
      )

    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
    (add-hook 'minibuffer-setup-hook 'my-minibuffer-mode-hook)))

;;; * magit
(use-package magit
  :config
  (progn

    ;; http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
    (require 's)

    (defun visit-pull-request-url ()
      "Visit the current branch's PR on Github."
      (interactive)
      (browse-url
       (format "https://github.com/%s/pull/new/%s"
               (car (s-split ".git"
                             (cadr (s-split ":"
                                            (magit-get "remote"
                                                       (magit-get-remote)
                                                       "url")))))
               (cdr (magit-get-remote-branch)))))

    ;; https://emacs.stackexchange.com/questions/13772/how-to-prevent-magit-to-ask-where-to-push-a-branch
    (defun my-magit-mode-hook
        ()

      (setq ediff-window-setup-function 'ediff-setup-windows-plain)

      (defun magit-push-arguments-maybe-upstream (magit-push-popup-fun &rest args)
        "Enable --set-upstream switch if there isn't a current upstream."
        (let ((magit-push-arguments
               (if (magit-get-remote) magit-push-arguments
                 (cons "--set-upstream" magit-push-arguments))))
          (apply magit-push-popup-fun args)))

      (advice-add 'magit-push-popup :around #'magit-push-arguments-maybe-upstream))

    (define-key magit-mode-map "v" #'visit-pull-request-url)

    (setq magit-completing-read-function #'magit-ido-completing-read)

    (add-hook 'magit-mode-hook 'my-magit-mode-hook))
  :bind (("C-c g s" . magit-status)
         ("C-c g l" . magit-file-log)
         ("C-c g L" . magit-log))
  :load-path "site-lisp/magit/lisp/")

;;; * magit-blame
(use-package magit-blame)
;;; * multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

;;; * org
(use-package org
  :config
  (progn
    (setq org-reverse-note-order t)
    (setq org-src-fontify-natively t)

    (defun jtc-org-tasks-closed-in-month (&optional month year match-string)
      "Produces an org agenda tags view list of the tasks completed
in the specified month and year. Month parameter expects a number
from 1 to 12. Year parameter expects a four digit number. Defaults
to the current month when arguments are not provided. Additional search
criteria can be provided via the optional match-string argument "
      (interactive)
      (let* ((today (calendar-current-date))
             (for-month (or month (calendar-extract-month today)))
             (for-year  (or year  (calendar-extract-year today))))
        (org-tags-view nil
                       (concat
                        match-string
                        (format "+CLOSED>=\"[%d-%02d-01]\""
                                for-year for-month)
                        (format "+CLOSED<=\"[%d-%02d-%02d]\""
                                for-year for-month
                                (calendar-last-day-of-month for-month for-year))))))

    (defun jtc-tasks-last-month ()
      "Produces an org agenda tags view list of all the tasks completed
last month."
      (interactive)
      (let* ((today (calendar-current-date))
             (for-month (calendar-extract-month today))
             (for-year  (calendar-extract-year today)))
        (calendar-increment-month for-month for-year -1)
        (jtc-org-tasks-closed-in-month
         for-month for-year "+TODO=\"DONE\"")))

    (defun my-common-org-mode-hook
        ()
      (message "my-common-org-mode-hook")
      (setq org-refile-targets '((nil :maxlevel . 2)))
      (setq org-use-speed-commands t)
      (setq org-archive-location "::* Archived Tasks")
      (setq org-log-done 'time)
      (setq org-export-with-sub-superscripts nil)
      (condition-case nil
          (progn
            (require 'org-bullets)
            (org-bullets-mode 1))
        (message "org-bullets not installed")))

    (add-hook 'org-mode-hook 'my-common-org-mode-hook))

  :bind (("C-c o a" . org-agenda)
         ("C-c o o" . org-open-at-point)
         ("C-c o l" . org-store-link)
         ("C-c o L" . org-insert-link)))

;;; * org-capture
(use-package org-capture
  :bind (("C-c o r" . org-capture)))

;;; * paredit
(use-package paredit
  :config
  (progn
    (defun my-paredit-mode-hook
      ()
      (local-set-key (kbd "M-q") 'fill-paragraph))

    (add-hook 'paredit-mode-hook 'my-paredit-mode-hook)))

;;; * projectile
(use-package projectile
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c M-p"))
    (projectile-global-mode))
  :bind (("C-c f f" . projectile-find-file)
         ("C-c f d" . projectile-find-dir)))

;;; * python
(use-package python
  :config
  (progn

    (defun python-goto-definition
        ()
      (interactive)
      (condition-case nil
          (elpy-goto-definition)
        (error
         (etags-select-find-tag-at-point)))
      )

    (defun python-set-breakpoint
        ()
      (interactive)
      (insert "import pudb; pudb.set_trace() #  noqa")
      )

    (defun my-python-mode-hook
        ()
      (elpy-mode)
      (setq elpy-rpc-backend "jedi")
      (setq python-indent-offset 4)
      (whitespace-mode)
      (whitespace-cleanup-mode)
      (local-set-key (kbd "M-.") 'python-goto-definition)
      )

    (defun python-get-test-filename
        (filename)
      "Return the name of the test file."
      (let ((dir (file-name-directory filename)))
       (if (string-suffix-p "__init__.py" filename)
           (concat dir "test_init.py")
         (concat dir "test_" (file-name-nondirectory filename)))))

    (defun python-test-file
        ()
      "Open the test file for the current buffer in an another
window and run the unit tests. "
      (interactive)
      (delete-other-windows)
      (buffer-file-name)
      (find-file-other-window (python-get-test-filename (buffer-file-name)))
      (split-window-below)
      (elpy-test)
      (enlarge-window 3))

    (add-hook 'python-mode-hook 'my-python-mode-hook)))

;;; * smex
(use-package smex
  :demand t
  :bind (("M-x" . smex)))

;;; * show-paren-mode
(use-package paren
  :init
  (progn
    (set-face-background 'show-paren-match "purple")
    (set-face-foreground 'show-paren-match "black")))

;;; * twitter
(use-package twittering-mode
  :disabled t
  :config
  (progn
    (defun load-twitter-credentials
        ()
      (interactive)
      (twittering-load-private-info)))
  :bind (("C-c t l" . load-twitter-credentials)
         ("C-c t t" . twit)))

;;; * uniquify
(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'forward)
    (setq uniquify-strip-common-suffix nil)))

;;; * undo-tree
(use-package undo-tree
  :config
  (progn
    (global-undo-tree-mode)))
;;; * paredit
(use-package paredit
  :config
  (progn
    (defun my-paredit-mode-hook
      ()
      (local-set-key (kbd "M-q") 'fill-paragraph))

    (add-hook 'paredit-mode-hook 'my-paredit-mode-hook)))

;;; * purescript
(use-package psc-ide
  :init
  (progn

    (eval-after-load 'flycheck
      '(flycheck-purescript-setup))

    (defun my-purescript-hook ()
      (psc-ide-mode)
      (company-mode)
      (flycheck-mode)
      (turn-on-purescript-indentation))

    (add-hook 'purescript-mode-hook 'my-purescript-hook)
    ))

;;; * whitespace-mode
(use-package whitespace
  :init
  (progn
    (setq whitespace-style '(face tabs trailing ;; lines-tail
                                  space-before-tab
                                 newline indentation empty space-after-tab
                                 tab-mark ;; newline-mark
                                 ))

    (defun my-whitespace-mode-hook
        ()
      (set-face-background 'whitespace-trailing "yellow"))

    (global-whitespace-mode)

    (add-hook 'whitespace-mode-hook 'my-whitespace-mode-hook))
  )


;;; * wgrep
(use-package wgrep)
;;; * end of file
(provide 'config)

;; Local Variables:
;; eval: (orgstruct-mode 1)
;; eval: (org-global-cycle)
;; orgstruct-heading-prefix-regexp: ";;; "
;; End:




