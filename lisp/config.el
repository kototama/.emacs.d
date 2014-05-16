;;; * beginning of file

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package/")
(require 'use-package)


;; (defvar user-init-files (directory-files "~/.emacs.d/lisp/" t "^init-.*el$")
;;   "a list of files containing the packages configuration")

;; (dolist (file user-init-files)
;;   (load-file file))


;;; * cider

(use-package cider-repl
  :init
  
  (defun my-cider-mode-hook
    ()
    (paredit-mode t)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-popup-stacktraces t)
    (setq cider-repl-popup-stacktraces t)
    ;; (bind-key "<S-return>" 'cider-repl-return cider-mode-map)
    )

  (add-hook 'cider-repl-mode-hook 'my-cider-mode-hook))

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
    
    (defun my-clojure-mode-hook
      ()
      (paredit-mode t)
      (auto-indent-mode t)
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
;;; * flycheck
(use-package flycheck
  :config
  (progn
    (use-package flycheck-hdevtools)))
;;; * haskell
(use-package haskell-mode
  :config
  (progn
    ;; (use-package shm
    ;;   :load-path "site-lisp/structured-haskell-mode/elisp"
    ;;   :config
    ;;   (progn
    ;;     ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    ;;     (setq exec-path
    ;;           (append exec-path
    ;;                   '(concat user-emacs-directory "site-lisp/structured-haskell-mode/.cabal-sandbox/bin")))))
    
    ;; (use-package ghc
    ;;   :load-path "site-lisp/ghc-mod/elisp"
    ;;   :config
    ;;   (progn
    ;;     (setq ghc-module-command
    ;;           (expand-file-name "~/.emacs.d/site-lisp/ghc-mod/.cabal-sandbox/bin/ghc-mod"))
    ;;     (setq ghc-check-command
    ;;           (expand-file-name "~/.emacs.d/site-lisp/ghc-mod/.cabal-sandbox/bin/ghc-mod"))
    ;;     (add-hook 'haskell-mode-hook 'ghc-init)))

    (use-package hdevtools
      :load-path "site-lisp/hdevtools-emacs")
    
    (defun my-haskell-mode-hook
      ()
      (turn-on-haskell-indentation)
      (turn-on-haskell-doc-mode)
      (flycheck-mode)
      (flycheck-select-checkers 'haskell-hdevtools)
      (local-set-key (kbd "M-p") nil)
      (local-set-key (kbd "M-n") nil)
      (local-set-key (kbd "C-c C-t") 'hdevtools/show-type-info)
      (local-set-key (kbd "C-c C-k") 'recompile)      
      )

    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
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

;;; * javascript
(use-package js-mode
  :config
  (add-hook 'js-mode-hook 'auto-indent-mode))

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

      (use-package auto-indent
        :config
        (auto-indent-mode t))
      
      )

    (defun my-minibuffer-mode-hook
      ()
      ;; (paredit-mode 1)
      )

    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
    (add-hook 'minibuffer-setup-hook 'my-minibuffer-mode-hook)))

;;; * magit

(use-package magit
  :bind (("C-c g s" . magit-status)
         ("C-c g l" . magit-file-log)
         ("C-c g L" . magit-log)))

;;; * multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

;;; * org

(use-package org
  :config
  (progn
    (setq org-reverse-note-order t)

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
      (setq org-refile-targets '((nil :maxlevel . 2)))
      (setq org-use-speed-commands t)
      (setq org-archive-location "::* Archived Tasks")
      (setq org-log-done 'time))

    (add-hook 'org-mode-hook 'my-common-org-mode-hook))
  :bind (("C-c o a" . org-agenda)
         ("C-c o o" . org-open-at-point)
         ("C-c o l" . org-store-link)
         ("C-c o L" . org-insert-link)))

;;; * org-capture

(use-package org-capture
  :bind (("C-c o r" . org-capture)))

;;; * org-sync
(use-package org-element
  :config
  (use-package os
    :load-path "site-lisp/org-sync"
    :config 
    (progn
      (mapc 'load
            '("org-element" "os" "os-bb" "os-github" "os-rmine"))))) 

;;; * paredit
(use-package paredit
  :config
  (progn
    (defun my-paredit-mode-hook
      ()
      (local-set-key (kbd "M-q") 'fill-paragraph))

    (add-hook 'paredit-mode-hook 'my-paredit-mode-hook)))
;;; * smex

(use-package smex
  :demand t
  :bind (("M-x" . smex)))

;;; * show-paren-mode

(use-package paren
  :init
  (progn
    (set-face-background 'show-paren-match-face "purple")
    (set-face-foreground 'show-paren-match-face "black")))

;;; * uniquify
(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'forward)))
<<<<<<< HEAD
;;; * paredit
(use-package paredit
  :config
  (progn
    (defun my-paredit-mode-hook
      ()
      (local-set-key (kbd "M-q") 'fill-paragraph))

    (add-hook 'paredit-mode-hook 'my-paredit-mode-hook)))
;;; * end of file

(provide 'config)

;; Local Variables:
;; eval: (orgstruct-mode 1)
;; eval: (org-global-cycle)
;; orgstruct-heading-prefix-regexp: ";;; "
;; End:




