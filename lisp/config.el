;;; * beginning of file

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
    ;; (bind-key "<S-return>" 'cider-repl-return cider-mode-map)
    )

  (add-hook 'cider-repl-mode-hook 'my-cider-mode-hook))

;;; * clojure
(use-package clojure-mode
  :config
  (progn
    (defun my-clojure-mode-hook
      ()
      (paredit-mode t))

    (add-hook 'clojure-mode-hook 'my-clojure-mode-hook)))
;;; * coffee

(use-package coffee-mode
  :config
  (progn
    (setq coffee-tab-width 2)))

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
;;; * ido

(use-package ido-mode
  :init
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (setq ido-use-virtual-buffers t)
    (setq ido-ignore-extensions t)
    (setq ido-default-buffer-method #'selected-window)

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

;;; * lisp

(use-package lisp-mode
  :config
  (progn
    (use-package paredit)
    (use-package elisp-slime-nav)

    (use-package auto-async-byte-compile
      :config
      (progn
        (setq auto-async-byte-compile-suppress-warnings t)
        (add-hook 'emacs-lisp-mode-hook
                  'enable-auto-async-byte-compile-mode)))
    
    (defun my-emacs-lisp-mode-hook
      ()
      (paredit-mode t)
      (auto-indent-mode t)
      (elisp-slime-nav-mode t))

    (defun my-minibuffer-mode-hook
      ()
      ;; (paredit-mode 1)
      )

    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
    (add-hook 'minibuffer-setup-hook 'my-minibuffer-mode-hook)))

;;; * magit

(use-package magit
  :bind (("C-c g s" . magit-status)))

;;; * org

(use-package org
  :config
  (progn
    (defun my-common-org-mode-hook
      ()
      (setq org-use-speed-commands t))

    (add-hook 'org-mode-hook 'my-common-org-mode-hook))
  :bind (("C-c o a" . org-agenda)
         ("C-c o o" . org-open-at-point)
         ("C-c o l" . org-store-link)
         ("C-c o L" . org-insert-link)))

;;; * org-capture

(use-package org-capture
  :bind (("C-c o r" . org-capture)))

;;; * smex

(use-package smex
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
;;; * haskell
(use-package haskell-mode
  :config
  (progn
    (use-package shm
      :load-path "site-lisp/structured-haskell-mode/elisp"
      :config
      (progn
        (add-hook 'haskell-mode-hook 'structured-haskell-mode)
        (setq exec-path
              (append exec-path
                      '(concat user-emacs-directory "site-lisp/structured-haskell-mode/.cabal-sandbox/bin")))))

    (defun my-haskell-mode-hook
      ()
      (flycheck-mode))

    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
    
    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
    ))
;;; * end of file

(provide 'config)

;; Local Variables:
;; eval: (orgstruct-mode 1)
;; eval: (org-global-cycle)
;; orgstruct-heading-prefix-regexp: ";;; "
;; End:




