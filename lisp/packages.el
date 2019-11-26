(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ;; ("elpy" . "http://jorgenschaefer.github.io/packages/")
        ("emacs-pe" . "https://emacs-pe.github.io/packages/")
        ))

;; loads packages used during the configuration
(add-to-list 'load-path "~/.emacs.d/site-lisp/dash.el/")
(load-file "~/.emacs.d/site-lisp/package/package+.el")

(require 'dash)

(defvar user-packages nil "The list of external packages used by the user.")

(defun add-packages
  (pkgs)
  "Add packages to the list of `user-packages`"
  (setq user-packages (-union user-packages pkgs)))

(defun load-role-file
  (name)
  "Loads a role file. Role files contain user and machine
specific configurations and are store in
~/.eroles/LOGIN@HOSTNAME/ "
  (let ((pathname (concat "~/.eroles/" user-login-name "@" system-name "/" name)))
    (when (file-exists-p pathname)
      (load-file pathname))))

(defun install-user-packages
  ()
  "Installs the user packages."
  (interactive)
  ;; adds the user/machine specific packages
  (load-role-file "setup.el")
  (define-common-packages)
  (apply #'package-manifest user-packages))

(defun define-common-packages
  ()
  (interactive)
  (add-packages '(
                  ag
                  ace-jump-mode
                  auto-async-byte-compile
                  auto-indent-mode
                  browse-kill-ring
                  clojure-mode
                  ;; clojure-test-mode
                  color-theme
                  company
                  dired-open
                  duplicate-thing
                  elisp-slime-nav
                  elixir-mode
                  expand-region
                  flycheck
                  flycheck-haskell
                  flycheck-purescript
                  ;; flycheck-hdevtools
                  haskell-mode
                  ido-completing-read+
                  imenu
                  intero
                  magit
                  markdown-mode
                  multiple-cursors
                  monokai-theme
                  org
                  org-bullets
                  org-plus-contrib
                  paredit
                  projectile
                  psc-ide
                  purescript-mode
                  s
                  smartparens
                  smex
                  undo-tree
                  visual-regexp
                  whitespace-cleanup-mode
                  wgrep-ag
                  yaml-mode
                  )))

(define-common-packages)

(provide 'packages)
