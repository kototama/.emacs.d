(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))

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
                  ack ;; for wgrep-ack
                  ack-menu
		  ace-jump-mode
                  elfeed
                  auto-async-byte-compile
                  auto-indent-mode
                  clojure-mode
                  ;; clojure-test-mode
                  color-theme
                  company
                  dired+
                  dired-open
                  elisp-slime-nav
                  etags-select
                  flycheck
                  flycheck-haskell
                  ;; flycheck-hdevtools
                  haskell-mode
                  ido-ubiquitous
                  imenu
                  magit
                  markdown-mode
                  multiple-cursors
                  org
                  org-bullets
                  org-plus-contrib
                  paredit
                  projectile
                  s
                  smex
                  visual-regexp
                  whitespace-cleanup-mode
                  wgrep-ack
                  yaml-mode
                  )))

(define-common-packages)

(provide 'packages)
