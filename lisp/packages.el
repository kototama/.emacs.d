(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

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
                 auto-async-byte-compile
                 auto-indent-mode
                 cider
                 clojure-mode
                 color-theme
                 coffee-mode
                 elisp-slime-nav
                 ido-ubiquitous
                 haskell-mode
                 magit
                 magit
                 markdown-mode
                 org
                 org-plus-contrib
                 paredit
                 s
                 smex
                 use-package
                 visual-regexp
                 )))

(define-common-packages)

(provide 'packages)
