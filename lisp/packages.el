(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; loads packages used during the configuration
(load-file "~/.emacs.d/site-lisp/dash.el/dash.el")
(load-file "~/.emacs.d/site-lisp/package/package+.el")

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
  (apply #'package-manifest user-packages))

;; adds common packages
(add-packages '(
                auto-async-byte-compile
                auto-indent-mode
                cider
                clojure-mode
                color-theme
                elisp-slime-nav
                ido-ubiquitous
                magit
                magit
                markdown-mode
                org
                paredit
                smex
                use-package
                ))

(provide 'packages)
