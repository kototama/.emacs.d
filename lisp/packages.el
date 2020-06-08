(require 'dash)

(defvar user-packages nil "The list of external packages used by the user.")

(defun add-packages
  (pkgs)
  "Add packages to the list of `user-packages`"
  (setq packages-pinned-packages pkgs)
  (setq user-packages (-union user-packages (-map #'car pkgs))))

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
                  (ag . "melpa-stable")
                  (ace-jump-mode . "melpa-stable")
                  (auto-indent-mode . "melpa-stable")
                  (browse-kill-ring . "melpa-stable")
                  (clojure-mode . "melpa-stable")
                  (company . "melpa-stable")
                  ;;                  duplicate-thing
                  (elisp-slime-nav . "melpa-stable")
                  (elixir-mode . "melpa-stable")
                  (expand-region . "melpa-stable")
                  (flycheck . "melpa-stable")
                  (flycheck-haskell . "melpa-stable")
                  (haskell-mode . "melpa-stable")
                  (ido-completing-read+ . "melpa-stable")
                  (imenu . "melpa-stable")
                  (intero . "melpa-stable")
                  (magit . "melpa-stable")
                  (markdown-mode . "melpa-stable")
                  (multiple-cursors . "melpa-stable")
                  (monokai-theme . "melpa-stable")
                  (org . "melpa-stable")
                  (org-bullets . "melpa-stable")
                  (org-plus-contrib . "melpa-stable")
                  (paredit . "melpa-stable")
                  (projectile . "melpa-stable")
                  (s . "melpa-stable")
                  (smartparens . "melpa-stable")
                  (smex . "melpa-stable")
                  ;;                  undo-tree
                  (visual-regexp . "melpa-stable")
                  (whitespace-cleanup-mode . "melpa-stable")
                  (wgrep-ag . "melpa-stable")
                  (yaml-mode . "melpa-stable")
                  )))

(define-common-packages)

(provide 'packages)
