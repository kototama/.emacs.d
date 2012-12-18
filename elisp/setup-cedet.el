(require 's)

(when (s-contains? "elan" system-name)
  ;; semantic.el experiments
  (load "~/.emacs.d/emacs-modes/cedet/cedet-devel-load.el")
  (eval-after-load "speedbar"
    (lambda ()
      (speedbar-add-supported-extension ".clj")
      (speedbar-add-supported-extension ".cljs")))

  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode))

(provide 'setup-cedet)
