(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/site-lisp/structured-haskell-mode/elisp")

(use-package haskell-mode
  :init (progn
          (use-package shm)
          (add-hook 'haskell-mode-hook 'structured-haskell-mode)))
