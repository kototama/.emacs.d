(use-package cider-repl
  :init
  
  (defun my-cider-mode-hook
    ()
    (paredit-mode t)
    ;; (bind-key "<S-return>" 'cider-repl-return cider-mode-map)
    )

  (add-hook 'cider-repl-mode-hook 'my-cider-mode-hook))
