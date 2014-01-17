(use-package coffee-mode
  :init (progn
          (defun my-coffee-mode-hook
            ()
            (setq coffee-tab-width 2)
            (flycheck-mode)
            (smartparens-mode)
            (auto-indent-mode 0))

          (add-hook 'coffee-mode-hook 'my-coffee-mode-hook)))
