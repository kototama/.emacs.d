(use-package coffee-mode
  :init (progn
          (defun my-coffee-mode-hook
            ()
            (setq whitespace-action '(auto-cleanup))
            (setq whitespace-style '(trailing space-before-tab indentation empty
                                              space-after-tab))
            (custom-set-variables '(coffee-tab-width 2))
            (flycheck-mode))

          (add-hook 'coffee-mode-hook 'my-coffee-mode-hook)))
