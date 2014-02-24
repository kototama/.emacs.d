(use-package lisp-mode
  :config
  (progn
    (use-package paredit)
    (use-package elisp-slime-nav)

    (use-package auto-async-byte-compile
      :config
      (progn
        (setq auto-async-byte-compile-suppress-warnings t)
        (add-hook 'emacs-lisp-mode-hook
                  'enable-auto-async-byte-compile-mode)))
    
    (defun my-emacs-lisp-mode-hook
      ()
      (paredit-mode t)
      (auto-indent-mode t)
      (elisp-slime-nav-mode t))

    (defun my-minibuffer-mode-hook
      ()
      (paredit-mode 1))

    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
    (add-hook 'minibuffer-setup-hook 'my-minibuffer-mode-hook)))

(provide 'init-lisp-mode)
