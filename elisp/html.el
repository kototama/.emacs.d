(setq auto-mode-alist (cons '("\\.html$" . nxml-mode) auto-mode-alist))

(add-hook 'nxml-mode-hook
          '(lambda ()
             (define-key nxml-mode-map
               (kbd "<C-return>") '(lambda ()
                                     (interactive)
                                     (switch-to-buffer nil)))))