
;; js2-mode configuration
(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js2-mode-map (kbd "C-a")
              'back-to-indentation-or-beginning-of-line)
            (define-key js2-mode-map (kbd "<return>")
              (lambda ()
                (interactive)
                (js2-enter-key)
                (indent-for-tab-command)))))

(provide 'setup-javascript)
