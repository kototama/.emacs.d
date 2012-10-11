(defun send-return
  ()
  (interactive)
  (term-send-raw-string "\n"))

(add-hook 'term-mode-hook
          '(lambda ()
             (setq term-unbind-key-list '("<return>"))
             ;; (add-to-list term-bind-key-alist '("<return>" . term-send-return))
             (add-to-list 'term-bind-key-alist '("<return>" . send-return))
             ))

(provide 'setup-programming)
