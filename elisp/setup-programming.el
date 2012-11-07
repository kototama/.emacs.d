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


;; term setting
(add-hook 'multi-term-mode-hook
          (lambda ()
            (define-key term-mode-map (kbd "<return>") 'term-send-raw)
            (setq term-buffer-maximum-size 2000)
            (setq term-bind-key-alist (delete '("M-o" . term-send-backspace)
                                              term-bind-key-alist))
            (setq term-bind-key-alist (delete '("C-p" . previous-line)
                                              term-bind-key-alist))))


(provide 'setup-programming)
