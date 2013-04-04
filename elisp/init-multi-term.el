(add-hook 'term-mode-hook
          '(lambda ()
             (ktm-mode nil) 
             (term-line-mode)
             (define-key term-mode-map (kbd "<return>") 'term-send-raw)
             (setq term-buffer-maximum-size 2000)
             (setq term-default-bg-color "black")
             (setq term-default-fg-color "gray")
             (setq term-bind-key-alist (delete '("M-o" . term-send-backspace)
                                               term-bind-key-alist))
             (setq term-bind-key-alist (delete '("C-p" . previous-line)
                                               term-bind-key-alist))))

