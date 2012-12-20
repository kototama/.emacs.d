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
(autoload 'magit "magit-mode" "A major mode for Git" t)

(add-hook 'magit-log-edit-mode-hook
          '(lambda ()
             (flyspell-mode t)))

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;; C programming
(setq-default c-basic-offset 4)

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (define-key flyspell-mode-map (kbd "C-;") nil)))

(provide 'setup-programming)
