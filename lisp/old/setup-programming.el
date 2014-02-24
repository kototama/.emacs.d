(defun send-return
  ()
  (interactive)
  (term-send-raw-string "\n"))

;; C programming
(setq-default c-basic-offset 4)

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (define-key flyspell-mode-map (kbd "C-;") nil)))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(provide 'setup-programming)
