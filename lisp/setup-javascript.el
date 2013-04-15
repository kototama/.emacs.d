(use-package js
  :init
  (progn

    (defun js-greek-lambda ()
       (font-lock-add-keywords nil `(("\\<function\\>"
           (0 (progn (compose-region (match-beginning 0) (match-end 0)
           ,(make-char 'greek-iso8859-7 107))
           nil))))))

    (defun my-return-and-indent
      ()
      (interactive)
      (newline)
      (indent-according-to-mode))

    (defun my-js-mode-hook
      ()
      (bind-key "<return>" 'my-return-and-indent js-mode-map))

    (add-hook 'js-mode-hook 'js-greek-lambda)
    (add-hook 'js-mode-hook 'my-js-mode-hook)))

(provide 'setup-javascript)
