(use-package js
  :init
  (progn
    (use-package smartparens)
    (use-package tern)
    (use-package tern-auto-complete)
    (use-package flycheck)
    (use-package flycheck-color-mode-line)

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
      (flycheck-define-checker javascript-jslint-reporter
                               "A JavaScript syntax and style checker based on JSLint Reporter.

 See URL `https://github.com/FND/jslint-reporter'."
                               :command ("~/local/opt/jslint-reporter/jslint-reporter" source)
                               :error-patterns
                               ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
                               :modes (js-mode js2-mode js3-mode))

      (smartparens-mode)
      (tern-mode)
      (tern-ac-setup)
      (bind-key "<return>" 'my-return-and-indent js-mode-map)
      ;; (flymake-jslint-load)
      (flycheck-select-checker
       'javascript-jslint-reporter)
      (flycheck-mode))

    (add-hook 'js-mode-hook 'js-greek-lambda)
    (add-hook 'js-mode-hook 'my-js-mode-hook)


    )
  )

(provide 'setup-javascript)
