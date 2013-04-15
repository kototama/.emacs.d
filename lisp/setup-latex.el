(require 'flymake)

(defun flymake-get-tex-args (file-name) (list "pdflatex"
    (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)

(provide 'setup-latex)
