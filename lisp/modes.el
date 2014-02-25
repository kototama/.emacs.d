(defvar user-active-modes
  '(ido-mode
    winner-mode
    show-parens-mode))

(dolist (m user-active-modes)
  (funcall m t))

(provide 'modes)
