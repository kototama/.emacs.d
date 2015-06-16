(defvar user-active-modes
  '(
    winner-mode
    show-paren-mode
    column-number-mode
    delete-selection-mode
    ))

(dolist (m user-active-modes)
  (funcall m t))

(provide 'modes)
