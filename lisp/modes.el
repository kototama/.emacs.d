(defvar user-active-modes
  '(
    ido-mode
    winner-mode
    show-paren-mode
    column-number-mode
    delete-selection-mode
    global-company-mode
    ace-jump-mode
    ))

(dolist (m user-active-modes)
  (funcall m t))

(provide 'modes)
