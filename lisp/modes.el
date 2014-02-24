(defvar user-active-modes
  '(ido-mode
    winner-mode))

(dolist (m user-active-modes)
  (funcall m t))

(provide 'modes)
