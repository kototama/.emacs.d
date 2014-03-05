(setq user-key-bindings
      '(("C-S-j" . join-line)
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph)))

(defun install-keybindings
  ()
  "Installs the key bindings defined in user-bindings"
  (dolist (binding user-key-bindings)
    (global-set-key (kbd (car binding)) (cdr binding))))

(install-keybindings)

(provide 'keybindings)
