;; installs inconsolate fonts if presents
(when (eq window-system 'x)
  (condition-case nil
      (progn
        (if (file-exists-p "~/.config/nixpkgs/home.nix")
            (progn
              (set-frame-font "Inconsolata-15")
              (add-to-list 'default-frame-alist '(font . "Inconsolata-15")))
          (progn
            (set-frame-font "Inconsolata-15")
              (add-to-list 'default-frame-alist '(font . "Inconsolata-15"))
            )))
    (error (message "Fonts Inconsolata can not be found. Please do 'sudo apt-get install ttf-inconsolata'."))))

(provide 'appearance)
