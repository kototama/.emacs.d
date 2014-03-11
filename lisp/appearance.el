;; installs inconsolate fonts if presents
(when (eq window-system 'x)
  (condition-case nil
      (progn
        (set-frame-font "Inconsolata-13")
        (add-to-list 'default-frame-alist '(font . "Inconsolata-13")))
    (error (message "Fonts Inconsolata can not be found. Please do 'sudo apt-get install ttf-inconsolata'."))))

(provide 'appearance)
