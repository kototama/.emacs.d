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


;; Fix annoying lsp-ui color for errors.
;; Found with M-x list-faces-display
;; Found a new color M-x list-colors-display
(set-face-attribute 'error nil
                    :foreground "gold"
                    ;; :weight 'bold
                    )

(set-face-attribute 'success nil
                    :foreground "spring green"
                    ;; :weight 'bold
                    )

;; (set-face-attribute 'compilation-line-number nil
;;                     :foreground "gold"
;;                     ;; :weight 'bold
;;                     )

(provide 'appearance)
