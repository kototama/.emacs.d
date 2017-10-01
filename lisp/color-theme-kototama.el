(require 'color-theme)

(defun color-theme-kototama ()
  "Black background theme with coral/fushia/blueish colors."
  (interactive)
  (color-theme-install
    '(color-theme-kototama
      ((foreground-color . "wheat")
       (background-color . "black")
       ;; (cursor-color . "deep sky blue")
       (cursor-color . "red")
       (background-mode . dark)
       (mode-line ((t (:foreground "black" :background "light slate gray"))))) 
      (flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
      (flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
      (font-lock-comment-face ((t (:foreground "SteelBlue1"))))
      (font-lock-function-name-face ((t (:foreground "gold"))))
      (font-lock-type-face ((t (:foreground "PaleGreen"))))
      (font-lock-variable-name-face ((t (:foreground "plum"))))
      (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
      ;; (font-lock-string-face ((t (:foreground "light salmon"))))
      (font-lock-string-face ((t (:foreground "orange red"))))
      ;; (font-lock-string-face ((t (:foreground "VioletRed1"))))
      (mode-line ((t (:foreground "black" :background "light slate gray"))))
      (region ((t (:foreground nil :background "#555555"))))
      (isearch ((t (:foreground "black" :background "cyan"))))
      ;; (font-lock-keyword-face ((t (:foreground "DodgerBlue1"))))
      (font-lock-keyword-face ((t (:foreground "mediumpurple1"))))
      ))
  )

(provide 'color-theme-kototama)

