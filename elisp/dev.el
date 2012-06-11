(require 'magit)

(add-hook 'magit-log-edit-mode-hook
          '(lambda ()
             (flyspell-mode t)))

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;; C programming
(setq-default c-basic-offset 4)

