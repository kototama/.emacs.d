(require 'use-package)

(defvar user-init-files (directory-files "~/.emacs.d/lisp/" t "^init-.*el$")
  "a list of files containing the packages configuration")

(dolist (file user-init-files)
  (load-file file))

(provide 'config)
