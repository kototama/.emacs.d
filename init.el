;; enables the use of packages
(require 'package)

;; loads user defined functions to manage packages
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'packages)

;; installs missing packages
;; (install-user-packages)

(package-initialize)

;;; loads the common settings

(require 'defaults)
(require 'appearance)

;; uses use-package to configure packages
(require 'use-package)
(require 'config)

;; activates default modes
(require 'modes)

;; setup global keybindings
(require 'keybindings)

;; adds the user/machine specific configuration
(load-role-file "init.el")

;; starts the emacs server
(condition-case nil
    (server-start)
  (error (message "Emacs server is already started.")))
