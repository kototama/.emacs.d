;; enables the use of packages
(require 'package)

;; loads user defined functions to manage packages
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'packages)

;;; initialize ELPA packages

(package-initialize)

;;; loads the common settings

(require 'defaults)
(require 'appearance)

;; loads package configurations
(require 'config)

;; activates default modes
(require 'modes)

;; setup global keybindings
(require 'keybindings)

;; adds the user/machine specific configuration
(load-role-file "init.el")

;; starts the emacs server
(unless (server-running-p)
  (server-start))
