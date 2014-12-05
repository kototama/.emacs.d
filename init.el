;; enables the use of packages
(require 'package)

;; loads user defined functions to manage packages
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.eroles/common")

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

;; setup keyboard accents
(require 'iso-transl)

;; adds the user/machine specific configuration
(load-role-file "init.el")

;; starts the emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

