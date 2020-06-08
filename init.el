;; number of bytes of consing before a garbage collection is invoked
;; 100 mega bytes
(setq gc-cons-threshold (* 100 1024 1024))

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/lisp")

  ;; loads the common settings
  (require 'defaults)
  (require 'appearance)
  (require 'layout)

  ;; loads package configurations
  (require 'config)

  ;; activates default modes
  (require 'modes)

  ;; setup global keybindings
  (require 'keybindings)

  ;; setup keyboard accents
  (require 'iso-transl)

  ;; adds the user/machine specific configuration
  ;; (load-role-file "init.el")

  ;; loads files lib
  (require 'files)

  ;; starts the emacs server
  (require 'server)
  (unless (server-running-p)
    (server-start)))
