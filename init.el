;; number of bytes of consing before a garbage collection is invoked
;; 100 mega bytes
(setq gc-cons-threshold (* 100 1024 1024))

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/lisp")

  ;; loads package configurations
  (require 'config)

  ;; setup keyboard accents
  (require 'iso-transl)

  ;; loads files lib
  (require 'files-util)

  ;; starts the emacs server
  (require 'server)
  (unless (server-running-p)
    (server-start)))
