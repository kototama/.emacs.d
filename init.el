(defvar *emacs-load-start* (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET
;; (setq wisent-debug-flag t)
;; (load "~/.emacs.d/elisp/setup-cedet.el")
;; (speedbar-add-supported-extension ".clj")
;; (speedbar-add-supported-extension ".cljs")
;; (add-hook 'wisent-grammar-mode-hook
;;           '(lambda ()
;;              (semantic-mode t)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq at-work (string-match "elan" system-name))

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repos
;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   (package-initialize)
;;   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-paths
(add-to-list 'load-path "~/.emacs.d/site-lisp/misc")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-ctable")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-octopress")
(add-to-list 'load-path "~/.emacs.d/site-lisp/orglue")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/utils")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'setup-elget)

;; for org-much
(add-to-list 'load-path "~/.emacs.d/el-get/org-mode/contrib/lisp")

(dolist (pconf '(setup-lisp
                 setup-javascript
		 ktm-mode
		 sane-defaults))
  (require pconf))

(when at-work
  (require 'setup-carneades))

;; always uses the following modes
(dolist (mode '(diminish
                undo-tree
                paren
                maxframe
                uniquify
                yasnippet
                ido
                ido-ubiquitous
                ;; hippie-exp
                color-theme-kototama
                auto-complete-config
                flx-ido))
  (require mode))

(color-theme-kototama)
(ido-mode t)
(global-undo-tree-mode t)
(show-paren-mode t)
(winner-mode t)
(column-number-mode t)
(ido-ubiquitous-mode 1)
(ac-config-default)
;; (savehist-mode 1)
;; load keybindings
(ktm-global-mode 1)
(auto-indent-global-mode 1)

(dolist (mode '(undo-tree-mode
                paredit-mode
                ktm-mode))
  (diminish mode))

(setq whitespace-style '(face tabs trailing lines empty))

(add-hook 'before-save-hook 'whitespace-cleanup)

;; fonts
(when (eq window-system 'x)
  (set-frame-font "Inconsolata-13")
  (add-to-list 'default-frame-alist '(font . "Inconsolata-13")))

;; files extensions associations
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;; starts emacs server
(server-start)

;; window
(add-hook 'window-setup-hook 'maximize-frame t)


(defun anarcat/time-to-ms (time)
  (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000) (car
  (cdr (cdr time)))))

(defun anarcat/display-timing ()
  (message ".emacs loaded in %fms" (/ (- (anarcat/time-to-ms
  (current-time)) (anarcat/time-to-ms *emacs-load-start*))
  1000000.0)))

(add-hook 'after-init-hook 'anarcat/display-timing t)
