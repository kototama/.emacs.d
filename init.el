;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET
(setq wisent-debug-flag t)
(load "~/.emacs.d/elisp/setup-cedet.el")
(speedbar-add-supported-extension ".clj")
(speedbar-add-supported-extension ".cljs")
(add-hook 'wisent-grammar-mode-hook
          '(lambda ()
             (semantic-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repos
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-paths
(add-to-list 'load-path "~/.emacs.d/site-lisp/misc")
;; (add-to-list 'load-path "~/.emacs.d/emacs-modes/yasnippet")
;; (add-to-list 'load-path "~/.emacs.d/emacs-modes/nrepl.el")
;; (add-to-list 'load-path "~/.emacs.d/emacs-modes/helm")

(add-to-list 'load-path "~/.emacs.d/elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When the env. variable UPDATE_EMACS_PACKAGEs is defined, packages
;; defined in ~/.emacs.d/package-spec.el
;; will be automatically downloaded.

(when (getenv "UPDATE_EMACS_PACKAGES")
  (require 'package-spec))

(autoload 'expand-region "expand-region" "expand region" t)
(autoload 'multi-term "multi-term" "multiple terms" t)
(autoload 'term-send-raw-string "multi-term" "multiple terms" t)
(autoload 'hippie-expand "hippie-expand" "expand stuff" t)

;; always uses the following modes
(dolist (mode '(undo-tree paren maxframe uniquify yasnippet ido
                          ido-ubiquitous hippie-exp color-theme-kototama
                          auto-complete-config))
  (require mode))

(color-theme-kototama)
(ido-mode t)
(global-undo-tree-mode t)
(highlight-parentheses-mode t)
(show-paren-mode t)
(winner-mode t)
(column-number-mode t)
(ido-ubiquitous-mode 1)

(ac-config-default)

;; load personal configuration 

;; personal configurations


(require 'setup-elget)
(add-to-list 'load-path "~/.emacs.d/el-get/org-mode/contrib/lisp")

(dolist (pconf '(setup-helm sane-defaults setup-hippie
                            setup-lisp
                            setup-carneades line-utils screen-utils file-utils
                            setup-programming ktm-mode setup-org
                            setup-notmuch
                            ;; setup-ldap
                            ))
  (require pconf))

;; load keybindings
(ktm-global-mode 1)

;; (smex-initialize)

(yas/initialize)

;; fonts
(when (eq window-system 'x)
  (set-frame-font "Inconsolata-13") 
  (add-to-list 'default-frame-alist '(font . "Inconsolata-13")))

;; window
(add-hook 'window-setup-hook 'maximize-frame t)

;; files extensions associations
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;; starts emacs server
(server-start)
