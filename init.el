;; semantic.el experiments
(load "~/.emacs.d/emacs-modes/cedet/cedet-devel-load.el")

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    ;; for Emacs23:
    ;; (load
    ;;  (expand-file-name "~/.emacs.d/emacs-modes/misc/package.el"))
    (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                             ("marmalade" . "http://marmalade-repo.org/packages/")
                             ))
  (package-initialize))

;; path for the modes that are not part of package
;; (add-to-list 'load-path "~/.emacs.d/emacs-modes/slime")
;; (add-to-list 'load-path "~/.emacs.d/emacs-modes/slime/contrib")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/misc")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/yasnippet")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/nrepl.el")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/helm")
(add-to-list 'load-path "~/.emacs.d/elisp")

(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)


;; fonts
(if (eq window-system 'x)
    (progn
      (set-frame-font "Inconsolata-13")))

(add-to-list 'default-frame-alist '(font . "Inconsolata-13"))

;; first loads package-spec.el
;; This will install any packages defined in
;; ~/.emacs.d/package-spec.el
(require 'package-spec)

(autoload 'expand-region "expand-region" "expand region" t)
(autoload 'igrep "igrep" "a better grep" t)
(autoload 'multi-term "multi-term" "multiple terms" t)
(autoload 'term-send-raw-string "multi-term" "multiple terms" t)
(autoload 'hippie-expand "hippie-expand" "expand stuff" t)

;; always uses the following modes
(require 'undo-tree)
(require 'paren)
(require 'maxframe)
(require 'smex)
(require 'uniquify)
(require 'yasnippet)
;; (require 'whitespace)
(require 'ido)
(require 'ido-ubiquitous)
(require 'hippie-exp)
(require 'color-theme-kototama)

(color-theme-kototama)
(ido-mode t)
(global-undo-tree-mode t)
(highlight-parentheses-mode t)
(show-paren-mode t)
(winner-mode t)
(column-number-mode t)
;; (global-whitespace-mode t)
(ido-ubiquitous-mode 1)

;; personal configurations
;; (require 'setup-colors)
(require 'setup-helm)
(require 'sane-defaults)
(require 'setup-hippie)
(require 'setup-dev)
(require 'setup-javascript)
(require 'setup-lisp)
(require 'setup-carneades)
(require 'line-utils)
(require 'screen-utils)
(require 'file-utils)
(require 'string-utils)
(require 'setup-programming)
(require 'ktm-mode)
(require 'setup-org)

(ktm-global-mode 1)

(smex-initialize)

(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/backbone-underscore-snippets")

(add-hook 'window-setup-hook 'maximize-frame t)

;; starts emacs server, if not already started
(server-start)
