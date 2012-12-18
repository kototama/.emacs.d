(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))

;; path for the modes that are not part of package
(add-to-list 'load-path "~/.emacs.d/emacs-modes/misc")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/yasnippet")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/nrepl.el")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/helm")
(add-to-list 'load-path "~/.emacs.d/elisp")

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
(dolist (mode '(undo-tree paren maxframe smex uniquify yasnippet ido
                          ido-ubiquitous hippie-exp color-theme-kototama))
  (require mode))

(color-theme-kototama)
(ido-mode t)
(global-undo-tree-mode t)
(highlight-parentheses-mode t)
(show-paren-mode t)
(winner-mode t)
(column-number-mode t)
;; (global-whitespace-mode t)
(ido-ubiquitous-mode 1)

;; load personal configuration
;; personal configurations
(dolist (pconf '(setup-helm sane-defaults setup-hippie setup-javascript
                            setup-lisp setup-carneades line-utils screen-utils file-utils
                            setup-programming ktm-mode setup-org setup-cedet))
  (require pconf))

(ktm-global-mode 1)

(smex-initialize)

(yas/initialize)

;; fonts
(if (eq window-system 'x)
    (progn
      (set-frame-font "Inconsolata-13")))

(add-to-list 'default-frame-alist '(font . "Inconsolata-13"))

;; window
(add-hook 'window-setup-hook 'maximize-frame t)

;; starts emacs server, if not already started
(server-start)
