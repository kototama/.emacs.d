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
(add-to-list 'load-path "~/.emacs.d/emacs-modes/slime")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/slime/contrib")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/misc")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/yasnippet")
(add-to-list 'load-path "~/.emacs.d/emacs-modes/emacs-oauth")
(add-to-list 'load-path "~/.emacs.d/elisp")

;; semantic.el experiments
(load "~/.emacs.d/emacs-modes/cedet/cedet-devel-load.el")
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
(autoload 'mark-more-like-this "mark-more-like-this" "mark-more-like-this" t)
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

(ido-mode t)
(global-undo-tree-mode t)
(highlight-parentheses-mode t)
(show-paren-mode t)
(winner-mode t)
(column-number-mode t)
;; (global-whitespace-mode t)
(ido-ubiquitous-mode 1)

;; personal configurations
(require 'setup-colors)
(require 'setup-anything)
(require 'key-bindings)
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

(setq uniquify-buffer-name-style 'forward)

(smex-initialize)

(yas/initialize)
(yas/load-directory "~/.emacs.d/emacs-modes/yasnippet/snippets")

(add-hook 'window-setup-hook 'maximize-frame t)

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "<C-return>") nil)
             (define-key org-mode-map (kbd "<C-tab>") nil)
             (define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
             (define-key org-mode-map (kbd "<backtab>") nil)))

;; term setting
(add-hook 'multi-term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 2000)
            (setq term-bind-key-alist (delete '("M-o" . term-send-backspace)
                                              term-bind-key-alist))
            (setq term-bind-key-alist (delete '("C-p" . previous-line)
                                              term-bind-key-alist))))

;; starts emacs server
(server-start)
