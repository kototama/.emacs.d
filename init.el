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
(add-to-list 'load-path "~/.emacs.d/elisp")

;; fonts
(if (eq window-system 'x)
    (progn
      (set-frame-font "Inconsolata-13")))

(add-to-list 'default-frame-alist '(font . "Inconsolata-13"))


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
(require 'whitespace)
(require 'ido)
(require 'ido-ubiquitous)

(ido-mode t)
(global-undo-tree-mode t)
(highlight-parentheses-mode t)
(show-paren-mode t)
(winner-mode t)
(column-number-mode t)
(global-whitespace-mode t)
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
(require 'line-utils)
(require 'screen-utils)
(require 'file-utils)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/Projects/carneades/TODO.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "wheat" :background "black"))))
 '(flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
 '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-face ((t (:foreground "SteelBlue1"))))
 '(font-lock-function-name-face ((t (:foreground "gold"))))
 '(font-lock-keyword-face ((t (:foreground "springgreen"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "plum"))))
 '(menu ((((type x-toolkit)) (:background "light slate gray" :foreground "wheat" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(mode-line ((t (:foreground "black" :background "light slate gray"))))
 '(slime-repl-result-face ((t (:foreground "orange"))))
 '(tool-bar ((((type x w32 mac) (class color)) (:background "midnight blue" :foreground "wheat" :box (:line-width 1 :style released-button))))))
