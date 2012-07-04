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

;; loads personal emacs functions and configurations
(load "dev")
(load "colors")
(load "lisp")
(load "javascript")
(load "html")
(load "smartbeol")

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

(require 'undo-tree)
(require 'paren)
(require 'maxframe)
(require 'smex)
(require 'uniquify)
(require 'yasnippet)
(require 'whitespace)
(require 'ido)
(require 'ido-ubiquitous)

;; personal configurations
(require 'setup-anything)
(require 'key-bindings)
(require 'sane-defaults)
(require 'setup-hippie)

(setq uniquify-buffer-name-style 'forward)

(smex-initialize)

(yas/initialize)
(yas/load-directory "~/.emacs.d/emacs-modes/yasnippet/snippets")

;; always uses the following modes
(ido-mode t)
(global-undo-tree-mode t)
(highlight-parentheses-mode t)
(show-paren-mode t)
(winner-mode t)
(column-number-mode t)
(global-whitespace-mode t)
(ido-ubiquitous-mode 1)

;; emacs lisp functions
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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

