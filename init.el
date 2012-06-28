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
(load "setup-anything")
(load "lisp")
(load "javascript")
(load "html")
(load "smartbeol")

;; disable blinking cursor
(blink-cursor-mode 0)

;; fonts
(if (eq window-system 'x)
    (progn
      (set-frame-font "Inconsolata-13")
      ;; (add-to-list 'default-frame-alist
      ;;              '(font . "Inconsolata-13"))
      ))

(add-to-list 'default-frame-alist '(font . "Inconsolata-13"))

;; save backup files in this directory
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

(require 'undo-tree)
(require 'paren)
;; (require 'package)
(require 'maxframe)
(require 'smex)
(require 'uniquify)
(require 'yasnippet)

(autoload 'expand-region "expand-region" "expand region" t)
(autoload 'mark-more-like-this "mark-more-like-this" "mark-more-like-this" t)
(autoload 'igrep "igrep" "a better grep" t)
(autoload 'multi-term "multi-term" "multiple terms" t)

(setq uniquify-buffer-name-style 'forward)

(smex-initialize)

(yas/initialize)
(yas/load-directory "~/.emacs.d/emacs-modes/yasnippet/snippets")

(menu-bar-mode 0)

;; ido-mode options
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-ignore-extensions t)
(setq ido-file-extensions-order
      '(".clj" "js" ".txt" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(setq ido-use-virtual-buffers t)

;; igrep options
(setq igrep-files-default 'ignore)
;; (put 'igrep-files-default 'clojure-mode
;;      (lambda () "*.clj\\|*.js")
;;      )
;; (put 'igrep-files-default 'js2-mode
;;      (lambda () "*.js"))

;; kill last and second line of igrep-find to allow next-error
(defadvice igrep-find
  (after kill-first-lines activate compile)
  (save-current-buffer
    (set-buffer "*igrep*")
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (forward-line -1)
    (kill-line)
    (goto-char (point-min))
    (forward-line)
    (kill-line)))


;; show paren options
(set-face-background 'show-paren-match-face "transparent")
(set-face-foreground 'show-paren-match-face "red")

(define-key undo-tree-map (kbd "C-?") nil)
(setq whitespace-style '(face tabs trailing lines))

;; always uses the following modes
(ido-mode t)
(global-undo-tree-mode t)
(highlight-parentheses-mode t)
(show-paren-mode t)
(winner-mode t)
(column-number-mode t)
(global-whitespace-mode t)

;; no toolbar
(tool-bar-mode -1)

;; no start screen
(setq inhibit-splash-screen t)

;; no tabs, spaces instead
(setq-default indent-tabs-mode nil)

;; changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; do not confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

;; integrate copy/paste with X
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; scroll
(setq scroll-step 1)

;; default shell to zsh
(setq multi-term-program "/bin/zsh")

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

;; global key bindings
(global-set-key (kbd "<C-tab>") 'other-window)
;; (global-set-key (kbd "<S-iso-lefttab>") '(lambda ()
;;                                      (other-window -1)))
(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-k") 'eager-kill-line)
(global-set-key (kbd "C-p") 'backward-char)
(global-set-key (kbd "C-S-p") 'previous-line)
(global-set-key (kbd "C-S-j") 'join-line)
;; (global-set-key (kbd "C-<prior>") 'tabbar-forward)
;; (global-set-key (kbd "C-<next>") 'tabbar-backward)
(global-set-key (kbd "C-M-s") 'igrep-find)
;; (global-set-key [C-kp-1] '(lambda ()
;;                             (ido-find-file "~/.emacs")))

(global-set-key (kbd "C-a") 'move-indentation-or-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-o") 'my-anything)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key [f1] 'multi-term)
(global-set-key [f2] 'multi-term-prev)
(global-set-key [f3] 'multi-term-next)
(global-set-key [(shift f3)] 'kmacro-start-macro-or-insert-counter)
(global-set-key [(shift f4)] 'kmacro-end-or-call-macro)
(global-set-key [f4] 'slime-connect)
(global-set-key [f6] 'next-error)
(global-set-key [f8] 'paredit-mode)
(global-set-key [f9] 'magit-status)

;; (global-set-key [f10] 'toggle-fullscreen)
(global-set-key [f12] '(lambda ()
                         (interactive)
                         (kill-buffer nil)))
;; (global-set-key (kbd "²") '(lambda ()
;;                                  (interactive)
;;                                  (kill-buffer nil)))

(global-set-key (kbd "<C-return>")
                '(lambda ()
                   (interactive)
                   (switch-to-buffer nil)))

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-M-<SPC>") 'er/expand-region)

;; (key-chord-define-global "op" 'my-anything)
;; (key-chord-define-global "ii" 'indent-region)
;; (key-chord-define-global "jk" 'ace-jump-mode)

(add-hook 'window-setup-hook 'maximize-frame t)

;; (setq auto-save-interval 20)

;; (add-hook 'org-mode-hook 'turn-on-real-auto-save)
;; (remove-hook 'org-mode-hook 'turn-on-real-auto-save)
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "<C-return>") nil)
             (define-key org-mode-map (kbd "<C-tab>") nil)
             (define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
             (define-key org-mode-map (kbd "<backtab>") nil)))

;; unicode
(set-language-environment "UTF-8")

;; term setting
(add-hook 'multi-term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 2000)
            (setq term-bind-key-alist (delete '("M-o" . term-send-backspace)
                                              term-bind-key-alist))
            (setq term-bind-key-alist (delete '("C-p" . previous-line)
                                              term-bind-key-alist))))


(setq browse-url-generic-program
      "/opt/google/chrome/chrome"
      browse-url-browser-function 'browse-url-generic)

(setq tags-table-list
           '("~/Documents/Projects/carneades/src"))

;; starts emacs server
(server-start)

