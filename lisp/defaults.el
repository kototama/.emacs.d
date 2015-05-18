(blink-cursor-mode 0)

;; saves backup and tmp files in the ~/.emacs.d/tmp directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups/"))))

(setq auto-save-file-name-transforms
      `((".*"  ,(concat user-emacs-directory "backups/") t)))


;; no menubar
(menu-bar-mode 0)

;; no toolbar
(tool-bar-mode -1)

;; no scroll bars
(scroll-bar-mode -1)

;; no start screen
(setq inhibit-splash-screen t)

;; no tabs, spaces instead
(setq-default indent-tabs-mode nil)

;; changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; do not confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

;; integrates copy/paste with X
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; unicode
(set-language-environment "UTF-8")

;; lines should be 80 characters wide, not 72
(setq fill-column 80)

;; sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; prefix buffer having the same name by a path element
(setq uniquify-buffer-name-style 'forward)

;; allows downcase-region command
(put 'downcase-region 'disabled nil)

;; saves a lot of recent files
(setq recentf-max-menu-items 300)

;; defines functions that can be executed by buffer local definitions
(setq safe-local-variable-values
      '((eval org-global-cycle)))

;; shift + movements do not activate a selection
(setq shift-select-mode nil)

;; fix emacs closing slowly on some systems
(setq x-select-enable-clipboard-manager nil)

;; use aspell instead of ispell
(setq-default ispell-program-name "aspell")

(provide 'defaults)
