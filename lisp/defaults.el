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
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; unicode
(set-language-environment "UTF-8")

;; lines should be 80 characters wide, not 72
(setq-default fill-column 80)

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

;; prevent magit 2.1 to hang emacs
;; see http://magit.vc/manual/magit/Emacs-245-hangs-when-loading-Magit.html#Emacs-245-hangs-when-loading-Magit
(setq tramp-ssh-controlmaster-options nil)

;; missing function in Emacs < 24.4
(when (<= (string-to-number emacs-version) 24.3)
  (defun string-suffix-p (str1 str2 &optional ignore-case)
    (let ((begin2 (- (length str2) (length str1)))
          (end2 (length str2)))
      (when (< begin2 0) (setq begin2 0))
      (eq t (compare-strings str1 nil nil
                             str2 begin2 end2
                             ignore-case)))))

;; do not show a message when saving a file
(setq save-silently t)

;; browse the hyperspec within Emacs
(setq browse-url-handlers '(("hyperspec" . eww-browse-url)
        ("." . browse-url-default-browser)))

;; lsp-mode tweaks https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq safe-local-variable-values '((eval outshine-cycle-buffer) (eval org-global-cycle)))

(provide 'defaults)
