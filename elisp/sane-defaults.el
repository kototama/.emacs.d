;; disable blinking cursor
(blink-cursor-mode 0)

;; save backup files in this directory
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

;; no menu
(menu-bar-mode 0)

;; ido-mode options
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-ignore-extensions t)
(setq ido-file-extensions-order
      '(".clj" "js" ".txt" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(setq ido-use-virtual-buffers t)

;; show paren options
;; (set-face-background 'show-paren-match-face "transparent")
;; (set-face-foreground 'show-paren-match-face "red")

(setq whitespace-style '(face tabs trailing lines))

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
(setq multi-term-program (when at-work
                             "/bin/zsh"
                           "/bin/bash"))

;; unicode
(set-language-environment "UTF-8")

(setq browse-url-generic-program
      "/opt/google/chrome/chrome"
      browse-url-browser-function 'browse-url-generic)

(setq tags-table-list
           '("~/Documents/Projects/carneades/src"))

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Show me empty lines after buffer end
;; (set-default 'indicate-empty-lines nil)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; entering text on a selected text deletes it
(delete-selection-mode 1)

;; prefix buffer having the same name by a path element
(setq uniquify-buffer-name-style 'forward)

;; no scroll bars
(scroll-bar-mode -1)

;; (put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(provide 'sane-defaults)

(setq recentf-max-menu-items 50)

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(setq ack-and-a-half-prompt-for-directory t)

(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       ;; type ~~ to go the ~/.emacs.d
       (cond ((looking-back "~/") (insert ".emacs.d/"))
             ((looking-back "/") (insert "~/"))
             (t (call-interactively 'self-insert-command)))))))
