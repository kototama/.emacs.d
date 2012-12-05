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
(setq multi-term-program "/bin/zsh")

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
(set-default 'indicate-empty-lines t)

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

