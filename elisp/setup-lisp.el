;; (require 'slime-autoloads)

(autoload 'paredit "paredit-mode" "A minor mode for parenthesis" t)
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(autoload 'elisp-slime-nav-mode "elisp-slime-nav" "SLIME-like for Elisp" t)
(autoload 'elisp-slime-nav-find-elisp-thing-at-point
  "elisp-slime-nav" "SLIME-like for ELisp" t)

(setq blink-matching-paren nil)

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
             return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
   open and indent an empty line between the cursor and the text.  Move the
   cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun paredit-eager-kill-line
  ()
  "Kills the current line or join the next line 
   if the point is at the end of the line"
  (interactive)
  (let ((current-point (point))
        (bol-point (line-beginning-position))
        (eol-point (line-end-position)))
    (if (and (= current-point eol-point)
             (/= current-point bol-point))
        (delete-indentation 1)
      (paredit-kill nil))))

(eval-after-load "paredit"
  '(progn (define-key paredit-mode-map (kbd "C-c 0") 'paredit-forward-slurp-sexp)
          (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-barf-sexp)
          (define-key paredit-mode-map (kbd "C-c 9") 'paredit-backward-slurp-sexp)
          (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-barf-sexp)
          (define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
          (define-key paredit-mode-map (kbd "M-r") nil)
          (define-key paredit-mode-map (kbd "C-k") 'paredit-eager-kill-line)))

(add-hook 'nrepl-mode-hook
          '(lambda ()
             (define-key nrepl-mode-map (kbd "<return>") 'nrepl-return)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode t)
            ;; (flyspell-prog-mode)
            (turn-on-eldoc-mode)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)

            (local-set-key (kbd "RET") 'electrify-return-if-match)
            (eldoc-add-command 'electrify-return-if-match)

            (show-paren-mode t)))

(defun earmuffy (&optional arg)
  (interactive "P")
  (let* ((variable (thing-at-point 'sexp))
         (bounds (bounds-of-thing-at-point 'sexp))
         (current-point (point))
         (earmuffed-variable (concat "*" variable "*")))
    (save-excursion
      (kill-region (car bounds) (cdr bounds))
      (if arg
          ;; unearmuffy
          (progn
            (insert (substring variable 1 (- (length variable) 1)))
            (goto-char (- current-point 1)))
        ;; earmuffy
        (progn
          (insert earmuffed-variable)
          (goto-char (+ current-point 1)))))))

(defun kill-compilation-buffer-when-no-errors ()
  (dolist (buffer (buffer-list))
    (when (string-match "Compilation*" (buffer-name buffer))
      (save-current-buffer
        (set-buffer buffer)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "0 compiler notes" nil t)
            (kill-buffer)))))))


(add-hook 'clojure-mode-hook
  '(lambda ()
     (paredit-mode t)
     (show-paren-mode t)
     (flyspell-prog-mode)
     (define-key clojure-mode-map [f5] 'slime-compile-and-load-file)
     (define-key clojure-mode-map [f7] 'slime-edit-definition-with-etags)
     (define-key clojure-mode-map (kbd "C-*") 'earmuffy)
     (define-key clojure-mode-map "{" 'paredit-open-brace)
     (define-key clojure-mode-map "}" 'paredit-close-brace)
     (define-key clojure-mode-map (kbd "C-M-/") 'anything-slime-complete)
     (define-key clojure-mode-map (kbd "M-/") 'dabbrev-expand)
     (define-key clojure-mode-map (kbd "C-?") 'anything-slime-apropos)
     ;; autocompile file when saved
     (define-key clojure-mode-map (kbd "C-x C-s") 
       '(lambda ()
          (interactive)
          (save-buffer)
          ;; TODO nrepl compile and load file?
          ))))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (paredit-mode t)
             (elisp-slime-nav-mode t)
             (define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
             (define-key emacs-lisp-mode-map (kbd "M-/") 'dabbrev-expand)
             (define-key emacs-lisp-mode-map (kbd "C-M-/") 'lisp-complete-symbol)
             (define-key emacs-lisp-mode-map [f5] 'eval-buffer)
             (define-key emacs-lisp-mode-map (kbd "M-o") nil)))

(defun clj-jack-in ()
  "Starts a term, runs lein swank in it and connects to it"
  (interactive)
  (split-window-right)
  (other-window 1)
  (let ((buffer (multi-term)))
    (switch-to-buffer buffer)
    (term-send-raw-string "lein swank\n")
    (run-at-time "25 sec" nil
                 (lambda ()
                   (slime-connect "localhost" 4005)
                   (switch-to-buffer "*slime-repl clojure*")
                   (other-window -1)
                   (slime-compile-and-load-file)))))

(provide 'setup-lisp)
