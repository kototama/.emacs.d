(require 'slime)
(require 'slime-repl)
(require 'clojure-mode)
(require 'cljdoc)
(require 'paredit)
(require 'elisp-slime-nav)

(slime-setup '(slime-fancy slime-asdf slime-c-p-c anything-slime))

;; paredit everywhere
;; (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

(setq blink-matching-paren nil)

(setq slime-protocol-version 'ignore)

;; By default inputs and results have the same color
(custom-set-faces
 '(slime-repl-result-face ((t (:foreground "orange")))))

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

(add-hook 'slime-mode-hook
          '(lambda ()
             ;; (define-key slime-mode-map (kbd "M-p") nil)
             ))



(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (clojure-mode-font-lock-setup)
             (paredit-mode t)
             (set-syntax-table clojure-mode-syntax-table)
             (define-key slime-repl-mode-map (kbd "C-c r")
               '(lambda ()
                  (interactive)
                  (slime-repl-previous-matching-input (slime-repl-current-input))))
             (define-key slime-repl-mode-map
               (kbd "<C-return>") '(lambda ()
                                     (interactive)
                                     (switch-to-buffer nil)))
             (define-key slime-repl-mode-map (kbd "C-c s") 'slime-repl-next-matching-input)
             (define-key slime-repl-mode-map (kbd "<return>") 'paredit-newline)
             (define-key slime-repl-mode-map (kbd "<S-return>") 'slime-repl-closing-return)
             (define-key slime-repl-mode-map "{" 'paredit-open-curly)
             (define-key slime-repl-mode-map "}" 'paredit-close-curly)
             (define-key slime-repl-mode-map (kbd "DEL") 'paredit-backward-delete)
             (define-key slime-repl-mode-map (kbd "M-r") 'anything-slime-repl-history)
             ))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode t)

            (turn-on-eldoc-mode)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)

            (local-set-key (kbd "RET") 'electrify-return-if-match)
            (eldoc-add-command 'electrify-return-if-match)

            (show-paren-mode t)))


(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode t)
            ;; (local-set-key (kbd "RET") 'electrify-return-if-match)
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
          (when (slime-current-connection)
            (slime-compile-and-load-file))))))

(defadvice slime-compilation-finished
  (after kill-buffer-if-necessary activate compile)
  (kill-compilation-buffer-when-no-errors))

;; (defadvice slime-connect
;;   (after goto-previous-buffer activate compile)
;;   (other-window -1))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (paredit-mode t)
             (elisp-slime-nav-mode t)
             (define-key emacs-lisp-mode-map (kbd "M-.")'elisp-slime-nav-find-elisp-thing-at-point)
             (define-key emacs-lisp-mode-map (kbd "M-/") 'dabbrev-expand)
             (define-key emacs-lisp-mode-map (kbd "C-M-/") 'lisp-complete-symbol)
             (define-key emacs-lisp-mode-map [f5] 'eval-buffer)
             (define-key emacs-lisp-mode-map (kbd "M-o") nil)))

(defun clj-jack-in ()
  "Starts a term, runs lein swank in it and connect to it"
  (interactive)
  (split-window-right)
  (other-window 1)
  (let (buffer (multi-term))
    (switch-to-buffer buffer)
    ;; (let ((proc (get-buffer-process (current-buffer))))
    ;;   (while (not proc)
    ;;     (setq proc (get-buffer-process (current-buffer)))))
    (term-send-raw-string "lein swank\n")
    (other-window -1)
    (goto-char (point-max))
    (previous-line 1)
    (beginning-of-line)
    (message "looking at:%s "(looking-at "Connection opened"))
    ;; (message "Swank is started. Connecting...")
    )
  )
