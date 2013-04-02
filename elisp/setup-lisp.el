(autoload 'elisp-slime-nav-mode "elisp-slime-nav" "SLIME-like for Elisp" t)
(autoload 'elisp-slime-nav-find-elisp-thing-at-point
  "elisp-slime-nav" "SLIME-like for ELisp" t)

(defvar lisp-modes  '(emacs-lisp-mode
                      inferior-emacs-lisp-mode
                      ielm-mode
                      lisp-mode
                      clojure-mode
                      inferior-lisp-mode
                      lisp-interaction-mode
                      slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(use-package lisp-mode
  :init
  (progn
    ;; (defface esk-paren-face
    ;;   '((((class color) (background dark))
    ;;      (:foreground "grey50"))
    ;;     (((class color) (background light))
    ;;      (:foreground "grey55")))
    ;;   "Face used to dim parentheses."
    ;;   :group 'starter-kit-faces)

    ;; Change lambda to an actual lambda symbol
    (mapc (lambda (major-mode)
            (font-lock-add-keywords
             major-mode
             '(("(\\(lambda\\)\\>"
                (0 (ignore
                    (compose-region (match-beginning 1)
                                    (match-end 1) ?λ))))
               ("(\\|)" . 'esk-paren-face)
               ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
                (1 font-lock-keyword-face)
                (2 font-lock-function-name-face
                 nil t)))))
          lisp-modes)

    (defvar slime-mode nil)
    (defvar lisp-mode-initialized nil)

    (defun initialize-lisp-mode ()
      (unless lisp-mode-initialized
        (setq lisp-mode-initialized t)

        (use-package redshank
          :diminish redshank-mode)

        (use-package elisp-slime-nav
          :diminish elisp-slime-nav-mode)

        (use-package edebug)

        (use-package eldoc
          :diminish eldoc-mode
          :defer t
          :init
          (use-package eldoc-extension
            :disabled t
            :defer t
            :init
            (add-hook 'emacs-lisp-mode-hook
                      #'(lambda () (require 'eldoc-extension)) t))

          :config
          (eldoc-add-command 'paredit-backward-delete
                             'paredit-close-round))

        (use-package cldoc
          :diminish cldoc-mode)

        (use-package ert
          :commands ert-run-tests-interactively
          :bind ("C-c e t" . ert-run-tests-interactively))

        (use-package elint
          :commands 'elint-initialize
          :init
          (defun elint-current-buffer ()
            (interactive)
            (elint-initialize)
            (elint-current-buffer))

          :config
          (progn
            (add-to-list 'elint-standard-variables 'current-prefix-arg)
            (add-to-list 'elint-standard-variables 'command-line-args-left)
            (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
            (add-to-list 'elint-standard-variables 'emacs-major-version)
            (add-to-list 'elint-standard-variables 'window-system)))

        (use-package highlight-cl
          :init
          (mapc (function
                 (lambda (mode-hook)
                   (add-hook mode-hook
                             'highlight-cl-add-font-lock-keywords)))
                lisp-mode-hooks))

        (defun my-elisp-indent-or-complete (&optional arg)
          (interactive "p")
          (call-interactively 'lisp-indent-line)
          (unless (or (looking-back "^\\s-*")
                      (bolp)
                      (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
            (call-interactively 'lisp-complete-symbol)))

        (defun my-lisp-indent-or-complete (&optional arg)
          (interactive "p")
          (if (or (looking-back "^\\s-*") (bolp))
              (call-interactively 'lisp-indent-line)
            (call-interactively 'slime-indent-and-complete-symbol)))

        (defun my-byte-recompile-file ()
          (save-excursion
            (byte-recompile-file buffer-file-name)))

        ;; Register Info manuals related to Lisp
        ;; (use-package info-lookmore
        ;;   :init
        ;;   (progn
        ;;     (info-lookmore-elisp-cl)
        ;;     (info-lookmore-elisp-userlast)
        ;;     (info-lookmore-elisp-gnus)
        ;;     (info-lookmore-apropos-elisp)))

        ;; (mapc (lambda (mode)
        ;;         (info-lookup-add-help
        ;;          :mode mode
        ;;          :regexp "[^][()'\" \t\n]+"
        ;;          :ignore-case t
        ;;          :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
        ;;       lisp-modes)
        ))

    (defun paredit-duplicate-after-point
      ()
      "Duplicates the content of the line that is after the point."
      (interactive)
      ;; skips to the next sexp
      (while (looking-at " ")
        (forward-char))
      (set-mark-command nil)
      ;; while we find sexps we move forward on the line
      (while (and (bounds-of-thing-at-point 'sexp)
                  (<= (point) (car (bounds-of-thing-at-point 'sexp)))
                  (not (= (point) (line-end-position))))
        (forward-sexp)
        (while (looking-at " ")
          (forward-char)))
      (kill-ring-save (mark) (point))
      ;; go to the next line and copy the sexprs we encountered
      (paredit-newline)
      (yank)
      (exchange-point-and-mark))

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

    (use-package paredit-mode
      (:bind (("C-S-d" . paredit-duplicate-after-point)
              ("C-k" . paredit-eager-kill-line)
              ("M-R" . paredit-raise-sexp)
              )))

    ;; (add-hook 'paredit-mode-hook
    ;;       (lambda ()
    ;;         (define-key paredit-mode-map (kbd "C-c 0") 'paredit-forward-slurp-sexp)
    ;;         (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-barf-sexp)
    ;;         (define-key paredit-mode-map (kbd "C-c 9") 'paredit-backward-slurp-sexp)
    ;;         (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-barf-sexp)
    ;;         (define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
    ;;         (define-key paredit-mode-map (kbd "M-r") nil)
    ;;         (define-key paredit-mode-map (kbd "C-k") 'paredit-eager-kill-line)
    ;;         (define-key paredit-mode-map )))


    (defun my-lisp-mode-hook ()
      (initialize-lisp-mode)

      (auto-fill-mode 1)
      (paredit-mode 1)
      ;; (redshank-mode 1)
      (elisp-slime-nav-mode 1)

      (local-set-key (kbd "<return>") 'paredit-newline)

      (add-hook 'after-save-hook 'check-parens nil t)

      (if (memq major-mode
                '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
          (progn
            (bind-key "<M-return>" 'outline-insert-heading emacs-lisp-mode-map)
            (bind-key "<tab>" 'my-elisp-indent-or-complete emacs-lisp-mode-map)
            (bind-key (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point))
        ;; (turn-on-cldoc-mode)

        (bind-key "<tab>" 'my-lisp-indent-or-complete lisp-mode-map))

      (yas/minor-mode 1)
      (eldoc-mode 1))

    (hook-into-modes #'my-lisp-mode-hook lisp-mode-hooks)))

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (message "Emacs lisp mode hook")
;;             (elisp-slime-nav-mode t)
;;             (define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
;;             (define-key emacs-lisp-mode-map (kbd "M-/") 'dabbrev-expand)
;;             (define-key emacs-lisp-mode-map (kbd "C-M-/") 'lisp-complete-symbol)
;;             (define-key emacs-lisp-mode-map [f5] 'eval-buffer)
;;             (define-key emacs-lisp-mode-map (kbd "M-o") nil)

;;             (paredit-mode t)
;;             ;; (flyspell-prog-mode)
;;             (turn-on-eldoc-mode)
;;             (eldoc-add-command
;;              'paredit-backward-delete
;;              'paredit-close-round)

;;             ;; (local-set-key (kbd "RET") 'nil)
;;             (eldoc-add-command 'electrify-return-if-match)

;;             (show-paren-mode t)))

(provide 'setup-lisp)
