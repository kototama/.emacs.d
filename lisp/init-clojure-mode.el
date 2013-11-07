(require 'use-package)

(use-package clojure-mode
  :init (progn

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

          (defun my-clojure-compile-on-save ()
            (interactive)
            (save-buffer)
            (when (and (get-buffer "*nrepl*")
                       (s-ends-with? ".clj" buffer-file-name)
                       (not (s-ends-with? "project.clj" buffer-file-name)))
              ;; when connected to nrepl and inside a Clojure
              ;; but not ClojureScript file, automatically
              ;; loads the file into the REPL upon saving
              (nrepl-load-current-buffer)))

          (defun my-clojure-switch-to-nrepl-buffer
            ()
            (interactive)
            (switch-to-buffer-other-window "*nrepl*"))

          (defun my-clojure-mode-hook ()
            (use-package cider
              :init
              (progn

                (use-package clojure-test-mode)
                (use-package ac-nrepl)

                (defun my-cider-init-mode-hook
                  ()
                  (ac-nrepl-setup)
                  (auto-complete-mode 1)
                  (bind-key "<S-return>" 'cider-repl-return cider-mode-map))

                (add-hook 'cider-repl-mode-hook 'my-cider-init-mode-hook)
                (add-hook 'cider-repl-mode-hook 'paredit-mode)))

            (setq nrepl-popup-stacktraces nil)
            (setq cider-popup-stacktraces-in-repl t)

            (paredit-mode t)
            (show-paren-mode t)
            ;; (flyspell-prog-mode nil)

            ;; (define-key clojure-mode-map "{" 'paredit-open-curly)
            ;; (define-key clojure-mode-map "}" 'paredit-close-curly)
            ;; (define-key clojure-mode-map (kbd "C-M-/") 'anything-slime-complete)
            ;; (define-key clojure-mode-map (kbd "M-/") 'dabbrev-expand)
            ;; (define-key clojure-mode-map (kbd "C-?") 'anything-slime-apropos)
            ;; (define-key clojure-mode-map (kbd "C-c C-k") 'nrepl-load-current-buffer)
            ;; (define-key clojure-mode-map (kbd "C-x C-e") 'nrepl-eval-last-expression)
            ;; (define-key clojure-mode-map (kbd "<return>")
            ;; 'paredit-newline)

            (eldoc-mode 0)

	    (bind-key "C-*" 'earmuffy)
            (bind-key "C-x C-s" 'my-clojure-compile-on-save clojure-mode-map)
            (bind-key "M-." 'cider-jump clojure-mode-map)
            (bind-key "M-n" 'move-down-a-few-lines
                      clojure-test-mode-map)
            )

          (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

  :bind (("C-;" . comment-region)))

;; (use-package clojure-test-mode
;;   :init (progn
;; 	  (use-package nrepl-mode)
;;           (bind-key "M-n" 'move-down-a-few-lines
;;                     clojure-test-mode-map)
;;           (bind-key "M-p" 'move-up-a-few-lines clojure-test-mode-map)))
