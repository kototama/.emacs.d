(require 'use-package)

(use-package clojure-mode
  :init (progn
          
          (use-package nrepl
            :init
            (progn

              (use-package ac-nrepl)
              
              (defun my-nrepl-init-mode-hook
                ()
                (auto-complete-mode 1)
                (paredit-mode 1))

              (defun my-nrepl-show-server-buffer
                ()
                (interactive)
                (switch-to-buffer "*nrepl-server*")
                (ktm-mode 1))

              (add-hook 'nrepl-mode-hook 'my-nrepl-init-mode-hook)
              (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
              (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup))

            :bind (("<S-return>" . nrepl-return)
                   ("C-S-e" . nrepl-eval-last-expression)
                   ("C-c n r" . nrepl-return)
                   ("C-c n b" . my-nrepl-show-server-buffer)))
          
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

          (defun my-compile-on-save ()
            (interactive)
            (save-buffer)
            (when (and (get-buffer "*nrepl*")
                       (s-ends-with? ".clj" buffer-file-name))
              ;; when connected to nrepl and inside a Clojure
              ;; but not ClojureScript file, automatically
              ;; loads the file into the REPL upon saving
              (nrepl-load-current-buffer)))

          (defun my-clojure-mode-hook ()
            (paredit-mode t)
            (show-paren-mode t)
            ;; (flyspell-prog-mode nil)
            (elisp-slime-nav-mode nil)

            ;; (define-key clojure-mode-map "{" 'paredit-open-curly)
            ;; (define-key clojure-mode-map "}" 'paredit-close-curly)
            ;; (define-key clojure-mode-map (kbd "C-M-/") 'anything-slime-complete)
            ;; (define-key clojure-mode-map (kbd "M-/") 'dabbrev-expand)
            ;; (define-key clojure-mode-map (kbd "C-?") 'anything-slime-apropos)
            ;; (define-key clojure-mode-map (kbd "C-c C-k") 'nrepl-load-current-buffer)
            ;; (define-key clojure-mode-map (kbd "C-x C-e") 'nrepl-eval-last-expression)
            ;; (define-key clojure-mode-map (kbd "<return>")
            ;; 'paredit-newline)

            (eldoc-mode nil))
          
          (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

  :bind (("C-c n j" . nrepl-jack-in)
         ("C-c n q" . nrepl-quit)
         ("C-x C-s" . my-compile-on-save)
         ("C-*" . earmuffy)
         ("C-;" . comment-region)))








            
