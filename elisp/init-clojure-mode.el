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

          (defun my-clojure-mode-hook ()
            (paredit-mode t)
            (show-paren-mode t)
            (flyspell-prog-mode)

            (define-key clojure-mode-map (kbd "C-*") 'earmuffy)
            (define-key clojure-mode-map "{" 'paredit-open-curly)
            (define-key clojure-mode-map "}" 'paredit-close-curly)
            (define-key clojure-mode-map (kbd "C-M-/") 'anything-slime-complete)
            (define-key clojure-mode-map (kbd "M-/") 'dabbrev-expand)
            ;; (define-key clojure-mode-map (kbd "C-?") 'anything-slime-apropos)
            (define-key clojure-mode-map (kbd "C-c C-k") 'nrepl-load-current-buffer)
            (define-key clojure-mode-map (kbd "C-x C-e") 'nrepl-eval-last-expression)
            (define-key clojure-mode-map (kbd "<return>")
            'paredit-newline)

            ;; autocompile file when saved
            (define-key clojure-mode-map (kbd "C-x C-s") 
              '(lambda ()
                 (interactive)
                 (save-buffer)
                 (when (and (get-buffer "*nrepl*")
                            (s-ends-with? ".clj" buffer-file-name))
                   ;; when connected to nrepl and inside a Clojure
                   ;; but not ClojureScript file, automatically
                   ;; loads the file into the REPL upon saving
                   (nrepl-load-current-buffer)))))

          (use-package nrepl)

          (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

  :bind (("C-c n j" . nrepl-jack-in)
         ("C-c n q" . nrepl-quit)))

(use-package nrepl
  (:init
   (progn
     (defun my-nrepl-init-mode-hook
       ()
       (paredit-mode 1))

     (add-hook 'nrepl-mode-hook 'my-nrepl-init-mode-hook)))

  (:bind (("<s-return>" . nrepl-return)
          ("C-c n r" . nrepl-return))))






            
