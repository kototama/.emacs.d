;;; * required libs
(eval-when-compile
  (require 'use-package)
  (require 's))

;;; * ace-jump
(use-package ace-jump-mode
  :bind (("M-SPC" . ace-jump-mode)))

;;; * browse-kill-ring
(use-package browse-kill-ring)

;;; * cider-repl
(use-package cider-repl
  :config

  (defun my-cider-mode-hook
    ()
    (paredit-mode t)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-popup-stacktraces t)
    (setq cider-repl-popup-stacktraces t)
    ;; (bind-key "<S-return>" 'cider-repl-return cider-mode-map)
    )

  (add-hook 'cider-repl-mode-hook 'my-cider-mode-hook)

  :disabled t)

;;; * clojure
(use-package clojure-mode
  :config
  (progn
    (defun cider-insert-required-ns-in-repl
      ()
      (interactive)
      (save-excursion
        (goto-char 0)
        (if (re-search-forward ":require" nil t)
            (let ((pos-first-require (1+ (point))))
              (re-search-forward ")")
              (let* ((pos-last-require (1- (point)))
                     (requires (buffer-substring-no-properties pos-first-require pos-last-require))
                     (quoted-requires (replace-regexp-in-string "\n +\\(\\[\\)" "\n'[" requires))
                     (form (concat "(require '" quoted-requires ")")))
                (switch-to-buffer-other-window (cider-find-or-create-repl-buffer))
                (goto-char (max-char))
                (insert form)))
          (message ":require form not found"))))

    (defun clojure-show-defs
      ()
      (interactive)
      (occur "def"))

    (defun my-clojure-mode-hook
      ()
      (message "my-clojure-mode-hook")
      (paredit-mode t)
      ;; (auto-indent-mode t)
      (clojure-test-mode t)
      ;; indents ring context function properly
      (put 'context 'clojure-indent-function 2))

    (add-hook 'clojure-mode-hook 'my-clojure-mode-hook)))

;;; * common-lisp
(use-package lisp-mode
  :config
  (setq inferior-lisp-program "sbcl")

  (slime-setup '(slime-fancy slime-company))

  (add-hook 'lisp-mode-hook
            (lambda ()
              (paredit-mode t)
              ))
)

;;; * color-theme
(use-package color-theme
  :disabled t
  :config
  (require 'color-theme-kototama)
  ;; (color-theme-kototama)
  )

;;; * company
(use-package company
  :config
  (progn
    (setq company-idle-delay 0.25)
    (setq company-dabbrev-downcase 0.25)
    (set-face-background 'company-tooltip "light slate blue")
    ;; (global-company-mode)
    ))

;;; * css-mode
(use-package css-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
  :config
  (progn
    ;; (add-hook 'css-mode-hook 'auto-indent-mode)
    ))

;;; * delsel
(use-package delsel
  :config
  (delete-selection-mode))
;;; * dired
(use-package dired
  :config
  (progn
    (use-package dired-open
      :config
      (progn
       (defun my-dired-open-hook
         ()
         (local-set-key (kbd "<S-return>") 'dired-open-xdg))

       (add-hook 'dired-mode-hook 'my-dired-open-hook))
      :disabled t)))

;;; * dired+
(use-package dired+
  :disabled t)

;;; * doom powerline
(use-package doom-modeline
  ;; run-once: (all-the-icons-install-fonts)
  :init (doom-modeline-mode 1)
  )
;;; * eldoc
(use-package eldoc
  :bind (("C-c h" . eldoc))
  )
;;; * eglot
(use-package eglot
  :config
  (push '(elixir-mode . ("elixir-ls")) eglot-server-programs))
;;; * elisp
(use-package elisp-mode
  :init
  (progn

    (defun my-elisp-mode-hook
        ()
      ;; (auto-indent-mode)
      (whitespace-mode)
      (company-mode)
      )

    ;; does not work when hook is elisp-mode-hook!
    (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)))

;;; * elixir
(use-package elixir-mode
  :config
  (progn
    (defun elixir-expected-ns ()
      (let* ((path (file-truename (buffer-file-name)))
             (sans-file-ext (substring path 0 (- (length (file-name-extension path t)))))
             (sans-project-dir (replace-regexp-in-string "\\(.*/lib/\\)" "" sans-file-ext))
             (sans-project-dir (replace-regexp-in-string "\\(.*/test/\\)" "" sans-project-dir))
             (with-capitals (mapconcat 'capitalize (split-string sans-project-dir "/") "."))
             (sans-underscores (replace-regexp-in-string "_" "" with-capitals)))
        sans-underscores))

    (defun elixir-insert-ns ()
      (interactive)
      (insert (elixir-expected-ns)))

    (defun create-elixir-scratch-buffer nil
       "create a scratch buffer"
       (interactive)
       (switch-to-buffer (get-buffer-create "*elixir-scratch*"))
       (elixir-mode))

    (defun my-elixir-mode-hook ()
      (whitespace-mode)
      (smartparens-mode)
      (linum-mode)
      (electric-indent-mode)
      ;; (auto-fill-mode)
      (require 'smartparens-elixir)
      (company-mode)
      (set-face-foreground 'elixir-atom-face "dark turquoise")
      (flycheck-mode)
      (exunit-mode)
      ;; (setq flycheck-elixir-credo-strict t)
      ;; (lsp)
      (yas-minor-mode) ;; required for lsp
      ;; (flymake-elixir-load)
      (when (not (s-ends-with? "/mix.exs" (buffer-file-name)))
        (message "Starting eglot")
        (eglot-ensure)
        )
      )

    (add-hook 'elixir-mode-hook 'my-elixir-mode-hook)
    )
  :bind (("C-c =" . elixir-format))
  :bind (("C-c n" . elixir-insert-ns))
  :bind (("C-c e b" . elixir-beginning-of-defun))
  :bind (("C-c e f" . elixir-end-of-defun))
  :bind (("C-c , " . exunit-transient))
  )

;;; * elpy
(use-package elpy
  :disabled t
  :config
  (progn

    (defun my-elpy-mode-hook ()
      (setq elpy-rpc-backend "rope")
      (setq elpy-rpc-timeout 3)
      (setq elpy-test-runner 'elpy-test-pytest-runner)
      (local-set-key (kbd "M-.") 'python-goto-definition))

    (add-hook 'elpy-mode-hook 'my-elpy-mode-hook)
    )
  )

;;; * expand region
(use-package expand-region
  :bind (("C-M-SPC" . er/expand-region))
)
;;; * flycheck
(use-package flycheck
  :config
  (progn
    (use-package flycheck-hdevtools
      :disabled t)))

;;; * geiser-repl
(use-package geiser-repl
  :config
  (progn
    (defun my-geiser-repl-hook ()
      (paredit-mode)
      (company-mode))

    (add-hook 'geiser-repl-mode-hook 'my-geiser-repl-hook)
    ))
;;; * grep

(use-package grep
  :config
  (progn
    (when (string-suffix-p "/fish" (getenv "SHELL"))
      ;; for grep-find
      (grep-apply-setting 'grep-find-command "find <D> -type f -exec grep -nH -e <R> \\{\\} +")
      ;; for rgrep
      (grep-apply-setting 'grep-find-template
                          "find . <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} \\\;")
      )))


;;; * haskell
(use-package haskell-mode
  :disabled t
  :config
  (progn

    (setq haskell-process-type 'stack-ghci)


    (defun my-haskell-mode-hook
      ()
      (intero-mode))

    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
    ))

;;; * helm
(use-package helm-mode
  :init
  (helm-mode 1)

  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("C-S-o" . helm-mini)
         ("C-c s" . helm-ag)
         )
)
;;; * hl
(use-package hl-line
  :config
  (progn
    (set-face-background 'hl-line "dark slate blue")
    ))
;;; * javascript
(use-package js
  :config
  (progn

    (defun my-js-mode-hook
        ()
      ;; (auto-indent-mode)
      (whitespace-mode)
      (flycheck-mode)
      (smartparens-mode))

    ;; for Jasmine
    (defun js-xit-tests
        ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\(^ +\\)it" nil t)
          (replace-match "\\1xit" nil nil))))

    (defun js-unxit-tests
        ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\(^ +\\)xit" nil t)
          (replace-match "\\1it" nil nil))))

    (add-hook 'js-mode-hook 'my-js-mode-hook)))

;;; * lisp
(use-package lisp-mode
  :config
  (progn

    (use-package auto-async-byte-compile
      :disabled
      :config
      (progn
        (setq auto-async-byte-compile-suppress-warnings t)
        (add-hook 'emacs-lisp-mode-hook
                  'enable-auto-async-byte-compile-mode)))

    (defun my-emacs-lisp-mode-hook
        ()
      (paredit-mode t)
      ;; (auto-indent-mode t)

      ;; (use-package auto-indent
      ;;   :config
      ;;   (auto-indent-mode t))
      )

    (defun my-minibuffer-mode-hook
      ()
      ;; (paredit-mode 1)
      )

    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
    (add-hook 'minibuffer-setup-hook 'my-minibuffer-mode-hook)))

;;; * magit
(use-package magit
  :config
  (progn

    ;; taken from https://github.com/magit/magit/issues/3717#issuecomment-734798341
    (transient-append-suffix 'magit-push "-u"
      '(1 "=s" "Skip gitlab pipeline" "--push-option=ci.skip"))
    (transient-append-suffix 'magit-push "-u"
      '(1 "=v" "Set CI variable" "--push-option=ci.variable="))
    (transient-append-suffix 'magit-push "-u"
      '(1 "=o" "Set push option" "--push-option="))

    ;; http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
    (require 's)

    (defun visit-pull-request-url ()
      "Visit the current branch's PR on Github."
      (interactive)
      (browse-url
       (format "https://github.com/%s/pull/new/%s"
               (car (s-split ".git"
                             (cadr (s-split ":"
                                            (magit-get "remote"
                                                       (magit-get-remote)
                                                       "url")))))
               (cdr (magit-get-remote-branch)))))

    ;; https://emacs.stackexchange.com/questions/13772/how-to-prevent-magit-to-ask-where-to-push-a-branch
    (defun my-magit-mode-hook
        ()

      (setq ediff-window-setup-function 'ediff-setup-windows-plain)

      (defun magit-push-arguments-maybe-upstream (magit-push-popup-fun &rest args)
        "Enable --set-upstream switch if there isn't a current upstream."
        (let ((magit-push-arguments
               (if (magit-get-remote) magit-push-arguments
                 (cons "--set-upstream" magit-push-arguments))))
          (apply magit-push-popup-fun args)))

      (advice-add 'magit-push-popup :around #'magit-push-arguments-maybe-upstream))

    (define-key magit-mode-map "v" #'visit-pull-request-url)

    ;; (setq magit-completing-read-function #'magit-ido-completing-read)

    (add-hook 'magit-mode-hook 'my-magit-mode-hook))

  :bind (("C-c m s" . magit-status)
         ("C-c m l" . magit-file-log)
         ("C-c m L" . magit-log)
         ("C-c m b" . magit-blame)))

;;; * magit-blame
(use-package magit-blame)
;;; * markdown
(use-package markdown-mode
  :init
  (progn
    (add-hook 'markdown-mode-hook 'whitespace-mode))
  :bind* (("M-p" . backward-paragraph)
          ("M-n" . forward-paragraph)))

;;; * multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

;;; * monokai
(use-package monokai-theme
  :init
  (progn
    (setq monokai-comments "SteelBlue1")
    (load-theme 'monokai t)
    (set-cursor-color "purple")
    )
)
;;; * move-lines
(use-package move-lines
  :bind (("M-<up>" . move-lines-up)
         ("M-<down>" . move-lines-down)
         ))
;;; * org
(use-package org
  :config
  (progn
    (setq org-reverse-note-order t)
    (setq org-src-fontify-natively t)

    (defun jtc-org-tasks-closed-in-month (&optional month year match-string)
      "Produces an org agenda tags view list of the tasks completed
in the specified month and year. Month parameter expects a number
from 1 to 12. Year parameter expects a four digit number. Defaults
to the current month when arguments are not provided. Additional search
criteria can be provided via the optional match-string argument "
      (interactive)
      (let* ((today (calendar-current-date))
             (for-month (or month (calendar-extract-month today)))
             (for-year  (or year  (calendar-extract-year today))))
        (org-tags-view nil
                       (concat
                        match-string
                        (format "+CLOSED>=\"[%d-%02d-01]\""
                                for-year for-month)
                        (format "+CLOSED<=\"[%d-%02d-%02d]\""
                                for-year for-month
                                (calendar-last-day-of-month for-month for-year))))))

    (defun jtc-tasks-last-month ()
      "Produces an org agenda tags view list of all the tasks completed
last month."
      (interactive)
      (let* ((today (calendar-current-date))
             (for-month (calendar-extract-month today))
             (for-year  (calendar-extract-year today)))
        (calendar-increment-month for-month for-year -1)
        (jtc-org-tasks-closed-in-month
         for-month for-year "+TODO=\"DONE\"")))

    (defun my-common-org-mode-hook
        ()
      (setq org-refile-targets '((nil :maxlevel . 2)))
      (setq org-use-speed-commands t)
      (setq org-archive-location "::* Archived Tasks")
      (setq org-log-done 'time)
      (setq org-export-with-sub-superscripts nil)
      (condition-case nil
          (progn
            (require 'org-bullets)
            (org-bullets-mode 1))
        (message "org-bullets not installed")))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)))

    (add-hook 'org-mode-hook 'my-common-org-mode-hook))

  :bind (("C-c o a" . org-agenda)
         ("C-c o o" . org-open-at-point)
         ("C-c o l" . org-store-link)
         ("C-c o L" . org-insert-link)))

;;; * org-capture
(use-package org-capture
  :bind (("C-c o r" . org-capture)))
;;; * ox-latex
(use-package ox-latex
  :config (progn
            (add-to-list 'org-latex-classes
                         '("myletter"
                           "\\documentclass\{letter\}
\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
\\makeatletter
\\let\\@texttop\\relax
\\makeatother
                            \[NO-DEFAULT-PACKAGES]
                            \[NO-PACKAGES]
                            \[EXTRA]"))
            (setq org-latex-with-hyperref nil)
            ))

;;; * plantuml
(use-package plantuml-mode
  :disabled t
  :config
  (progn
    (setq plantuml-jar-path "~/local/opt/plantuml.jar")
    (setq plantuml-default-exec-mode 'jar)))
;;; * paredit
(use-package paredit
  :config
  (progn
    (defun my-paredit-mode-hook
      ()
      (local-set-key (kbd "M-q") 'fill-paragraph))

    (add-hook 'paredit-mode-hook 'my-paredit-mode-hook)))

;;; * projectile
(use-package projectile
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c M-p"))
    (projectile-global-mode))
  :bind (("C-c f f" . projectile-find-file)
         ("C-c f d" . projectile-find-dir)
         ("C-c f a" . projectile-ag)))

;;; * python
(use-package python
  :config
  (progn

    (defun python-goto-definition
        ()
      (interactive)
      (condition-case nil
          (elpy-goto-definition)
        (error
         (etags-select-find-tag-at-point)))
      )

    (defun python-set-breakpoint
        ()
      (interactive)
      (insert "import pudb; pudb.set_trace() #  noqa")
      )

    (defun my-python-mode-hook
        ()
      (message "my-python-mode-hook")
      ;; (elpy-mode)
      ;; (setq elpy-rpc-backend "jedi")
      (setq python-indent-offset 4)
      (whitespace-mode)
      (whitespace-cleanup-mode)
      ;; (local-set-key (kbd "M-.") 'python-goto-definition)
      (anaconda-mode)
      )

    (defun python-get-test-filename
        (filename)
      "Return the name of the test file."
      (let ((dir (file-name-directory filename)))
       (if (string-suffix-p "__init__.py" filename)
           (concat dir "test_init.py")
         (concat dir "test_" (file-name-nondirectory filename)))))

    (defun python-test-file
        ()
      "Open the test file for the current buffer in an another
window and run the unit tests. "
      (interactive)
      (delete-other-windows)
      (buffer-file-name)
      (find-file-other-window (python-get-test-filename (buffer-file-name)))
      (split-window-below)
      (elpy-test)
      (enlarge-window 3))

    (add-hook 'python-mode-hook 'my-python-mode-hook)))

;;; * simple
(use-package simple
  :config
  (column-number-mode))

;;; * smartparens
(use-package smartparens
  :config
  (progn
    ;; (set-face-foreground 'sp-pair-overlay-face nil)

    (defun my-smarparens-mode-hook ()
      (set-face-background 'sp-pair-overlay-face "DodgerBlue4"))

    (add-hook 'smartparens-mode-hook 'my-smarparens-mode-hook)))
;;; * racket
(use-package racket-mode
  :config
  (progn
    (defun my-racket-mode-hook ()
      (paredit-mode)
      (geiser-mode)
      (company-mode)
      )

    (add-hook 'racket-mode-hook 'my-racket-mode-hook)
    )

  :init
  (add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode)))

;;; * recentf
(use-package recentf
  :init
  (recentf-mode)

  :config
  (progn
    (setq recentf-max-menu-items 25)
    (setq recentf-max-saved-items 25)
    (let ((save-silently t))
      (run-at-time (current-time) 60 'recentf-save-list))))

;;; * rust
(use-package rust-mode
  :config
  (progn
    (defun my-rust-mode-hook ()
      (linum-mode)
      ;; (add-hook 'before-save-hook 'rust-format-buffer nil 'make-it-local)
      (setq rust-format-on-save t)
      )

    (add-hook 'rust-mode-hook 'my-rust-mode-hook)))
;;; * slime-repl
(use-package slime-repl
  :config
  (add-hook 'slime-repl-mode-hook
            (lambda ()
               (paredit-mode t)
               )))
;;; * show-paren-mode
(use-package paren
  :init
  (progn
    (set-face-background 'show-paren-match "purple")
    (set-face-foreground 'show-paren-match "black"))
  :config
  (show-paren-mode))

;;; * uniquify
(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'forward)
    (setq uniquify-strip-common-suffix nil)))

;;; * undo-tree
(use-package undo-tree
  :config
  (progn
    (setq undo-tree-auto-save-history nil)
    (global-undo-tree-mode)))
;;; * paredit
(use-package paredit
  :config
  (progn
    (defun my-paredit-mode-hook
      ()
      (local-set-key (kbd "M-q") 'fill-paragraph))

    (add-hook 'paredit-mode-hook 'my-paredit-mode-hook)))

;;; * tuareg
(use-package tuareg
  :init
  (progn
    (defun my-tuareg-hook ()
      (merlin-mode)
      (company-mode)
      (smartparens-mode)
      (flycheck-mode)
      (whitespace-mode)
      (set-face-foreground 'tuareg-font-lock-governing-face "goldenrod")
      (setq merlin-command "ocamlmerlin")
      )

    (add-hook 'tuareg-mode-hook 'my-tuareg-hook)
    ))
;;; * web-mode
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\html.eex$" . web-mode))
  :config
  (web-mode-toggle-current-element-highlight))
;;; * whitespace-mode
(use-package whitespace
  :init
  (progn
   (setq whitespace-style '(face tabs trailing ;; lines-tail
                                 space-before-tab
                                 newline indentation empty space-after-tab
                                 tab-mark ;; newline-mark
                                 lines
                                 ))
   (setq whitespace-line-column 110)

   (defun my-whitespace-hook
       ()
     (set-face-foreground 'whitespace-trailing "red")
     (set-face-background 'whitespace-trailing "black"))
   
   (add-hook 'whitespace-mode-hook 'my-whitespace-hook)
   ))



;;; * winner-mode
(use-package winner
  :config
  (winner-mode))
;;; * wgrep
(use-package wgrep)

;;; * yasnippet
(use-package yasnippet
  :init
  (progn
    (setq yas-snippet-dirs
         '("~/.emacs.d/snippets"))
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
    (yas-global-mode)))
;;; * end of file
(provide 'config)

;;; * autoload

;; Local Variables:
;; eval: (outshine-mode 1)
;; eval: (outshine-cycle-buffer)
;; End:
