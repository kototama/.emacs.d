;;; * required libs
(eval-when-compile
  (require 'use-package)
  (require 's))

;;; * global settings
(use-package emacs
  :init
  ;; installs inconsolate fonts if presents
  (when (eq window-system 'x)
    (condition-case nil
        (progn
          (set-frame-font "Inconsolata-15")
          (add-to-list 'default-frame-alist '(font . "Inconsolata-15")))
      (error (message "Fonts Inconsolata can not be found. Please do 'sudo apt-get install ttf-inconsolata'."))))


  ;; Fix annoying lsp-ui color for errors.
  ;; Found with M-x list-faces-display
  ;; Found a new color M-x list-colors-display
  (set-face-attribute 'error nil
                      :foreground "gold"
                      ;; :weight 'bold
                      )

  (set-face-attribute 'success nil
                      :foreground "spring green"
                      ;; :weight 'bold
                      )

  ;; (set-face-attribute 'compilation-line-number nil
  ;;                     :foreground "gold"
  ;;                     ;; :weight 'bold
  ;;                     )

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

  ;; lines should be 110 characters wide, not 72
  (setq-default fill-column 110)

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

  (defun transpose-windows (arg)
    "Transpose the buffers shown in two windows."
    (interactive "p")
    (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
      (while (/= arg 0)
        (let ((this-win (window-buffer))
              (next-win (window-buffer (funcall selector))))
          (set-window-buffer (selected-window) next-win)
          (set-window-buffer (funcall selector) this-win)
          (select-window (funcall selector)))
        (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))

  :bind
  (("C-c p" . pop-to-mark-command)
   ("C-c p" . pop-to-mark-command)
   ("C-S-j" . join-line)
   ("M-n" . forward-paragraph)
   ("M-p" . backward-paragraph)
   ("M-o" . other-window)
   ("M-S-o" . (lambda ()
    (interactive)
    (other-window -1)))
   ("C-S-k" . (lambda ()
                (interactive)
                (kill-buffer (current-buffer))))
   ("C-c d" . duplicate-thing)
   ("C-c f p" . ffap)
   ("C-c k" . delete-region)

   ("C-x C-c" . nil)
   ("C-c q q" . save-buffers-kill-terminal)

   ("C-+" . text-scale-increase)
   ("C--" . text-scale-decrease)))

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
  :custom
  (set-face-attribute 'company-tooltip nil :background "#2E3440" :foreground "#D8DEE9")
  (set-face-attribute 'company-tooltip-selection nil :background "#5E81AC" :foreground "#ECEFF4")
  (set-face-attribute 'company-tooltip-common nil :foreground "#88C0D0")
  (set-face-attribute 'company-tooltip-common-selection nil :foreground "#BF616A" :weight 'bold)
  (set-face-attribute 'company-scrollbar-bg nil :background "#3B4252")
  (set-face-attribute 'company-scrollbar-fg nil :background "#4C566A")
  (setq company-idle-delay 0.25)
  (setq company-dabbrev-downcase 0.25)
  ;; (set-face-background 'company-tooltip "light slate blue")
  ;; (global-company-mode)
  )

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
  ;; run-once: (nerd-icons-install-fonts)
  :init (doom-modeline-mode 1)
  )
;;; * eldoc
(use-package eldoc
  :bind (("C-c h" . eldoc))
  )
;;; * eglot
(use-package eglot
  :config
  (push '(elixir-mode . ("elixir-ls")) eglot-server-programs)
  (push '(elixir-ts-mode . ("elixir-ls")) eglot-server-programs)
  (setq-default eglot-workspace-configuration '(:elixirLS (:dialyzerEnabled :json-false)))
  :bind
  (("C-c e i" . eglot-find-implementation)
   ))
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
(use-package elixir-ts-mode
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

    (defun elixir-in-doc-or-comment-p
      ()
      (elixir-ppss-comment-or-string-start (syntax-ppss)))

    (defun sp-elixir-in-heredoc-p (_id _action context)
      (save-excursion
        (re-search-backward "\"\"\"" nil t)
        (backward-char 4)
        (looking-at "doc ")))

    (defun my-elixir-mode-hook ()
      (whitespace-mode)
      (smartparens-mode)
      (require 'smartparens-elixir)

      (sp-with-modes '(elixir-mode elixir-ts-mode)
        (sp-local-pair "if" "end"
                       :when '(("SPC" "RET" "<evil-ret>"))
                       :post-handlers '(sp-elixir-do-block-post-handler)
                       :skip-match 'sp-elixir-skip-keyword-list-def-p
                       :unless '(sp-in-comment-p sp-in-string-p sp-elixir-in-heredoc-p))
        (sp-local-pair "do" "end"
                       :when '(("SPC" "RET" "<evil-ret>"))
                       :post-handlers '(sp-elixir-do-block-post-handler)
                       :skip-match 'sp-elixir-skip-keyword-list-def-p
                       :unless '(sp-in-comment-p sp-in-string-p sp-elixir-in-heredoc-p))
        (sp-local-pair "for" "end"
                       :when '(("SPC" "RET" "<evil-ret>"))
                       :post-handlers '(sp-elixir-do-block-post-handler)
                       :skip-match 'sp-elixir-skip-keyword-list-def-p
                       :unless '(sp-in-comment-p sp-in-string-p sp-elixir-in-heredoc-p))
        (sp-local-pair "with" "end"
                       :when '(("SPC" "RET" "<evil-ret>"))
                       :post-handlers '(sp-elixir-do-block-post-handler)
                       :skip-match 'sp-elixir-skip-keyword-list-def-p
                       :unless '(sp-in-comment-p sp-in-string-p sp-elixir-in-heredoc-p)))

      (display-line-numbers-mode)
      (electric-indent-mode)
      ;; (auto-fill-mode)
      (company-mode)
      ;; (set-face-foreground 'elixir-atom-face "dark turquoise")
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

      (add-hook 'before-save-hook 'elixir-format nil t)
      )

    (add-hook 'elixir-ts-mode-hook 'my-elixir-mode-hook)
    )
  :bind (:map elixir-ts-mode-map
              ("C-c =" . elixir-format)
              ("C-c n" . elixir-insert-ns)
              ("C-c , " . exunit-transient))
  )

;;; * envrc
(use-package envrc
  :init (envrc-global-mode 1))
;;; * expand region
(use-package expand-region
  :bind (("C-M-SPC" . er/expand-region))
)
;;; * files
(use-package files
  :custom
  (auto-save-default nil)
  (make-backup-files nil)
  (major-mode-remap-alist
   '((elixir-mode . elixir-ts-mode)
     (js-json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (rust-mode . rust-mode)
     )))
 ;;; * flycheck
(use-package flycheck
  :config
  (progn

    ;; ;; Fix annoying lsp-ui color for info.
    ;; (set-face-attribute 'flycheck-info nil
    ;;                 :foreground nil
    ;;                 :underline '(:color "spring green")
    ;;                 ;; :weight 'bold
    ;;                 )

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


;;; * helm
(use-package helm-mode
  :init
  (helm-mode 1)

  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("C-S-o" . helm-mini)
         ;; ("C-c s" . helm-rg)
         )
)
;;; * javascript
(use-package js
  :config
  (progn

    (defun my-js-mode-hook
        ()
      (setq js-indent-level 2)
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

;;; * lsp-mode
(use-package lsp-mode
  :init
  (setq
   lsp-eldoc-render-all nil
   lsp-ui-doc-enable t
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-hover nil
   lsp-signature-doc-lines 5
   ;; lsp-prefer-capf t
   lsp-idle-delay 0.5)
  :config
  (progn
    ;; lsp-mode tweaks https://emacs-lsp.github.io/lsp-mode/page/performance/
    ;; (setq lsp-idle-delay 0.500)
    ;; https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.rebar3?\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cargo\\'")
    (define-key lsp-mode-map (kbd "C-l") lsp-command-map)))

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
      (set-face-foreground 'magit-diff-hunk-heading "gold")
      (set-face-background 'magit-diff-hunk-heading "gray28")
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
         ("C-c m l" . magit-log-buffer-file)
         ("C-c m L" . magit-log)
         ("C-c m b" . magit-blame))
  :custom  (magit-blame-echo-style 'show-lines))

;;; * magit-blame
(use-package magit-blame)
;;; * markdown
(use-package markdown-mode
  :init
  (progn
    (add-hook 'markdown-mode-hook 'whitespace-mode))
  :bind* (("M-p" . backward-paragraph)
          ("M-n" . forward-paragraph)))

;;; * merlin
(use-package merlin
  :config
  (set-face-attribute 'merlin-compilation-error-face nil :underline '(:color "OliveDrab1" :style wave))
  ;; (set-face-background 'merlin-compilation-error-face "LavenderBlush1")
  )
;;; * multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

;;; * monokai
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t)
  (custom-theme-set-faces
   'monokai
   '(font-lock-comment-delimiter-face ((t (:foreground "Steelblue1"))))
   '(font-lock-comment-face ((t (:foreground "Steelblue1"))))
   '(cursor ((t (:background "gold")))))
  (enable-theme 'monokai))

;;; * move-lines
(use-package move-lines
  :bind (("M-<up>" . move-lines-up)
         ("M-<down>" . move-lines-down)
         ))
;;; * ocamlformat
(use-package ocamlformat
  :hook (before-save . ocamlformat-before-save))
;;; * org
(use-package org
  :config
  (progn
    (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    (setq org-reverse-note-order t)
    (setq org-src-fontify-natively t)
    (setq org-tags-match-list-sublevels nil)

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
      (setq org-hide-emphasis-markers t)
      (mixed-pitch-mode t)
      (condition-case nil
          (progn
            (require 'org-bullets)
            (org-bullets-mode 1))
        (message "org-bullets not installed")))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)))

    (setq org-agenda-files '("~/syncthing/org_notes/recipes.org"))

    (add-hook 'org-mode-hook 'my-common-org-mode-hook))

  :bind (("C-c o a" . org-agenda)
         ("C-c o o" . org-open-at-point)
         ("C-c o l" . org-store-link)
         ("C-c o L" . org-insert-link)
         ("C-c o c" . org-copy-subtree)
         ("C-c o p" . org-paste-subtree)
         )
  :custom-face
      (org-code ((t (:foreground "LightSeaGreen"))))
      (org-level-1 ((t (:foreground "goldenrod" :height 150))))
      (org-level-2 ((t (:height 150))))
  )

;;; * org-capture
(use-package org-capture
  :config
  (setq org-capture-templates
      '(("i" "recipe from internet" entry (file "~/syncthing/org_notes/recipes.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual recipe entry" entry (file "~/syncthing/org_notes/recipes.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))
  :bind (("C-c o r" . org-capture)))

;;; * org-chef
(use-package org-chef
:init
;; override the code to get the "- [ ]" prefix for each ingredient instead of just "-"

(defun org-chef-to-unordered-list-fixup (list)
  "Convert a LIST of strings into an org-element plain list"
  (if (null list)
      nil
    `(plain-list nil ,(mapcar #'(lambda (x) `(item (:bullet "- [ ]" :pre-blank 0)  ,(concat "[ ] " x))) list))))

 (eval-after-load "org-chef"
   '(defun org-chef-recipe-to-org-element (recipe)
      "Convert a RECIPE into an `org-element` AST."
      `(headline (:title ,(cdr (assoc 'name recipe)) :level 1)
                 (property-drawer nil
                                  ((node-property (:key "source-url" :value ,(cdr (assoc 'source-url recipe))))
                                   (node-property (:key "servings"   :value ,(cdr (assoc 'servings recipe))))
                                   (node-property (:key "prep-time"  :value ,(format "%s" (cdr (assoc 'prep-time recipe)))))
                                   (node-property (:key "cook-time"  :value ,(format "%s" (cdr (assoc 'cook-time recipe)))))
                                   (node-property (:key "ready-in"   :value ,(format "%s" (cdr (assoc 'ready-in recipe)))))))
                 (headline (:title "Ingredients" :level 2 :pre-blank 1)
                           ,(org-chef-to-unordered-list-fixup (cdr (assoc 'ingredients recipe))))
                 (headline (:title "Directions" :level 2 :pre-blank 1)
                           ,(org-chef-to-ordered-list (cdr (assoc 'directions recipe))))))))

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

    (defun silent-recentf-save-list (orig-fun &rest args)
      (let ((save-silently t)
            (inhibit-message t))
        (apply orig-fun args)))

    (advice-add 'recentf-save-list :around #'silent-recentf-save-list)

    (run-at-time (current-time) 60 'recentf-save-list)))

;;; * rg
(use-package rg
  :init (rg-enable-default-bindings)
;;  :bind (("C-c s" . ))
  )
;;; * rust
(use-package rust-mode
  :init
  ;; otherwise lsp does not start anymore? 2025-09-01
  (setq lsp-inline-completion-enable t)

  (setq rust-indent-offset 4)

  (setq rust-mode-treesitter-derive t)
  ;; :init
  ;; (add-hook 'eglot-managed-mode-hook
  ;;           (lambda ()
  ;;             ;; (put 'eglot-note 'flymake-overlay-control nil)
  ;;             (put 'eglot-warning 'flymake-overlay-control nil)
  ;;             (put 'eglot-error 'flymake-overlay-control nil)

  ;;             ;; Show flymake diagnostics first.
  ;;             (setq eldoc-documentation-functions (cons #'flymake-eldoc-function
  ;;                                                       (remove #'flymake-eldoc-function
  ;;                                                               eldoc-documentation-functions)))
  ;;             (eglot-inlay-hints-mode -1)))
  :config
  (progn
    (defun fix-lsp-minibuffer ()
      ;; https://github.com/emacs-lsp/lsp-mode/pull/1740#issuecomment-1776493727

      ;; do not cache the shitty result from rust-analyzer
      (advice-add #'lsp-eldoc-function :after (lambda (&rest _) (setq lsp--hover-saved-bounds nil)))

      ;; extract and show short signature for rust-analyzer
      (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
        (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
               (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
               (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
                                ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
                                (t nil)))
               (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
               (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
                                ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
                                (t (-first-item groups))))
               (sig (->> sig-group
                         (--drop-while (s-equals? "```rust" it))
                         (--take-while (not (s-equals? "```" it)))
                         (--map (s-replace-regexp "//.*" "" it))
                         (--map (s-trim it))
                         (s-join " "))))
          (lsp--render-element (concat "```rust\n" sig cmt "\n```")))))

    (defun my-rust-mode-hook ()
      (fix-lsp-minibuffer)

      ;; does not work: (setq lsp-rust-analyzer-proc-macro-enable nil)
      (lsp)
      ;; (eglot-ensure)
      (display-line-numbers-mode)
      (company-mode)
      (whitespace-mode)

      ;; (add-hook 'before-save-hook 'rust-format-buffer nil 'make-it-local)
      (setq rust-format-on-save t))

    (add-hook 'rust-ts-mode-hook 'my-rust-mode-hook)))
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
  (web-mode-toggle-current-element-highlight)
  :custom
  (web-mode-markup-indent-offset 2))
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
