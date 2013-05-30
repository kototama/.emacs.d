((ac-nrepl status "installed" recipe
           (:name ac-nrepl :description "Nrepl completion source for Emacs auto-complete package" :type github :pkgname "purcell/ac-nrepl" :depends
                  (auto-complete nrepl)
                  :features ac-nrepl))
 (ace-jump-mode status "installed" recipe
                (:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (ack-and-a-half status "installed" recipe
                 (:name ack-and-a-half :description "Yet another front-end for ack" :type github :pkgname "jhelwig/ack-and-a-half" :prepare
                        (progn
                          (defalias 'ack 'ack-and-a-half)
                          (defalias 'ack-same 'ack-and-a-half-same)
                          (defalias 'ack-find-file 'ack-and-a-half-find-file)
                          (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))))
 (auto-complete status "installed" recipe
                (:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
                       (popup fuzzy)))
 (auto-complete-emacs-lisp status "installed" recipe
                           (:name auto-complete-emacs-lisp :description "Auto-complete sources for emacs lisp" :type http :url "http://www.cx4a.org/pub/auto-complete-emacs-lisp.el" :depends auto-complete))
 (auto-indent-mode status "installed" recipe
                   (:name auto-indent-mode :website "https://github.com/mlf176f2/auto-indent-mode.el" :description "Automatically Indent  when pressing return, pasting, and other customizable features." :type github :pkgname "mlf176f2/auto-indent-mode.el"))
 (cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (cljsbuild-mode status "installed" recipe
                 (:name cljsbuild-mode :description "Minor mode for the ClojureScript 'lein cljsbuild' command" :type github :pkgname "kototama/cljsbuild-mode"))
 (clojure-mode status "installed" recipe
               (:name clojure-mode :website "https://github.com/technomancy/clojure-mode" :description "Emacs support for the Clojure language." :type github :pkgname "technomancy/clojure-mode"))
 (color-theme status "installed" recipe
              (:name color-theme :description "An Emacs-Lisp package with more than 50 color themes for your use. For questions about color-theme" :website "http://www.nongnu.org/color-theme/" :type http-tar :options
                     ("xzf")
                     :url "http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.tar.gz" :load "color-theme.el" :features "color-theme" :post-init
                     (progn
                       (color-theme-initialize)
                       (setq color-theme-is-global t))))
 (control-lock status "installed" recipe
               (:name control-lock :auto-generated t :type emacswiki :description "Like caps-lock, but for your control key.  Give your pinky a rest!" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/control-lock.el"))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (elisp-slime-nav status "installed" recipe
                  (:name elisp-slime-nav :type github :pkgname "purcell/elisp-slime-nav" :description "Slime-style navigation for Emacs Lisp" :prepare
                         (add-hook 'emacs-lisp-mode-hook
                                   (defun turn-elisp-slime-nav-on nil
                                     (elisp-slime-nav-mode t)))))
 (expand-region status "installed" recipe
                (:name expand-region :type github :pkgname "magnars/expand-region.el" :description "Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want." :website "https://github.com/magnars/expand-region.el#readme" :features expand-region))
 (fuzzy status "installed" recipe
        (:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (haskell-mode status "installed" recipe
               (:name haskell-mode :description "A Haskell editing mode" :type github :pkgname "haskell/haskell-mode" :load "haskell-site-file.el" :post-init
                      (progn
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))
 (highlight status "installed" recipe
            (:name highlight :auto-generated t :type emacswiki :description "Highlighting commands." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/highlight.el"))
 (highlight-parentheses status "installed" recipe
                        (:name highlight-parentheses :description "Highlight the matching parentheses surrounding point." :type http :url "http://nschum.de/src/emacs/highlight-parentheses/highlight-parentheses.el" :features highlight-parentheses))
 (highlight-sexp status "installed" recipe
                 (:name highlight-sexp :auto-generated t :type emacswiki :description "" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/highlight-sexp.el"))
 (ido-ubiquitous status "installed" recipe
                 (:name ido-ubiquitous :description "Use ido (nearly) everywhere" :type elpa))
 (magit status "installed" recipe
        (:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :depends
               (cl-lib)))
 (markdown-mode status "installed" recipe
                (:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :type git :url "git://jblevins.org/git/markdown-mode.git" :before
                       (add-to-list 'auto-mode-alist
                                    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (maxframe status "installed" recipe
           (:name maxframe :description "Maxframe provides the ability to maximize the emacs frame and stay within the display resolution." :type github :pkgname "rmm5t/maxframe.el" :prepare
                  (progn
                    (autoload 'maximize-frame "maxframe" "Maximizes the frame to fit the display if under a windowing\nsystem." t)
                    (autoload 'restore-frame "maxframe" "Restores a maximized frame.  See `maximize-frame'." t))))
 (multi-term status "installed" recipe
             (:name multi-term :description "A mode based on term.el, for managing multiple terminal buffers in Emacs." :type emacswiki :features multi-term))
 (multiple-cursors status "installed" recipe
                   (:name multiple-cursors :description "An experiment in adding multiple cursors to emacs" :type github :pkgname "magnars/multiple-cursors.el" :features multiple-cursors))
 (notmuch status "installed" recipe
          (:name notmuch :description "Run notmuch(thread-based email index, search and tagging) within emacs" :type git :url "git://notmuchmail.org/git/notmuch" :load-path
                 ("./emacs")
                 :build
                 ("./configure" "make")))
 (nrepl status "installed" recipe
        (:name nrepl :description "An Emacs client for nREPL, the Clojure networked REPL server." :type github :pkgname "kingtim/nrepl.el" :depends clojure-mode))
 (nrepl-eval-sexp-fu status "installed" recipe
                     (:name nrepl-eval-sexp-fu :description "Fancy highlighting of sexps on eval" :type github :pkgname "samaaron/nrepl-eval-sexp-fu" :features nrepl-eval-sexp-fu))
 (nyan-mode status "installed" recipe
            (:name nyan-mode :description "Nyan Cat for Emacs! Nyanyanyanyanyanyanyanyanyan!" :type github :pkgname "TeMPOraL/nyan-mode" :features nyan-mode))
 (offlineimap status "installed" recipe
              (:name offlineimap :description "Run OfflineIMAP from Emacs" :type git :url "git://git.naquadah.org/offlineimap-el.git" :features offlineimap))
 (org-mode status "installed" recipe
           (:name org-mode :website "http://orgmode.org/" :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system." :type git :url "git://orgmode.org/org-mode.git" :info "doc" :build/berkeley-unix `,(mapcar
                                                                                                                                                                                                                                                                                                       (lambda
                                                                                                                                                                                                                                                                                                         (target)
                                                                                                                                                                                                                                                                                                         (list "gmake" target
                                                                                                                                                                                                                                                                                                               (concat "EMACS="
                                                                                                                                                                                                                                                                                                                       (shell-quote-argument el-get-emacs))))
                                                                                                                                                                                                                                                                                                       '("oldorg"))
                  :build `,(mapcar
                            (lambda
                              (target)
                              (list "make" target
                                    (concat "EMACS="
                                            (shell-quote-argument el-get-emacs))))
                            '("oldorg"))
                  :load-path
                  ("." "lisp" "contrib/lisp")))
 (package status "installed" recipe
          (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin "24" :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
                 (progn
                   (setq package-user-dir
                         (expand-file-name
                          (convert-standard-filename
                           (concat
                            (file-name-as-directory default-directory)
                            "elpa")))
                         package-directory-list
                         (list
                          (file-name-as-directory package-user-dir)
                          "/usr/share/emacs/site-lisp/elpa/"))
                   (make-directory package-user-dir t)
                   (unless
                       (boundp 'package-subdirectory-regexp)
                     (defconst package-subdirectory-regexp "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$" "Regular expression matching the name of\n a package subdirectory. The first subexpression is the package\n name. The second subexpression is the version string."))
                   (setq package-archives
                         '(("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (paredit status "installed" recipe
          (:name paredit :description "Minor mode for editing parentheses" :type http :prepare
                 (progn
                   (autoload 'enable-paredit-mode "paredit")
                   (autoload 'disable-paredit-mode "paredit"))
                 :url "http://mumble.net/~campbell/emacs/paredit.el"))
 (popup status "installed" recipe
        (:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (powerline status "installed" recipe
            (:name powerline :website "https://github.com/jonathanchu/emacs-powerline" :description "Powerline for Emacs" :type github :pkgname "jonathanchu/emacs-powerline" :features powerline))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el" :features s))
 (undo-tree status "installed" recipe
            (:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
                   (progn
                     (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
                     (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t))))
 (visual-regexp status "installed" recipe
                (:name visual-regexp :description "A regexp/replace command for Emacs with\n       interactive visual feedback" :type github :depends cl-lib :pkgname "benma/visual-regexp.el"))
 (wgrep status "installed" recipe
        (:name wgrep :auto-generated t :type emacswiki :description "Writable grep buffer and apply the changes to files" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/wgrep.el"))
 (yasnippet status "installed" recipe
            (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :features "yasnippet" :pre-init
                   (unless
                       (or
                        (boundp 'yas/snippet-dirs)
                        (get 'yas/snippet-dirs 'customized-value))
                     (setq yas/snippet-dirs
                           (list
                            (concat el-get-dir
                                    (file-name-as-directory "yasnippet")
                                    "snippets"))))
                   :post-init
                   (put 'yas/snippet-dirs 'standard-value
                        (list
                         (list 'quote
                               (list
                                (concat el-get-dir
                                        (file-name-as-directory "yasnippet")
                                        "snippets")))))
                   :compile nil :submodule nil)))
