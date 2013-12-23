;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ac-nrepl-popup-doc ac-nrepl-setup) "ac-nrepl/ac-nrepl"
;;;;;;  "ac-nrepl/ac-nrepl.el" (21170 10628 693217 834000))
;;; Generated autoloads from ac-nrepl/ac-nrepl.el

(defface ac-nrepl-candidate-face '((t (:inherit ac-candidate-face))) "\
Face for nrepl candidates." :group (quote auto-complete))

(defface ac-nrepl-selection-face '((t (:inherit ac-selection-face))) "\
Face for the nrepl selected candidate." :group (quote auto-complete))

(defconst ac-nrepl-source-defaults '((available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (prefix . ac-nrepl-symbol-start-pos) (document . ac-nrepl-documentation)) "\
Defaults common to the various completion sources.")

(defvar ac-source-nrepl-ns (append '((candidates . ac-nrepl-candidates-ns) (symbol . "n")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl ns completion.")

(defvar ac-source-nrepl-vars (append '((candidates . ac-nrepl-candidates-vars) (symbol . "v")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl var completion.")

(defvar ac-source-nrepl-ns-classes (append '((candidates . ac-nrepl-candidates-ns-classes) (symbol . "c")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl ns-specific class completion.")

(defvar ac-source-nrepl-all-classes (append '((candidates . ac-nrepl-candidates-all-classes) (symbol . "c")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl all class completion.")

(defvar ac-source-nrepl-java-methods (append '((candidates . ac-nrepl-candidates-java-methods) (symbol . "m")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl java method completion.")

(defvar ac-source-nrepl-static-methods (append '((candidates . ac-nrepl-candidates-static-methods) (symbol . "s")) ac-nrepl-source-defaults) "\
Auto-complete source for nrepl java static method completion.")

(autoload 'ac-nrepl-setup "ac-nrepl/ac-nrepl" "\
Add the nrepl completion source to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

(autoload 'ac-nrepl-popup-doc "ac-nrepl/ac-nrepl" "\
A popup alternative to `nrepl-doc'.

\(fn)" t nil)

;;;***

;;;### (autoloads (ace-jump-mode ace-jump-line-mode ace-jump-word-mode
;;;;;;  ace-jump-char-mode ace-jump-mode-pop-mark) "ace-jump-mode/ace-jump-mode"
;;;;;;  "ace-jump-mode/ace-jump-mode.el" (21170 10632 957217 911000))
;;; Generated autoloads from ace-jump-mode/ace-jump-mode.el

(autoload 'ace-jump-mode-pop-mark "ace-jump-mode/ace-jump-mode" "\
Pop up a postion from `ace-jump-mode-mark-ring', and jump back to that position

\(fn)" t nil)

(autoload 'ace-jump-char-mode "ace-jump-mode/ace-jump-mode" "\
AceJump char mode

\(fn QUERY-CHAR)" t nil)

(autoload 'ace-jump-word-mode "ace-jump-mode/ace-jump-mode" "\
AceJump word mode.
You can set `ace-jump-word-mode-use-query-char' to nil to prevent
asking for a head char, that will mark all the word in current
buffer.

\(fn HEAD-CHAR)" t nil)

(autoload 'ace-jump-line-mode "ace-jump-mode/ace-jump-mode" "\
AceJump line mode.
Marked each no empty line and move there

\(fn)" t nil)

(autoload 'ace-jump-mode "ace-jump-mode/ace-jump-mode" "\
AceJump mode is a minor mode for you to quick jump to a
position in the curret view.
   There is three submode now:
     `ace-jump-char-mode'
     `ace-jump-word-mode'
     `ace-jump-line-mode'

You can specify the sequence about which mode should enter
by customize `ace-jump-mode-submode-list'.

If you do not want to query char for word mode, you can change
`ace-jump-word-mode-use-query-char' to nil.

If you don't like the default move keys, you can change it by
setting `ace-jump-mode-move-keys'.

You can constrol whether use the case sensitive via
`ace-jump-mode-case-fold'.

\(fn &optional PREFIX)" t nil)

;;;***

;;;### (autoloads (ack-and-a-half-find-file-same ack-and-a-half-find-file
;;;;;;  ack-and-a-half-same ack-and-a-half) "ack-and-a-half/ack-and-a-half"
;;;;;;  "ack-and-a-half/ack-and-a-half.el" (21170 10636 653217 977000))
;;; Generated autoloads from ack-and-a-half/ack-and-a-half.el

(autoload 'ack-and-a-half "ack-and-a-half/ack-and-a-half" "\
Run ack.
PATTERN is interpreted as a regular expression, iff REGEXP is
non-nil.  If called interactively, the value of REGEXP is
determined by `ack-and-a-half-regexp-search'.  A prefix arg
toggles the behavior.  DIRECTORY is the root directory.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns'.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.

\(fn PATTERN &optional REGEXP DIRECTORY)" t nil)

(autoload 'ack-and-a-half-same "ack-and-a-half/ack-and-a-half" "\
Run ack with --type matching the current `major-mode'.
The types of files searched are determined by
`ack-and-a-half-mode-type-alist' and
`ack-and-a-half-mode-extension-alist'.  If no type is configured,
the buffer's file extension is used for the search.  PATTERN is
interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by
`ack-and-a-half-regexp-search'.  A prefix arg toggles that value.
DIRECTORY is the directory in which to start searching.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns`.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.`

\(fn PATTERN &optional REGEXP DIRECTORY)" t nil)

(autoload 'ack-and-a-half-find-file "ack-and-a-half/ack-and-a-half" "\
Prompt to find a file found by ack in DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'ack-and-a-half-find-file-same "ack-and-a-half/ack-and-a-half" "\
Prompt to find a file found by ack in DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads (auto-complete) "auto-complete/auto-complete" "auto-complete/auto-complete.el"
;;;;;;  (21170 10586 417217 75000))
;;; Generated autoloads from auto-complete/auto-complete.el

(autoload 'auto-complete "auto-complete/auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

;;;***

;;;### (autoloads (auto-indent-global-mode auto-indent-mode-on auto-indent-mode
;;;;;;  auto-indent-eol-char-newline auto-indent-eol-newline) "auto-indent-mode/auto-indent-mode"
;;;;;;  "auto-indent-mode/auto-indent-mode.el" (21170 10641 29218
;;;;;;  56000))
;;; Generated autoloads from auto-indent-mode/auto-indent-mode.el

(autoload 'auto-indent-eol-newline "auto-indent-mode/auto-indent-mode" "\
Auto-indent function for `end-of-line' and then newline.

\(fn)" t nil)

(autoload 'auto-indent-eol-char-newline "auto-indent-mode/auto-indent-mode" "\
Auto-indent function for `end-of-line', insert `auto-indent-eol-char', and then newline.

\(fn)" t nil)

(autoload 'auto-indent-mode "auto-indent-mode/auto-indent-mode" "\
Auto Indent minor mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When auto-indent-mode minor mode is enabled, yanking or pasting automatically indents

Fall back to default, non-indented yanking by preceding the yanking commands with C-u.

Based on auto-indentation posts, slightly redefined to allow it to be a minor mode

http://www.emacswiki.org/emacs/AutoIndentation

\(fn &optional ARG)" t nil)

(autoload 'auto-indent-mode-on "auto-indent-mode/auto-indent-mode" "\
Turn on auto-indent minor mode.

\(fn)" t nil)

(defvar auto-indent-global-mode nil "\
Non-nil if Auto-Indent-Global mode is enabled.
See the command `auto-indent-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `auto-indent-global-mode'.")

(custom-autoload 'auto-indent-global-mode "auto-indent-mode/auto-indent-mode" nil)

(autoload 'auto-indent-global-mode "auto-indent-mode/auto-indent-mode" "\
Toggle Auto-Indent mode in all buffers.
With prefix ARG, enable Auto-Indent-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Indent mode is enabled in all buffers where
`auto-indent-mode-on' would do it.
See `auto-indent-mode' for more information on Auto-Indent mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cider cider-jack-in) "cider/cider" "cider/cider.el"
;;;;;;  (21170 10623 769217 746000))
;;; Generated autoloads from cider/cider.el

(autoload 'cider-jack-in "cider/cider" "\
Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(autoload 'cider "cider/cider" "\
Connect to an nREPL server identified by HOST and PORT.

\(fn HOST PORT)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") 'cider-jack-in) (define-key clojure-mode-map (kbd "C-c M-c") 'cider)))

;;;***

;;;### (autoloads (cider-macroexpand-all cider-macroexpand-1) "cider/cider-macroexpansion"
;;;;;;  "cider/cider-macroexpansion.el" (21170 10623 765217 746000))
;;; Generated autoloads from cider/cider-macroexpansion.el

(autoload 'cider-macroexpand-1 "cider/cider-macroexpansion" "\
Invoke 'macroexpand-1' on the expression at point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'.

\(fn &optional PREFIX)" t nil)

(autoload 'cider-macroexpand-all "cider/cider-macroexpansion" "\
Invoke 'clojure.walk/macroexpand-all' on the expression at point.

\(fn)" t nil)

;;;***

;;;### (autoloads (cider-mode) "cider/cider-mode" "cider/cider-mode.el"
;;;;;;  (21170 10623 765217 746000))
;;; Generated autoloads from cider/cider-mode.el

(autoload 'cider-mode "cider/cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cider-selector) "cider/cider-selector" "cider/cider-selector.el"
;;;;;;  (21170 10623 765217 746000))
;;; Generated autoloads from cider/cider-selector.el

(autoload 'cider-selector "cider/cider-selector" "\
Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer.  The `?' character describes then
available methods.  OTHER-WINDOW provides an optional target.

See `def-cider-selector-method' for defining new methods.

\(fn &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads (cljsbuild-auto cljsbuild-mode) "cljsbuild-mode/cljsbuild-mode"
;;;;;;  "cljsbuild-mode/cljsbuild-mode.el" (21170 10654 17218 289000))
;;; Generated autoloads from cljsbuild-mode/cljsbuild-mode.el

(autoload 'cljsbuild-mode "cljsbuild-mode/cljsbuild-mode" "\
ClojureScript Build mode

\(fn &optional ARG)" t nil)

(autoload 'cljsbuild-auto "cljsbuild-mode/cljsbuild-mode" "\
Run \"lein cljsbuild auto\" in a background buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (clojure-mode) "clojure-mode/clojure-mode" "clojure-mode/clojure-mode.el"
;;;;;;  (21170 10609 209217 484000))
;;; Generated autoloads from clojure-mode/clojure-mode.el

(autoload 'clojure-mode "clojure-mode/clojure-mode" "\
Major mode for editing Clojure code - similar to Lisp mode.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.clj[sx]?\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.dtm\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

(add-to-list 'interpreter-mode-alist '("jark" . clojure-mode))

(add-to-list 'interpreter-mode-alist '("cake" . clojure-mode))

;;;***

;;;### (autoloads (clojure-find-clojure-test clojure-test-mode) "clojure-mode/clojure-test-mode"
;;;;;;  "clojure-mode/clojure-test-mode.el" (21170 10609 209217 484000))
;;; Generated autoloads from clojure-mode/clojure-test-mode.el

(autoload 'clojure-test-mode "clojure-mode/clojure-test-mode" "\
A minor mode for running Clojure tests.

\\{clojure-test-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'clojure-find-clojure-test "clojure-mode/clojure-test-mode" "\


\(fn)" nil nil)

(defun clojure-test-maybe-enable nil "\
Enable clojure-test-mode if the current buffer contains a \"clojure.test\" bit in it." (when (clojure-find-clojure-test) (save-window-excursion (clojure-test-mode t))))

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;;;***

;;;### (autoloads (color-theme-initialize color-theme-submit color-theme-install
;;;;;;  color-theme-compare color-theme-make-snapshot color-theme-analyze-defun
;;;;;;  color-theme-print color-theme-install-at-point-for-current-frame
;;;;;;  color-theme-install-at-mouse color-theme-describe color-theme-select)
;;;;;;  "color-theme/color-theme" "color-theme/color-theme.el" (17529
;;;;;;  41105 0 0))
;;; Generated autoloads from color-theme/color-theme.el

(autoload 'color-theme-select "color-theme/color-theme" "\
Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors.

\(fn &optional ARG)" t nil)

(autoload 'color-theme-describe "color-theme/color-theme" "\
Describe color theme listed at point.
This shows the documentation of the value of text-property color-theme
at point.  The text-property color-theme should be a color theme
function.  See `color-themes'.

\(fn)" t nil)

(autoload 'color-theme-install-at-mouse "color-theme/color-theme" "\
Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`color-theme-install-at-point' is called.

\(fn EVENT)" t nil)

(autoload 'color-theme-install-at-point-for-current-frame "color-theme/color-theme" "\
Install color theme at point for current frame only.
Binds `color-theme-is-global' to nil and calls
`color-theme-install-at-point'.

\(fn)" t nil)

(autoload 'color-theme-print "color-theme/color-theme" "\
Print the current color theme function.

You can contribute this function to <URL:news:gnu.emacs.sources> or
paste it into your .emacs file and call it.  That should recreate all
the settings necessary for your color theme.

Example:

    (require 'color-theme)
    (defun my-color-theme ()
      \"Color theme by Alex Schroeder, created 2000-05-17.\"
      (interactive)
      (color-theme-install
       '(...
	 ...
	 ...)))
    (my-color-theme)

If you want to use a specific color theme function, you can call the
color theme function in your .emacs directly.

Example:

    (require 'color-theme)
    (color-theme-gnome2)

\(fn &optional BUF)" t nil)

(autoload 'color-theme-analyze-defun "color-theme/color-theme" "\
Once you have a color-theme printed, check for missing faces.
This is used by maintainers who receive a color-theme submission
and want to make sure it follows the guidelines by the color-theme
author.

\(fn)" t nil)

(autoload 'color-theme-make-snapshot "color-theme/color-theme" "\
Return the definition of the current color-theme.
The function returned will recreate the color-theme in use at the moment.

\(fn)" nil nil)

(autoload 'color-theme-compare "color-theme/color-theme" "\
Compare two color themes.
This will print the differences between installing THEME-A and
installing THEME-B.  Note that the order is important: If a face is
defined in THEME-A and not in THEME-B, then this will not show up as a
difference, because there is no reset before installing THEME-B.  If a
face is defined in THEME-B and not in THEME-A, then this will show up as
a difference.

\(fn THEME-A THEME-B)" t nil)

(autoload 'color-theme-install "color-theme/color-theme" "\
Install a color theme defined by frame parameters, variables and faces.

The theme is installed for all present and future frames; any missing
faces are created.  See `color-theme-install-faces'.

THEME is a color theme definition.  See below for more information.

If you want to install a color theme from your .emacs, use the output
generated by `color-theme-print'.  This produces color theme function
which you can copy to your .emacs.

A color theme definition is a list:
\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)

FUNCTION is the color theme function which called `color-theme-install'.
This is no longer used.  There was a time when this package supported
automatic factoring of color themes.  This has been abandoned.

FRAME-PARAMETERS is an alist of frame parameters.  These are installed
with `color-theme-install-frame-params'.  These are installed last such
that any changes to the default face can be changed by the frame
parameters.

VARIABLE-DEFINITIONS is an alist of variable settings.  These are
installed with `color-theme-install-variables'.

FACE-DEFINITIONS is an alist of face definitions.  These are installed
with `color-theme-install-faces'.

If `color-theme-is-cumulative' is nil, a color theme will undo face and
frame-parameter settings of previous color themes.

\(fn THEME)" nil nil)

(autoload 'color-theme-submit "color-theme/color-theme" "\
Submit your color-theme to the maintainer.

\(fn)" t nil)

(autoload 'color-theme-initialize "color-theme/color-theme" "\
Initialize the color theme package by loading color-theme-libraries.

\(fn)" t nil)

;;;***

;;;### (autoloads (diminished-modes diminish-undo diminish) "diminish/diminish"
;;;;;;  "diminish/diminish.el" (21170 10660 257218 401000))
;;; Generated autoloads from diminish/diminish.el

(autoload 'diminish "diminish/diminish" "\
Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\").

Interactively, enter (with completion) the name of any minor mode, followed
on the next line by what you want it diminished to (default empty string).
The response to neither prompt should be quoted.  However, in Lisp code,
both args must be quoted, the first as a symbol, the second as a string,
as in (diminish 'jiggle-mode \" Jgl\").

The mode-line displays of minor modes usually begin with a space, so
the modes' names appear as separate words on the mode line.  However, if
you're having problems with a cramped mode line, you may choose to use single
letters for some modes, without leading spaces.  Capitalizing them works
best; if you then diminish some mode to \"X\" but have abbrev-mode enabled as
well, you'll get a display like \"AbbrevX\".  This function prepends a space
to TO-WHAT if it's > 1 char long & doesn't already begin with a space.

\(fn MODE &optional TO-WHAT)" t nil)

(autoload 'diminish-undo "diminish/diminish" "\
Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked M-x diminish).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo 'diminished-modes).

\(fn MODE)" t nil)

(autoload 'diminished-modes "diminish/diminish" "\
Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor.

\(fn)" t nil)

;;;***

;;;### (autoloads (el-get-checksum el-get-make-recipes el-get-cd
;;;;;;  el-get-self-update el-get-update-packages-of-type el-get-update-all
;;;;;;  el-get-version) "el-get/el-get" "el-get/el-get.el" (21170
;;;;;;  10449 561214 616000))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-update-packages-of-type "el-get/el-get" "\
Update all installed packages of type TYPE.

\(fn TYPE)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE &optional PACKAGE-STATUS-ALIST)" t nil)

;;;***

;;;### (autoloads (el-get-list-packages) "el-get/el-get-list-packages"
;;;;;;  "el-get/el-get-list-packages.el" (21170 10449 557214 616000))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads (elisp-slime-nav-describe-elisp-thing-at-point
;;;;;;  elisp-slime-nav-find-elisp-thing-at-point turn-on-elisp-slime-nav-mode
;;;;;;  elisp-slime-nav-mode) "elisp-slime-nav/elisp-slime-nav" "elisp-slime-nav/elisp-slime-nav.el"
;;;;;;  (21170 10663 673218 462000))
;;; Generated autoloads from elisp-slime-nav/elisp-slime-nav.el

(autoload 'elisp-slime-nav-mode "elisp-slime-nav/elisp-slime-nav" "\
Enable Slime-style navigation of elisp symbols using M-. and M-,

\(fn &optional ARG)" t nil)

(autoload 'turn-on-elisp-slime-nav-mode "elisp-slime-nav/elisp-slime-nav" "\
Explicitly enable `elisp-slime-nav-mode'.

\(fn)" nil nil)

(autoload 'elisp-slime-nav-find-elisp-thing-at-point "elisp-slime-nav/elisp-slime-nav" "\
Jump to the elisp thing at point, be it a function, variable, library or face.
With a prefix arg, prompt for the symbol to jump to.
Argument SYM-NAME thing to find.

\(fn SYM-NAME)" t nil)

(autoload 'elisp-slime-nav-describe-elisp-thing-at-point "elisp-slime-nav/elisp-slime-nav" "\
Display the full documentation of the elisp thing at point.
The named subject may be a function, variable, library or face.
With a prefix arg, prompt for the symbol to jump to.
Argument SYM-NAME thing to find.

\(fn SYM-NAME)" t nil)

;;;***

;;;### (autoloads (async-start async-start-process) "emacs-async/async"
;;;;;;  "emacs-async/async.el" (21170 10667 317218 528000))
;;; Generated autoloads from emacs-async/async.el

(autoload 'async-start-process "emacs-async/async" "\
Start the executable PROGRAM asynchronously.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.

\(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)" nil nil)

(autoload 'async-start "emacs-async/async" "\
Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value form the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     'ignore)

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'.

\(fn START-FUNC &optional FINISH-FUNC)" nil t)

;;;***

;;;### (autoloads (er/expand-region) "expand-region/expand-region"
;;;;;;  "expand-region/expand-region.el" (21170 10672 245218 616000))
;;; Generated autoloads from expand-region/expand-region.el

(autoload 'er/expand-region "expand-region/expand-region" "\
Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (expand-region-exclude-text-mode-expansions expand-region-reset-fast-key
;;;;;;  expand-region-contract-fast-key expand-region-fast-keys-enabled
;;;;;;  expand-region-skip-whitespace expand-region-autocopy-register
;;;;;;  expand-region-guess-python-mode expand-region-preferred-python-mode
;;;;;;  expand-region) "expand-region/expand-region-custom" "expand-region/expand-region-custom.el"
;;;;;;  (21170 10672 245218 616000))
;;; Generated autoloads from expand-region/expand-region-custom.el

(let ((loads (get 'expand-region 'custom-loads))) (if (member '"expand-region/expand-region-custom" loads) nil (put 'expand-region 'custom-loads (cons '"expand-region/expand-region-custom" loads))))

(defvar expand-region-preferred-python-mode 'python "\
The name of your preferred python mode")

(custom-autoload 'expand-region-preferred-python-mode "expand-region/expand-region-custom" t)

(defvar expand-region-guess-python-mode t "\
If expand-region should attempt to guess your preferred python mode")

(custom-autoload 'expand-region-guess-python-mode "expand-region/expand-region-custom" t)

(defvar expand-region-autocopy-register "" "\
If set to a string of a single character (try \"e\"), then the
contents of the most recent expand or contract command will
always be copied to the register named after that character.")

(custom-autoload 'expand-region-autocopy-register "expand-region/expand-region-custom" t)

(defvar expand-region-skip-whitespace t "\
If expand-region should skip past whitespace on initial expansion")

(custom-autoload 'expand-region-skip-whitespace "expand-region/expand-region-custom" t)

(defvar expand-region-fast-keys-enabled t "\
If expand-region should bind fast keys after initial expand/contract")

(custom-autoload 'expand-region-fast-keys-enabled "expand-region/expand-region-custom" t)

(defvar expand-region-contract-fast-key "-" "\
Key to use after an initial expand/contract to contract once more.")

(custom-autoload 'expand-region-contract-fast-key "expand-region/expand-region-custom" t)

(defvar expand-region-reset-fast-key "0" "\
Key to use after an initial expand/contract to undo.")

(custom-autoload 'expand-region-reset-fast-key "expand-region/expand-region-custom" t)

(defvar expand-region-exclude-text-mode-expansions '(html-mode nxml-mode) "\
List of modes which derive from `text-mode' for which text mode expansions are not appropriate.")

(custom-autoload 'expand-region-exclude-text-mode-expansions "expand-region/expand-region-custom" t)

;;;***

;;;### (autoloads (turn-on-fci-mode fci-mode) "fill-column-indicator/fill-column-indicator"
;;;;;;  "fill-column-indicator/fill-column-indicator.el" (21170 10677
;;;;;;  165218 705000))
;;; Generated autoloads from fill-column-indicator/fill-column-indicator.el

(autoload 'fci-mode "fill-column-indicator/fill-column-indicator" "\
Toggle fci-mode on and off.
Fci-mode indicates the location of the fill column by drawing a
thin line (a `rule') at the fill column.

With prefix ARG, turn fci-mode on if and only if ARG is positive.

The following options control the appearance of the fill-column
rule: `fci-rule-column', `fci-rule-width', `fci-rule-color',
`fci-rule-use-dashes', `fci-dash-pattern', `fci-rule-character',
and `fci-rule-character-color'.  For further options, see the
Customization menu or the package file.  (See the latter for tips
on troubleshooting.)

\(fn &optional ARG)" t nil)

(autoload 'turn-on-fci-mode "fill-column-indicator/fill-column-indicator" "\
Turn on fci-mode unconditionally.

\(fn)" t nil)

;;;***

;;;### (autoloads (find-file-in-project) "find-file-in-project/find-file-in-project"
;;;;;;  "find-file-in-project/find-file-in-project.el" (21170 10680
;;;;;;  657218 768000))
;;; Generated autoloads from find-file-in-project/find-file-in-project.el

(autoload 'find-file-in-project "find-file-in-project/find-file-in-project" "\
Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
an `.emacs-project' file.  You can override this by locally
setting the variable `ffip-project-root'.

\(fn)" t nil)

(defalias 'ffip 'find-file-in-project)

(put 'ffip-patterns 'safe-local-variable 'listp)

(put 'ffip-find-options 'safe-local-variable 'stringp)

(put 'ffip-project-file 'safe-local-variable 'stringp)

(put 'ffip-project-root 'safe-local-variable 'stringp)

(put 'ffip-project-root-function 'safe-local-variable 'functionp)

(put 'ffip-limit 'safe-local-variable 'integerp)

;;;***

;;;### (autoloads (flx-ido-mode) "flx-ido/flx-ido" "flx-ido/flx-ido.el"
;;;;;;  (21170 10684 429218 835000))
;;; Generated autoloads from flx-ido/flx-ido.el

(defvar flx-ido-mode nil "\
Non-nil if Flx-Ido mode is enabled.
See the command `flx-ido-mode' for a description of this minor mode.")

(custom-autoload 'flx-ido-mode "flx-ido/flx-ido" nil)

(autoload 'flx-ido-mode "flx-ido/flx-ido" "\
Toggle flx ido mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (flymake-jslint-load flymake-jslint-args flymake-jslint-command
;;;;;;  flymake-jslint-detect-trailing-comma) "flymake-jslint/flymake-jslint"
;;;;;;  "flymake-jslint/flymake-jslint.el" (21170 10692 53218 972000))
;;; Generated autoloads from flymake-jslint/flymake-jslint.el

(defvar flymake-jslint-detect-trailing-comma t "\
Whether or not to report warnings about trailing commas.")

(custom-autoload 'flymake-jslint-detect-trailing-comma "flymake-jslint/flymake-jslint" t)

(defvar flymake-jslint-command (if (executable-find "jsl") "jsl" "jslint") "\
Name (and optionally full path) of jslint executable.")

(custom-autoload 'flymake-jslint-command "flymake-jslint/flymake-jslint" t)

(defvar flymake-jslint-args (unless (string-equal "jsl" flymake-jslint-command) (mapcar 'symbol-name '(--white --undef --nomen --regexp --plusplus --bitwise --newcap --sloppy --vars --eqeq))) "\
Command-line args for jslint executable.")

(custom-autoload 'flymake-jslint-args "flymake-jslint/flymake-jslint" t)

(autoload 'flymake-jslint-load "flymake-jslint/flymake-jslint" "\
Configure flymake mode to check the current buffer's javascript syntax.

\(fn)" t nil)

;;;***

;;;### (autoloads (git-commit-mode) "git-modes/git-commit-mode" "git-modes/git-commit-mode.el"
;;;;;;  (21170 10696 381219 50000))
;;; Generated autoloads from git-modes/git-commit-mode.el

(autoload 'git-commit-mode "git-modes/git-commit-mode" "\
Major mode for editing git commit messages.

This mode helps with editing git commit messages both by
providing commands to do common tasks, and by highlighting the
basic structure of and errors in git commit messages.

\(fn)" t nil)

(dolist (pattern '("/COMMIT_EDITMSG\\'" "/NOTES_EDITMSG\\'" "/MERGE_MSG\\'" "/TAG_EDITMSG\\'" "/PULLREQ_EDITMSG\\'")) (add-to-list 'auto-mode-alist (cons pattern 'git-commit-mode)))

;;;***

;;;### (autoloads (git-rebase-mode) "git-modes/git-rebase-mode" "git-modes/git-rebase-mode.el"
;;;;;;  (21170 10696 381219 50000))
;;; Generated autoloads from git-modes/git-rebase-mode.el

(autoload 'git-rebase-mode "git-modes/git-rebase-mode" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . git-rebase-mode))

;;;***

;;;### (autoloads (gitattributes-mode) "git-modes/gitattributes-mode"
;;;;;;  "git-modes/gitattributes-mode.el" (21170 10696 385219 50000))
;;; Generated autoloads from git-modes/gitattributes-mode.el

(autoload 'gitattributes-mode "git-modes/gitattributes-mode" "\
A major mode for editing .gitattributes files.
\\{gitattributes-mode-map}

\(fn)" t nil)

(dolist (pattern '("/\\.gitattributes\\'" "/\\.git/info/attributes\\'" "/git/attributes\\'")) (add-to-list 'auto-mode-alist (cons pattern #'gitattributes-mode)))

;;;***

;;;### (autoloads (gitconfig-mode) "git-modes/gitconfig-mode" "git-modes/gitconfig-mode.el"
;;;;;;  (21170 10696 385219 50000))
;;; Generated autoloads from git-modes/gitconfig-mode.el

(autoload 'gitconfig-mode "git-modes/gitconfig-mode" "\
A major mode for editing .gitconfig files.

\(fn)" t nil)

(dolist (pattern (list (rx "/.gitconfig" string-end) (rx "/.git/config" string-end) (rx "/git/config" string-end))) (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

;;;***

;;;### (autoloads (gitignore-mode) "git-modes/gitignore-mode" "git-modes/gitignore-mode.el"
;;;;;;  (21170 10696 385219 50000))
;;; Generated autoloads from git-modes/gitignore-mode.el

(autoload 'gitignore-mode "git-modes/gitignore-mode" "\
A major mode for editing .gitignore files.

\(fn)" t nil)

(dolist (pattern (list (rx "/.gitignore" string-end) (rx "/.git/info/exclude" string-end) (rx "/git/ignore" string-end))) (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

;;;***

;;;### (autoloads (god-local-mode god-mode) "god-mode/god-mode" "god-mode/god-mode.el"
;;;;;;  (21170 11209 245228 263000))
;;; Generated autoloads from god-mode/god-mode.el

(autoload 'god-mode "god-mode/god-mode" "\
Toggle global God mode.

\(fn)" t nil)

(autoload 'god-local-mode "god-mode/god-mode" "\
Minor mode for running commands.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ghc-core-mode ghc-core-create-core) "haskell-mode/ghc-core"
;;;;;;  "haskell-mode/ghc-core.el" (21170 11214 681228 361000))
;;; Generated autoloads from haskell-mode/ghc-core.el

(autoload 'ghc-core-create-core "haskell-mode/ghc-core" "\
Compile and load the current buffer as tidy core.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))

(autoload 'ghc-core-mode "haskell-mode/ghc-core" "\
Major mode for GHC Core files.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-align-imports) "haskell-mode/haskell-align-imports"
;;;;;;  "haskell-mode/haskell-align-imports.el" (21170 11214 681228
;;;;;;  361000))
;;; Generated autoloads from haskell-mode/haskell-align-imports.el

(autoload 'haskell-align-imports "haskell-mode/haskell-align-imports" "\
Align all the imports in the buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-c-mode) "haskell-mode/haskell-c" "haskell-mode/haskell-c.el"
;;;;;;  (21170 11214 681228 361000))
;;; Generated autoloads from haskell-mode/haskell-c.el

(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-c-mode))

(autoload 'haskell-c-mode "haskell-mode/haskell-c" "\
Major mode for Haskell FFI files.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-cabal-visit-file haskell-cabal-get-dir
;;;;;;  haskell-cabal-mode) "haskell-mode/haskell-cabal" "haskell-mode/haskell-cabal.el"
;;;;;;  (21170 11214 681228 361000))
;;; Generated autoloads from haskell-mode/haskell-cabal.el

(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(autoload 'haskell-cabal-mode "haskell-mode/haskell-cabal" "\
Major mode for Cabal package description files.

\(fn)" t nil)

(autoload 'haskell-cabal-get-dir "haskell-mode/haskell-cabal" "\
Get the Cabal dir for a new project. Various ways of figuring this out,
   and indeed just prompting the user. Do them all.

\(fn)" nil nil)

(autoload 'haskell-cabal-visit-file "haskell-mode/haskell-cabal" "\
Locate and visit package description file for file visited by current buffer.
This uses `haskell-cabal-find-file' to locate the closest
\".cabal\" file and open it.  This command assumes a common Cabal
project structure where the \".cabal\" file is in the top-folder
of the project, and all files related to the project are in or
below the top-folder.  If called with non-nil prefix argument
OTHER-WINDOW use `find-file-other-window'.

\(fn OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads (haskell-compile) "haskell-mode/haskell-compile"
;;;;;;  "haskell-mode/haskell-compile.el" (21170 11214 685228 361000))
;;; Generated autoloads from haskell-mode/haskell-compile.el

(autoload 'haskell-compile "haskell-mode/haskell-compile" "\
Compile the Haskell program including the current buffer.
Tries to locate the next cabal description in current or parent
folders via `haskell-cabal-find-dir' and if found, invoke
`haskell-compile-cabal-build-command' from the cabal package root
folder. If no cabal package could be detected,
`haskell-compile-command' is used instead.

If prefix argument EDIT-COMMAND is non-nil (and not a negative
prefix `-'), `haskell-compile' prompts for custom compile
command.

If EDIT-COMMAND contains the negative prefix argument `-',
`haskell-compile' calls the alternative command defined in
`haskell-compile-cabal-build-alt-command' if a cabal package was
detected.

`haskell-compile' uses `haskell-compilation-mode' which is
derived from `compilation-mode'. See Info
node `(haskell-mode)compilation' for more details.

\(fn &optional EDIT-COMMAND)" t nil)

;;;***

;;;### (autoloads (haskell-decl-scan-mode turn-on-haskell-decl-scan
;;;;;;  haskell-ds-create-imenu-index) "haskell-mode/haskell-decl-scan"
;;;;;;  "haskell-mode/haskell-decl-scan.el" (21170 11214 685228 361000))
;;; Generated autoloads from haskell-mode/haskell-decl-scan.el

(autoload 'haskell-ds-create-imenu-index "haskell-mode/haskell-decl-scan" "\
Function for finding `imenu' declarations in Haskell mode.
Finds all declarations (classes, variables, imports, instances and
datatypes) in a Haskell file for the `imenu' package.

\(fn)" nil nil)

(autoload 'turn-on-haskell-decl-scan "haskell-mode/haskell-decl-scan" "\
Unconditionally activate `haskell-decl-scan-mode'.

\(fn)" t nil)

(autoload 'haskell-decl-scan-mode "haskell-mode/haskell-decl-scan" "\
Toggle Haskell declaration scanning minor mode on or off.
With a prefix argument ARG, enable minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

See also info node `(haskell-mode)haskell-decl-scan-mode' for
more details about this minor mode.

Top-level declarations are scanned and listed in the menu item
\"Declarations\" (if enabled via option
`haskell-decl-scan-add-to-menubar').  Selecting an item from this
menu will take point to the start of the declaration.

\\[beginning-of-defun] and \\[end-of-defun] move forward and backward to the start of a declaration.

This may link with `haskell-doc-mode'.

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (set automatically by `literate-haskell-mode')
is `bird', a Bird-style literate script is assumed.  If it is nil
or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-mode-hook' on activation.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (haskell-doc-show-type haskell-doc-current-info
;;;;;;  haskell-doc-mode) "haskell-mode/haskell-doc" "haskell-mode/haskell-doc.el"
;;;;;;  (21170 11214 689228 361000))
;;; Generated autoloads from haskell-mode/haskell-doc.el

(autoload 'haskell-doc-mode "haskell-mode/haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional ARG)" t nil)

(defalias 'turn-on-haskell-doc-mode 'haskell-doc-mode)

(defalias 'turn-on-haskell-doc 'haskell-doc-mode)

(autoload 'haskell-doc-current-info "haskell-mode/haskell-doc" "\
Return the info about symbol at point.
Meant for `eldoc-documentation-function'.

\(fn)" nil nil)

(autoload 'haskell-doc-show-type "haskell-mode/haskell-doc" "\
Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

;;;***

;;;### (autoloads (haskell-font-lock-choose-keywords) "haskell-mode/haskell-font-lock"
;;;;;;  "haskell-mode/haskell-font-lock.el" (21170 11214 689228 361000))
;;; Generated autoloads from haskell-mode/haskell-font-lock.el

(autoload 'haskell-font-lock-choose-keywords "haskell-mode/haskell-font-lock" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (haskell-indent-mode turn-on-haskell-indent) "haskell-mode/haskell-indent"
;;;;;;  "haskell-mode/haskell-indent.el" (21170 11214 693228 361000))
;;; Generated autoloads from haskell-mode/haskell-indent.el

(autoload 'turn-on-haskell-indent "haskell-mode/haskell-indent" "\
Turn on ``intelligent'' Haskell indentation mode.

\(fn)" nil nil)

(autoload 'haskell-indent-mode "haskell-mode/haskell-indent" "\
``Intelligent'' Haskell indentation mode.
This deals with the layout rule of Haskell.
\\[haskell-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Invokes `haskell-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (turn-on-haskell-indentation haskell-indentation-mode)
;;;;;;  "haskell-mode/haskell-indentation" "haskell-mode/haskell-indentation.el"
;;;;;;  (21170 11214 693228 361000))
;;; Generated autoloads from haskell-mode/haskell-indentation.el

(autoload 'haskell-indentation-mode "haskell-mode/haskell-indentation" "\
Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-haskell-indentation "haskell-mode/haskell-indentation" "\
Turn on the haskell-indentation minor mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-interactive-mode-reset-error haskell-interactive-mode-echo
;;;;;;  haskell-interactive-switch haskell-interactive-bring haskell-interactive-mode)
;;;;;;  "haskell-mode/haskell-interactive-mode" "haskell-mode/haskell-interactive-mode.el"
;;;;;;  (21170 11214 693228 361000))
;;; Generated autoloads from haskell-mode/haskell-interactive-mode.el

(autoload 'haskell-interactive-mode "haskell-mode/haskell-interactive-mode" "\
Interactive mode for Haskell.

See Info node `(haskell-mode)haskell-interactive-mode' for more
information.

Key bindings:
\\{haskell-interactive-mode-map}

\(fn)" t nil)

(autoload 'haskell-interactive-bring "haskell-mode/haskell-interactive-mode" "\
Bring up the interactive mode for this session.

\(fn)" t nil)

(autoload 'haskell-interactive-switch "haskell-mode/haskell-interactive-mode" "\
Switch to the interactive mode for this session.

\(fn)" t nil)

(autoload 'haskell-interactive-mode-echo "haskell-mode/haskell-interactive-mode" "\
Echo a read only piece of text before the prompt.

\(fn SESSION MESSAGE &optional MODE)" nil nil)

(autoload 'haskell-interactive-mode-reset-error "haskell-mode/haskell-interactive-mode" "\
Reset the error cursor position.

\(fn SESSION)" t nil)

;;;***

;;;### (autoloads (haskell-menu) "haskell-mode/haskell-menu" "haskell-mode/haskell-menu.el"
;;;;;;  (21170 11214 693228 361000))
;;; Generated autoloads from haskell-mode/haskell-menu.el

(autoload 'haskell-menu "haskell-mode/haskell-menu" "\
Launch the Haskell sessions menu.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-hayoo haskell-hoogle literate-haskell-mode
;;;;;;  haskell-mode haskell-customize haskell-mode-view-news haskell-version)
;;;;;;  "haskell-mode/haskell-mode" "haskell-mode/haskell-mode.el"
;;;;;;  (21170 11214 697228 361000))
;;; Generated autoloads from haskell-mode/haskell-mode.el

(autoload 'haskell-version "haskell-mode/haskell-mode" "\
Show the `haskell-mode` version in the echo area.
With prefix argument HERE, insert it at point.
When FULL is non-nil, use a verbose version string.
When MESSAGE is non-nil, display a message with the version.

\(fn &optional HERE)" t nil)

(autoload 'haskell-mode-view-news "haskell-mode/haskell-mode" "\
Display information on recent changes to haskell-mode.

\(fn)" t nil)

(autoload 'haskell-customize "haskell-mode/haskell-mode" "\
Browse the haskell customize sub-tree.
This calls 'customize-browse' with haskell as argument and makes
sure all haskell customize definitions have been loaded.

\(fn)" t nil)

(defvar haskell-mode-map (let ((map (make-sparse-keymap))) (define-key map [24 4] 'inferior-haskell-send-decl) (define-key map [3 26] 'switch-to-haskell) (define-key map [3 12] 'inferior-haskell-load-file) (define-key map [3 2] 'switch-to-haskell) (define-key map (kbd "C-c C-t") 'inferior-haskell-type) (define-key map (kbd "C-c C-i") 'inferior-haskell-info) (define-key map (kbd "C-c M-.") 'inferior-haskell-find-definition) (define-key map (kbd "C-c C-d") 'inferior-haskell-find-haddock) (define-key map [3 22] 'haskell-check) (define-key map (kbd "C-c C-.") 'haskell-mode-format-imports) (define-key map [remap delete-indentation] 'haskell-delete-indentation) map) "\
Keymap used in Haskell mode.")

(autoload 'haskell-mode "haskell-mode/haskell-mode" "\
Major mode for editing Haskell programs.

See also Info node `(haskell-mode)Getting Started' for more
information about this mode.

\\<haskell-mode-map>
Literate scripts are supported via `literate-haskell-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `haskell-version' to find out what version of Haskell mode you are
currently using.

Additional Haskell mode modules can be hooked in via `haskell-mode-hook';
see documentation for that variable for more details.

\(fn)" t nil)

(autoload 'literate-haskell-mode "haskell-mode/haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)
(add-to-list 'auto-mode-alist        '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.l[gh]s\\'" . literate-haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(autoload 'haskell-hoogle "haskell-mode/haskell-mode" "\
Do a Hoogle search for QUERY.

\(fn QUERY)" t nil)

(defalias 'hoogle 'haskell-hoogle)

(autoload 'haskell-hayoo "haskell-mode/haskell-mode" "\
Do a Hayoo search for QUERY.

\(fn QUERY)" t nil)

(defalias 'hayoo 'haskell-hayoo)

;;;***

;;;### (autoloads (haskell-move-nested-left haskell-move-nested-right
;;;;;;  haskell-move-nested) "haskell-mode/haskell-move-nested" "haskell-mode/haskell-move-nested.el"
;;;;;;  (21170 11214 697228 361000))
;;; Generated autoloads from haskell-mode/haskell-move-nested.el

(autoload 'haskell-move-nested "haskell-mode/haskell-move-nested" "\
Shift the nested off-side-rule block adjacent to point by COLS columns to the right.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" nil nil)

(autoload 'haskell-move-nested-right "haskell-mode/haskell-move-nested" "\
Increase indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

(autoload 'haskell-move-nested-left "haskell-mode/haskell-move-nested" "\
Decrease indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

;;;***

;;;### (autoloads (haskell-navigate-imports-return haskell-navigate-imports-go
;;;;;;  haskell-navigate-imports) "haskell-mode/haskell-navigate-imports"
;;;;;;  "haskell-mode/haskell-navigate-imports.el" (21170 11214 697228
;;;;;;  361000))
;;; Generated autoloads from haskell-mode/haskell-navigate-imports.el

(autoload 'haskell-navigate-imports "haskell-mode/haskell-navigate-imports" "\
Cycle the Haskell import lines or return to point (with prefix arg).

\(fn &optional RETURN)" t nil)

(autoload 'haskell-navigate-imports-go "haskell-mode/haskell-navigate-imports" "\
Go to the first line of a list of consequtive import lines. Cycles.

\(fn)" t nil)

(autoload 'haskell-navigate-imports-return "haskell-mode/haskell-navigate-imports" "\
Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-process haskell-process-start haskell-process-cabal
;;;;;;  haskell-process-cabal-build haskell-process-load-or-reload
;;;;;;  haskell-process-reload-file haskell-process-load-file haskell-process-do-info
;;;;;;  haskell-process-do-type haskell-process-generate-tags) "haskell-mode/haskell-process"
;;;;;;  "haskell-mode/haskell-process.el" (21170 11214 697228 361000))
;;; Generated autoloads from haskell-mode/haskell-process.el

(autoload 'haskell-process-generate-tags "haskell-mode/haskell-process" "\
Regenerate the TAGS table.

\(fn &optional AND-THEN-FIND-THIS-TAG)" t nil)

(autoload 'haskell-process-do-type "haskell-mode/haskell-process" "\
Print the type of the given expression.

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-process-do-info "haskell-mode/haskell-process" "\
Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer.

\(fn &optional PROMPT-VALUE)" t nil)

(autoload 'haskell-process-load-file "haskell-mode/haskell-process" "\
Load the current buffer file.

\(fn)" t nil)

(autoload 'haskell-process-reload-file "haskell-mode/haskell-process" "\
Re-load the current buffer file.

\(fn)" t nil)

(autoload 'haskell-process-load-or-reload "haskell-mode/haskell-process" "\
Load or reload. Universal argument toggles which.

\(fn &optional TOGGLE)" t nil)

(autoload 'haskell-process-cabal-build "haskell-mode/haskell-process" "\
Build the Cabal project.

\(fn)" t nil)

(autoload 'haskell-process-cabal "haskell-mode/haskell-process" "\
Prompts for a Cabal command to run.

\(fn)" t nil)

(autoload 'haskell-process-start "haskell-mode/haskell-process" "\
Start the inferior Haskell process.

\(fn SESSION)" nil nil)

(autoload 'haskell-process "haskell-mode/haskell-process" "\
Get the current process from the current session.

\(fn)" nil nil)

;;;***

;;;### (autoloads (haskell-session-process haskell-session haskell-session-maybe
;;;;;;  haskell-session-installed-modules haskell-session-all-modules)
;;;;;;  "haskell-mode/haskell-session" "haskell-mode/haskell-session.el"
;;;;;;  (21170 11214 701228 361000))
;;; Generated autoloads from haskell-mode/haskell-session.el

(autoload 'haskell-session-all-modules "haskell-mode/haskell-session" "\
Get all modules -- installed or in the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-installed-modules "haskell-mode/haskell-session" "\
Get the modules installed in the current package set.
If DONTCREATE is non-nil don't create a new session.

\(fn &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-maybe "haskell-mode/haskell-session" "\
Maybe get the Haskell session, return nil if there isn't one.

\(fn)" nil nil)

(autoload 'haskell-session "haskell-mode/haskell-session" "\
Get the Haskell session, prompt if there isn't one or fail.

\(fn)" nil nil)

(autoload 'haskell-session-process "haskell-mode/haskell-session" "\
Get the session process.

\(fn S)" nil nil)

;;;***

;;;### (autoloads (haskell-show-parse haskell-show-parse-and-insert
;;;;;;  haskell-show-replace) "haskell-mode/haskell-show" "haskell-mode/haskell-show.el"
;;;;;;  (21170 11214 701228 361000))
;;; Generated autoloads from haskell-mode/haskell-show.el

(autoload 'haskell-show-replace "haskell-mode/haskell-show" "\
Replace the given region containing a Show value with a pretty
  printed collapsible version.

\(fn START END)" nil nil)

(autoload 'haskell-show-parse-and-insert "haskell-mode/haskell-show" "\
Parse a `string' containing a Show instance value and insert
  it pretty printed into the current buffer.

\(fn GIVEN)" nil nil)

(autoload 'haskell-show-parse "haskell-mode/haskell-show" "\
Parse the given input into a tree.

\(fn GIVEN)" nil nil)

;;;***

;;;### (autoloads (turn-on-haskell-simple-indent haskell-simple-indent-mode)
;;;;;;  "haskell-mode/haskell-simple-indent" "haskell-mode/haskell-simple-indent.el"
;;;;;;  (21170 11214 701228 361000))
;;; Generated autoloads from haskell-mode/haskell-simple-indent.el

(autoload 'haskell-simple-indent-mode "haskell-mode/haskell-simple-indent" "\
Simple Haskell indentation mode that uses simple heuristic.
In this minor mode, `indent-for-tab-command' (bound to <tab> by
default) will move the cursor to the next indent point in the
previous nonblank line, whereas `haskell-simple-indent-backtab'
\(bound to <backtab> by default) will move the cursor the
previous indent point.  An indent point is a non-whitespace
character following whitespace.

Runs `haskell-simple-indent-hook' on activation.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-haskell-simple-indent "haskell-mode/haskell-simple-indent" "\
Turn on function `haskell-simple-indent-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-sort-imports) "haskell-mode/haskell-sort-imports"
;;;;;;  "haskell-mode/haskell-sort-imports.el" (21170 11214 701228
;;;;;;  361000))
;;; Generated autoloads from haskell-mode/haskell-sort-imports.el

(autoload 'haskell-sort-imports "haskell-mode/haskell-sort-imports" "\
Sort the import list at the point.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-is-prefix-of haskell-string-take haskell-trim)
;;;;;;  "haskell-mode/haskell-string" "haskell-mode/haskell-string.el"
;;;;;;  (21170 11214 701228 361000))
;;; Generated autoloads from haskell-mode/haskell-string.el

(autoload 'haskell-trim "haskell-mode/haskell-string" "\


\(fn STRING)" nil nil)

(autoload 'haskell-string-take "haskell-mode/haskell-string" "\
Take n chars from string.

\(fn STRING N)" nil nil)

(autoload 'haskell-is-prefix-of "haskell-mode/haskell-string" "\
Is x string a prefix of y string?

\(fn X Y)" nil nil)

;;;***

;;;### (autoloads (turn-on-haskell-unicode-input-method) "haskell-mode/haskell-unicode-input-method"
;;;;;;  "haskell-mode/haskell-unicode-input-method.el" (21170 11214
;;;;;;  701228 361000))
;;; Generated autoloads from haskell-mode/haskell-unicode-input-method.el

(autoload 'turn-on-haskell-unicode-input-method "haskell-mode/haskell-unicode-input-method" "\
Set input method `haskell-unicode'.
See Info node `Unicode(haskell-mode)' for more details.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-yas-complete) "haskell-mode/haskell-yas"
;;;;;;  "haskell-mode/haskell-yas.el" (21170 11214 701228 361000))
;;; Generated autoloads from haskell-mode/haskell-yas.el

(autoload 'haskell-yas-complete "haskell-mode/haskell-yas" "\


\(fn &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (inferior-haskell-find-haddock inferior-haskell-find-definition
;;;;;;  inferior-haskell-info inferior-haskell-kind inferior-haskell-type
;;;;;;  inferior-haskell-send-decl inferior-haskell-load-and-run
;;;;;;  inferior-haskell-load-file switch-to-haskell) "haskell-mode/inf-haskell"
;;;;;;  "haskell-mode/inf-haskell.el" (21170 11214 705228 361000))
;;; Generated autoloads from haskell-mode/inf-haskell.el

(defalias 'run-haskell 'switch-to-haskell)

(autoload 'switch-to-haskell "haskell-mode/inf-haskell" "\
Show the inferior-haskell buffer.  Start the process if needed.

\(fn &optional ARG)" t nil)

(autoload 'inferior-haskell-load-file "haskell-mode/inf-haskell" "\
Pass the current buffer's file to the inferior haskell process.
If prefix arg \\[universal-argument] is given, just reload the previous file.

\(fn &optional RELOAD)" t nil)

(autoload 'inferior-haskell-load-and-run "haskell-mode/inf-haskell" "\
Pass the current buffer's file to haskell and then run a COMMAND.

\(fn COMMAND)" t nil)

(autoload 'inferior-haskell-send-decl "haskell-mode/inf-haskell" "\
Send current declaration to inferior-haskell process.

\(fn)" t nil)

(autoload 'inferior-haskell-type "haskell-mode/inf-haskell" "\
Query the haskell process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `haskell-doc-mode'.

\(fn EXPR &optional INSERT-VALUE)" t nil)

(autoload 'inferior-haskell-kind "haskell-mode/inf-haskell" "\
Query the haskell process for the kind of the given expression.

\(fn TYPE)" t nil)

(autoload 'inferior-haskell-info "haskell-mode/inf-haskell" "\
Query the haskell process for the info of the given expression.

\(fn SYM)" t nil)

(autoload 'inferior-haskell-find-definition "haskell-mode/inf-haskell" "\
Attempt to locate and jump to the definition of the given expression.

\(fn SYM)" t nil)

(autoload 'inferior-haskell-find-haddock "haskell-mode/inf-haskell" "\
Find and open the Haddock documentation of SYM.
Make sure to load the file into GHCi or Hugs first by using C-c C-l.
Only works for functions in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it.

\(fn SYM)" t nil)

;;;***

;;;### (autoloads (highlight-parentheses-mode) "highlight-parentheses/highlight-parentheses"
;;;;;;  "highlight-parentheses/highlight-parentheses.el" (21170 11227
;;;;;;  597228 593000))
;;; Generated autoloads from highlight-parentheses/highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses/highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (hlt-copy-props hlt-mouse-copy-props hlt-yank-props
;;;;;;  hlt-toggle-use-overlays-flag hlt-mouse-face-each-line hlt-highlight-single-quotations
;;;;;;  hlt-highlight-enclosing-list hlt-replace-highlight-face hlt-unhighlight-region-for-face
;;;;;;  hlt-unhighlight-regexp-to-end hlt-highlight-regexp-to-end
;;;;;;  hlt-unhighlight-regexp-region hlt-highlight-regexp-region
;;;;;;  hlt-unhighlight-region hlt-highlight-region hlt-highlight
;;;;;;  hlt-eraser-mouse hlt-highlighter-mouse hlt-eraser hlt-highlighter
;;;;;;  hlt-previous-face hlt-next-face hlt-choose-default-face hlt-default-copy/yank-props
;;;;;;  hlt-use-overlays-flag hlt-max-region-no-warning) "highlight/highlight"
;;;;;;  "highlight/highlight.el" (21170 11226 17228 565000))
;;; Generated autoloads from highlight/highlight.el

(defvar hlt-max-region-no-warning 100000 "\
*Maximum size (chars) of region to highlight without confirmation.
This is used only for highlighting of a regexp, which can be slow.")

(custom-autoload 'hlt-max-region-no-warning "highlight/highlight" t)

(defvar hlt-use-overlays-flag 'only "\
*Non-nil means use overlays to highlight; nil means use text properties.
This value also affects some actions, such as unhighlighting, for text
that is highlighted.  If the value is `only' (the default value), then
those actions only affect overlay highlighting.  Otherwise, they
affect both kinds of highlighting.")

(custom-autoload 'hlt-use-overlays-flag "highlight/highlight" t)

(defvar hlt-default-copy/yank-props '(face) "\
*Properties that `hlt-copy-props' and `hlt-yank-props' use by default.
You can use a prefix argument with those commands to override the
default behavior.
Either a list of properties (symbols) or `t', meaning all properties.")

(custom-autoload 'hlt-default-copy/yank-props "highlight/highlight" t)

(autoload 'hlt-choose-default-face "highlight/highlight" "\
Choose a face for highlighting.
Set `hlt-last-face' to the face, and return it.

You can choose a face name or a color name.  If a color is chosen, it
is used for the face background.  The face foreground is determined by
the value of `hlt-auto-face-foreground'.

\(fn FACE)" t nil)

(autoload 'hlt-next-face "highlight/highlight" "\
Choose the next face for highlighting and unhighlighting.
Use `hlt-auto-face-backgrounds' and `hlt-auto-face-foreground'.

A non-negative numeric prefix arg N means use the Nth entry of
`hlt-auto-face-backgrounds'.  Counting is 0-based.

A negative numeric prefix arg N means count from the end, not the
beginning, of `hlt-auto-face-backgrounds': -1 means the last entry, -2
means the next-to-last, etc. (counting from the end is 1-based, not
0-based).

From LISP, if the first argument is `previous' then choose the
previous face, not the next one.

\(fn &optional FACE-NB MSGP)" t nil)

(autoload 'hlt-previous-face "highlight/highlight" "\
Like `hlt-next-face', but previous, not next.
Use of a numeric prefix arg is the same as for `hlt-next-face'.

\(fn &optional FACE-NB MSGP)" t nil)

(autoload 'hlt-highlighter "highlight/highlight" "\
Highlight the text you drag the mouse over.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose the default
face to use.

If `hlt-auto-faces-flag' is non-nil then this command cycles to the
next color/face.  This is the case even if you do not drag the
mouse (empty highlight).  A message tells you what the face is.

A numeric prefix arg N means use the face represented by the Nth entry
of `hlt-auto-face-backgrounds' (uses `hlt-next-face').

\(fn START-EVENT &optional FACE-NB)" t nil)

(autoload 'hlt-eraser "highlight/highlight" "\
Erase highlights that you click or drag the mouse over.
If `hlt-use-overlays-flag' is non-nil, then remove overlay
highlighting for the last face that was used for highlighting.  (You
can use command `hlt-choose-default-face' first to choose a different
face.)

If `hlt-use-overlays-flag' is not `only', then remove text-property
highlighting for *ALL* faces (not just highlighting faces).  This
means, in particular, that a value of nil erases both overlays for the
last face and text properties for all faces.

With a numeric prefix arg N, if `hlt-use-overlays-flag' is `only',
erase the face represented by the Nth entry of
`hlt-auto-face-backgrounds' (uses `hlt-next-face').

\(fn START-EVENT &optional FACE-NB)" t nil)

(autoload 'hlt-highlighter-mouse "highlight/highlight" "\
Same as `hlt-highlighter', but for binding to a menu item.

\(fn)" t nil)

(autoload 'hlt-eraser-mouse "highlight/highlight" "\
Same as `hlt-eraser', but for binding to a menu item.

\(fn)" t nil)

(autoload 'hlt-highlight "highlight/highlight" "\
Highlight or unhighlight.
If the region is not active or it is empty, then use the whole buffer.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face.

This is several commands rolled into one, depending on the prefix arg:

* No prefix arg: highlight all text in region/buffer
* Plain prefix arg (`C-u') or zero prefix arg (`C-0'): UNhighlight all
* Positive prefix arg (`C-1'): highlight regexp matches
* Negative prefix arg (`C--'): UNhighlight regexp matches

You can also used the individual commands:

* `hlt-highlight-region'          - same as no prefix arg
* `hlt-unhighlight-region'        - same as `C-u' or `C-0'
* `hlt-highlight-regexp-region'   - same as `C-1'
* `hlt-unhighlight-regexp-region' - same as `C--'

\(fn &optional PREFIX)" t nil)

(autoload 'hlt-highlight-region "highlight/highlight" "\
Highlight either the region/buffer or new input that you type.
Use the region if active, or the buffer otherwise.

If *ALL* of the following are true then apply the last-used face as a
text property to the next and subsequent characters that you type, and
add that face to a Facemenu menu (`Text Properties' or one of its
submenus):

 * You call this command interactively.
 * You use no prefix arg.
 * Option `hlt-use-overlays-flag' is nil
 * The last property used for highlighting was `face'.

Otherwise, the behavior respects `hlt-use-overlays-flag' and depends
on the optional arguments, as follows:

 Args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.

 Third arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)

 Fourth arg MSG-P non-nil means to display a progress message.
  Interactively, MSG-P is t.

 Fifth arg MOUSE-P non-nil means use `mouse-face', not `face'.
  Interactively, MOUSE-P is provided by the prefix arg.

\(fn &optional START END FACE MSG-P MOUSE-P)" t nil)

(autoload 'hlt-unhighlight-region "highlight/highlight" "\
Remove all highlighting in region or buffer.
Use the region if active, or the buffer otherwise.
The arguments are the same as those for `hlt-highlight-region'.

If `hlt-use-overlays-flag' is non-nil, then remove overlay highlighting.
If `hlt-use-overlays-flag' is not `only', then remove text-property
highlighting.  This means, in particular, that a value of nil removes
both overlays and text properties.

\(fn &optional START END FACE MSG-P MOUSE-P)" t nil)

(autoload 'hlt-highlight-regexp-region "highlight/highlight" "\
Highlight regular expression REGEXP in region/buffer.
Use the region if active, or the buffer otherwise.

Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 4th arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 5th arg MSG-P:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 6th arg MOUSE-P non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 7th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)

\(fn &optional START END REGEXP FACE MSG-P MOUSE-P NTH)" t nil)

(autoload 'hlt-unhighlight-regexp-region "highlight/highlight" "\
Unhighlight text matching regular expression REGEXP in region/buffer.
This is like `hlt-highlight-regexp-region' (which see), but opposite.
Where `hlt-highlight-regexp-region' highlights REGEXP matches, this
unhighlights the matches.

\(fn &optional START END REGEXP FACE MSG-P MOUSE-P NTH)" t nil)

(autoload 'hlt-highlight-regexp-to-end "highlight/highlight" "\
Highlight text after cursor that matches REGEXP.
The behavior respects `hlt-use-overlays-flag' and depends on the
optional arguments, as follows:

 Optional 2nd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different
  face.)

 Optional 3rd arg MSG-P non-nil means to display a progress message.
  Interactively, MSG-P is t.

 Optional 4th arg MOUSE-P non-nil means use property `mouse-face', not
 `face'.  Interactively, MOUSE-P is provided by the prefix arg.

 Optional 5th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)

\(fn REGEXP &optional FACE MSG-P MOUSE-P NTH)" t nil)

(autoload 'hlt-unhighlight-regexp-to-end "highlight/highlight" "\
UNhighlight text after cursor that matches REGEXP.
This is like `hlt-highlight-regexp-to-end' (which see), but opposite.
Where `hlt-highlight-regexp-to-end' highlights REGEXP matches, this
unhighlights the matches.

\(fn REGEXP &optional FACE MSG-P MOUSE-P NTH)" t nil)

(autoload 'hlt-unhighlight-region-for-face "highlight/highlight" "\
Remove any highlighting in the region that uses FACE.
Same as `hlt-unhighlight-region', but removes only highlighting
that uses FACE.  Interactively, you are prompted for the face.

This works only for overlay highlighting, not text-property
highlighting.

Note: When text in the region has been highlighted using more than one
face, unhighlighting for one of those faces can mean that adjacent
highlighting outside the region appears to change.  That outside text
still has the same multiple-overlay face highlighting, but the overlay
stacking order is not the same as it was.

Optional arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg.

\(fn &optional FACE START END MOUSE-P)" t nil)

(autoload 'hlt-replace-highlight-face "highlight/highlight" "\
Replace OLD-FACE by NEW-FACE in overlay highlighting in the region.
This command applies only to overlay highlighting created by library
`highlight.el'.

Update the last-used highlighting face.

With a prefix argument, replace OLD-FACE as the `mouse-face' property,
 not the `face' property.

Other arguments:
 Optional args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.
 Optional arg MSG-P non-nil means display a progress message.
 Optional arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg.

\(fn OLD-FACE NEW-FACE &optional START END MSG-P MOUSE-P)" t nil)

(autoload 'hlt-highlight-enclosing-list "highlight/highlight" "\
Highlight the ARGth level sexp enclosing point.
ARG is the numeric prefix value.

A negative prefix arg prompts you for the face to use.  This face is
used by default from then on.  You can also choose the default face
using command `hlt-choose-default-face'.  The same face is used as the
default for all `hlt-*' functions.

When used in Lisp code:
 MOUSEP non-nil means use property `mouse-face', not `face'.

\(fn ARG &optional FACE MOUSEP)" t nil)

(autoload 'hlt-highlight-single-quotations "highlight/highlight" "\
Highlight single-quoted text in the region.
This means, for example, commands and keys between `'s: `foobar'.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face.

\(fn &optional FACE)" t nil)

(autoload 'hlt-mouse-face-each-line "highlight/highlight" "\
Put `mouse-face' on each line of buffer in region.
If the region is active and not empty, then limit mouse-face
highlighting to the region.  Otherwise, use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MSG-P non-nil means display a progress message.

\(fn &optional START END FACE MSG-P)" t nil)

(autoload 'hlt-toggle-use-overlays-flag "highlight/highlight" "\
Toggle `hlt-use-overlays-flag'.
If the current value is non-nil, it is set to nil.
If the current value is nil, it is set to the last non-nil value.

\(fn)" t nil)

(defalias 'hlt-paste-props 'hlt-yank-props)

(autoload 'hlt-yank-props "highlight/highlight" "\
Yank (paste) copied text properties over the active region.
Interactively, do nothing if there is no nonempty active region.
By default, yank only the copied properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, yank all copied properties.
With a negative prefix arg, you are prompted for the copied properties
 to yank.  To finish entering properties, hit `RET RET' (i.e., twice).

NOTE: If the list of copied text properties is empty, then yanking
      REMOVES ALL PROPERTIES from the text in the region.  This
      provides an easy way to UNpropertize text.

\(fn START END &optional ARG MSGP)" t nil)

(autoload 'hlt-mouse-copy-props "highlight/highlight" "\
Same as `hlt-copy-props', but copy at mouse pointer, not at point.

\(fn &optional EVENT ARG MSGP)" t nil)

(autoload 'hlt-copy-props "highlight/highlight" "\
Copy text properties at point for use by `hlt-yank-props'.
Properties are copied to `hlt-copied-props'.
By default, copy the properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, copy all properties.
With a negative prefix arg, you are prompted for the properties to
 copy.  To finish entering properties, hit `RET RET' (i.e., twice).

\(fn &optional POSITION ARG MSGP)" t nil)

;;;***

;;;### (autoloads (ido-ubiquitous-mode) "ido-ubiquitous/ido-ubiquitous"
;;;;;;  "ido-ubiquitous/ido-ubiquitous.el" (21170 11234 877228 724000))
;;; Generated autoloads from ido-ubiquitous/ido-ubiquitous.el

(define-obsolete-variable-alias 'ido-ubiquitous 'ido-ubiquitous-mode "0.8")

(define-obsolete-function-alias 'ido-ubiquitous 'ido-ubiquitous-mode "0.8")

(defvar ido-ubiquitous-mode nil "\
Non-nil if Ido-Ubiquitous mode is enabled.
See the command `ido-ubiquitous-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-ubiquitous-mode'.")

(custom-autoload 'ido-ubiquitous-mode "ido-ubiquitous/ido-ubiquitous" nil)

(autoload 'ido-ubiquitous-mode "ido-ubiquitous/ido-ubiquitous" "\
Use `ido-completing-read' instead of `completing-read' almost everywhere.

  This mode has no effect unles `ido-mode' is also enabled.

  If this mode causes problems for a function, you can customize
  when ido completion is or is not used by customizing
  `ido-ubiquitous-command-overrides' or
  `ido-ubiquitous-function-overrides'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (magit-run-gitk magit-run-git-gui-blame magit-run-git-gui
;;;;;;  magit-init magit-branch-manager magit-show magit-dired-jump
;;;;;;  magit-add-change-log-entry-other-window magit-add-change-log-entry
;;;;;;  magit-wazzup magit-diff-stash magit-diff-unstaged magit-diff-staged
;;;;;;  magit-diff-working-tree magit-diff magit-interactive-resolve
;;;;;;  magit-save-index magit-cherry magit-reflog-head magit-reflog
;;;;;;  magit-file-log magit-log-long-ranged magit-log-long magit-log-ranged
;;;;;;  magit-log magit-bisect-run magit-bisect-skip magit-bisect-bad
;;;;;;  magit-bisect-good magit-bisect-reset magit-bisect-start magit-submodule-sync
;;;;;;  magit-submodule-init magit-submodule-update-init magit-submodule-update
;;;;;;  magit-stash-snapshot magit-stash magit-delete-tag magit-tag
;;;;;;  magit-commit-squash magit-commit-fixup magit-commit-reword
;;;;;;  magit-commit-extend magit-commit-amend magit-commit magit-push
;;;;;;  magit-push-tags magit-git-command magit-shell-command magit-pull
;;;;;;  magit-remote-update magit-fetch-current magit-fetch magit-reset-working-tree
;;;;;;  magit-reset-head-hard magit-reset-head magit-interactive-rebase
;;;;;;  magit-rename-remote magit-remove-remote magit-add-remote
;;;;;;  magit-rename-branch magit-delete-branch magit-create-branch
;;;;;;  magit-checkout-branch-at-point magit-checkout magit-unstage-all
;;;;;;  magit-stage-all magit-merge-abort magit-merge magit-status
;;;;;;  magit-show-commit) "magit/magit" "magit/magit.el" (21170
;;;;;;  11248 933228 976000))
;;; Generated autoloads from magit/magit.el

(autoload 'magit-show-commit "magit/magit" "\
Show information about COMMIT.

\(fn COMMIT &optional NOSELECT INHIBIT-HISTORY)" t nil)

(autoload 'magit-status "magit/magit" "\
Open a Magit status buffer for the Git repository containing DIR.
If DIR is not within a Git repository, offer to create a Git
repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git
control.  Two prefix arguments means to ignore `magit-repo-dirs'
when asking for user input.

\(fn DIR)" t nil)

(autoload 'magit-merge "magit/magit" "\
Merge REVISION into the current 'HEAD', leaving changes uncommitted.
With a prefix argument, skip editing the log message and commit.
\('git merge [--no-commit] REVISION').

\(fn REVISION &optional DO-COMMIT)" t nil)

(autoload 'magit-merge-abort "magit/magit" "\
Abort the current merge operation.

\(fn)" t nil)

(autoload 'magit-stage-all "magit/magit" "\
Add all remaining changes in tracked files to staging area.
With a prefix argument, add remaining untracked files as well.
\('git add [-u] .').

\(fn &optional INCLUDING-UNTRACKED)" t nil)

(autoload 'magit-unstage-all "magit/magit" "\
Remove all changes from staging area.
\('git reset --mixed HEAD').

\(fn)" t nil)

(autoload 'magit-checkout "magit/magit" "\
Switch 'HEAD' to REVISION and update working tree.
Fails if working tree or staging area contain uncommitted changes.
If REVISION is a remote branch, offer to create a local tracking branch.
\('git checkout [-b] REVISION').

\(fn REVISION)" t nil)

(autoload 'magit-checkout-branch-at-point "magit/magit" "\
Checkout the branch at point.
If there is no branch at point, then prompt for one.

\(fn)" t nil)

(autoload 'magit-create-branch "magit/magit" "\
Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
\('git checkout -b BRANCH REVISION').

\(fn BRANCH PARENT)" t nil)

(autoload 'magit-delete-branch "magit/magit" "\
Delete the BRANCH.
If the branch is the current one, offers to switch to `master' first.
With prefix, forces the removal even if it hasn't been merged.
Works with local or remote branches.
\('git branch [-d|-D] BRANCH' or 'git push <remote-part-of-BRANCH> :refs/heads/BRANCH').

\(fn BRANCH &optional FORCE)" t nil)

(autoload 'magit-rename-branch "magit/magit" "\
Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\('git branch [-m|-M] OLD NEW').

\(fn OLD NEW &optional FORCE)" t nil)

(autoload 'magit-add-remote "magit/magit" "\
Add the REMOTE and fetch it.
\('git remote add REMOTE URL').

\(fn REMOTE URL)" t nil)

(autoload 'magit-remove-remote "magit/magit" "\
Delete the REMOTE.
\('git remote rm REMOTE').

\(fn REMOTE)" t nil)

(autoload 'magit-rename-remote "magit/magit" "\
Rename remote OLD to NEW.
\('git remote rename OLD NEW').

\(fn OLD NEW)" t nil)

(autoload 'magit-interactive-rebase "magit/magit" "\
Start a git rebase -i session, old school-style.

\(fn COMMIT)" t nil)

(autoload 'magit-reset-head "magit/magit" "\
Switch 'HEAD' to REVISION, keeping prior working tree and staging area.
Any differences from REVISION become new changes to be committed.
With prefix argument, all uncommitted changes in working tree
and staging area are lost.
\('git reset [--soft|--hard] REVISION').

\(fn REVISION &optional HARD)" t nil)

(autoload 'magit-reset-head-hard "magit/magit" "\
Switch 'HEAD' to REVISION, losing all changes.
Uncomitted changes in both working tree and staging area are lost.
\('git reset --hard REVISION').

\(fn REVISION)" t nil)

(autoload 'magit-reset-working-tree "magit/magit" "\
Revert working tree and clear changes from staging area.
\('git reset --hard HEAD').

With a prefix arg, also remove untracked files.
With two prefix args, remove ignored files as well.

\(fn &optional ARG)" t nil)

(autoload 'magit-fetch "magit/magit" "\
Fetch from REMOTE.

\(fn REMOTE)" t nil)

(autoload 'magit-fetch-current "magit/magit" "\
Run fetch for default remote.

If there is no default remote, ask for one.

\(fn)" t nil)

(autoload 'magit-remote-update "magit/magit" "\
Update all remotes.

\(fn)" t nil)

(autoload 'magit-pull "magit/magit" "\
Run git pull.

If there is no default remote, the user is prompted for one and
its values is saved with git config.  If there is no default
merge branch, the user is prompted for one and its values is
saved with git config.  With a prefix argument, the default
remote is not used and the user is prompted for a remote.  With
two prefix arguments, the default merge branch is not used and
the user is prompted for a merge branch.  Values entered by the
user because of prefix arguments are not saved with git config.

\(fn)" t nil)

(autoload 'magit-shell-command "magit/magit" "\
Perform arbitrary shell COMMAND.

\(fn COMMAND)" t nil)

(autoload 'magit-git-command "magit/magit" "\
Perform arbitrary Git COMMAND.

Similar to `magit-shell-command', but involves slightly less
typing and automatically refreshes the status buffer.

\(fn COMMAND)" t nil)

(autoload 'magit-push-tags "magit/magit" "\
Push tags to a remote repository.

Push tags to the current branch's remote.  If that isn't set push
to \"origin\" or if that remote doesn't exit but only a single
remote is defined use that.  Otherwise or with a prefix argument
ask the user what remote to use.

\(fn)" t nil)

(autoload 'magit-push "magit/magit" "\
Push the current branch to a remote repository.

By default push to the remote specified by the git-config(1) option
branch.<name>.remote or else origin.  Otherwise or with a prefix
argument instead ask the user what remote to push to.

When pushing to branch.<name>.remote push to the branch specified by
branch.<name>.merge.  When pushing to another remote or if the latter
option is not set push to the remote branch with the same name as the
local branch being pushed.  With two or more prefix arguments instead
ask the user what branch to push to.  In this last case actually push
even if `magit-set-upstream-on-push's value is `refuse'.

\(fn)" t nil)

(autoload 'magit-commit "magit/magit" "\
Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.
\('git commit [--amend]').

\(fn &optional AMENDP)" t nil)

(autoload 'magit-commit-amend "magit/magit" "\
Amend the last commit.
\('git commit --amend').

\(fn)" t nil)

(autoload 'magit-commit-extend "magit/magit" "\
Amend the last commit, without editing the message.
With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-extend-override-date' can be
used to inverse the meaning of the prefix argument.
\('git commit --no-edit --amend [--keep-date]').

\(fn &optional OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-reword "magit/magit" "\
Reword the last commit, ignoring staged changes.

With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-rewrite-override-date' can be
used to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

\('git commit --only --amend').

\(fn &optional OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-fixup "magit/magit" "\
Create a fixup commit.
With a prefix argument the user is always queried for the commit
to be fixed.  Otherwise the current or marked commit may be used
depending on the value of option `magit-commit-squash-commit'.
\('git commit [--no-edit] --fixup=COMMIT').

\(fn &optional COMMIT)" t nil)

(autoload 'magit-commit-squash "magit/magit" "\
Create a squash commit.
With a prefix argument the user is always queried for the commit
to be fixed.  Otherwise the current or marked commit may be used
depending on the value of option `magit-commit-squash-commit'.
\('git commit [--no-edit] --fixup=COMMIT').

\(fn &optional COMMIT FIXUP)" t nil)

(autoload 'magit-tag "magit/magit" "\
Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\('git tag [--annotate] NAME REV').

\(fn NAME REV &optional ANNOTATE)" t nil)

(autoload 'magit-delete-tag "magit/magit" "\
Delete the tag with the given NAME.
\('git tag -d NAME').

\(fn NAME)" t nil)

(autoload 'magit-stash "magit/magit" "\
Create new stash of working tree and staging area named DESCRIPTION.
Working tree and staging area revert to the current 'HEAD'.
With prefix argument, changes in staging area are kept.
\('git stash save [--keep-index] DESCRIPTION')

\(fn DESCRIPTION)" t nil)

(autoload 'magit-stash-snapshot "magit/magit" "\
Create new stash of working tree and staging area; keep changes in place.
\('git stash save \"Snapshot...\"; git stash apply stash@{0}')

\(fn)" t nil)

(autoload 'magit-submodule-update "magit/magit" "\
Update the submodule of the current git repository.
With a prefix arg, do a submodule update --init.

\(fn &optional INIT)" t nil)

(autoload 'magit-submodule-update-init "magit/magit" "\
Update and init the submodule of the current git repository.

\(fn)" t nil)

(autoload 'magit-submodule-init "magit/magit" "\
Initialize the submodules.

\(fn)" t nil)

(autoload 'magit-submodule-sync "magit/magit" "\
Synchronizes submodule's remote URL configuration.

\(fn)" t nil)

(autoload 'magit-bisect-start "magit/magit" "\


\(fn BAD GOOD)" t nil)

(autoload 'magit-bisect-reset "magit/magit" "\


\(fn)" t nil)

(autoload 'magit-bisect-good "magit/magit" "\


\(fn)" t nil)

(autoload 'magit-bisect-bad "magit/magit" "\


\(fn)" t nil)

(autoload 'magit-bisect-skip "magit/magit" "\


\(fn)" t nil)

(autoload 'magit-bisect-run "magit/magit" "\
Bisect automatically by running commands after each step.

\(fn CMDLINE)" t nil)

(autoload 'magit-log "magit/magit" "\


\(fn &optional RANGE)" t nil)

(autoload 'magit-log-ranged "magit/magit" "\


\(fn RANGE)" t nil)

(autoload 'magit-log-long "magit/magit" "\


\(fn &optional RANGE)" t nil)

(autoload 'magit-log-long-ranged "magit/magit" "\


\(fn RANGE)" t nil)

(autoload 'magit-file-log "magit/magit" "\
Display the log for the currently visited file or another one.
With a prefix argument show the log graph.

\(fn FILE &optional USE-GRAPH)" t nil)

(autoload 'magit-reflog "magit/magit" "\


\(fn REF)" t nil)

(autoload 'magit-reflog-head "magit/magit" "\


\(fn)" t nil)

(autoload 'magit-cherry "magit/magit" "\


\(fn HEAD UPSTREAM)" t nil)

(autoload 'magit-save-index "magit/magit" "\
Add the content of current file as if it was the index.

\(fn)" t nil)

(autoload 'magit-interactive-resolve "magit/magit" "\


\(fn FILE)" t nil)

(autoload 'magit-diff "magit/magit" "\


\(fn RANGE &optional WORKING ARGS)" t nil)

(autoload 'magit-diff-working-tree "magit/magit" "\


\(fn REV)" t nil)

(autoload 'magit-diff-staged "magit/magit" "\
Show differences between index and HEAD.

\(fn)" t nil)

(autoload 'magit-diff-unstaged "magit/magit" "\
Show differences between working tree and index.

\(fn)" t nil)

(autoload 'magit-diff-stash "magit/magit" "\


\(fn STASH &optional NOSELECT)" t nil)

(autoload 'magit-wazzup "magit/magit" "\


\(fn BRANCH)" t nil)

(autoload 'magit-add-change-log-entry "magit/magit" "\
Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil)

(autoload 'magit-add-change-log-entry-other-window "magit/magit" "\


\(fn &optional WHOAMI FILE-NAME)" t nil)

(autoload 'magit-dired-jump "magit/magit" "\
Visit current item in dired.
With a prefix argument, visit in other window.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'magit-show "magit/magit" "\
Display and select a buffer containing FILE as stored in REV.

Insert the contents of FILE as stored in the revision REV into a
buffer.  Then select the buffer using `pop-to-buffer' or with a
prefix argument using `switch-to-buffer'.  Non-interactivity use
SWITCH-FUNCTION to switch to the buffer, if that is nil simply
return the buffer, without displaying it.

\(fn REV FILE &optional SWITCH-FUNCTION)" t nil)

(autoload 'magit-branch-manager "magit/magit" "\


\(fn)" t nil)

(autoload 'magit-init "magit/magit" "\
Initialize git repository in the DIR directory.

\(fn DIR)" t nil)

(autoload 'magit-run-git-gui "magit/magit" "\
Run `git gui' for the current git repository.

\(fn)" t nil)

(autoload 'magit-run-git-gui-blame "magit/magit" "\
Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the HEAD, with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

\(fn COMMIT FILENAME &optional LINENUM)" t nil)

(autoload 'magit-run-gitk "magit/magit" "\
Run `gitk --all' for the current git repository.

\(fn)" t nil)

;;;***

;;;### (autoloads (magit-blame-mode) "magit/magit-blame" "magit/magit-blame.el"
;;;;;;  (21170 11248 921228 976000))
;;; Generated autoloads from magit/magit-blame.el

(autoload 'magit-blame-mode "magit/magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "magit/magit-key-mode" "magit/magit-key-mode.el"
;;;;;;  (21170 11248 921228 976000))
;;; Generated autoloads from magit/magit-key-mode.el

(defvar magit-key-mode-groups '((dispatch (actions ("b" "Branching" magit-key-mode-popup-branching) ("B" "Bisecting" magit-key-mode-popup-bisecting) ("c" "Committing" magit-key-mode-popup-committing) ("d" "Diff worktree" magit-diff-working-tree) ("D" "Diff" magit-diff) ("f" "Fetching" magit-key-mode-popup-fetching) ("F" "Pulling" magit-key-mode-popup-pulling) ("g" "Refresh Buffers" magit-refresh-all) ("l" "Logging" magit-key-mode-popup-logging) ("m" "Merging" magit-key-mode-popup-merging) ("M" "Remoting" magit-key-mode-popup-remoting) ("P" "Pushing" magit-key-mode-popup-pushing) ("o" "Submoduling" magit-key-mode-popup-submodule) ("r" "Rewriting" magit-key-mode-popup-rewriting) ("s" "Show Status" magit-status) ("S" "Stage all" magit-stage-all) ("t" "Tagging" magit-key-mode-popup-tagging) ("U" "Unstage all" magit-unstage-all) ("v" "Show Commit" magit-show-commit) ("V" "Show File" magit-show) ("w" "Wazzup" magit-wazzup) ("X" "Reset worktree" magit-reset-working-tree) ("y" "Cherry" magit-cherry) ("z" "Stashing" magit-key-mode-popup-stashing) ("!" "Running" magit-key-mode-popup-running) ("$" "Show Process" magit-display-process))) (logging (man-page "git-log") (actions ("l" "Short" magit-log) ("L" "Long" magit-log-long) ("h" "Head Reflog" magit-reflog-head) ("f" "File log" magit-file-log) ("rl" "Ranged short" magit-log-ranged) ("rL" "Ranged long" magit-log-long-ranged) ("rh" "Reflog" magit-reflog)) (switches ("-m" "Only merge commits" "--merges") ("-do" "Date Order" "--date-order") ("-f" "First parent" "--first-parent") ("-i" "Case insensitive patterns" "-i") ("-pr" "Pickaxe regex" "--pickaxe-regex") ("-g" "Show Graph" "--graph") ("-n" "Name only" "--name-only") ("-am" "All match" "--all-match") ("-al" "All" "--all")) (arguments ("=r" "Relative" "--relative=" read-directory-name) ("=c" "Committer" "--committer=" read-from-minibuffer) ("=>" "Since" "--since=" read-from-minibuffer) ("=<" "Before" "--before=" read-from-minibuffer) ("=a" "Author" "--author=" read-from-minibuffer) ("=g" "Grep messages" "--grep=" read-from-minibuffer) ("=G" "Grep patches" "-G" read-from-minibuffer) ("=L" "Trace evolution of line range [long log only]" "-L" magit-read-file-trace) ("=s" "Pickaxe search" "-S" read-from-minibuffer) ("=b" "Branches" "--branches=" read-from-minibuffer) ("=R" "Remotes" "--remotes=" read-from-minibuffer))) (running (actions ("!" "Command from root" magit-shell-command) (":" "Git command" magit-git-command) ("g" "git gui" magit-run-git-gui) ("k" "gitk" magit-run-gitk))) (fetching (man-page "git-fetch") (actions ("f" "Current" magit-fetch-current) ("a" "All" magit-remote-update) ("o" "Other" magit-fetch)) (switches ("-p" "Prune" "--prune"))) (pushing (man-page "git-push") (actions ("P" "Push" magit-push) ("t" "Push tags" magit-push-tags)) (switches ("-f" "Force" "--force") ("-d" "Dry run" "-n") ("-u" "Set upstream" "-u"))) (pulling (man-page "git-pull") (actions ("F" "Pull" magit-pull)) (switches ("-f" "Force" "--force") ("-r" "Rebase" "--rebase"))) (branching (man-page "git-branch") (actions ("v" "Branch manager" magit-branch-manager) ("b" "Checkout" magit-checkout) ("c" "Create" magit-create-branch) ("r" "Rename" magit-rename-branch) ("k" "Delete" magit-delete-branch)) (switches ("-t" "Set upstream configuration" "--track") ("-m" "Merged to HEAD" "--merged") ("-M" "Merged to master" "--merged=master") ("-n" "Not merged to HEAD" "--no-merged") ("-N" "Not merged to master" "--no-merged=master")) (arguments ("=c" "Contains" "--contains=" magit-read-rev-with-default) ("=m" "Merged" "--merged=" magit-read-rev-with-default) ("=n" "Not merged" "--no-merged=" magit-read-rev-with-default))) (remoting (man-page "git-remote") (actions ("v" "Remote manager" magit-branch-manager) ("a" "Add" magit-add-remote) ("r" "Rename" magit-rename-remote) ("k" "Remove" magit-remove-remote))) (tagging (man-page "git-tag") (actions ("t" "Create" magit-tag) ("k" "Delete" magit-delete-tag)) (switches ("-a" "Annotate" "--annotate") ("-f" "Force" "--force") ("-s" "Sign" "--sign"))) (stashing (man-page "git-stash") (actions ("z" "Save" magit-stash) ("s" "Snapshot" magit-stash-snapshot)) (switches ("-k" "Keep index" "--keep-index") ("-u" "Include untracked files" "--include-untracked") ("-a" "Include all files" "--all"))) (committing (man-page "git-commit") (actions ("c" "Commit" magit-commit) ("a" "Amend" magit-commit-amend) ("e" "Extend" magit-commit-extend) ("r" "Reword" magit-commit-reword) ("f" "Fixup" magit-commit-fixup) ("s" "Squash" magit-commit-squash)) (switches ("-r" "Replace the tip of current branch" "--amend") ("-R" "Claim authorship and reset author date" "--reset-author") ("-a" "Stage all modified and deleted files" "--all") ("-e" "Allow empty commit" "--allow-empty") ("-v" "Show diff of changes to be committed" "--verbose") ("-n" "Bypass git hooks" "--no-verify") ("-s" "Add Signed-off-by line" "--signoff") ("-S" "Sign using gpg" "--gpg-sign"))) (merging (man-page "git-merge") (actions ("m" "Merge" magit-merge) ("A" "Abort" magit-merge-abort)) (switches ("-ff" "Fast-forward only" "--ff-only") ("-nf" "No fast-forward" "--no-ff") ("-sq" "Squash" "--squash")) (arguments ("-st" "Strategy" "--strategy=" read-from-minibuffer))) (rewriting (actions ("b" "Begin" magit-rewrite-start) ("s" "Stop" magit-rewrite-stop) ("a" "Abort" magit-rewrite-abort) ("f" "Finish" magit-rewrite-finish) ("*" "Set unused" magit-rewrite-set-unused) ("." "Set used" magit-rewrite-set-used))) (apply-mailbox (man-page "git-am") (actions ("J" "Apply Mailbox" magit-apply-mailbox)) (switches ("-s" "add a Signed-off-by line to the commit message" "--signoff") ("-3" "allow fall back on 3way merging if needed" "--3way") ("-k" "pass -k flag to git-mailinfo" "--keep") ("-c" "strip everything before a scissors line" "--scissors") ("-p" "pass it through git-apply" "-p") ("-r" "override error message when patch failure occurs" "--resolvemsg") ("-d" "lie about committer date" "--committer-date-is-author-date") ("-D" "use current timestamp for author date" "--ignore-date") ("-b" "pass -b flag to git-mailinfo" "--keep-non-patch")) (arguments ("=p" "format the patch(es) are in" "--patch-format"))) (submodule (man-page "git-submodule") (actions ("u" "Update" magit-submodule-update) ("b" "Both update and init" magit-submodule-update-init) ("i" "Init" magit-submodule-init) ("s" "Sync" magit-submodule-sync))) (bisecting (man-page "git-bisect") (actions ("b" "Bad" magit-bisect-bad) ("g" "Good" magit-bisect-good) ("k" "Skip" magit-bisect-skip) ("r" "Reset" magit-bisect-reset) ("s" "Start" magit-bisect-start) ("u" "Run" magit-bisect-run))) (diff-options (actions ("s" "Set" magit-set-diff-options) ("d" "Set default" magit-set-default-diff-options) ("c" "Save default" magit-save-default-diff-options) ("r" "Reset to default" magit-reset-diff-options) ("h" "Toggle Hunk Refinement" magit-toggle-diff-refine-hunk)) (switches ("-m" "Show smallest possible diff" "--minimal") ("-p" "Use patience diff algorithm" "--patience") ("-h" "Use histogram diff algorithm" "--histogram") ("-b" "Ignore whitespace changes" "--ignore-space-change") ("-w" "Ignore all whitespace" "--ignore-all-space") ("-W" "Show surrounding functions" "--function-context")))) "\
Holds the key, help, function mapping for the log-mode.
If you modify this make sure you reset `magit-key-mode-keymaps'
to nil.")
 (mapc (lambda (g) (eval `(autoload ',(intern (concat "magit-key-mode-popup-" (symbol-name (car g)))) "magit-key-mode" ,(concat "Key menu for " (symbol-name (car g))) t))) magit-key-mode-groups)

;;;***

;;;### (autoloads (turn-on-magit-stgit magit-stgit-mode magit-stgit-show
;;;;;;  magit-stgit-discard magit-stgit-rebase magit-stgit-repair
;;;;;;  magit-stgit-refresh) "magit/magit-stgit" "magit/magit-stgit.el"
;;;;;;  (21170 11248 921228 976000))
;;; Generated autoloads from magit/magit-stgit.el

(autoload 'magit-stgit-refresh "magit/magit-stgit" "\
Refresh a StGit patch.

\(fn &optional PATCH)" t nil)

(autoload 'magit-stgit-repair "magit/magit-stgit" "\
Repair StGit metadata if branch was modified with git commands.
In the case of Git commits these will be imported as new patches
into the series.

\(fn)" t nil)

(autoload 'magit-stgit-rebase "magit/magit-stgit" "\
Rebase a StGit patch series.

\(fn)" t nil)

(autoload 'magit-stgit-discard "magit/magit-stgit" "\
Discard a StGit patch.

\(fn PATCH)" t nil)

(autoload 'magit-stgit-show "magit/magit-stgit" "\
Show diff of a StGit patch.

\(fn PATCH)" t nil)

(autoload 'magit-stgit-mode "magit/magit-stgit" "\
StGit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-stgit "magit/magit-stgit" "\
Unconditionally turn on `magit-stgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-svn magit-svn-mode magit-svn-fetch-externals
;;;;;;  magit-svn-remote-update magit-svn-dcommit magit-svn-rebase
;;;;;;  magit-svn-create-tag magit-svn-create-branch magit-svn-find-rev)
;;;;;;  "magit/magit-svn" "magit/magit-svn.el" (21170 11248 921228
;;;;;;  976000))
;;; Generated autoloads from magit/magit-svn.el

(autoload 'magit-svn-find-rev "magit/magit-svn" "\
Find commit for svn REVISION in BRANCH.

\(fn REV &optional BRANCH)" t nil)

(autoload 'magit-svn-create-branch "magit/magit-svn" "\
Create svn branch NAME.

\(fn NAME)" t nil)

(autoload 'magit-svn-create-tag "magit/magit-svn" "\
Create svn tag NAME.

\(fn NAME)" t nil)

(autoload 'magit-svn-rebase "magit/magit-svn" "\
Run git-svn rebase.

\(fn)" t nil)

(autoload 'magit-svn-dcommit "magit/magit-svn" "\
Run git-svn dcommit.

\(fn)" t nil)

(autoload 'magit-svn-remote-update "magit/magit-svn" "\
Run git-svn fetch.

\(fn)" t nil)

(autoload 'magit-svn-fetch-externals "magit/magit-svn" "\
Loops through all external repos found by `magit-svn-external-directories'
   and runs git svn fetch, and git svn rebase on each of them.

\(fn)" t nil)

(autoload 'magit-svn-mode "magit/magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-svn "magit/magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-topgit magit-topgit-mode) "magit/magit-topgit"
;;;;;;  "magit/magit-topgit.el" (21170 11248 921228 976000))
;;; Generated autoloads from magit/magit-topgit.el

(autoload 'magit-topgit-mode "magit/magit-topgit" "\
Topgit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-topgit "magit/magit-topgit" "\
Unconditionally turn on `magit-topgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (global-magit-wip-save-mode magit-wip-save-mode)
;;;;;;  "magit/magit-wip" "magit/magit-wip.el" (21170 11248 921228
;;;;;;  976000))
;;; Generated autoloads from magit/magit-wip.el

(autoload 'magit-wip-save-mode "magit/magit-wip" "\
Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a
writable git repository then it is also committed to a special
work-in-progress ref.

\(fn &optional ARG)" t nil)

(defvar global-magit-wip-save-mode nil "\
Non-nil if Global-Magit-Wip-Save mode is enabled.
See the command `global-magit-wip-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-wip-save-mode'.")

(custom-autoload 'global-magit-wip-save-mode "magit/magit-wip" nil)

(autoload 'global-magit-wip-save-mode "magit/magit-wip" "\
Toggle Magit-Wip-Save mode in all buffers.
With prefix ARG, enable Global-Magit-Wip-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-Save mode is enabled in all buffers where
`turn-on-magit-wip-save' would do it.
See `magit-wip-save-mode' for more information on Magit-Wip-Save mode.

\(fn &optional ARG)" t nil)
;;;***

;;;### (autoloads (gfm-mode markdown-mode) "markdown-mode/markdown-mode"
;;;;;;  "markdown-mode/markdown-mode.el" (21176 6751 196602 828000))
;;; Generated autoloads from markdown-mode/markdown-mode.el

(autoload 'markdown-mode "markdown-mode/markdown-mode" "\
Major mode for editing Markdown files.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode/markdown-mode" "\
Major mode for editing GitHub Flavored Markdown files.

\(fn)" t nil)

;;;***

;;;### (autoloads (restore-frame maximize-frame x-restore-frame x-maximize-frame
;;;;;;  w32-restore-frame w32-maximize-frame) "maxframe/maxframe"
;;;;;;  "maxframe/maxframe.el" (21170 11272 541229 400000))
;;; Generated autoloads from maxframe/maxframe.el

(autoload 'w32-maximize-frame "maxframe/maxframe" "\
Maximize the current frame (windows only)

\(fn)" t nil)

(autoload 'w32-restore-frame "maxframe/maxframe" "\
Restore a minimized/maximized frame (windows only)

\(fn)" t nil)

(autoload 'x-maximize-frame "maxframe/maxframe" "\
Maximize the current frame (x or mac only)

\(fn &optional THE-FRAME)" t nil)

(autoload 'x-restore-frame "maxframe/maxframe" "\
Restore the current frame (x or mac only)

\(fn &optional THE-FRAME)" t nil)

(autoload 'maximize-frame "maxframe/maxframe" "\
Maximizes the frame to fit the display if under a windowing
system.

\(fn &optional THE-FRAME)" t nil)

(autoload 'restore-frame "maxframe/maxframe" "\
Restores a maximized frame.  See `maximize-frame'.

\(fn &optional THE-FRAME)" t nil)

(defalias 'mf 'maximize-frame)

;;;***

;;;### (autoloads (mc/edit-beginnings-of-lines mc/edit-ends-of-lines
;;;;;;  mc/edit-lines) "multiple-cursors/mc-edit-lines" "multiple-cursors/mc-edit-lines.el"
;;;;;;  (21170 11276 949229 480000))
;;; Generated autoloads from multiple-cursors/mc-edit-lines.el

(autoload 'mc/edit-lines "multiple-cursors/mc-edit-lines" "\
Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

What is done with lines which are not long enough is governed by
`mc/edit-lines-empty-lines'.  The prefix argument ARG can be used
to override this.  If ARG is a symbol (when called from Lisp),
that symbol is used instead of `mc/edit-lines-empty-lines'.
Otherwise, if ARG negative, short lines will be ignored.  Any
other non-nil value will cause short lines to be padded.

\(fn &optional ARG)" t nil)

(autoload 'mc/edit-ends-of-lines "multiple-cursors/mc-edit-lines" "\
Add one cursor to the end of each line in the active region.

\(fn)" t nil)

(autoload 'mc/edit-beginnings-of-lines "multiple-cursors/mc-edit-lines" "\
Add one cursor to the beginning of each line in the active region.

\(fn)" t nil)

;;;***

;;;### (autoloads (mc/mark-sgml-tag-pair mc/add-cursor-on-click mc/mark-all-symbols-like-this-in-defun
;;;;;;  mc/mark-all-words-like-this-in-defun mc/mark-all-like-this-in-defun
;;;;;;  mc/mark-all-dwim mc/mark-all-like-this-dwim mc/mark-more-like-this-extended
;;;;;;  mc/mark-all-in-region mc/mark-all-symbols-like-this mc/mark-all-words-like-this
;;;;;;  mc/mark-all-like-this mc/skip-to-previous-like-this mc/skip-to-next-like-this
;;;;;;  mc/unmark-previous-like-this mc/unmark-next-like-this mc/mark-previous-lines
;;;;;;  mc/mark-next-lines mc/mark-previous-symbol-like-this mc/mark-previous-word-like-this
;;;;;;  mc/mark-previous-like-this mc/mark-next-symbol-like-this
;;;;;;  mc/mark-next-word-like-this mc/mark-next-like-this) "multiple-cursors/mc-mark-more"
;;;;;;  "multiple-cursors/mc-mark-more.el" (21170 11276 949229 480000))
;;; Generated autoloads from multiple-cursors/mc-mark-more.el

(autoload 'mc/mark-next-like-this "multiple-cursors/mc-mark-more" "\
Find and mark the next part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

\(fn ARG)" t nil)

(autoload 'mc/mark-next-word-like-this "multiple-cursors/mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-next-symbol-like-this "multiple-cursors/mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-previous-like-this "multiple-cursors/mc-mark-more" "\
Find and mark the previous part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

\(fn ARG)" t nil)

(autoload 'mc/mark-previous-word-like-this "multiple-cursors/mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-previous-symbol-like-this "multiple-cursors/mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-next-lines "multiple-cursors/mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-previous-lines "multiple-cursors/mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/unmark-next-like-this "multiple-cursors/mc-mark-more" "\
Deselect next part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/unmark-previous-like-this "multiple-cursors/mc-mark-more" "\
Deselect prev part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/skip-to-next-like-this "multiple-cursors/mc-mark-more" "\
Skip the current one and select the next part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/skip-to-previous-like-this "multiple-cursors/mc-mark-more" "\
Skip the current one and select the prev part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/mark-all-like-this "multiple-cursors/mc-mark-more" "\
Find and mark all the parts of the buffer matching the currently active region

\(fn)" t nil)

(autoload 'mc/mark-all-words-like-this "multiple-cursors/mc-mark-more" "\


\(fn)" t nil)

(autoload 'mc/mark-all-symbols-like-this "multiple-cursors/mc-mark-more" "\


\(fn)" t nil)

(autoload 'mc/mark-all-in-region "multiple-cursors/mc-mark-more" "\
Find and mark all the parts in the region matching the given search

\(fn BEG END)" t nil)

(autoload 'mc/mark-more-like-this-extended "multiple-cursors/mc-mark-more" "\
Like mark-more-like-this, but then lets you adjust with arrows key.
The adjustments work like this:

   <up>    Mark previous like this and set direction to 'up
   <down>  Mark next like this and set direction to 'down

If direction is 'up:

   <left>  Skip past the cursor furthest up
   <right> Remove the cursor furthest up

If direction is 'down:

   <left>  Remove the cursor furthest down
   <right> Skip past the cursor furthest down

The bindings for these commands can be changed. See `mc/mark-more-like-this-extended-keymap'.

\(fn)" t nil)

(autoload 'mc/mark-all-like-this-dwim "multiple-cursors/mc-mark-more" "\
Tries to guess what you want to mark all of.
Can be pressed multiple times to increase selection.

With prefix, it behaves the same as original `mc/mark-all-like-this'

\(fn ARG)" t nil)

(autoload 'mc/mark-all-dwim "multiple-cursors/mc-mark-more" "\
Tries even harder to guess what you want to mark all of.

If the region is active and spans multiple lines, it will behave
as if `mc/mark-all-in-region'. With the prefix ARG, it will call
`mc/edit-lines' instead.

If the region is inactive or on a single line, it will behave like
`mc/mark-all-like-this-dwim'.

\(fn ARG)" t nil)

(autoload 'mc/mark-all-like-this-in-defun "multiple-cursors/mc-mark-more" "\
Mark all like this in defun.

\(fn)" t nil)

(autoload 'mc/mark-all-words-like-this-in-defun "multiple-cursors/mc-mark-more" "\
Mark all words like this in defun.

\(fn)" t nil)

(autoload 'mc/mark-all-symbols-like-this-in-defun "multiple-cursors/mc-mark-more" "\
Mark all symbols like this in defun.

\(fn)" t nil)

(autoload 'mc/add-cursor-on-click "multiple-cursors/mc-mark-more" "\
Add a cursor where you click.

\(fn EVENT)" t nil)

(autoload 'mc/mark-sgml-tag-pair "multiple-cursors/mc-mark-more" "\
Mark the tag we're in and its pair for renaming.

\(fn)" t nil)

;;;***

;;;### (autoloads (mc/mark-pop) "multiple-cursors/mc-mark-pop" "multiple-cursors/mc-mark-pop.el"
;;;;;;  (21170 11276 949229 480000))
;;; Generated autoloads from multiple-cursors/mc-mark-pop.el

(autoload 'mc/mark-pop "multiple-cursors/mc-mark-pop" "\
Add a cursor at the current point, pop off mark ring and jump
to the popped mark.

\(fn)" t nil)

;;;***

;;;### (autoloads (mc/sort-regions mc/reverse-regions mc/insert-numbers)
;;;;;;  "multiple-cursors/mc-separate-operations" "multiple-cursors/mc-separate-operations.el"
;;;;;;  (21170 11276 949229 480000))
;;; Generated autoloads from multiple-cursors/mc-separate-operations.el

(autoload 'mc/insert-numbers "multiple-cursors/mc-separate-operations" "\
Insert increasing numbers for each cursor, starting at 0 or ARG.

\(fn ARG)" t nil)

(autoload 'mc/reverse-regions "multiple-cursors/mc-separate-operations" "\


\(fn)" t nil)

(autoload 'mc/sort-regions "multiple-cursors/mc-separate-operations" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (set-rectangular-region-anchor) "multiple-cursors/rectangular-region-mode"
;;;;;;  "multiple-cursors/rectangular-region-mode.el" (21170 11276
;;;;;;  953229 480000))
;;; Generated autoloads from multiple-cursors/rectangular-region-mode.el

(autoload 'set-rectangular-region-anchor "multiple-cursors/rectangular-region-mode" "\
Anchors the rectangular region at point.

Think of this one as `set-mark' except you're marking a rectangular region. It is
an exceedingly quick way of adding multiple cursors to multiple lines.

\(fn)" t nil)

;;;***

;;;### (autoloads (nginx-mode) "nginx-mode/nginx-mode" "nginx-mode/nginx-mode.el"
;;;;;;  (21170 11281 277229 557000))
;;; Generated autoloads from nginx-mode/nginx-mode.el

(autoload 'nginx-mode "nginx-mode/nginx-mode" "\
Major mode for highlighting nginx config files.

The variable nginx-indent-level controls the amount of indentation.
\\{nginx-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (notmuch-cycle-notmuch-buffers notmuch) "notmuch/emacs/notmuch"
;;;;;;  "notmuch/emacs/notmuch.el" (21170 11441 521232 436000))
;;; Generated autoloads from notmuch/emacs/notmuch.el

(put 'notmuch-search 'notmuch-doc "Search for messages.")

(autoload 'notmuch "notmuch/emacs/notmuch" "\
Run notmuch and display saved searches, known tags, etc.

\(fn)" t nil)

(autoload 'notmuch-cycle-notmuch-buffers "notmuch/emacs/notmuch" "\
Cycle through any existing notmuch buffers (search, show or hello).

If the current buffer is the only notmuch buffer, bury it. If no
notmuch buffers exist, run `notmuch'.

\(fn)" t nil)

;;;***

;;;### (autoloads (notmuch-hello) "notmuch/emacs/notmuch-hello" "notmuch/emacs/notmuch-hello.el"
;;;;;;  (21170 11441 513232 436000))
;;; Generated autoloads from notmuch/emacs/notmuch-hello.el

(autoload 'notmuch-hello "notmuch/emacs/notmuch-hello" "\
Run notmuch and display saved searches, known tags, etc.

\(fn &optional NO-DISPLAY)" t nil)

;;;***

;;;### (autoloads (notmuch-show) "notmuch/emacs/notmuch-show" "notmuch/emacs/notmuch-show.el"
;;;;;;  (21170 11441 517232 436000))
;;; Generated autoloads from notmuch/emacs/notmuch-show.el

(autoload 'notmuch-show "notmuch/emacs/notmuch-show" "\
Run \"notmuch show\" with the given thread ID and display results.

ELIDE-TOGGLE, if non-nil, inverts the default elide behavior.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).

The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.

The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used.

\(fn THREAD-ID &optional ELIDE-TOGGLE PARENT-BUFFER QUERY-CONTEXT BUFFER-NAME)" t nil)

;;;***

;;;### (autoloads (nyan-mode) "nyan-mode/nyan-mode" "nyan-mode/nyan-mode.el"
;;;;;;  (21170 11507 881233 628000))
;;; Generated autoloads from nyan-mode/nyan-mode.el

(defvar nyan-mode nil "\
Non-nil if Nyan mode is enabled.
See the command `nyan-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nyan-mode'.")

(custom-autoload 'nyan-mode "nyan-mode/nyan-mode" nil)

(autoload 'nyan-mode "nyan-mode/nyan-mode" "\
Use NyanCat to show buffer size and position in mode-line.
You can customize this minor mode, see option `nyan-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (offlineimap) "offlineimap/offlineimap" "offlineimap/offlineimap.el"
;;;;;;  (21170 11516 37233 775000))
;;; Generated autoloads from offlineimap/offlineimap.el

(autoload 'offlineimap "offlineimap/offlineimap" "\
Start OfflineIMAP.

\(fn)" t nil)

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "org-mode/contrib/lisp/htmlize"
;;;;;;  "org-mode/contrib/lisp/htmlize.el" (21170 11908 65240 817000))
;;; Generated autoloads from org-mode/contrib/lisp/htmlize.el

(autoload 'htmlize-buffer "org-mode/contrib/lisp/htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "org-mode/contrib/lisp/htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "org-mode/contrib/lisp/htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "org-mode/contrib/lisp/htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "org-mode/contrib/lisp/htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (org-bullets-mode) "org-mode/contrib/lisp/org-bullets"
;;;;;;  "org-mode/contrib/lisp/org-bullets.el" (21170 11908 69240
;;;;;;  817000))
;;; Generated autoloads from org-mode/contrib/lisp/org-bullets.el

(autoload 'org-bullets-mode "org-mode/contrib/lisp/org-bullets" "\
UTF-8 bullets for `org-mode'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (org-columns-number-to-string org-columns-compute
;;;;;;  org-columns-get-format-and-top-level org-columns-remove-overlays)
;;;;;;  "org-mode/contrib/lisp/org-colview-xemacs" "org-mode/contrib/lisp/org-colview-xemacs.el"
;;;;;;  (21170 11908 73240 817000))
;;; Generated autoloads from org-mode/contrib/lisp/org-colview-xemacs.el

(autoload 'org-columns-remove-overlays "org-mode/contrib/lisp/org-colview-xemacs" "\
Remove all currently active column overlays.

\(fn)" t nil)

(autoload 'org-columns-get-format-and-top-level "org-mode/contrib/lisp/org-colview-xemacs" "\


\(fn)" nil nil)

(autoload 'org-columns-compute "org-mode/contrib/lisp/org-colview-xemacs" "\
Sum the values of property PROPERTY hierarchically, for the entire buffer.

\(fn PROPERTY)" t nil)

(autoload 'org-columns-number-to-string "org-mode/contrib/lisp/org-colview-xemacs" "\
Convert a computed column number to a string value, according to FMT.

\(fn N FMT &optional PRINTF)" nil nil)

;;;***

;;;### (autoloads (org-contacts) "org-mode/contrib/lisp/org-contacts"
;;;;;;  "org-mode/contrib/lisp/org-contacts.el" (21170 11908 73240
;;;;;;  817000))
;;; Generated autoloads from org-mode/contrib/lisp/org-contacts.el

(autoload 'org-contacts "org-mode/contrib/lisp/org-contacts" "\
Create agenda view for contacts matching NAME.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads (org-registry-update org-registry-insinuate org-registry-initialize
;;;;;;  org-registry-visit org-registry-show) "org-mode/contrib/lisp/org-registry"
;;;;;;  "org-mode/contrib/lisp/org-registry.el" (21170 11908 89240
;;;;;;  818000))
;;; Generated autoloads from org-mode/contrib/lisp/org-registry.el

(autoload 'org-registry-show "org-mode/contrib/lisp/org-registry" "\
Show Org files where there are links pointing to the current
buffer.

\(fn &optional VISIT)" t nil)

(autoload 'org-registry-visit "org-mode/contrib/lisp/org-registry" "\
If an Org file contains a link to the current location, visit
this file.

\(fn)" t nil)

(autoload 'org-registry-initialize "org-mode/contrib/lisp/org-registry" "\
Initialize `org-registry-alist'.
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'.

\(fn &optional FROM-SCRATCH)" t nil)

(autoload 'org-registry-insinuate "org-mode/contrib/lisp/org-registry" "\
Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit.

\(fn)" t nil)

(autoload 'org-registry-update "org-mode/contrib/lisp/org-registry" "\
Update the registry for the current Org file.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-screenshot-show-unused org-screenshot-rotate-next
;;;;;;  org-screenshot-rotate-prev org-screenshot-take) "org-mode/contrib/lisp/org-screenshot"
;;;;;;  "org-mode/contrib/lisp/org-screenshot.el" (21170 11908 89240
;;;;;;  818000))
;;; Generated autoloads from org-mode/contrib/lisp/org-screenshot.el

(autoload 'org-screenshot-take "org-mode/contrib/lisp/org-screenshot" "\
Take a screenshot and insert link to it at point, if image
display is already on (see \\[org-toggle-inline-images])
screenshot will be displayed as an image

Screen area for the screenshot is selected with the mouse, left
click on a window screenshots that window, while left click and
drag selects a region. Pressing any key cancels the screen shot

With `C-u' universal argument waits one second after target is
selected before taking the screenshot. With double `C-u' wait two
seconds.

With triple `C-u' wait 3 seconds, and also rings the bell when
screenshot is done, any more `C-u' after that increases delay by
2 seconds

\(fn &optional DELAY)" t nil)

(autoload 'org-screenshot-rotate-prev "org-mode/contrib/lisp/org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-rotate-next "org-mode/contrib/lisp/org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-show-unused "org-mode/contrib/lisp/org-screenshot" "\
Open A Dired buffer with unused screenshots marked

\(fn)" t nil)

;;;***

;;;### (autoloads (org-toc-show) "org-mode/contrib/lisp/org-toc"
;;;;;;  "org-mode/contrib/lisp/org-toc.el" (21170 11908 93240 818000))
;;; Generated autoloads from org-mode/contrib/lisp/org-toc.el

(autoload 'org-toc-show "org-mode/contrib/lisp/org-toc" "\
Show the table of contents of the current Org-mode buffer.

\(fn &optional DEPTH POSITION)" t nil)

;;;***

;;;### (autoloads (org-track-compile-org org-track-fetch-package)
;;;;;;  "org-mode/contrib/lisp/org-track" "org-mode/contrib/lisp/org-track.el"
;;;;;;  (21170 11908 93240 818000))
;;; Generated autoloads from org-mode/contrib/lisp/org-track.el

(autoload 'org-track-fetch-package "org-mode/contrib/lisp/org-track" "\
Fetch Org package depending on `org-track-fetch-package-extension'.
If DIRECTORY is defined, unpack the package there, i.e. add the
subdirectory org-mode/ to DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'org-track-compile-org "org-mode/contrib/lisp/org-track" "\
Compile all *.el files that come with org-mode.
Generate the autoloads file `org-loaddefs.el'.

DIRECTORY is where the directory org-mode/ lives (i.e. the
          parent directory of your local repo.

\(fn &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads (org-freemind-export-to-freemind) "org-mode/contrib/lisp/ox-freemind"
;;;;;;  "org-mode/contrib/lisp/ox-freemind.el" (21170 11908 97240
;;;;;;  818000))
;;; Generated autoloads from org-mode/contrib/lisp/ox-freemind.el

(autoload 'org-freemind-export-to-freemind "org-mode/contrib/lisp/ox-freemind" "\
Export current buffer to a Freemind Mindmap file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

;;;***

;;;### (autoloads (org-koma-letter-export-to-pdf org-koma-letter-export-to-latex
;;;;;;  org-koma-letter-export-as-latex) "org-mode/contrib/lisp/ox-koma-letter"
;;;;;;  "org-mode/contrib/lisp/ox-koma-letter.el" (21170 11908 101240
;;;;;;  818000))
;;; Generated autoloads from org-mode/contrib/lisp/ox-koma-letter.el

(autoload 'org-koma-letter-export-as-latex "org-mode/contrib/lisp/ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a proeprty list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org KOMA-LETTER Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-latex "org-mode/contrib/lisp/ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-pdf "org-mode/contrib/lisp/ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

;;;***

;;;### (autoloads (org-rss-publish-to-rss org-rss-export-to-rss org-rss-export-as-rss)
;;;;;;  "org-mode/contrib/lisp/ox-rss" "org-mode/contrib/lisp/ox-rss.el"
;;;;;;  (21170 11908 101240 818000))
;;; Generated autoloads from org-mode/contrib/lisp/ox-rss.el

(autoload 'org-rss-export-as-rss "org-mode/contrib/lisp/ox-rss" "\
Export current buffer to a RSS buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RSS Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-export-to-rss "org-mode/contrib/lisp/ox-rss" "\
Export current buffer to a RSS file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-publish-to-rss "org-mode/contrib/lisp/ox-rss" "\
Publish an org file to RSS.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

;;;***

;;;### (autoloads (org-taskjuggler-export-process-and-open org-taskjuggler-export-and-process
;;;;;;  org-taskjuggler-export) "org-mode/contrib/lisp/ox-taskjuggler"
;;;;;;  "org-mode/contrib/lisp/ox-taskjuggler.el" (21170 11908 105240
;;;;;;  818000))
;;; Generated autoloads from org-mode/contrib/lisp/ox-taskjuggler.el

(autoload 'org-taskjuggler-export "org-mode/contrib/lisp/ox-taskjuggler" "\
Export current buffer to a TaskJuggler file.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-and-process "org-mode/contrib/lisp/ox-taskjuggler" "\
Export current buffer to a TaskJuggler file and process it.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return a list of reports.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-process-and-open "org-mode/contrib/lisp/ox-taskjuggler" "\
Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-taskjuggler-export-and-process' and open the generated
reports with a browser.

If you are targeting TaskJuggler 2.4 (see
`org-taskjuggler-target-version') the processing and display of
the reports is done using the TaskJuggler GUI.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

;;;***

;;;### (autoloads (org-customize org-reload org-submit-bug-report
;;;;;;  org-cycle-agenda-files org-switchb org-open-link-from-string
;;;;;;  org-open-at-point-global org-insert-link-global org-store-link
;;;;;;  org-run-like-in-org-mode turn-on-orgstruct++ turn-on-orgstruct
;;;;;;  orgstruct-mode org-global-cycle org-cycle org-mode org-clock-persistence-insinuate
;;;;;;  turn-on-orgtbl org-version org-babel-load-file org-babel-do-load-languages)
;;;;;;  "org-mode/lisp/org" "org-mode/lisp/org.el" (21170 11908 269240
;;;;;;  821000))
;;; Generated autoloads from org-mode/lisp/org.el

(autoload 'org-babel-do-load-languages "org-mode/lisp/org" "\
Load the languages defined in `org-babel-load-languages'.

\(fn SYM VALUE)" nil nil)

(autoload 'org-babel-load-file "org-mode/lisp/org" "\
Load Emacs Lisp source code blocks in the Org-mode FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With prefix
arg (noninteractively: 2nd arg) COMPILE the tangled Emacs Lisp
file to byte-code before it is loaded.

\(fn FILE &optional COMPILE)" t nil)

(autoload 'org-version "org-mode/lisp/org" "\
Show the org-mode version in the echo area.
With prefix argument HERE, insert it at point.
When FULL is non-nil, use a verbose version string.
When MESSAGE is non-nil, display a message with the version.

\(fn &optional HERE FULL MESSAGE)" t nil)

(autoload 'turn-on-orgtbl "org-mode/lisp/org" "\
Unconditionally turn on `orgtbl-mode'.

\(fn)" nil nil)

(autoload 'org-clock-persistence-insinuate "org-mode/lisp/org" "\
Set up hooks for clock persistence.

\(fn)" nil nil)

(autoload 'org-mode "org-mode/lisp/org" "\
Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org-mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org-mode is
implemented on top of outline-mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org-mode file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}

\(fn)" t nil)

(autoload 'org-cycle "org-mode/lisp/org" "\
TAB-action and visibility cycling for Org-mode.

This is the command invoked in Org-mode by the TAB key.  Its main purpose
is outline visibility cycling, but it also invokes other actions
in special contexts.

- When this function is called with a prefix argument, rotate the entire
  buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.
  When called with two `C-u C-u' prefixes, switch to the startup visibility,
  determined by the variable `org-startup-folded', and by any VISIBILITY
  properties in the buffer.
  When called with three `C-u C-u C-u' prefixed, show the entire buffer,
  including any drawers.

- When inside a table, re-align the table and move to the next field.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
  If there is no subtree, switch directly from CHILDREN to FOLDED.

- When point is at the beginning of an empty headline and the variable
  `org-cycle-level-after-item/entry-creation' is set, cycle the level
  of the headline by demoting and promoting it to likely levels.  This
  speeds up creation document structure by pressing TAB once or several
  times right after creating a new headline.

- When there is a numeric prefix, go up to a heading with level ARG, do
  a `show-subtree' and return to the previous cursor position.  If ARG
  is negative, go up that many levels.

- When point is not at the beginning of a headline, execute the global
  binding for TAB, which is re-indenting the line.  See the option
  `org-cycle-emulate-tab' for details.

- Special case: if point is at the beginning of the buffer and there is
  no headline in line 1, this function will act as if called with prefix arg
  (C-u TAB, same as S-TAB) also when called without prefix arg.
  But only if also the variable `org-cycle-global-at-bob' is t.

\(fn &optional ARG)" t nil)

(autoload 'org-global-cycle "org-mode/lisp/org" "\
Cycle the global visibility.  For details see `org-cycle'.
With \\[universal-argument] prefix arg, switch to startup visibility.
With a numeric prefix, show all headlines up to that level.

\(fn &optional ARG)" t nil)
(put 'orgstruct-heading-prefix-regexp 'safe-local-variable 'stringp)

(autoload 'orgstruct-mode "org-mode/lisp/org" "\
Toggle the minor mode `orgstruct-mode'.
This mode is for using Org-mode structure commands in other
modes.  The following keys behave as if Org-mode were active, if
the cursor is on a headline, or on a plain list item (both as
defined by Org-mode).

\(fn &optional ARG)" t nil)

(autoload 'turn-on-orgstruct "org-mode/lisp/org" "\
Unconditionally turn on `orgstruct-mode'.

\(fn)" nil nil)

(autoload 'turn-on-orgstruct++ "org-mode/lisp/org" "\
Unconditionally turn on `orgstruct++-mode'.

\(fn)" nil nil)

(autoload 'org-run-like-in-org-mode "org-mode/lisp/org" "\
Run a command, pretending that the current buffer is in Org-mode.
This will temporarily bind local variables that are typically bound in
Org-mode to the values they have in Org-mode, and then interactively
call CMD.

\(fn CMD)" nil nil)

(autoload 'org-store-link "org-mode/lisp/org" "\
\\<org-mode-map>Store an org-link to the current location.
This link is added to `org-stored-links' and can later be inserted
into an org-buffer with \\[org-insert-link].

For some link types, a prefix arg is interpreted.
For links to Usenet articles, arg negates `org-gnus-prefer-web-links'.
For file links, arg negates `org-context-in-file-links'.

A double prefix arg force skipping storing functions that are not
part of Org's core.

A triple prefix arg force storing a link for each line in the
active region.

\(fn ARG)" t nil)

(autoload 'org-insert-link-global "org-mode/lisp/org" "\
Insert a link like Org-mode does.
This command can be called in any mode to insert a link in Org-mode syntax.

\(fn)" t nil)

(autoload 'org-open-at-point-global "org-mode/lisp/org" "\
Follow a link like Org-mode does.
This command can be called in any mode to follow a link that has
Org-mode syntax.

\(fn)" t nil)

(autoload 'org-open-link-from-string "org-mode/lisp/org" "\
Open a link in the string S, as if it was in Org-mode.

\(fn S &optional ARG REFERENCE-BUFFER)" t nil)

(autoload 'org-switchb "org-mode/lisp/org" "\
Switch between Org buffers.
With one prefix argument, restrict available buffers to files.
With two prefix arguments, restrict available buffers to agenda files.

Defaults to `iswitchb' for buffer name completion.
Set `org-completion-use-ido' to make it use ido instead.

\(fn &optional ARG)" t nil)

(defalias 'org-ido-switchb 'org-switchb)

(defalias 'org-iswitchb 'org-switchb)

(autoload 'org-cycle-agenda-files "org-mode/lisp/org" "\
Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file.

\(fn)" t nil)

(autoload 'org-submit-bug-report "org-mode/lisp/org" "\
Submit a bug report on Org-mode via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org-mode version and configuration.

\(fn)" t nil)

(autoload 'org-reload "org-mode/lisp/org" "\
Reload all org lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions.

\(fn &optional UNCOMPILED)" t nil)

(autoload 'org-customize "org-mode/lisp/org" "\
Call the customize function with org as argument.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-agenda-to-appt org-calendar-goto-agenda org-agenda-set-restriction-lock
;;;;;;  org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item
;;;;;;  org-diary org-agenda-list-stuck-projects org-tags-view org-todo-list
;;;;;;  org-search-view org-agenda-list org-batch-store-agenda-views
;;;;;;  org-store-agenda-views org-batch-agenda-csv org-batch-agenda
;;;;;;  org-agenda org-toggle-sticky-agenda) "org-mode/lisp/org-agenda"
;;;;;;  "org-mode/lisp/org-agenda.el" (21170 11908 201240 820000))
;;; Generated autoloads from org-mode/lisp/org-agenda.el

(autoload 'org-toggle-sticky-agenda "org-mode/lisp/org-agenda" "\
Toggle `org-agenda-sticky'.

\(fn &optional ARG)" t nil)

(autoload 'org-agenda "org-mode/lisp/org-agenda" "\
Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a command to execute.  Any prefix arg will be passed
on to the selected command.  The default selections are:

a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
L     Create a timeline for the current buffer.
e     Export views to associated files.
s     Search entries for keywords.
S     Search entries for keywords, only with TODO keywords.
/     Multi occur across all agenda files and also files listed
      in `org-agenda-text-search-extra-files'.
<     Restrict agenda commands to buffer, subtree, or region.
      Press several times to get the desired effect.
>     Remove a previous restriction.
#     List \"stuck\" projects.
!     Configure what \"stuck\" means.
C     Configure custom agenda commands.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org-mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
\(until the next use of \\[org-agenda]) restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
\(if active).

\(fn &optional ARG ORG-KEYS RESTRICTION)" t nil)

(autoload 'org-batch-agenda "org-mode/lisp/org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

\(fn CMD-KEY &rest PARAMETERS)" nil t)

(autoload 'org-batch-agenda-csv "org-mode/lisp/org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        Sting with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed

\(fn CMD-KEY &rest PARAMETERS)" nil t)

(autoload 'org-store-agenda-views "org-mode/lisp/org-agenda" "\
Store agenda views.

\(fn &rest PARAMETERS)" t nil)

(autoload 'org-batch-store-agenda-views "org-mode/lisp/org-agenda" "\
Run all custom agenda commands that have a file argument.

\(fn &rest PARAMETERS)" nil t)

(autoload 'org-agenda-list "org-mode/lisp/org-agenda" "\
Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm.

\(fn &optional ARG START-DAY SPAN WITH-HOUR)" t nil)

(autoload 'org-search-view "org-mode/lisp/org-agenda" "\
Show all entries that contain a phrase or words or regular expressions.

With optional prefix argument TODO-ONLY, only consider entries that are
TODO entries.  The argument STRING can be used to pass a default search
string into this function.  If EDIT-AT is non-nil, it means that the
user should get a chance to edit this string, with cursor at position
EDIT-AT.

The search string can be viewed either as a phrase that should be found as
is, or it can be broken into a number of snippets, each of which must match
in a Boolean way to select an entry.  The default depends on the variable
`org-agenda-search-view-always-boolean'.
Even if this is turned off (the default) you can always switch to
Boolean search dynamically by preceding the first word with  \"+\" or \"-\".

The default is a direct search of the whole phrase, where each space in
the search string can expand to an arbitrary amount of whitespace,
including newlines.

If using a Boolean search, the search string is split on whitespace and
each snippet is searched separately, with logical AND to select an entry.
Words prefixed with a minus must *not* occur in the entry.  Words without
a prefix or prefixed with a plus must occur in the entry.  Matching is
case-insensitive.  Words are enclosed by word delimiters (i.e. they must
match whole words, not parts of a word) if
`org-agenda-search-view-force-full-words' is set (default is nil).

Boolean search snippets enclosed by curly braces are interpreted as
regular expressions that must or (when preceded with \"-\") must not
match in the entry.  Snippets enclosed into double quotes will be taken
as a whole, to include whitespace.

- If the search string starts with an asterisk, search only in headlines.
- If (possibly after the leading star) the search string starts with an
  exclamation mark, this also means to look at TODO entries only, an effect
  that can also be achieved with a prefix argument.
- If (possibly after star and exclamation mark) the search string starts
  with a colon, this will mean that the (non-regexp) snippets of the
  Boolean search must match as full words.

This command searches the agenda files, and in addition the files listed
in `org-agenda-text-search-extra-files'.

\(fn &optional TODO-ONLY STRING EDIT-AT)" t nil)

(autoload 'org-todo-list "org-mode/lisp/org-agenda" "\
Show all (not done) TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using \\[universal-argument], you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'.

\(fn &optional ARG)" t nil)

(autoload 'org-tags-view "org-mode/lisp/org-agenda" "\
Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries.

\(fn &optional TODO-ONLY MATCH)" t nil)

(autoload 'org-agenda-list-stuck-projects "org-mode/lisp/org-agenda" "\
Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'.

\(fn &rest IGNORE)" t nil)

(autoload 'org-diary "org-mode/lisp/org-agenda" "\
Return diary information from org files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  For a list of arguments allowed here, see the
variable `org-agenda-entry-types'.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default value
of `org-agenda-entry-types' is used: (:deadline :scheduled :timestamp :sexp).
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead.

\(fn &rest ARGS)" nil nil)

(autoload 'org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item "org-mode/lisp/org-agenda" "\
Do we have a reason to ignore this TODO entry because it has a time stamp?

\(fn &optional END)" nil nil)

(autoload 'org-agenda-set-restriction-lock "org-mode/lisp/org-agenda" "\
Set restriction lock for agenda, to current subtree or file.
Restriction will be the file if TYPE is `file', or if type is the
universal prefix '(4), or if the cursor is before the first headline
in the file.  Otherwise, restriction will be to the current subtree.

\(fn &optional TYPE)" t nil)

(autoload 'org-calendar-goto-agenda "org-mode/lisp/org-agenda" "\
Compute the Org-mode agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'.

\(fn)" t nil)

(autoload 'org-agenda-to-appt "org-mode/lisp/org-agenda" "\
Activate appointments found in `org-agenda-files'.
With a \\[universal-argument] prefix, refresh the list of
appointments.

If FILTER is t, interactively prompt the user for a regular
expression, and filter out entries that don't match it.

If FILTER is a string, use this string as a regular expression
for filtering entries out.

If FILTER is a function, filter out entries against which
calling the function returns nil.  This function takes one
argument: an entry from `org-agenda-get-day-entries'.

FILTER can also be an alist with the car of each cell being
either 'headline or 'category.  For example:

  '((headline \"IMPORTANT\")
    (category \"Work\"))

will only add headlines containing IMPORTANT or headlines
belonging to the \"Work\" category.

ARGS are symbols indicating what kind of entries to consider.
By default `org-agenda-to-appt' will use :deadline*, :scheduled*
\(i.e., deadlines and scheduled items with a hh:mm specification)
and :timestamp entries.  See the docstring of `org-diary' for
details and examples.

If an entry has a APPT_WARNTIME property, its value will be used
to override `appt-message-warning-time'.

\(fn &optional REFRESH FILTER &rest ARGS)" t nil)

;;;***

;;;### (autoloads (org-capture-import-remember-templates org-capture
;;;;;;  org-capture-string) "org-mode/lisp/org-capture" "org-mode/lisp/org-capture.el"
;;;;;;  (21170 11908 209240 820000))
;;; Generated autoloads from org-mode/lisp/org-capture.el

(autoload 'org-capture-string "org-mode/lisp/org-capture" "\
Capture STRING with the template selected by KEYS.

\(fn STRING &optional KEYS)" t nil)

(autoload 'org-capture "org-mode/lisp/org-capture" "\
Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and then
file the newly captured information.  The text is immediately inserted
at the target location, and an indirect buffer is shown where you can
edit it.  Pressing \\[org-capture-finalize] brings you back to the previous state
of Emacs, so that you can continue your work.

When called interactively with a \\[universal-argument] prefix argument GOTO, don't capture
anything, just go to the file/headline where the selected template
stores its notes.  With a double prefix argument \\[universal-argument] \\[universal-argument], go to the last note
stored.

When called with a `C-0' (zero) prefix, insert a template at point.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time.

\(fn &optional GOTO KEYS)" t nil)

(autoload 'org-capture-import-remember-templates "org-mode/lisp/org-capture" "\
Set `org-capture-templates' to be similar to `org-remember-templates'.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-agenda-columns org-insert-columns-dblock org-dblock-write:columnview
;;;;;;  org-columns-number-to-string org-columns-compute org-columns
;;;;;;  org-columns-get-format-and-top-level org-columns-remove-overlays)
;;;;;;  "org-mode/lisp/org-colview" "org-mode/lisp/org-colview.el"
;;;;;;  (21170 11908 213240 820000))
;;; Generated autoloads from org-mode/lisp/org-colview.el

(autoload 'org-columns-remove-overlays "org-mode/lisp/org-colview" "\
Remove all currently active column overlays.

\(fn)" t nil)

(autoload 'org-columns-get-format-and-top-level "org-mode/lisp/org-colview" "\


\(fn)" nil nil)

(autoload 'org-columns "org-mode/lisp/org-colview" "\
Turn on column view on an org-mode file.
When COLUMNS-FMT-STRING is non-nil, use it as the column format.

\(fn &optional COLUMNS-FMT-STRING)" t nil)

(autoload 'org-columns-compute "org-mode/lisp/org-colview" "\
Sum the values of property PROPERTY hierarchically, for the entire buffer.

\(fn PROPERTY)" t nil)

(autoload 'org-columns-number-to-string "org-mode/lisp/org-colview" "\
Convert a computed column number to a string value, according to FMT.

\(fn N FMT &optional PRINTF)" nil nil)

(autoload 'org-dblock-write:columnview "org-mode/lisp/org-colview" "\
Write the column view table.
PARAMS is a property list of parameters:

:width    enforce same column widths with <N> specifiers.
:id       the :ID: property of the entry where the columns view
	  should be built.  When the symbol `local', call locally.
	  When `global' call column view with the cursor at the beginning
	  of the buffer (usually this means that the whole buffer switches
	  to column view).  When \"file:path/to/file.org\", invoke column
	  view at the start of that file.  Otherwise, the ID is located
	  using `org-id-find'.
:hlines   When t, insert a hline before each item.  When a number, insert
	  a hline before each level <= that number.
:vlines   When t, make each column a colgroup to enforce vertical lines.
:maxlevel When set to a number, don't capture headlines below this level.
:skip-empty-rows
	  When t, skip rows where all specifiers other than ITEM are empty.
:format   When non-nil, specify the column view format to use.

\(fn PARAMS)" nil nil)

(autoload 'org-insert-columns-dblock "org-mode/lisp/org-colview" "\
Create a dynamic block capturing a column view table.

\(fn)" t nil)

(autoload 'org-agenda-columns "org-mode/lisp/org-colview" "\
Turn on or update column view in the agenda.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-check-version) "org-mode/lisp/org-compat"
;;;;;;  "org-mode/lisp/org-compat.el" (21170 11908 213240 820000))
;;; Generated autoloads from org-mode/lisp/org-compat.el

(autoload 'org-check-version "org-mode/lisp/org-compat" "\
Try very hard to provide sensible version strings.

\(fn)" nil t)

;;;***

;;;### (autoloads (org-load-noerror-mustsuffix) "org-mode/lisp/org-macs"
;;;;;;  "org-mode/lisp/org-macs.el" (21170 11908 233240 820000))
;;; Generated autoloads from org-mode/lisp/org-macs.el

(autoload 'org-load-noerror-mustsuffix "org-mode/lisp/org-macs" "\
Load FILE with optional arguments NOERROR and MUSTSUFFIX.  Drop the MUSTSUFFIX argument for XEmacs, which doesn't recognize it.

\(fn FILE)" nil t)

;;;***

;;;### (autoloads (org-git-version org-release) "org-mode/lisp/org-version"
;;;;;;  "org-mode/lisp/org-version.el" (21170 11933 249241 270000))
;;; Generated autoloads from org-mode/lisp/org-version.el

(autoload 'org-release "org-mode/lisp/org-version" "\
The release version of org-mode.
  Inserted by installing org-mode or when a release is made.

\(fn)" nil nil)

(autoload 'org-git-version "org-mode/lisp/org-version" "\
The Git version of org-mode.
  Inserted by installing org-mode or when a release is made.

\(fn)" nil nil)

(defvar org-odt-data-dir "/usr/share/emacs/etc/org" "\
The location of ODT styles.")

;;;***

;;;### (autoloads (org-tree-slide-skip-comments-toggle org-tree-slide-skip-done-toggle
;;;;;;  org-tree-slide-heading-emphasis-toggle org-tree-slide-slide-in-effect-toggle
;;;;;;  org-tree-slide-display-header-toggle org-tree-slide-narrowing-control-profile
;;;;;;  org-tree-slide-presentation-profile org-tree-slide-simple-profile
;;;;;;  org-tree-slide-content org-tree-slide-without-init-play org-tree-slide-play-with-timer
;;;;;;  org-tree-slide-mode) "org-tree-slide/org-tree-slide" "org-tree-slide/org-tree-slide.el"
;;;;;;  (21170 11955 617241 672000))
;;; Generated autoloads from org-tree-slide/org-tree-slide.el

(autoload 'org-tree-slide-mode "org-tree-slide/org-tree-slide" "\
A presentation tool for org-mode.

Usage:
  - Set minimal recommendation settings in .emacs
    (global-set-key (kbd \"<f8>\") 'org-tree-slide-mode)
    (global-set-key (kbd \"S-<f8>\") 'org-tree-slide-skip-done-toggle)
  - Open an org file
  - Type <f8> to start org-tree-slide-mode
  - Type <left>/<right> to move between trees
  - To exit this minor mode, just type <f8> again.

Profiles:

  - [ Simple ]
 => M-x `org-tree-slide-simple-profile'

    1. No header display
    2. No slide-in effect
    3. The cursor will move to the head of buffer when exit
    4. No slide number display in mode line
    5. Display every type of tree

  - [ Presentation ]
 => M-x `org-tree-slide-presentation-profile'

    1. Display header
    2. Enable slide-in effect
    3. The cursor will move to the head of buffer when exit
    4. Display slide number in mode line
    5. Display every type of tree

  - [ TODO Pursuit with narrowing ]
 => M-x `org-tree-slide-narrowing-control-profile'

    1. No header display
    2. No slide-in effect
    3. The cursor will keep the same position when exit
    4. Display slide number in mode line
    5. Display TODO trees only

\(fn &optional ARG)" t nil)

(autoload 'org-tree-slide-play-with-timer "org-tree-slide/org-tree-slide" "\
Start slideshow with setting a count down timer.

\(fn)" t nil)

(autoload 'org-tree-slide-without-init-play "org-tree-slide/org-tree-slide" "\
Start slideshow without the init play. Just enter org-tree-slide-mode.

\(fn)" t nil)

(autoload 'org-tree-slide-content "org-tree-slide/org-tree-slide" "\
Change the display for viewing content of the org file during
   the slide view mode is active.

\(fn)" t nil)

(autoload 'org-tree-slide-simple-profile "org-tree-slide/org-tree-slide" "\
Set variables for simple use.
  `org-tree-slide-header'            => nil
  `org-tree-slide-slide-in-effect'   => nil
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => t
  `org-tree-slide-modeline-display'  => nil
  `org-tree-slide-skip-done'         => nil
  `org-tree-slide-skip-comments'     => t

\(fn)" t nil)

(autoload 'org-tree-slide-presentation-profile "org-tree-slide/org-tree-slide" "\
Set variables for presentation use.
  `org-tree-slide-header'            => t
  `org-tree-slide-slide-in-effect'   => t
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => t
  `org-tree-slide-modeline-display'  => 'outside
  `org-tree-slide-skip-done'         => nil
  `org-tree-slide-skip-comments'     => t

\(fn)" t nil)

(autoload 'org-tree-slide-narrowing-control-profile "org-tree-slide/org-tree-slide" "\
Set variables for TODO pursuit with narrowing.
  `org-tree-slide-header'            => nil
  `org-tree-slide-slide-in-effect'   => nil
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => nil
  `org-tree-slide-modeline-display'  => 'lighter
  `org-tree-slide-skip-done'         => t
  `org-tree-slide-skip-comments'     => t

\(fn)" t nil)

(autoload 'org-tree-slide-display-header-toggle "org-tree-slide/org-tree-slide" "\
Toggle displaying the slide header

\(fn)" t nil)

(autoload 'org-tree-slide-slide-in-effect-toggle "org-tree-slide/org-tree-slide" "\
Toggle using slide-in effect

\(fn)" t nil)

(autoload 'org-tree-slide-heading-emphasis-toggle "org-tree-slide/org-tree-slide" "\
Toggle applying emphasis to heading

\(fn)" t nil)

(autoload 'org-tree-slide-skip-done-toggle "org-tree-slide/org-tree-slide" "\
Toggle show TODO item only or not

\(fn)" t nil)

(autoload 'org-tree-slide-skip-comments-toggle "org-tree-slide/org-tree-slide" "\
Toggle show COMMENT item or not

\(fn)" t nil)

;;;***

;;;### (autoloads (paredit-mode) "paredit/paredit" "paredit/paredit.el"
;;;;;;  (21170 11964 733241 835000))
;;; Generated autoloads from paredit/paredit.el

(autoload 'paredit-mode "paredit/paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  unbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are unbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing unbalanced parentheses instead.
\\<paredit-mode-map>

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (pkg-info-version-info pkg-info-package-version
;;;;;;  pkg-info-defining-library-version pkg-info-library-version)
;;;;;;  "pkg-info/pkg-info" "pkg-info/pkg-info.el" (21170 10618 53217
;;;;;;  643000))
;;; Generated autoloads from pkg-info/pkg-info.el

(autoload 'pkg-info-library-version "pkg-info/pkg-info" "\
Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

\(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-version "pkg-info/pkg-info" "\
Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION (as by
`pkg-info-locate-library-version').  Signal an error if FUNCTION
is not a valid function, if its defining library was not found,
or if the library had no proper version header.

\(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-package-version "pkg-info/pkg-info" "\
Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

\(fn PACKAGE &optional SHOW)" t nil)

(autoload 'pkg-info-version-info "pkg-info/pkg-info" "\
Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

\(fn LIBRARY &optional PACKAGE SHOW)" t nil)

;;;***

;;;### (autoloads (turn-on-pretty-mode global-pretty-mode pretty-mode)
;;;;;;  "pretty-mode/pretty-mode" "pretty-mode/pretty-mode.el" (21170
;;;;;;  11967 77241 877000))
;;; Generated autoloads from pretty-mode/pretty-mode.el

(autoload 'pretty-mode "pretty-mode/pretty-mode" "\
Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes.

\(fn &optional ARG)" t nil)

(defvar global-pretty-mode t "\
Non-nil if Global-Pretty mode is enabled.
See the command `global-pretty-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pretty-mode'.")

(custom-autoload 'global-pretty-mode "pretty-mode/pretty-mode" nil)

(autoload 'global-pretty-mode "pretty-mode/pretty-mode" "\
Toggle Pretty mode in all buffers.
With prefix ARG, enable Global-Pretty mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Pretty mode is enabled in all buffers where
`turn-on-pretty-if-desired' would do it.
See `pretty-mode' for more information on Pretty mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-pretty-mode "pretty-mode/pretty-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (turn-off-show-smartparens-mode turn-on-show-smartparens-mode
;;;;;;  show-smartparens-global-mode show-smartparens-mode turn-off-smartparens-mode
;;;;;;  turn-on-smartparens-mode smartparens-global-mode turn-on-smartparens-strict-mode
;;;;;;  smartparens-global-strict-mode smartparens-strict-mode smartparens-mode
;;;;;;  sp-use-smartparens-bindings sp-use-paredit-bindings sp-cheat-sheet)
;;;;;;  "smartparens/smartparens" "smartparens/smartparens.el" (21170
;;;;;;  10603 309217 378000))
;;; Generated autoloads from smartparens/smartparens.el

(autoload 'sp-cheat-sheet "smartparens/smartparens" "\
Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation.

\(fn &optional ARG)" t nil)

(defvar sp-keymap (make-sparse-keymap) "\
Keymap used for `smartparens-mode'.")

(autoload 'sp-use-paredit-bindings "smartparens/smartparens" "\
Initiate `sp-keymap' with paredit-compatible bindings for
corresponding functions provided by smartparens.  See variable
`sp-paredit-bindings'.

\(fn)" t nil)

(autoload 'sp-use-smartparens-bindings "smartparens/smartparens" "\
Initiate `sp-keymap' with smartparens bindings for navigation functions.
See variable `sp-smartparens-bindings'.

\(fn)" t nil)

(autoload 'smartparens-mode "smartparens/smartparens" "\
Toggle smartparens mode.

You can enable pre-set bindings by customizing
`sp-base-key-bindings' variable.  The current content of
`sp-keymap' is:

 \\{sp-keymap}

\(fn &optional ARG)" t nil)

(autoload 'smartparens-strict-mode "smartparens/smartparens" "\
Toggle the strict smartparens mode.

When strict mode is active, `delete-char', `kill-word' and their
backward variants will skip over the pair delimiters in order to
keep the structure always valid (the same way as `paredit-mode'
does).  This is accomplished by remapping them to
`sp-delete-char' and `sp-kill-word'.  There is also function
`sp-kill-symbol' that deletes symbols instead of words, otherwise
working exactly the same (it is not bound to any key by default).

When strict mode is active, this is indicated with \"/s\"
after the smartparens indicator in the mode list.

\(fn &optional ARG)" t nil)

(defvar smartparens-global-strict-mode nil "\
Non-nil if Smartparens-Global-Strict mode is enabled.
See the command `smartparens-global-strict-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-strict-mode'.")

(custom-autoload 'smartparens-global-strict-mode "smartparens/smartparens" nil)

(autoload 'smartparens-global-strict-mode "smartparens/smartparens" "\
Toggle Smartparens-Strict mode in all buffers.
With prefix ARG, enable Smartparens-Global-Strict mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens-Strict mode is enabled in all buffers where
`turn-on-smartparens-strict-mode' would do it.
See `smartparens-strict-mode' for more information on Smartparens-Strict mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-strict-mode "smartparens/smartparens" "\
Turn on `smartparens-mode'.

\(fn)" t nil)

(defvar smartparens-global-mode nil "\
Non-nil if Smartparens-Global mode is enabled.
See the command `smartparens-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-mode'.")

(custom-autoload 'smartparens-global-mode "smartparens/smartparens" nil)

(autoload 'smartparens-global-mode "smartparens/smartparens" "\
Toggle Smartparens mode in all buffers.
With prefix ARG, enable Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens mode is enabled in all buffers where
`turn-on-smartparens-mode' would do it.
See `smartparens-mode' for more information on Smartparens mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-mode "smartparens/smartparens" "\
Turn on `smartparens-mode'.

\(fn)" t nil)

(autoload 'turn-off-smartparens-mode "smartparens/smartparens" "\
Turn off `smartparens-mode'.

\(fn)" t nil)

(autoload 'show-smartparens-mode "smartparens/smartparens" "\
Toggle visualization of matching pairs.  When enabled, any
matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs.

\(fn &optional ARG)" t nil)

(defvar show-smartparens-global-mode nil "\
Non-nil if Show-Smartparens-Global mode is enabled.
See the command `show-smartparens-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `show-smartparens-global-mode'.")

(custom-autoload 'show-smartparens-global-mode "smartparens/smartparens" nil)

(autoload 'show-smartparens-global-mode "smartparens/smartparens" "\
Toggle Show-Smartparens mode in all buffers.
With prefix ARG, enable Show-Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Show-Smartparens mode is enabled in all buffers where
`turn-on-show-smartparens-mode' would do it.
See `show-smartparens-mode' for more information on Show-Smartparens mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-show-smartparens-mode "smartparens/smartparens" "\
Turn on `show-smartparens-mode'.

\(fn)" t nil)

(autoload 'turn-off-show-smartparens-mode "smartparens/smartparens" "\
Turn off `show-smartparens-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (smex-initialize smex) "smex/smex" "smex/smex.el"
;;;;;;  (21170 11976 361242 44000))
;;; Generated autoloads from smex/smex.el

(autoload 'smex "smex/smex" "\


\(fn)" t nil)

(autoload 'smex-initialize "smex/smex" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (tern-mode tern-use-server) "tern/emacs/tern" "tern/emacs/tern.el"
;;;;;;  (21170 11982 133242 148000))
;;; Generated autoloads from tern/emacs/tern.el

(autoload 'tern-use-server "tern/emacs/tern" "\


\(fn PORT)" t nil)

(autoload 'tern-mode "tern/emacs/tern" "\
Minor mode binding to the Tern JavaScript analyzer

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (tern-ac-setup) "tern/emacs/tern-auto-complete"
;;;;;;  "tern/emacs/tern-auto-complete.el" (21170 11982 133242 148000))
;;; Generated autoloads from tern/emacs/tern-auto-complete.el

(autoload 'tern-ac-setup "tern/emacs/tern-auto-complete" "\
Setup auto-complete for tern-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (global-undo-tree-mode undo-tree-mode) "undo-tree/undo-tree"
;;;;;;  "undo-tree/undo-tree.el" (21170 11992 801242 340000))
;;; Generated autoloads from undo-tree/undo-tree.el

(autoload 'undo-tree-mode "undo-tree/undo-tree" "\
Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-undo-tree-mode nil "\
Non-nil if Global-Undo-Tree mode is enabled.
See the command `global-undo-tree-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.")

(custom-autoload 'global-undo-tree-mode "undo-tree/undo-tree" nil)

(autoload 'global-undo-tree-mode "undo-tree/undo-tree" "\
Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global-Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (vr/query-replace vr/replace vr/mc-mark) "visual-regexp/visual-regexp"
;;;;;;  "visual-regexp/visual-regexp.el" (21170 10650 461218 225000))
;;; Generated autoloads from visual-regexp/visual-regexp.el

(autoload 'vr/mc-mark "visual-regexp/visual-regexp" "\
Convert regexp selection to multiple cursors.

\(fn REGEXP START END)" t nil)

(autoload 'vr/replace "visual-regexp/visual-regexp" "\
Regexp-replace with live visual feedback.

\(fn REGEXP REPLACE START END)" t nil)

(autoload 'vr/query-replace "visual-regexp/visual-regexp" "\
Use vr/query-replace like you would use query-replace-regexp.

\(fn REGEXP REPLACE START END)" t nil)

;;;***

;;;### (autoloads (wgrep-setup) "wgrep/wgrep" "wgrep/wgrep.el" (21170
;;;;;;  11997 817242 430000))
;;; Generated autoloads from wgrep/wgrep.el

(autoload 'wgrep-setup "wgrep/wgrep" "\
Setup wgrep preparation.

\(fn)" nil nil)
(add-hook 'grep-setup-hook 'wgrep-setup)

;;;***

;;;### (autoloads (wgrep-ack-setup wgrep-ack-and-a-half-setup) "wgrep/wgrep-ack"
;;;;;;  "wgrep/wgrep-ack.el" (21170 11997 817242 430000))
;;; Generated autoloads from wgrep/wgrep-ack.el

(autoload 'wgrep-ack-and-a-half-setup "wgrep/wgrep-ack" "\


\(fn)" nil nil)

(autoload 'wgrep-ack-setup "wgrep/wgrep-ack" "\


\(fn)" nil nil)
(add-hook 'ack-and-a-half-mode-hook 'wgrep-ack-and-a-half-setup)
(add-hook 'ack-mode-hook 'wgrep-ack-setup)

;;;***

;;;### (autoloads (wgrep-ag-setup) "wgrep/wgrep-ag" "wgrep/wgrep-ag.el"
;;;;;;  (21170 11997 817242 430000))
;;; Generated autoloads from wgrep/wgrep-ag.el

(autoload 'wgrep-ag-setup "wgrep/wgrep-ag" "\


\(fn)" nil nil)

(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;;;***

;;;### (autoloads (wgrep-helm-setup) "wgrep/wgrep-helm" "wgrep/wgrep-helm.el"
;;;;;;  (21170 11997 817242 430000))
;;; Generated autoloads from wgrep/wgrep-helm.el

(autoload 'wgrep-helm-setup "wgrep/wgrep-helm" "\


\(fn)" nil nil)
(add-hook 'helm-grep-mode-hook 'wgrep-helm-setup)

;;;***

;;;### (autoloads (yas-global-mode yas-minor-mode) "yasnippet/yasnippet"
;;;;;;  "yasnippet/yasnippet.el" (21170 12005 873242 574000))
;;; Generated autoloads from yasnippet/yasnippet.el

(autoload 'yas-minor-mode "yasnippet/yasnippet" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

\(fn &optional ARG)" t nil)

(defvar yas-global-mode nil "\
Non-nil if Yas-Global mode is enabled.
See the command `yas-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.")

(custom-autoload 'yas-global-mode "yasnippet/yasnippet" nil)

(autoload 'yas-global-mode "yasnippet/yasnippet" "\
Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-complete-emacs-lisp/auto-complete-emacs-lisp.el"
;;;;;;  "auto-complete/auto-complete-config.el" "auto-complete/auto-complete-pkg.el"
;;;;;;  "auto-indent-mode/auto-indent-test.el" "cider/cider-client.el"
;;;;;;  "cider/cider-eldoc.el" "cider/cider-interaction.el" "cider/cider-repl.el"
;;;;;;  "cider/cider-util.el" "cider/cider-version.el" "cider/nrepl-client.el"
;;;;;;  "color-theme/color-theme-autoloads.el" "control-lock/control-lock.el"
;;;;;;  "dash/dash-functional.el" "dash/dash.el" "el-get/el-get-autoloads.el"
;;;;;;  "el-get/el-get-build.el" "el-get/el-get-byte-compile.el"
;;;;;;  "el-get/el-get-core.el" "el-get/el-get-custom.el" "el-get/el-get-dependencies.el"
;;;;;;  "el-get/el-get-install.el" "el-get/el-get-methods.el" "el-get/el-get-notify.el"
;;;;;;  "el-get/el-get-recipes.el" "el-get/el-get-status.el" "emacs-async/async-file.el"
;;;;;;  "emacs-async/async-pkg.el" "emacs-async/async-test.el" "emacs-async/dired-async.el"
;;;;;;  "emacs-async/helm-async.el" "emacs-async/smtpmail-async.el"
;;;;;;  "epl/epl.el" "expand-region/cc-mode-expansions.el" "expand-region/clojure-mode-expansions.el"
;;;;;;  "expand-region/cperl-mode-expansions.el" "expand-region/css-mode-expansions.el"
;;;;;;  "expand-region/enh-ruby-mode-expansions.el" "expand-region/er-basic-expansions.el"
;;;;;;  "expand-region/erlang-mode-expansions.el" "expand-region/expand-region-core.el"
;;;;;;  "expand-region/expand-region-pkg.el" "expand-region/feature-mode-expansions.el"
;;;;;;  "expand-region/html-mode-expansions.el" "expand-region/js-mode-expansions.el"
;;;;;;  "expand-region/js2-mode-expansions.el" "expand-region/jsp-expansions.el"
;;;;;;  "expand-region/latex-mode-expansions.el" "expand-region/nxml-mode-expansions.el"
;;;;;;  "expand-region/octave-expansions.el" "expand-region/python-el-expansions.el"
;;;;;;  "expand-region/python-el-fgallina-expansions.el" "expand-region/python-mode-expansions.el"
;;;;;;  "expand-region/ruby-mode-expansions.el" "expand-region/sml-mode-expansions.el"
;;;;;;  "expand-region/text-mode-expansions.el" "expand-region/the-org-mode-expansions.el"
;;;;;;  "expand-region/web-mode-expansions.el" "fill-column-indicator/fci-osx-23-fix.el"
;;;;;;  "flx-ido/flx.el" "flymake-easy/flymake-easy.el" "fuzzy/fuzzy.el"
;;;;;;  "haskell-mode/haskell-bot.el" "haskell-mode/haskell-checkers.el"
;;;;;;  "haskell-mode/haskell-compat.el" "haskell-mode/haskell-mode-autoloads.el"
;;;;;;  "haskell-mode/haskell-package.el" "haskell-mode/haskell-presentation-mode.el"
;;;;;;  "haskell-mode/haskell-str.el" "haskell-mode/haskell-utils.el"
;;;;;;  "highlight-sexp/highlight-sexp.el" "ido-ubiquitous/ido-ubiquitous-autoloads.el"
;;;;;;  "ido-ubiquitous/ido-ubiquitous-pkg.el" "magit/magit-autoloads.el"
;;;;;;  "magit/magit-version.el" "multiple-cursors/mc-cycle-cursors.el"
;;;;;;  "multiple-cursors/multiple-cursors-core.el" "multiple-cursors/multiple-cursors-pkg.el"
;;;;;;  "multiple-cursors/multiple-cursors.el" "notmuch/emacs/coolj.el"
;;;;;;  "notmuch/emacs/make-deps.el" "notmuch/emacs/notmuch-address.el"
;;;;;;  "notmuch/emacs/notmuch-crypto.el" "notmuch/emacs/notmuch-lib.el"
;;;;;;  "notmuch/emacs/notmuch-maildir-fcc.el" "notmuch/emacs/notmuch-message.el"
;;;;;;  "notmuch/emacs/notmuch-mua.el" "notmuch/emacs/notmuch-parser.el"
;;;;;;  "notmuch/emacs/notmuch-print.el" "notmuch/emacs/notmuch-query.el"
;;;;;;  "notmuch/emacs/notmuch-tag.el" "notmuch/emacs/notmuch-tree.el"
;;;;;;  "notmuch/emacs/notmuch-wash.el" "org-mode/contrib/lisp/ob-D.el"
;;;;;;  "org-mode/contrib/lisp/ob-eukleides.el" "org-mode/contrib/lisp/ob-fomus.el"
;;;;;;  "org-mode/contrib/lisp/ob-julia.el" "org-mode/contrib/lisp/ob-mathomatic.el"
;;;;;;  "org-mode/contrib/lisp/ob-oz.el" "org-mode/contrib/lisp/ob-tcl.el"
;;;;;;  "org-mode/contrib/lisp/org-annotate-file.el" "org-mode/contrib/lisp/org-bibtex-extras.el"
;;;;;;  "org-mode/contrib/lisp/org-bookmark.el" "org-mode/contrib/lisp/org-checklist.el"
;;;;;;  "org-mode/contrib/lisp/org-choose.el" "org-mode/contrib/lisp/org-collector.el"
;;;;;;  "org-mode/contrib/lisp/org-contribdir.el" "org-mode/contrib/lisp/org-depend.el"
;;;;;;  "org-mode/contrib/lisp/org-drill.el" "org-mode/contrib/lisp/org-effectiveness.el"
;;;;;;  "org-mode/contrib/lisp/org-elisp-symbol.el" "org-mode/contrib/lisp/org-eval-light.el"
;;;;;;  "org-mode/contrib/lisp/org-eval.el" "org-mode/contrib/lisp/org-expiry.el"
;;;;;;  "org-mode/contrib/lisp/org-git-link.el" "org-mode/contrib/lisp/org-index.el"
;;;;;;  "org-mode/contrib/lisp/org-interactive-query.el" "org-mode/contrib/lisp/org-invoice.el"
;;;;;;  "org-mode/contrib/lisp/org-jira.el" "org-mode/contrib/lisp/org-learn.el"
;;;;;;  "org-mode/contrib/lisp/org-license.el" "org-mode/contrib/lisp/org-mac-iCal.el"
;;;;;;  "org-mode/contrib/lisp/org-mac-link.el" "org-mode/contrib/lisp/org-mairix.el"
;;;;;;  "org-mode/contrib/lisp/org-man.el" "org-mode/contrib/lisp/org-mew.el"
;;;;;;  "org-mode/contrib/lisp/org-mime.el" "org-mode/contrib/lisp/org-mtags.el"
;;;;;;  "org-mode/contrib/lisp/org-notify.el" "org-mode/contrib/lisp/org-notmuch.el"
;;;;;;  "org-mode/contrib/lisp/org-panel.el" "org-mode/contrib/lisp/org-screen.el"
;;;;;;  "org-mode/contrib/lisp/org-secretary.el" "org-mode/contrib/lisp/org-static-mathjax.el"
;;;;;;  "org-mode/contrib/lisp/org-sudoku.el" "org-mode/contrib/lisp/org-velocity.el"
;;;;;;  "org-mode/contrib/lisp/org-vm.el" "org-mode/contrib/lisp/org-wikinodes.el"
;;;;;;  "org-mode/contrib/lisp/org-wl.el" "org-mode/contrib/lisp/orgtbl-sqlinsert.el"
;;;;;;  "org-mode/contrib/lisp/ox-bibtex.el" "org-mode/contrib/lisp/ox-confluence.el"
;;;;;;  "org-mode/contrib/lisp/ox-deck.el" "org-mode/contrib/lisp/ox-groff.el"
;;;;;;  "org-mode/contrib/lisp/ox-s5.el" "org-mode/lisp/ob-C.el"
;;;;;;  "org-mode/lisp/ob-R.el" "org-mode/lisp/ob-abc.el" "org-mode/lisp/ob-asymptote.el"
;;;;;;  "org-mode/lisp/ob-awk.el" "org-mode/lisp/ob-calc.el" "org-mode/lisp/ob-clojure.el"
;;;;;;  "org-mode/lisp/ob-comint.el" "org-mode/lisp/ob-core.el" "org-mode/lisp/ob-css.el"
;;;;;;  "org-mode/lisp/ob-ditaa.el" "org-mode/lisp/ob-dot.el" "org-mode/lisp/ob-ebnf.el"
;;;;;;  "org-mode/lisp/ob-emacs-lisp.el" "org-mode/lisp/ob-eval.el"
;;;;;;  "org-mode/lisp/ob-exp.el" "org-mode/lisp/ob-fortran.el" "org-mode/lisp/ob-gnuplot.el"
;;;;;;  "org-mode/lisp/ob-haskell.el" "org-mode/lisp/ob-io.el" "org-mode/lisp/ob-java.el"
;;;;;;  "org-mode/lisp/ob-js.el" "org-mode/lisp/ob-keys.el" "org-mode/lisp/ob-latex.el"
;;;;;;  "org-mode/lisp/ob-ledger.el" "org-mode/lisp/ob-lilypond.el"
;;;;;;  "org-mode/lisp/ob-lisp.el" "org-mode/lisp/ob-lob.el" "org-mode/lisp/ob-makefile.el"
;;;;;;  "org-mode/lisp/ob-matlab.el" "org-mode/lisp/ob-maxima.el"
;;;;;;  "org-mode/lisp/ob-mscgen.el" "org-mode/lisp/ob-ocaml.el"
;;;;;;  "org-mode/lisp/ob-octave.el" "org-mode/lisp/ob-org.el" "org-mode/lisp/ob-perl.el"
;;;;;;  "org-mode/lisp/ob-picolisp.el" "org-mode/lisp/ob-plantuml.el"
;;;;;;  "org-mode/lisp/ob-python.el" "org-mode/lisp/ob-ref.el" "org-mode/lisp/ob-ruby.el"
;;;;;;  "org-mode/lisp/ob-sass.el" "org-mode/lisp/ob-scala.el" "org-mode/lisp/ob-scheme.el"
;;;;;;  "org-mode/lisp/ob-screen.el" "org-mode/lisp/ob-shell.el"
;;;;;;  "org-mode/lisp/ob-shen.el" "org-mode/lisp/ob-sql.el" "org-mode/lisp/ob-sqlite.el"
;;;;;;  "org-mode/lisp/ob-table.el" "org-mode/lisp/ob-tangle.el"
;;;;;;  "org-mode/lisp/ob.el" "org-mode/lisp/org-archive.el" "org-mode/lisp/org-attach.el"
;;;;;;  "org-mode/lisp/org-bbdb.el" "org-mode/lisp/org-bibtex.el"
;;;;;;  "org-mode/lisp/org-clock.el" "org-mode/lisp/org-crypt.el"
;;;;;;  "org-mode/lisp/org-ctags.el" "org-mode/lisp/org-datetree.el"
;;;;;;  "org-mode/lisp/org-docview.el" "org-mode/lisp/org-element.el"
;;;;;;  "org-mode/lisp/org-entities.el" "org-mode/lisp/org-eshell.el"
;;;;;;  "org-mode/lisp/org-faces.el" "org-mode/lisp/org-feed.el"
;;;;;;  "org-mode/lisp/org-footnote.el" "org-mode/lisp/org-gnus.el"
;;;;;;  "org-mode/lisp/org-habit.el" "org-mode/lisp/org-id.el" "org-mode/lisp/org-indent.el"
;;;;;;  "org-mode/lisp/org-info.el" "org-mode/lisp/org-inlinetask.el"
;;;;;;  "org-mode/lisp/org-install.el" "org-mode/lisp/org-irc.el"
;;;;;;  "org-mode/lisp/org-list.el" "org-mode/lisp/org-macro.el"
;;;;;;  "org-mode/lisp/org-mhe.el" "org-mode/lisp/org-mobile.el"
;;;;;;  "org-mode/lisp/org-mouse.el" "org-mode/lisp/org-pcomplete.el"
;;;;;;  "org-mode/lisp/org-plot.el" "org-mode/lisp/org-protocol.el"
;;;;;;  "org-mode/lisp/org-rmail.el" "org-mode/lisp/org-src.el" "org-mode/lisp/org-table.el"
;;;;;;  "org-mode/lisp/org-timer.el" "org-mode/lisp/org-w3m.el" "org-mode/lisp/ox-ascii.el"
;;;;;;  "org-mode/lisp/ox-beamer.el" "org-mode/lisp/ox-html.el" "org-mode/lisp/ox-icalendar.el"
;;;;;;  "org-mode/lisp/ox-latex.el" "org-mode/lisp/ox-man.el" "org-mode/lisp/ox-md.el"
;;;;;;  "org-mode/lisp/ox-odt.el" "org-mode/lisp/ox-org.el" "org-mode/lisp/ox-publish.el"
;;;;;;  "org-mode/lisp/ox-texinfo.el" "org-mode/lisp/ox.el" "ox-reveal/ox-reveal.el"
;;;;;;  "popup/popup.el" "s/s.el" "smartparens/smartparens-config.el"
;;;;;;  "smartparens/smartparens-html.el" "smartparens/smartparens-latex.el"
;;;;;;  "smartparens/smartparens-lua.el" "smartparens/smartparens-pkg.el"
;;;;;;  "smartparens/smartparens-ruby.el" "wgrep/wgrep-test.el" "yasnippet/yasnippet-debug.el"
;;;;;;  "yasnippet/yasnippet-tests.el") (21176 6753 486817 838000))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
