;; Personal global mode so that keybindings applies everywhere
;; see http://www.reddit.com/r/emacs/comments/y76sl/proper_way_of_overriding_mode_keys/
(require 'line-utils)
(require 'file-utils)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(define-minor-mode ktm-mode
  "Ktm mode"
  :init-value nil
  :lighter " ktm"
  :keymap
  (let ((ktm-mode-map (make-sparse-keymap)))
    (define-key ktm-mode-map (kbd "C-S-k") (lambda ()
                                       (interactive)
                                       (kill-buffer
                                        (current-buffer))))

    (define-key ktm-mode-map (kbd "C-c o") 'browse-url-at-point)
    (define-key ktm-mode-map (kbd "<S-f1>") 'elisp-index-search)
    (define-key ktm-mode-map (kbd "C-x C-r") 'rename-current-buffer-file)
    (define-key ktm-mode-map (kbd "C-x r r") 'rotate-windows)

    ;; (define-key ktm-mode-map "\r" 'newline-and-indent)
    (define-key ktm-mode-map (kbd "C-;") 'comment-region)
    ;; (define-key ktm-mode-map (kbd "C-k") 'eager-kill-line)

    ;;; MOVEMENTS, DEPLACEMENTS
    (define-key ktm-mode-map (kbd "C-p") 'backward-char)
    (define-key ktm-mode-map (kbd "C-S-p") 'previous-line)
    (define-key ktm-mode-map (kbd "C-S-a") 'beginning-of-buffer)
    (define-key ktm-mode-map (kbd "C-S-e") 'end-of-buffer)
    (define-key ktm-mode-map (kbd "M-p") '(lambda (arg)
                                   (interactive "p")
                                   (previous-line (+ arg 4) nil)))
    (define-key ktm-mode-map (kbd "M-n") '(lambda (arg)
                                   (interactive "p")
                                   (next-line (+ arg 4) nil)))
    (define-key ktm-mode-map (kbd "C-a") 'back-to-indentation-or-beginning-of-line)
    (define-key ktm-mode-map (kbd "M-g") 'goto-line-with-feedback)
    (define-key ktm-mode-map (kbd "M-j") 'ace-jump-mode)
    (define-key ktm-mode-map (kbd "C-x p") 'pop-to-mark-command)
    (define-key ktm-mode-map (kbd "<C-tab>") 'other-window)
    (define-key ktm-mode-map (kbd "<C-S-down>") 'move-line-down)
    (define-key ktm-mode-map (kbd "<C-S-up>") 'move-line-up)

    (define-key ktm-mode-map (kbd "C-S-j") 'join-line)
    ;; (define-key ktm-mode-map (kbd "C-S-d") 'duplicate-line)
    ;; (define-key ktm-mode-map (kbd "C-S-d") 'nil)


    (define-key ktm-mode-map (kbd "C-M-s") 'ack-and-a-half)

    (define-key ktm-mode-map (kbd "C-c l") 'org-store-link)

    (define-key ktm-mode-map (kbd "C-x b") 'ido-switch-buffer)


    ;; shift rocks
    (define-key ktm-mode-map (kbd "C-S-o") 'ido-switch-buffer)
    (define-key ktm-mode-map (kbd "C-S-n") 'next-error)
    (define-key ktm-mode-map (kbd "C-S-f") 'ido-find-file)

    (define-key ktm-mode-map [f1] 'multi-term)
    (define-key ktm-mode-map [f2] 'multi-term-prev)
    (define-key ktm-mode-map [f3] 'multi-term-next)
    (define-key ktm-mode-map [(shift f3)] 'kmacro-start-macro-or-insert-counter)
    (define-key ktm-mode-map [(shift f4)] 'kmacro-end-or-call-macro)
    (define-key ktm-mode-map [f4] 'slime-connect)
    (define-key ktm-mode-map [f6] 'next-error)
    (define-key ktm-mode-map [f7] 'el-get-install)
    (define-key ktm-mode-map [f8] 'paredit-mode)
    (define-key ktm-mode-map [f9] 'magit-status)
    (define-key ktm-mode-map [f10] 'org-agenda)

    (define-key ktm-mode-map [f12] '(lambda ()
                             (interactive)
                             (kill-buffer nil)))
    (define-key ktm-mode-map (kbd "<C-return>")
                    '(lambda ()
                       (interactive)
                       (switch-to-buffer nil)))
    (define-key ktm-mode-map (kbd "C-c t") 'multi-term-next)
    (define-key ktm-mode-map (kbd "C-c T") 'multi-term)
    (define-key ktm-mode-map (kbd "C-<") 'mark-previous-like-this)
    (define-key ktm-mode-map (kbd "C->") 'mark-next-like-this)
    (define-key ktm-mode-map (kbd "C-M-m") 'mark-sexp)
    (define-key ktm-mode-map (kbd "C-M-S-m") 'mc/mark-next-like-this)
    (define-key ktm-mode-map (kbd "C-M-<SPC>") 'mark-sexp)
    (define-key ktm-mode-map (kbd "C-M-S-<SPC>") 'er/expand-region)
    (define-key ktm-mode-map (kbd "C-.") 'hippie-expand)
    ;; (define-key undo-tree-map (kbd "C-?") nil)

    (define-key ktm-mode-map (kbd "C-M-i") 'indent-region)

    (define-key ktm-mode-map (kbd "S-<home>") 'beginning-of-buffer)

    (define-key ktm-mode-map (kbd "S-<end>") 'end-of-buffer)

    ;; altgr-S-e
    (define-key ktm-mode-map (kbd "¢") 'mc/edit-lines)

    (define-key ktm-mode-map [C-down-mouse-1] 'browse-url-at-mouse)

    ktm-mode-map)

  :gkeymaproup 'ktm-mode)

(define-globalized-minor-mode ktm-global-mode ktm-mode
  (lambda ()
    (if (not (minibufferp (current-buffer)))
        (ktm-mode 1))))

(add-to-list 'emulation-mode-map-alists '(ktm-global-mode ktm-mode-map))

(provide 'ktm-mode)
