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

(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "C-c 0") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-barf-sexp)
            (define-key paredit-mode-map (kbd "C-c 9") 'paredit-backward-slurp-sexp)
            (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-barf-sexp)
            (define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
            (define-key paredit-mode-map (kbd "M-r") nil)
            (define-key paredit-mode-map (kbd "C-k") 'paredit-eager-kill-line)
            (define-key paredit-mode-map (kbd "C-S-d") 'paredit-duplicate-after-point)))
