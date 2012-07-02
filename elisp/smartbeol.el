
(defun back-to-indentation-or-beginning-of-line
  ()
  "Moves to the beginning of indentation or to the beginning of
   the line if the point is already on the first indentation"
  (interactive)
  (let ((point-before-identation (point)))
    (back-to-indentation)
    (let ((point-second-indentation (point)))
      (if (equal point-before-identation point-second-indentation)
          (move-beginning-of-line nil)))))

(defun eager-kill-line
  ()
  "Kills the current line or join the next line 
   if the point is at the end of the line"
  (interactive)
  (if (and (eolp)
           (not (bolp)))
      (delete-indentation 1)
    (kill-line nil)))

