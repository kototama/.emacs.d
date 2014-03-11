;;; helper functions to manipulate files
(load-file "~/.emacs.d/site-lisp/s/s.el")
(require 's)

;; renaming

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s." new-name)))

;; file search functions

(defun find-files-helper
  (directory filter-fn visited)
  "Helper for the find-files function"
  (let ((files (directory-files directory t)))
    (-reduce-from
     (lambda (visited file)
       (cond ((and (not (string= "/." (s-right 2 file)))
                   (not (string= "/.." (s-right 3 file))) 
                   (file-accessible-directory-p file))
              (find-files-helper file filter-fn visited))
             ((funcall filter-fn file) (cons file visited))
             (t visited)))
     visited
     files)))

(defun find-files
  (directory filter-fn)
  "Recursively visit the files and directories in `directory`
and returns the file names satisfying the `filter-fn` function."
  (find-files-helper directory filter-fn ()))

(defun clj-file-p
  (file)
  (string= ".clj" (s-right 4 file)))

(defun cljs-file-p
  (file)
  (string= ".cljs" (s-right 5 file)))

(defun js-file-p
  (file)
  (string= ".js" (s-right 3 file)))

(defun coffee-file-p
  (file)
  (string= ".coffee" (s-right 7 file)))

;; encoding functions

(defun change-encoding-to-utf8 (dir pattern)
  (interactive "DChange files to UTF-8 encoding in directory: 
sMatching file pattern (regexp): ")
  (let ((files (directory-files dir t pattern)))
    (mapc (lambda (file)
            (find-file file)
            (set-buffer-file-coding-system 'utf-8)
            (save-buffer)
            (kill-buffer (current-buffer)))
          files)))

(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "b buffer to convert" )
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t)
    (replace-match "" nil t)))

(provide 'files)
