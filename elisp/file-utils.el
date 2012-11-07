
(defun file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun list-project-files (filename)
  "Returns the filenames contained in the file, assuming one filename per line."
  (let ((content (file-string filename)))
    (split-string content "\n")))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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

(provide 'file-utils)
