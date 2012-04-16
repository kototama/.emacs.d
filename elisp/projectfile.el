
(defun file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun list-project-files (filename)
  "Returns the filenames contained in the file, assuming one filename per line."
  (let ((content (file-string filename)))
    (split-string content "\n")))

