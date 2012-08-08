(defvar carneades-src-directory "~/Documents/Projects/carneades/src/")

(defun carneades-update-tags-file
  ()
  (interactive)
  (let ((default-directory carneades-src-directory))
    (cd default-directory)
    (start-file-process-shell-command "genetags" "genetags" "~/local/bin/genetags.sh")))

(provide 'setup-carneades)
