(require 'anything)
(require 'anything-config)

(autoload 'anything "anything" "anything selections" t)
(autoload 'anything-config "anything-config" "anything selections" t)

(load-file "~/.emacs.d/elisp/projectfile.el")

(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and in its sub-directories."
  (interactive "DDirectory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (message (car (car current-directory-list)))
      (cond
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list))))
       
       (t
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       
       )
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))

(defun directories-in-below-directory (directory)
  "Returns recursivly all directories under directory."
  (interactive "DDirectory name: ")
  (let (directories-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq directories-list
                (append
                 (list (car (car current-directory-list)))
                 (directories-in-below-directory
                  (car (car current-directory-list)))
                 directories-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    directories-list))

;; anything conf
(defun make-anything-directory-source (source-name dir)
  "Returns an anything source for a particular directory"
  `((name . ,(concat source-name))
    (candidates . (lambda ()
                    (directory-files
                     ,dir)))
    (action . find-file)
    (type . file)))

(defun make-anything-directories-in-directory-source (source-name dir)
  "Returns an anything source for a particular directory"
  `((name . ,(concat source-name))
    (candidates . (lambda ()
                    (directories-in-below-directory
                     ,dir)))
    (action . find-file)
    (type . file)))

(defun make-anything-recursive-directory-source (source-name dir)
  "Returns an anything source for a particular directory"
  `((name . ,(concat source-name))
	(candidates . (lambda ()
                        (files-in-below-directory
                         ,dir)))
	(action . find-file)
	(type . file)))

(defun make-anything-project-files-source (source-name filepath)
  "Returns an anything source for the file of directory"
  `((name . ,(concat source-name))
	(candidates . (lambda ()
                        (list-project-files
                         ,filepath)))
	(action . find-file)
	(type . file)))


(setf anything-elisp-source (make-anything-recursive-directory-source "Elisp files" "~/.emacs.d/elisp"))
;; (setf anything-carneades-project-source 
;;       (make-anything-directories-in-directory-source "Carneades" "~/Documents/Projects/carneades/src/CarneadesEngine/src"))

(setf anything-carneades-files (make-anything-project-files-source
                                "Carneades files"
                                "/home/pal/Documents/Projects/carneades/src/FILES"))

(setq anything-etags-enable-tag-file-dir-cache t)
(setq anything-etags-cache-tag-file-dir "~/Documents/Projects/carneades/src/")

(defun my-anything ()
       (interactive)
       (anything-other-buffer
        '(anything-c-source-buffers
          anything-c-source-file-name-history
          ;; anything-c-source-info-pages
          ;; anything-c-source-info-elisp
          ;; anything-c-source-man-pages
          ;; anything-c-source-locate
          ;; anything-c-source-emacs-commands
          ;; anything-elisp-source
          anything-c-source-bookmarks
          ;; anything-carneades-project-source
          anything-carneades-files
          anything-c-source-etags-select
          )
        "*my-anything*"))

(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-mode-map (kbd "M-o") nil)))

(add-hook 'anything-after-initialize-hook
          '(lambda ()
             (defun anything-c-transform-file-browse-url (actions candidate)
               "Disables anything config function to not browse http | ftp files"
               actions)))

(provide 'setup-anything)
