(require 's)
(require 'helm-config)
(require 'helm-files)
(require 'file-utils)

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

;; helm conf
(defun make-helm-directory-source (source-name dir)
  "Returns an helm source for a particular directory"
  `((name . ,(concat source-name))
    (candidates . (lambda ()
                    (directory-files
                     ,dir)))
    (action . find-file)
    (type . file)))

(defun make-helm-directories-in-directory-source (source-name dir)
  "Returns an helm source for a particular directory"
  `((name . ,(concat source-name))
    (candidates . (lambda ()
                    (directories-in-below-directory
                     ,dir)))
    (action . find-file)
    (type . file)))

(defun make-helm-recursive-directory-source (source-name dir)
  "Returns an helm source for a particular directory"
  `((name . ,(concat source-name))
	(candidates . (lambda ()
                        (files-in-below-directory
                         ,dir)))
	(action . find-file)
	(type . file)))

(defun make-helm-project-files-source (source-name filepath)
  "Returns an helm source for the file of directory"
  `((name . ,(concat source-name))
	(candidates . (lambda ()
                        (list-project-files
                         ,filepath)))
	(action . find-file)
	(type . file)))


(setf helm-elisp-source (make-helm-recursive-directory-source "Elisp files" "~/.emacs.d/elisp"))
;; (setf helm-carneades-project-source 
;;       (make-helm-directories-in-directory-source "Carneades" "~/Documents/Projects/carneades/src/CarneadesEngine/src"))

(setf helm-carneades-files (make-helm-project-files-source
                                "Carneades files"
                                "/home/pal/Documents/Projects/carneades/src/FILES"))

(setq helm-etags-enable-tag-file-dir-cache t)
(setq helm-etags-cache-tag-file-dir "~/Documents/Projects/carneades/src/")

(setq helm-work-sources (list
                         'helm-c-source-ffap-line
                         'helm-c-source-ffap-guesser
                         'helm-c-source-buffers-list
                         'helm-c-source-files-in-current-dir
                         'helm-c-source-file-cache
                         'helm-c-source-recentf
                         'helm-c-source-file-name-history
                         'helm-c-source-bookmarks
                         'helm-c-source-etags-select
                         'helm-carneades-files
                         'helm-elisp-source
                         ))
(setq helm-home-sources (list
                         'helm-c-source-ffap-line
                         'helm-c-source-ffap-guesser
                         'helm-c-source-buffers-list
                         'helm-c-source-files-in-current-dir
                         'helm-c-source-file-cache
                         'helm-c-source-recentf
                         'helm-c-source-file-name-history
                         'helm-c-source-bookmarks
                         'helm-c-source-etags-select
                         'helm-elisp-source))

(setq helm-sources (if (s-contains? "elan" system-name)
                       helm-work-sources
                     helm-home-sources))

(defun my-helm ()
       (interactive)
       (helm-other-buffer
        helm-sources
        
        "*my-helm*"))

(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-mode-map (kbd "M-o") nil)))

;; (add-hook 'helm-after-initialize-hook
;;           '(lambda ()
;;              (defun helm-c-transform-file-browse-url (actions candidate)
;;                "Disables helm config function to not browse http | ftp files"
;;                actions)))

(provide 'setup-helm)
