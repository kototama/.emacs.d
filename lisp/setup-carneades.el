(require 's)
(require 'file-utils)
(require 'time-stamp)

(defvar carneades-src-directory "~/Documents/Projects/carneades/src/")

(defvar appdir "/home/pal/Documents/Projects/carneades/src/CarneadesWebApp")

(defvar carneade-license-year (format-time-string "%Y" (current-time)))

(setq carneades-clj-license
      (concat (format ";;; Copyright (c) %s Fraunhofer Gesellschaft\n"
                      carneade-license-year)
              ";;; Licensed under the EUPL V.1.1\n"))

(setq carneades-js-license
      (concat (format "// Copyright (c) %s Fraunhofer Gesellschaft\n"
                      carneade-license-year)
              "// Licensed under the EUPL V.1.1\n"))

(defun carneades-update-tags-file
  ()
  (interactive)
  (let ((default-directory carneades-src-directory))
    (cd default-directory)
    (start-file-process-shell-command "genetags" "genetags" "~/local/bin/genetags.sh")))

(defun carneades-insert-copyright
  ()
  "Inserts a copyright notice in the current file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond ((eq major-mode 'clojure-mode) (insert-string carneades-clj-license))
          ((eq major-mode 'javascript-mode) (insert-string carneades-js-license)))))

(defun carneades-prompt-insert-copyright
  (file)
  "Opens file and ask the user if he wants to insert a copyright notice for it.
The buffer is saved if a copyright notice is inserted."
  (find-file file)
  (goto-char (point-min))
  (when (yes-or-no-p "Insert copyright notice?")
    (carneades-insert-copyright)
    (save-buffer)))

(defun find-clj-or-cljs-files
  (directory)
  "Returns t if the file name is a .clj or .cljs file"
  (find-files directory (lambda (file)
                          (or (clj-file-p file)
                              (cljs-file-p file)))))

(defun carneades-insert-license-in-clj-cljs-files
  ()
  (-each (find-clj-or-cljs-files carneades-src-directory)
          'carneades-prompt-insert-copyright))

(defun carneades-update-ns
  ()
  (-each (find-clj-or-cljs-files carneades-src-directory)
         (lambda (filename) (when (not (string= "project.clj" filename))
                              (find-file filename)))))

(defun carneades-insert-license-app-files
  ()
  (-each (find-js-files appdir)
         'carneades-prompt-insert-copyright))


(defvar carneades-translation-file
  "/home/pal/Documents/Projects/carneades/src/PolicyModellingTool/resources/policymodellingtool/public/site/Messages.properties")

(defun translate-region (start end)
  "Translate the region."
  (interactive "r")
  (let ((key (read-from-minibuffer "Key:"))
        (text (buffer-substring (mark) (point))))
    (add-text-to-18n-file carneades-translation-file key text)
    (kill-region (mark) (point))
    (insert-string (concat "{{" key "}}"))))

(defun carneades-start-project
  ()
  "Open the routes file, compiles and starts the project "
  (interactive)
  (cd "/home/pal/Documents/Projects/carneades/src")
  (find-file "/home/pal/Documents/Projects/carneades/src/CarneadesWebApp/src/carneades/analysis/web/routes_dev.clj")
  (end-of-buffer)
  (nrepl-jack-in))

(defun carneades-create-new-file
  ()
  (interactive)
  (let ((buffer (ido-find-file)))
    (switch-to-buffer buffer)
    (carneades-insert-copyright)
    (goto-char (point-max))
    (save-buffer)
    (insert "\n(ns x\n(:require []))")
    (indent-region (point-min) (point-max))
    (clojure-update-ns)
    (re-search-backward "\\[")
    (forward-char)))

(provide 'setup-carneades)
