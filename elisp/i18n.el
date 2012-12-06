(require 'dash)
(require 's)
(require 'file-utils)

(defun add-text-to-18n-file
  (file i18-key text)
  (save-excursion
   (find-file file)
   (goto-char (point-max))
   (insert-string (format "%s=%s\n" i18-key text))
   (save-buffer)
   (switch-to-buffer (other-buffer))))

(defun i18n-file
  (file i18n-fn-text i18n-file)
  (let ((bds nil)
        (b-of-s nil)
        (e-of-s nil)
        (text nil))
    (find-file file)
    (fundamental-mode) ;; because of bug in forward-sexp in JS2-mode
    (goto-char (point-min))
    (while (re-search-forward "\"" nil t)
      (backward-char 1)
      (setq b-of-s (point))
      (forward-sexp)
      (setq e-of-s (point))
      (setq text (buffer-substring-no-properties b-of-s e-of-s))
      (let ((translate (yes-or-no-p "i18n string?"))
            (i18n-key nil))
        (when translate
          (setq i18n-key (read-from-minibuffer "Key:"))
          (backward-sexp)
          (kill-sexp)
          (insert-string (format i18n-fn-text i18n-key))
          (add-text-to-18n-file i18n-file i18n-key text))))))

(defun js-i18n-directory
  (directory i18n-fn-text i18n-file)
  (-each (find-js-files directory)
         (lambda (file)
           (i18n-file file i18n-fn-text i18n-file))))

;; (read-from-minibuffer "Key:")


;; (js-i18n-directory
;;  appdir
;;  "$.i18n.prop('%s')"
;;  "/home/pal/Documents/Projects/carneades/src/PolicyModellingTool/resources/policymodellingtool/public/site/Messages.properties")

;; (i18n-file "/tmp/toto.js" "$.i18n.prop('%s')" "/tmp/Messages.properties")

