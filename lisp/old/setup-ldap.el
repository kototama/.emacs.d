(require 'ldap)
(require 'eudc)

(setq eudc-default-return-attributes nil
      eudc-strict-return-matches nil)

(load "~/.work-config.el")

(setq ldap-ldapsearch-args (quote ("-tt" "-LLL" "-x")))
(setq eudc-inline-query-format '((name)
                                 (firstname)
                                 (firstname name)
                                 (email)
                                 ))

(setq ldap-host-parameters-alist
      (list (list ldap-server 'base ldap-base
                  'binddn "")))

(eudc-set-server ldap-server 'ldap t)
(setq eudc-server-hotlist (list (cons ldap-server 'ldap)))
(setq eudc-inline-expansion-servers 'hotlist)

(defun enz-eudc-expand-inline()
  (interactive)
  (move-end-of-line 1)
  (insert "*")
  (unless (condition-case nil
              (eudc-expand-inline)
            (error nil))
    (backward-delete-char-untabify 1))
  )

;; Adds some hooks

(eval-after-load "message"
  '(define-key message-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
(eval-after-load "sendmail"
  '(define-key mail-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
(eval-after-load "post"
  '(define-key post-mode-map (kbd "TAB") 'enz-eudc-expand-inline))


(provide 'setup-ldap)
