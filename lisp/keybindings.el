(require 'bind-key)

(bind-key* "C-S-j"  'join-line)
(bind-key* "M-n"  'forward-paragraph)
(bind-key* "M-p"  'backward-paragraph)
(bind-key* "M-o"  'other-window)
(bind-key* "M-S-o"  (lambda ()
                       (interactive)
                       (other-window -1)))
(bind-key* "C-S-k"  (lambda ()
                       (interactive)
                       (kill-buffer (current-buffer))))'
(bind-key* "C-c p"  'pop-to-mark-command)

(provide 'keybindings)
