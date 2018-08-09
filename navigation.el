(global-set-key (kbd "M-i") 'imenu)

;; Open file or link in Org mode with the same key shortcut
(define-key org-mode-map (kbd "s-.") 'org-open-at-point)
(define-key org-mode-map (kbd "s-,") 'org-mark-ring-goto)
