;; C-:  Jump to any character on the window
;; (global-unset-key "\C-'")
(global-set-key (kbd "M-s-j") 'avy-goto-char)
(global-set-key (kbd "M-j") 'avy-goto-char-in-line)

(global-set-key (kbd "M-i") 'imenu)

(define-key prog-mode-map (kbd "M-p") 'beginning-of-defun)
(define-key prog-mode-map (kbd "M-n") 'end-of-defun)
