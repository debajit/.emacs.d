;;; -*- lexical-binding: t -*-
;; C-:  Jump to any character on the window
;; (global-unset-key "\C-'")
(global-set-key (kbd "M-s-j") 'avy-goto-char)
(global-set-key (kbd "M-j") 'avy-goto-char-in-line)

(global-set-key (kbd "M-i") 'imenu)

(global-set-key (kbd "s-u") 'view-mode)

(define-key prog-mode-map (kbd "M-p") 'beginning-of-defun)
(define-key prog-mode-map (kbd "M-n") 'end-of-defun)

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "h") #'backward-char)
  (define-key view-mode-map (kbd "j") #'next-line)
  (define-key view-mode-map (kbd "k") #'previous-line)
  (define-key view-mode-map (kbd "l") #'forward-char))
