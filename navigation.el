;; Jump to matching parenthesis by pressing % (like in Vim). See
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html

(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;; C-:  Jump to any character on the window
;; (global-unset-key "\C-'")
(global-set-key (kbd "M-s-j") 'avy-goto-char)
(global-set-key (kbd "M-j") 'avy-goto-char-in-line)

(global-set-key (kbd "M-i") 'imenu)

(define-key prog-mode-map (kbd "M-p") 'beginning-of-defun)
(define-key prog-mode-map (kbd "M-n") 'end-of-defun)
