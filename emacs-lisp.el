(defun smart-eval-emacs-lisp (beginning end)
  (interactive "r")
  (if (use-region-p)
      (eval-region beginning end)
    (eval-buffer)))

(define-key emacs-lisp-mode-map (kbd "s-r") 'smart-eval-emacs-lisp)
