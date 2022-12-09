;;
;; Make hs-minor-mode understand Ruby delimiters. Taken from
;; https://coderwall.com/p/u-l0ra/ruby-code-folding-in-emacs
;;
(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

;; Mark ruby block
(global-set-key (kbd "M-H") 'er/mark-ruby-block-up)

(with-eval-after-load "ruby-mode"
  (define-key ruby-mode-map (kbd "M-s-c") 'ruby/class)
  (define-key ruby-mode-map (kbd "M-s-f") 'ruby/insert-function)
  (define-key ruby-mode-map (kbd "M-s-m") 'ruby/module)
  (define-key ruby-mode-map (kbd "M-s-t") 'ruby/test)
  )
