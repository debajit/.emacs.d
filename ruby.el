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
