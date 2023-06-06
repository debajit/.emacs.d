;; Mark all text inside a sexp like a string, or a set of parentheses etc.
(fset 'debajit-mark-sexp
   [?\C-\M-u ?\C-f ?\C-  ?\C-o ?\C-b])

(fset 'list-itemify
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 45 134217760 134217848 117 112 99 97 115 101 45 99 104 97 114 return 14 134217837] 0 "%d")) arg)))

(fset 'italicize-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217837 47 5 47 down 134217837] 0 "%d")) arg)))

(fset 'list-item-with-heading
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 45 134217760 67108896 19 46 return 134217848 99 97 112 105 return 24 24 8388706 down] 0 "%d")) arg)))

(fset 'embolden-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217837 67108896 5 8388706 14 134217837] 0 "%d")) arg)))

(fset 'codify-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\355~~\355" 1 "%d")) arg)))

;; (fset 'standup
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 134217848 121 97 115 32 105 110 down return down down down down down return f9 tab f10 tab] 0 "%d")) arg)))

;; (fset 'standup
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 89 69 83 84 69 82 68 65 89 32 40 f9 134217839 return 84 79 68 65 89 32 40 f10 up] 0 "%d")) arg)))
;; (fset 'standup2
;;    [?\M-m ?/ ?m ?d ?  S-f9 return ?- ?  return return S-f10 return ?- ?  up up up ?\C-e])
(fset 'standup
   [?/ ?m ?d ?  ?# ?# ?# ?  ?Y ?e ?s ?t ?e ?r ?d ?a ?y return return ?# ?# ?# ?  ?T ?o ?d ?a ?y return])

(fset 'ruby/test
   [?\C-a tab ?t ?e ?s ?t ?  ?\" ?# backspace ?\C-e ?  ?d ?o return ?e ?n ?d ?\C-p ?\C-f ?\C-f ?\C-f ?\C-f])

(fset 'ruby/class
   [tab ?c ?l ?a ?s ?s ?  return ?e ?n ?d ?\C-p ?\C-e ? ])

(fset 'ruby/module
   [?m ?o ?d ?u ?l ?e ?  return ?e ?n ?d ?\C-p ?\C-e ? ])

(fset 'ruby/insert-function
   [tab ?d ?e ?f return ?e ?n ?d ?\C-p ? ])

;; Convert old-style Ruby symbols to the new style. Very useful!
(fset 'ruby/symbol/to-new-style
   [?\C-s ?: return backspace ?\C-s ?= return ?\C-d backspace ?\M-\\ ?: ? ])

(fset 'write-standup
   [?\s-\S-  ?s ?t ?a ?n return ?\M-> ?\s-j ?\M-> return ?\M-x ?s ?t ?a ?n ?d ?u ?p return up up])

(fset 'format-google-calendar-dates
   (kmacro-lambda-form [?\M-x ?n ?a ?r ?r ?o ?w ?  ?  ?r ?e backspace backspace backspace ?t ?o ?  ?r ?e ?g ?i ?o return ?\M-< ?\C-\M-% up up up up return ?! ?\M-< ?\M-% up up up up return ?! ?\M-< ?\M-% up up up return ?! ?\M-< ?\M-% up up up up up return ?! ?\M-x ?w ?i ?d ?e ?n return] 0 "%d"))
