(fset 'listify
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 45 32 134217827 down 1] 0 "%d")) arg)))

(fset 'italicize-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217837 47 5 47 down 134217837] 0 "%d")) arg)))

(fset 'list-item-with-heading
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 45 134217760 67108896 19 46 return 134217848 99 97 112 105 return 24 24 8388706 down] 0 "%d")) arg)))

(fset 'standup
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 134217848 121 97 115 32 105 110 down return down down down down down return f9 tab f10 tab] 0 "%d")) arg)))
