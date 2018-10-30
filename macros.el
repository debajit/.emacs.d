;; (fset 'listify-old
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 45 32 134217827 down 1] 0 "%d")) arg)))

;; (fset 'list-itemify-old
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 45 134217760 134217848 117 112 99 97 115 101 down down down return 14 134217837] 0 "%d")) arg)))

(fset 'list-itemify
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 45 134217760 134217848 117 112 99 97 115 101 45 99 104 97 114 return 14 134217837] 0 "%d")) arg)))

(fset 'italicize-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217837 47 5 47 down 134217837] 0 "%d")) arg)))

(fset 'list-item-with-heading
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 45 134217760 67108896 19 46 return 134217848 99 97 112 105 return 24 24 8388706 down] 0 "%d")) arg)))

(fset 'embolden-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217837 67108896 5 8388706 14 134217837] 0 "%d")) arg)))

;; (fset 'standup
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 134217848 121 97 115 32 105 110 down return down down down down down return f9 tab f10 tab] 0 "%d")) arg)))

(fset 'standup
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 89 69 83 84 69 82 68 65 89 32 40 f9 134217839 return 84 79 68 65 89 32 40 f10 up] 0 "%d")) arg)))
