;;----------------------------------------------------------------------
;; Date shortcuts
;;----------------------------------------------------------------------

;; Yesterday’s date
(global-set-key (kbd "<f9>")
                (lambda ()
                  "Insert yesterday’s date in the format: Sunday, January 1, 2018"
                  (interactive)
                  (insert (format-time-string "%A, %B %d, %Y"
                                              (time-subtract (current-time) (* 24 60 60))))))

;; Today’s date
(global-set-key (kbd "<f10>")
                (lambda ()
                  "Insert today’s date in the format: Sunday, January 1, 2018"
                  (interactive)
                  (insert (format-time-string "%A, %B %d, %Y"))))
