;;
;; Date shortcuts
;;
;;       F9 - Insert today’s date
;;      F10 - Insert tomorrow’s date
;; Shift+F9 - Insert yesterday’s date
;;

(defun day-label (&optional time)
  ;; %A = Full day of week e.g. Wednesday
  ;; %B = Full month name
  ;; %-d = numeric day of the month, not padded
  ;; %Y = year
  (format-time-string "%A, %B %-d, %Y" time))

(defun tomorrow-time ()
  (time-add (current-time) (* 24 60 60)))

(defun yesterday-time ()
  (time-subtract (current-time) (* 24 60 60)))

(global-set-key (kbd "<f9>")
                (lambda ()
                  (interactive)
                  (insert (day-label))))

(global-set-key (kbd "<f10>")
                (lambda ()
                  (interactive)
                  (insert (day-label (tomorrow-time)))))

(global-set-key (kbd "<S-f9>")
                (lambda ()
                  (interactive)
                  (insert (day-label (yesterday-time)))))


(use-package svg-clock
  :ensure t)
