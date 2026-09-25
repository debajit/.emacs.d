;;; -*- lexical-binding: t -*-
;;
;; Calendar, Date and Time
;;
;; Keys:
;;
;; | Key         | Description     |
;; |-------------+-----------------|
;; | Calendar    | Super+Control+c |
;; | World clock | Super+Control+w |
;;


;;
;; Holidays
;;

(setq org-agenda-include-diary t)       ; Show holidays in org-agenda


;;
;; Calendar
;;

;; Set longitude and latitude, for sunrise and sunset calculations
(setq calendar-latitude 47.6)
(setq calendar-longitude -122.2)
(setq calendar-location-name "Bellevue, WA")

(defun my-calendar-hide-trailing-whitespace ()
  "Do not highlight the padding spaces in Calendar buffers."
  (setq-local show-trailing-whitespace nil))

(add-hook 'calendar-mode-hook #'my-calendar-hide-trailing-whitespace)

(global-set-key (kbd "C-s-c") 'calendar)     ; Super+Control+c => Calendar


;;
;; World clock.
;; See https://emacsredux.com/blog/2024/03/11/tracking-world-time-with-emacs/
;;

(setq world-clock-time-format "%a %e %b %I:%M %p %Z")
(setq world-clock-list
      '(
        ("America/Los_Angeles" "Seattle")
        ("America/Denver" "Denver")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Asia/Calcutta" "Mumbai")
        ("Australia/Sydney" "Sydney")
        ))

(global-set-key (kbd "C-s-t") 'world-clock)  ; Super+Control+w => World clock
