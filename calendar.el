;; Set longitude and latitude, for sunrise and sunset calculations
(setq calendar-latitude 47.6)
(setq calendar-longitude -122.2)
(setq calendar-location-name "Bellevue, WA")

(global-set-key (kbd "C-s-c") 'calendar)     ; Super+Control+c => Calendar

;; World clock.
;; See https://emacsredux.com/blog/2024/03/11/tracking-world-time-with-emacs/
(setq world-clock-time-format "%a %e %b %I:%M %p %Z")
(setq world-clock-list
      '(("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Asia/Calcutta" "Mumbai")
        ))

(global-set-key (kbd "C-s-w") 'world-clock)  ; Super+Control+w => World clock
