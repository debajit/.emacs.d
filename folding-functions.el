;; hs-deeper and hs-shallower written by Derek Upham

(defvar-local hideshow-current-level nil)
(defvar hideshow-level-increment 1)

(defun hs-deeper ()
  (interactive)
  (require 'hideshow)
  (if hideshow-current-level
      (cl-incf hideshow-current-level hideshow-level-increment)
    (setq hideshow-current-level hideshow-level-increment)
    (hs-minor-mode 1))
  (save-excursion
    (goto-char (point-min))
    (hs-hide-level hideshow-current-level))
  (message "hideshow-current-level: %S" hideshow-current-level))

(defun hs-shallower ()
  (interactive)
  (require 'hideshow)
  (when hideshow-current-level
    (cl-decf hideshow-current-level hideshow-level-increment)
    (if (< 0 hideshow-current-level)
        (progn
          (save-excursion
            (goto-char (point-min))
            (hs-hide-level hideshow-current-level)))
      (setq hideshow-current-level nil)
      (hs-show-all)
      (hs-minor-mode -1)))
  (message "hideshow-current-level: %S" hideshow-current-level))
