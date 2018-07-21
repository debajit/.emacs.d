;;
;; ~/.emacs.d/typography.el
;;
;; Typography customizations.
;;

;;----------------------------------------------------------------------
;; Default font settings
;;----------------------------------------------------------------------

(defconst monospaced-font-family "Consolas"
  "The default monospaced typeface to use for code and other
  fixed-width text")

(defconst proportional-font-family "Ideal Sans"
  "The default proportional typeface to use for longform text,
  notes etc.")

(defconst monospaced-font-size "16"
  "The default monospaced font size")

(defconst proportional-font-size "18"
  "The default proportional font size")

;; Set default fonts
(when (display-graphic-p)
  (when (member monospaced-font-family (font-family-list))
    (set-face-font 'default
                   (concat monospaced-font-family "-" monospaced-font-size))
    (copy-face 'default 'fixed-pitch))
  (when (member proportional-font-family (font-family-list))
    (set-face-font 'variable-pitch
                   (concat proportional-font-family "-" proportional-font-size))))

;;
;; Variable-width font settings.
;;
;; Largely adapted from
;; http://www.xiangji.me/2015/07/13/a-few-of-my-org-mode-customizations/
;;

(defun set-buffer-variable-pitch ()
  (interactive)
  (variable-pitch-mode t)
  (setq line-spacing 3)
  (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch))

(add-hook 'org-mode-hook 'set-buffer-variable-pitch)
;; (add-hook 'org-agenda-mode-hook 'set-buffer-variable-pitch)
(add-hook 'eww-mode-hook 'set-buffer-variable-pitch)
(add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
(add-hook 'Info-mode-hook 'set-buffer-variable-pitch)


;;----------------------------------------------------------------------
;; Typographic Pairs and Quotes
;;----------------------------------------------------------------------

(electric-pair-mode 1)                  ; Enable paired characters
(electric-quote-mode 1)                 ; Use typographic curly quotes
