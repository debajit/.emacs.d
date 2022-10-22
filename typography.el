;;
;; ~/.emacs.d/typography.el
;;
;; Typography customizations.
;;

;;----------------------------------------------------------------------
;; Default font settings
;;----------------------------------------------------------------------

(defconst monospaced-font-family "Cascadia Code"
  "The default monospaced typeface to use for code and other
  fixed-width text. Examples:
  - Cascadia Code (12pt on GNU, 18pt on macOS)
  - Operator Mono (13pt)
  - Consolas (13pt)")

(defconst monospaced-font-size "18"
  "The default monospaced font size")

(defconst proportional-font-family "Chirp"
  "The default proportional typeface to use for longform text,
  notes etc. Examples:
  - Ideal Sans (14pt)
  - Chirp (16pt on macOS)")

(defconst proportional-font-size "16"
  "The default proportional font size")

;; The default (proportional) typeface to use for headings
(setq heading-font-family "Verlag")

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
  )

(add-hook 'org-mode-hook 'set-buffer-variable-pitch)
;; (add-hook 'org-agenda-mode-hook 'set-buffer-variable-pitch)
(add-hook 'eww-mode-hook 'set-buffer-variable-pitch)
(add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
(add-hook 'Info-mode-hook 'set-buffer-variable-pitch)
(add-hook 'imenu-list-major-mode-hook 'set-buffer-variable-pitch)


;;----------------------------------------------------------------------
;; Typographic Pairs and Quotes
;;----------------------------------------------------------------------

(electric-pair-mode 1)                  ; Enable paired characters
(electric-quote-mode 1)                 ; Use typographic curly quotes


;;----------------------------------------------------------------------
;; Unicode optimizations
;;----------------------------------------------------------------------

;; Disable font caches to prevent slowness with Unicode characters
;; like emoji. See
;; https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness/33514
(setq inhibit-compacting-font-caches t)


;;----------------
;; Ligatures
;;----------------

(use-package ligature
  :load-path "ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
