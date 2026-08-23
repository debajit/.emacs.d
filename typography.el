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
  - Cascadia Code (12pt)
  - Operator Mono (13pt)
  - Consolas")

(defconst proportional-font-family "Ideal Sans"
  "The default proportional typeface to use for longform text,
  notes etc. Examples:
  - Ideal Sans (14pt)")

;; (defconst proportional-font-family "IBM Plex Sans Condensed"
;;   "The default proportional typeface to use for longform text,
;;   notes etc. Examples:
;;   - Ideal Sans (14pt)")

;; The default (proportional) typeface to use for headings. This is
;; the single place to change the heading font: `apply-heading-font'
;; below pushes it onto every heading face listed in
;; `heading-faces', overriding whatever the active theme or
;; custom.el says.
(defconst heading-font-family "Verlag"
  "The default proportional typeface to use for headings. Examples:
  - Verlag Condensed
  - Verlag Compressed
  - Verlag
  - Ideal Sans")

(defconst heading-font-weight 'bold
  "The weight to use for every heading face, or nil.

This overrides the per-heading weights set by the active theme and
by custom.el, so that all headings share a single weight. Set it
to nil to leave those weights alone -- but see the warning below.

Beware: a family exposes far fewer weights to Emacs than the
number of font files on disk suggests, because foundries often
split the weights into separate families. Verlag Condensed ships
five weights as files, yet Emacs offers only `light' and `normal'
under the family name `Verlag Condensed'; its Book face *is*
`normal', and its Bold and Black faces are only reachable as the
separate families `Verlag Condensed Bold' and `Verlag Condensed
Black'. Asking a face for a weight its family does not have makes
Emacs abandon the family altogether and fall back to the default
font, which is why headings can suddenly turn monospaced. That is
also the hazard in setting this to nil: the `:weight bold' that
custom.el puts on org-level-1..4 would then apply to a family with
no bold.

`apply-heading-font' guards against this: it checks the requested
weight against `heading-font-weights' and falls back to `normal'
rather than let the family fall back. Use \\[heading-font-weights]
to see what the current family actually offers.")

(defconst monospaced-font-size "12"
  "The default monospaced font size. Recommendations:
  - GNU: Cascadia Code 12pt, Operator Mono 13pt,
  - macOS: Cascadia Code 12pt, Operator Mono 13pt, Consolas 13pt")

(defconst proportional-font-size "14"
  "The default proportional font size")

;; Set default fonts.
;;
;; `set-face-font' is deliberately avoided here. Given a font name it
;; pins a complete font-spec on the face -- :font, :foundry, :width
;; and :slant included -- and every face that inherits from `default'
;; or `variable-pitch' inherits all of it. That breaks headings in two
;; ways: a pinned :font takes precedence over :family, so a heading
;; face can no longer choose its own typeface at all; and a pinned
;; :foundry or :width belonging to some *other* family (Ideal Sans is
;; H&Co and normal width, Verlag Condensed is H&FJ and condensed)
;; makes the heading font unsatisfiable, whereupon Emacs quietly falls
;; back to the default monospaced font.
;;
;; Naming the attributes individually leaves everything else alone,
;; which is what lets `apply-heading-font' below work. But the
;; `default' face has a rule of its own, and breaking it is silent:
;;
;;   Never leave any attribute of `default' `unspecified'.
;;
;; The `default' face is the frame font, and Emacs only pushes a
;; change onto the frame while it can build a *complete* font spec out
;; of the face. Unspecify even one of :font, :foundry, :width or
;; :slant -- the obvious way to clear a stale value -- and from then
;; on every :height lands on the face but never reaches the frame:
;; `describe-face' reports the new size while the text on screen keeps
;; the old one, and the only things that appear to respond are the
;; faces that are *not* the frame font. That is the asymmetry where
;; Org buffers resize (they render code and tables in `fixed-pitch')
;; and programming buffers do not. faces.el makes the same point in
;; `face-spec-reset-face': "For the default face, avoid making any
;; attribute unspecified."
;;
;; So a stale attribute is cleared by naming a concrete neutral value
;; instead -- `normal', or a foundry of "nil", which means any.
(defun font-height (size)
  "Convert SIZE, a point size as a string, to a :height attribute."
  (* 10 (string-to-number size)))

(defun apply-base-fonts ()
  "Render `default' and `fixed-pitch' in `monospaced-font-family',
and `variable-pitch' in `proportional-font-family', each at its
configured size.

`fixed-pitch' and `variable-pitch' are installed as face
*override* specs rather than copied or set attribute by attribute,
so that they survive theme loads and, more to the point, outrank
custom.el -- which init.el loads *after* this file, and which has
historically carried a `fixed-pitch' of its own, complete with a
stale :height that silently won.

Call this interactively after changing any of the font constants
above to see the result without restarting Emacs."
  (interactive)
  (when (display-graphic-p)
    (when (member monospaced-font-family (font-family-list))
      ;; :foundry, :width, :weight and :slant are named here only to
      ;; overwrite whatever an earlier `set-face-font' may have left
      ;; pinned; see the rule above for why they cannot simply be
      ;; unspecified.
      (set-face-attribute 'default nil
                          :family monospaced-font-family
                          :foundry "nil"
                          :width 'normal
                          :weight 'normal
                          :slant 'normal
                          :height (font-height monospaced-font-size))
      (face-spec-set 'fixed-pitch
                     `((t (:family ,monospaced-font-family
                           :height ,(font-height monospaced-font-size))))
                     'face-override-spec))
    (when (member proportional-font-family (font-family-list))
      (face-spec-set 'variable-pitch
                     `((t (:family ,proportional-font-family
                           :height ,(font-height proportional-font-size)
                           :weight normal)))
                     'face-override-spec))))

(apply-base-fonts)

;; Keep the saved desktop from restoring a stale frame font.
;;
;; `desktop-save-mode' (enabled in init.el) saves the frame
;; configuration -- the frame's `font' parameter included -- and
;; restores it from `after-init-hook', which is to say *after* this
;; file has run. Whatever size happened to be in effect when the
;; desktop was last saved would then be restored over the size set
;; above, and saved again on exit, outliving every later change to
;; `monospaced-font-size'.
;;
;; The font belongs to this file, not to the saved session, so tell
;; frameset never to save or restore it. (`GUI:font' is where
;; frameset shelves the font when a frame is restored on a tty.)
(with-eval-after-load 'frameset
  (dolist (parameter '(font GUI:font))
    (setf (alist-get parameter frameset-filter-alist) :never)))

;;----------------------------------------------------------------------
;; Heading fonts
;;----------------------------------------------------------------------

(defconst heading-faces
  '(;; Org
    org-document-title
    org-level-1 org-level-2 org-level-3 org-level-4
    org-level-5 org-level-6 org-level-7 org-level-8
    ;; Outline (Org headings inherit from these in a stock setup)
    outline-1 outline-2 outline-3 outline-4
    outline-5 outline-6 outline-7 outline-8
    ;; Markdown
    markdown-header-face
    markdown-header-face-1 markdown-header-face-2 markdown-header-face-3
    markdown-header-face-4 markdown-header-face-5 markdown-header-face-6
    ;; Info
    info-title-1 info-title-2 info-title-3 info-title-4
    ;; Misc. headers elsewhere
    magit-section-heading
    magit-header-line
    deft-header-face
    helm-source-header
    treemacs-header-face)
  "Faces that should be rendered in `heading-font-family'.
Only the family and `heading-font-weight' are touched; colours and
heights still come from the active theme and from custom.el.")

(defun heading-font-weights ()
  "Return the weights `heading-font-family' actually offers Emacs.

Returns nil when that cannot be determined yet, which is the case
before the first graphical frame exists (a daemon at startup)."
  (interactive)
  (when (and (display-graphic-p) (fboundp 'x-family-fonts))
    ;; Element 3 of each vector is the weight. See `x-family-fonts'.
    (let ((weights (delete-dups (mapcar (lambda (font) (aref font 3))
                                        (x-family-fonts heading-font-family)))))
      (when (called-interactively-p 'interactive)
        (message "%s offers: %s" heading-font-family weights))
      weights)))

(defun apply-heading-font ()
  "Render every face in `heading-faces' in `heading-font-family'.

Also applies `heading-font-weight', unless that is nil.

Both are installed as a face *override* spec, so they survive
theme loads, `custom-set-faces' in custom.el, and new frames
created by emacsclient. Faces whose packages have not been loaded
yet are handled too: the override applies as soon as the face is
defined.

Call this interactively after changing either constant to see the
result without restarting Emacs."
  (interactive)
  (let* ((available (heading-font-weights))
         ;; Emacs names the same weight `normal' or `regular'
         ;; depending on where it is read from.
         (usable (or (null available)
                     (memq heading-font-weight available)
                     (and (memq heading-font-weight '(normal regular))
                          (or (memq 'normal available)
                              (memq 'regular available)))))
         (weight (cond ((null heading-font-weight) nil)
                       (usable heading-font-weight)
                       (t (message (concat "Heading font: %s has no %s weight "
                                           "(only %s); using normal instead")
                                   heading-font-family heading-font-weight
                                   available)
                          'normal)))
         (attributes (append (list :family heading-font-family)
                             (when weight (list :weight weight)))))
    (dolist (face heading-faces)
      (face-spec-set face `((t ,attributes)) 'face-override-spec))))

(apply-heading-font)


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
  :ensure t

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


;;----------------------------------------------------------------------
;; Emoji
;;----------------------------------------------------------------------

(global-set-key (kbd "C-s-j") 'emoji-search) ;; Emoji search
(global-set-key (kbd "C-s-k") 'emoji-insert) ;; Emoji search
