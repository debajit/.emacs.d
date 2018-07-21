;;
;; ~/.emacs.d/selection.el
;;
;; Selection settings.
;;

;;----------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------

;; Cut: s-x   (default: C-w)
(global-set-key (kbd "s-x") 'kill-region)

;; Copy: s-c   (default: M-w)
(global-set-key (kbd "s-c") 'kill-ring-save)

;; Paste: s-v   (default: C-y)
(global-set-key (kbd "s-v") 'yank)

;; Select all: s-a   (default: C-x h)
(global-set-key (kbd "s-a") 'mark-whole-buffer)


;;----------------------------------------------------------------------
;; Narrowing and widening
;;----------------------------------------------------------------------

; Enable narrowing (disabled by default)
(put 'narrow-to-region 'disabled nil)
