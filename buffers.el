;;
;; ~/.emacs.d/buffers.el
;;
;; Buffer-related customizations.
;;
;; Keys:
;;
;;       s-s     Save buffer
;;       s-w     Previous buffer (soft close)
;;       s-e     Next buffer
;;       s-q     Close buffer (i.e. kill current buffer)
;;   s-S-SPC Show buffers

;;----------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------

;; Save buffer: Command + s
(global-set-key (kbd "s-s") 'save-buffer)

;; Soft "Close" the current buffer:  Command + w
(global-set-key (kbd "s-w") 'previous-buffer) ; Move to previous buffer instead

;; Hard close the current buffer:  Command + q
(global-set-key (kbd "s-q") (lambda () (interactive) (kill-buffer (current-buffer)))) ; Actually kill the buffer

;; Switch to buffer --- Control + Shift + Space.
;;
;; We will override this keybinding later to use helm for more
;; features. (This is done so that if external package loading (for
;; helm) fails for any reason, this keybinding will still work with
;; vanilla Emacs)
;;
(global-set-key (kbd "s-SPC") 'switch-to-buffer)

;; Previous buffer: Command + w (“Close” the buffer)

;; Next buffer: Command + e
(global-set-key (kbd "s-e") 'next-buffer)


;;----------------------------------------------------------------------
;; Customizations
;;----------------------------------------------------------------------

;; Save recently closed buffers list, so that they can be opened quickly
(recentf-mode 1)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
