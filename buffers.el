;;
;; ~/.emacs.d/buffers.el
;;
;; Buffer-related customizations.
;;

;;----------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------

;; Save buffer: Command + s
(global-set-key (kbd "s-s") 'save-buffer)

;; Close buffer: Command + w
(global-set-key (kbd "s-w") (lambda () (interactive) (kill-buffer (current-buffer))))

;; Switch to buffer --- Control + Shift + Space.
;;
;; We will override this keybinding later to use helm for more
;; features. (This is done so that if external package loading (for
;; helm) fails for any reason, this keybinding will still work with
;; vanilla Emacs)
;;
(global-set-key (kbd "s-SPC") 'switch-to-buffer)

;; Previous buffer: Control + Shift + j
(global-set-key (kbd "s-J") 'previous-buffer)

;; Next buffer: Control + Shift + k
(global-set-key (kbd "s-K") 'next-buffer)


;;----------------------------------------------------------------------
;; Customizations
;;----------------------------------------------------------------------

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
