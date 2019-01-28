;;
;; ~/.emacs.d/window-management.el
;;
;; Window management settings.
;;
;; The following documentation uses the terms “panes” and “windows” to
;; refer to Emacs’ windows and frames respectively.
;;

;;
;; Keybindings
;;

;;----------------------------------------------------------------------
;; Navigating between panes
;;----------------------------------------------------------------------

;; Jump to the next pane (in cyclical order):  Command + j
(global-set-key (kbd "s-j") 'other-window)

;; Move pane to next position:  Command + Shift + M
(global-set-key (kbd "s-M") 'window-swap-states)

;; Jump to window on left
(global-set-key (kbd "<s-left>") 'windmove-left)

;; Jump to window on right
(global-set-key (kbd "<s-right>") 'windmove-right)

;; Jump to window above
(global-set-key (kbd "<s-up>") 'windmove-up)

;; Jump to window below
(global-set-key (kbd "<s-down>") 'windmove-down)


;;----------------------------------------------------------------------
;; Opening a window
;;----------------------------------------------------------------------

;; New window: s-n   (default: C-x 5 2)
;;
;; We are using this key for narrowing at the moment, which is much
;; more useful. New window was rarely used.
;;
;; (global-set-key (kbd "s-n") (lambda () (interactive) (find-file-other-frame "/tmp/scratch.org")))

;; New window: s-N
(global-set-key (kbd "s-N") 'make-frame-command)


;;----------------------------------------------------------------------
;; Closing panes and windows
;;----------------------------------------------------------------------

;; Close pane: s-W   (default: C-x 0)
(global-set-key (kbd "s-W") (lambda () (interactive) (delete-window) (balance-windows)))

;; Close window (Emacs' frame): s-q
(global-set-key (kbd "s-Q") 'delete-frame)


;;----------------------------------------------------------------------
;; Splitting panes
;;----------------------------------------------------------------------

;; Split horizontally: s-J   (default: C-x 3)
(global-set-key (kbd "s-J") (lambda () (interactive) (split-window-right) (windmove-right)))

;; Split horizontally: s-K   (default: C-x 2)
(global-set-key (kbd "s-K") (lambda () (interactive) (split-window-below) (windmove-down)))


;;----------------------------------------------------------------------
;; Sizing panes and windows
;;----------------------------------------------------------------------

;; Balance windows
(global-set-key (kbd "s-:") 'balance-windows)

;; Increase window size: s-]
(global-set-key (kbd "M-]")
                (lambda () (interactive) (enlarge-window-horizontally 20)))

;; Decrease window size: s-[
(global-set-key (kbd "M-[")
                (lambda () (interactive) (shrink-window-horizontally 20)))

;; Maximize window: s-m   (default: C-x 1)
(global-set-key (kbd "s-m") 'delete-other-windows)


;;----------------------------------------------------------------------
;; Undoing and Redoing pane Layouts
;;----------------------------------------------------------------------

;; Winner mode (undo and redo window layouts)
(winner-mode 1)
(global-set-key (kbd "s-y") 'winner-undo)
(global-set-key (kbd "s-Y") 'winner-redo)
