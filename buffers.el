;;; -*- lexical-binding: t -*-
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

;; Packages
(use-package ultra-scroll
  ;; See https://github.com/jdtsmith/ultra-scroll
  ;;
  ;; Ultra Scroll is available from MELPA.  `:ensure t' makes this
  ;; declaration self-contained: use-package installs it when missing,
  ;; without requiring a separate `package-vc-install' step.
  :ensure t
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))


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
;; This fallback remains useful if the external completion packages fail to
;; load; Consult overrides it later during normal startup.
;;
(global-set-key (kbd "s-SPC") 'switch-to-buffer)

;; Previous buffer: Command + w (“Close” the buffer)

;; Next buffer: Command + e
;; (global-set-key (kbd "s-e") 'next-buffer)


;;----------------------------------------------------------------------
;; Customizations
;;----------------------------------------------------------------------

;; Save recently closed buffers list, so that they can be opened quickly
(recentf-mode 1)

;; Revert buffers automatically when underlying files are changed
;; externally. Emacs uses file notification (inotify on Linux) rather
;; than polling, so this is cheap.
(setq auto-revert-use-notify t              ; Use inotify instead of polling
      auto-revert-avoid-polling t           ; Don't also poll watched buffers
      auto-revert-remote-files nil          ; Never auto-revert TRAMP buffers
      auto-revert-verbose nil               ; No “Reverting buffer...” messages
      global-auto-revert-non-file-buffers t) ; Also refresh Dired, Buffer Menu, etc.
(global-auto-revert-mode 1)
