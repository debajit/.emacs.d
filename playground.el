;; ;; https://orgmode.org/manual/Editing-source-code.html
;; ;; https://stackoverflow.com/a/53979381/2288585
;; (require 'color)
;; (set-face-attribute 'org-block nil :background
;;                     (color-darken-name
;;                      (face-attribute 'default :background) 3))
;; (set-face-attribute 'org-block-begin-line nil :background
;;                     (color-darken-name
;;                      (face-attribute 'default :background) 4))
;; (set-face-attribute 'org-block-end-line nil :background
;;                     (color-darken-name
;;                      (face-attribute 'default :background) 4))

(use-package treemacs-projectile
  :ensure t
  :bind ("M-T" . treemacs))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "s-u") 'dired-up-directory)
  )

;; From https://emacs.stackexchange.com/a/41502/12922

(defun ska-point-to-register ()
  "Store cursorposition _fast_ in a register. Use ska-jump-to-register
to jump back to the stored position."
  (interactive)
  (point-to-register 8))

(defun ska-jump-to-register ()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(global-set-key (kbd "C-.") 'ska-point-to-register)
;; From https://emacs.stackexchange.com/a/41502/12922

(defun ska-point-to-register ()
  "Store cursorposition _fast_ in a register. Use ska-jump-to-register
to jump back to the stored position."
  (interactive)
  (point-to-register 8))

(defun ska-jump-to-register ()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(global-set-key (kbd "C-.") 'ska-point-to-register)
(global-set-key (kbd "C-,") 'ska-jump-to-register)

;; Taken from https://www.reddit.com/r/emacs/comments/43b42y/i_just_realized_emacs_has_a_fast_infix_calculator/
(defun calc-eval-region (beg end)
  "Eval the arithmetic expression in the region and replace it with the result"
  (interactive "r")
  (let ((val (calc-eval (buffer-substring beg end))))
    (delete-region beg end)
    (insert val)))

(global-set-key (kbd "M-+") 'calc-eval-region)

(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US")

(global-set-key (kbd "M-I") 'org-agenda-show-custom-perspective)

(global-set-key (kbd "M-i") 'crux-switch-to-previous-buffer)

;; Save ivy views
;; taken from https://github.com/abo-abo/swiper/issues/1079

(defun peng-save-ivy-views ()
(interactive)
(with-temp-file "~/.emacs.d/ivy-views"
(prin1 ivy-views (current-buffer))
(message "save ivy-views to ~/.emacs.d/ivy-views")))

(defun peng-load-ivy-views ()
(interactive)
(setq ivy-views
(with-temp-buffer
(insert-file-contents "~/.emacs.d/ivy-views")
(read (current-buffer))))
(message "load ivy-views"))

;; (use-package nano-theme
;;   :ensure nil
;;   :defer t
;;   :quelpa (nano-theme
;;            :fetcher github
;;            :repo "rougier/nano-theme"))

;; Fireplace
(use-package fireplace
  :ensure t)
