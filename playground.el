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


;; Make emacs open all files in last emacs session.
;; Taken from ergoemacs, credits go to ergoemacs authors.

;; This functionality is provided by desktop-save-mode
;; (“feature” name: “desktop”).
;;
;; The mode is not on by default in emacs 23.1, and has a lot options.
;; The following is init settings for the mode for ErgoEmacs.
;; Goal: have emacs always auto open the set of opened files in last session,
;; even if emacs crashed in last session or the OS crashed in last session.
;; Also, don't bother users by asking questions like “do you want to save
;; desktop?” or “do you want to override last session file?”, because these are
;; annoying and terms like “session” or “desktop” are confusing to most users
;; because it can have many meanings.
;;
;; Some tech detail: set the desktop session file 〔.emacs.desktop〕
;; at the variable “user-emacs-directory” (default value is “~/.emacs.d/”).
;; This file is our desktop file. It will be auto created and or over-written.
;; If a emacs expert has other desktop session files elsewhere, he can still use
;; or manage those.

(require 'desktop)

(defun desktop-settings-setup ()
  "Some settings setup for desktop-save-mode."
  (interactive)

  ;; At this point the desktop.el hook in after-init-hook was
  ;; executed, so (desktop-read) is avoided.
  (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet
    ;; Here we activate the desktop mode
    (desktop-save-mode 1)

    ;; The default desktop is saved always
    (setq desktop-save t)

    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)

    ;; Set the location to save/load default desktop
    (setq desktop-dirname user-emacs-directory)

    ;; Make sure that even if emacs or OS crashed, emacs
    ;; still have last opened files.
    (add-hook 'find-file-hook
     (lambda ()
       (run-with-timer 5 nil
          (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save user-emacs-directory)))))

    ;; Read default desktop
    (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
        (desktop-read desktop-dirname))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook
              (lambda ()
                ;; Reset desktop modification time so the user is not bothered
                (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
    )
  )

(add-hook 'after-init-hook
          'desktop-settings-setup
          (lambda ()
            ;; No splash screen
            (setq inhibit-startup-screen t)

            ;; If the *scratch* buffer is the current one, then create a new
            ;; empty untitled buffer to hide *scratch*
            (if (string= (buffer-name) "*scratch*")
                (new-empty-buffer))
            )
          t) ;; append this hook to the tail
