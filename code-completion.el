;;; -*- lexical-binding: t -*-
;;
;; ~/.emacs.d/code-completion.el
;;
;; In-buffer completion: Corfu (popup UI) + Cape (extra completion
;; sources). This is the in-buffer counterpart of Vertico in the
;; minibuffer, and it reuses the same `completion-styles' (Orderless).
;;
;; Quick reference (while the popup is visible):
;;
;;   TAB / S-TAB      Select next / previous candidate
;;   RET              Insert selected candidate (plain newline if none)
;;   M-SPC            Insert an Orderless separator, keep filtering
;;   M-h / M-g        Show documentation / location of candidate
;;   C-g              Close the popup
;;   M-m              Move the candidates to the minibuffer (Vertico)
;;
;; Elsewhere:
;;
;;   TAB              Indent, or complete if already indented
;;   C-c p ...        Cape commands (see `cape-prefix-map')
;;

;;----------------------------------------------------------------------
;; Built-in settings that affect completion-at-point
;;----------------------------------------------------------------------

;; TAB indents first, then completes if the line is already indented.
(setq tab-always-indent 'complete)

;; Don't offer dictionary words in text modes (Org, Markdown...).
;; It is noisy with an auto popup; pabbrev already covers prose.
(setq text-mode-ispell-word-completion nil)

;; Hide commands that don't apply to the current mode from M-x.
(setq read-extended-command-predicate #'command-completion-default-include-p)


;;----------------------------------------------------------------------
;; Corfu
;;----------------------------------------------------------------------

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                        ; Show popup automatically
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)                 ; ...after 2 characters
  (corfu-cycle t)                       ; Wrap around at the ends
  (corfu-preselect 'prompt)             ; Don't preselect a candidate
  (corfu-quit-no-match 'separator)      ; Quit on no match, unless filtering
  (corfu-on-exact-match nil)            ; Never auto-insert a sole match
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Docs popup beside the menu
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("M-m" . corfu-move-to-minibuffer))
  :init
  (global-corfu-mode 1)
  (corfu-history-mode 1)                ; Sort recently used first
  (corfu-popupinfo-mode 1)
  ;; Persist Corfu history across sessions via savehist.
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :config
  ;; RET inserts the selected candidate, but when nothing is selected
  ;; it falls through to a normal newline instead of being swallowed.
  (keymap-set corfu-map "RET"
              `(menu-item "" nil :filter
                          ,(lambda (&optional _)
                             (and (>= corfu--index 0) #'corfu-insert))))

  ;; Don't pop up automatically in the minibuffer (Vertico is there),
  ;; but allow it in M-: and other minibuffers that want completion.
  (setq global-corfu-minibuffer
        (lambda ()
          (not (or (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map)))))

  (defun corfu-move-to-minibuffer ()
    "Move the current Corfu candidates to the minibuffer (Vertico)."
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))


;;----------------------------------------------------------------------
;; Cape --- extra completion-at-point sources
;;----------------------------------------------------------------------

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Fallback sources, tried after the mode's own (e.g. Eglot, Elisp).
  ;; `add-hook' with a non-nil DEPTH appends, so these come last.
  (add-hook 'completion-at-point-functions #'cape-dabbrev 20)
  (add-hook 'completion-at-point-functions #'cape-file 20)
  (add-hook 'completion-at-point-functions #'cape-elisp-block 20))
