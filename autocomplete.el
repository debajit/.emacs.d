;;
;; ~/.emacs.d/autocomplete.el
;;
;; Autocomplete customizations.
;;

;;----------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------

(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "M-/") 'hippie-expand)


;;----------------------------------------------------------------------
;; Configure hippie-expand
;;----------------------------------------------------------------------

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;; TODO: Move this to another file if required
;;
;; Autocomplete with Tab (Yas + dabbrev)
;;
;; Configure the Tab key to autocomplete / indent / expand-snippet
;; depending on the position of the cursor. See
;; http://stackoverflow.com/questions/13576156/emacs-smart-tab-with-yasnippets
;;
;; Auto complete settings / tab settings
;; http://emacsblog.org/2007/03/12/tab-completion-everywhere/ <-- in the comments

;; (global-set-key [(tab)] 'smart-tab)
;; (defun smart-tab ()
;;   "This smart tab is minibuffer compliant: it acts as usual in
;;     the minibuffer. Else, if mark is active, indents region. Else if
;;     point is at the end of a symbol, expands it. Else indents the
;;     current line."
;;   (interactive)
;;   (if (minibufferp)
;;       (unless (minibuffer-complete)
;;         (dabbrev-expand nil))
;;     (if mark-active
;;         (indent-region (region-beginning)
;;                        (region-end))
;;       (if (looking-at "\\_>")
;;           (let ((yas/fallback-behavior nil))
;;             (unless (yas/expand)
;;               (dabbrev-expand nil)))
;;         (indent-for-tab-command)))))


;; For Pabbrev mode see below, in the third-party packages section
