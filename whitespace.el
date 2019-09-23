;;
;; ~/.emacs.d/whitespace.el
;;
;; Determines how whitespace is handled.
;;

;;----------------------------------------------------------------------
;; Broad whitespace fixes
;;----------------------------------------------------------------------

;; Cleanup trailing whitespace and other whitespace after a buffer is saved
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Add a newline at the end of the file
(setq require-final-newline t)


;;----------------------------------------------------------------------
;; Indentation
;;----------------------------------------------------------------------

;; Use spaces not tabs
(setq-default indent-tabs-mode nil)   ; Don't use tabs to indent
(setq-default tab-width 4)            ; Ensure tabs are aligned well

;; C++, C indentation
(setq c-default-style "linux" ; Microsoft-style --- with { on new line
      c-basic-offset 4)       ; Indent 4 spaces

;; Perl
(setq cperl-indent-level 2)
(setq perl-indent-level 2)

;; Shell Script Mode
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; Indent rigidly to tab stop (a la TextMate or Sublime Text)
(global-set-key (kbd "s-]") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-[") 'indent-rigidly-left-to-tab-stop)
