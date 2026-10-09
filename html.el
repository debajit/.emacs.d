;;; -*- lexical-binding: t -*-
(define-minor-mode my-html-preview-mode
  "Provide a browser preview shortcut throughout an HTML buffer."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "s-r") #'browse-url-of-buffer)
            map))

;; This could live inside `use-package mhtml-mode'; keeping it outside is
;; an organizational choice.  A binding in `mhtml-mode-map' alone would be
;; shadowed by the separate CSS and JavaScript keymaps in <style> and
;; <script> blocks.  The minor-mode map applies throughout the buffer, and
;; `html-mode-hook' enables it for both ordinary HTML and HTML+ buffers.
(add-hook 'html-mode-hook (lambda () (my-html-preview-mode 1)))

;; mhtml Mode
(use-package mhtml-mode
  :ensure t
  :mode (("\\.html$" . mhtml-mode)
         ("\\.jsx$" . mhtml-mode))
  )
