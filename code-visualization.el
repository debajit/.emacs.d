;; Rainbow mode. Colorise colour names in certain modes. (Taken from
;; https://github.com/bodil/ohai-emacs/blob/master/modules/ohai-html.el)
(use-package rainbow-mode
  :ensure t
  :config
  (dolist (mode '(css-mode less-css-mode html-mode plantuml-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (rainbow-mode))))
  :diminish rainbow-mode)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
