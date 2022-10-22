;; Elasticsearch support (es-mode)
;; https://github.com/dakrone/es-mode

(use-package es-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.es$" . es-mode))

  )
