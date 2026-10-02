;; -*- lexical-binding: t; -*-
(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . eglot-ensure)
  :config
  (setq terraform-format-on-save t))
