(use-package elixir-mode
  :ensure t)

(use-package elixir-yasnippets
  :ensure t)

(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

;; Alchemist mode for Elixir
(use-package alchemist
  :ensure t
  :bind (:map alchemist-mode-map
              ("C-T" . alchemist-mix-test-stale)
              ("s-T" . alchemist-project-toggle-file-and-tests-other-window)))
