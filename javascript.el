(setq js-indent-level 2)

;; JS2 Mode
(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("\\.es6$" . js2-mode))
  :init
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)
  )

;; (use-package tide)
