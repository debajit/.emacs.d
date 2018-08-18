(require 'hide-comnt)

;; Enable hideshow minor mode for all programming modes
(add-hook 'prog-mode-hook
  (lambda () (hs-minor-mode)))

;; Toggle (“fold”) all comments - F5
(global-set-key (kbd "<f5>") 'hide/show-comments-toggle)

(global-set-key (kbd "<f6>") (lambda () (interactive) (hs-hide-level 1)))
(global-set-key (kbd "<f7>") 'hs-show-all)
