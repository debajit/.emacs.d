;;
;; ~/.emacs/folding.el
;;
;; Keybindings defined:
;;
;;   F5  -  Toggle comments
;;   F6  -  Hide/show deeper
;;   F7  -  Hide/show shallower
;;

(require 'hide-comnt)

;; Enable hideshow minor mode for all programming modes
(add-hook 'prog-mode-hook
  (lambda () (hs-minor-mode)))

(global-set-key (kbd "<f5>") 'hide/show-comments-toggle)
(global-set-key (kbd "<f6>") 'hs-deeper)
(global-set-key (kbd "<f7>") 'hs-shallower)
