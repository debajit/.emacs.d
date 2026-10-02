;;; -*- lexical-binding: t -*-
;; Truncate lines by default. Press s-p to toggle smart-wrapping
(setq-default truncate-lines t)

;; (s-U) - Enable wrap
(global-set-key (kbd "s-U") 'visual-line-mode)
