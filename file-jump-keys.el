;;; -*- lexical-binding: t -*-
;;
;; ~/.emacs.d/keys-file-shortcuts.el
;;
;; Keybindings to open certain preconfigured files quickly.
;;

(global-set-key (kbd "C-s-;") (lambda () (interactive) (find-file "~/Documents/draft.org")))
(global-set-key (kbd "C-s-e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "M-E")   (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-s-a") (lambda () (interactive) (find-file "~/Archive/Knowledge/Notes/20221202172447-algorithms_and_data_structures.org")))
(global-set-key (kbd "C-s-s") (lambda () (interactive) (find-file "~/Archive/Knowledge/Notes/20221126000441-system_software_design_interviews.org")))
(global-set-key (kbd "C-s-m") (lambda () (interactive) (find-file "~/Archive/Knowledge/Notes/Morning_Routine.org")))
(global-set-key (kbd "C-s-i") (lambda () (interactive) (find-file inbox-tasks-file)))

(global-set-key (kbd "C-s-l") (lambda () (interactive) (find-file "~/Work/Docs/Documents/DEVELOPMENT.org")))
(global-set-key (kbd "C-s-r") (lambda () (interactive) (find-file "~/Work/Knowledge/WorkNotes/Planning/Sprint Retrospective.org")))
(global-set-key (kbd "C-s-w") (lambda () (interactive) (find-file "~/Projects/Tasks/org-agenda/work-log.org")))
