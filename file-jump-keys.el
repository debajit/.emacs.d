;;
;; ~/.emacs.d/keys-file-shortcuts.el
;;
;; Keybindings to open certain preconfigured files quickly.
;;

;; Jump to scratch file --- Command + Shift + ;
(global-set-key (kbd "C-s-;")
                (lambda () (interactive) (find-file "~/Projects/Drafts/draft.org")))


;; Jump to Milestones --- Command + Control + k
(global-set-key (kbd "C-s-m")
                (lambda () (interactive) (find-file "~/Projects/Knowledge/Milestones.org")))

(global-set-key (kbd "C-s-l")
                (lambda () (interactive) (find-file "~/Projects/Knowledge/DEVELOPMENT.org")))


;; Jump to the Emacs configuration file, init.el --- Command + Control + e
(global-set-key (kbd "C-s-e")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-s-o")
                (lambda () (interactive) (find-file "~/Projects/Knowledge/WorkNotes/Oncall.org")))

(global-set-key (kbd "M-E")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))


;; Jump to the Inbox --- Command + Control + i
(global-set-key (kbd "C-s-i")
                (lambda () (interactive) (find-file inbox-tasks-file)))
;; (global-set-key (kbd "<C-s-268632073>")
;;                 (lambda () (interactive) (find-file inbox-tasks-file)))


;; Jump to the Journal --- Command + Control + j
(global-set-key (kbd "C-s-j")
                (lambda () (interactive) (find-file work-journal-file)))
;; (global-set-key (kbd "<C-s-268632074>")
;;                 (lambda () (interactive) (find-file work-journal-file)))

(global-set-key (kbd "C-s-g")
                (lambda () (interactive) (find-file "~/workspace/source/golinks/src/main/web/src/App/index.tsx")))

(global-set-key (kbd "C-s-w")
                (lambda () (interactive) (find-file "~/Documents/Tasks/Todo/work.org")))

(global-set-key (kbd "C-s-r")
                (lambda () (interactive) (find-file "~/Documents/Tasks/Todo/work-retrospective.org")))
