;;
;; ~/.emacs.d/keys-file-shortcuts.el
;;
;; Keybindings to open certain preconfigured files quickly.
;;
;; | File                   | Key   | Notes         |
;; |------------------------+-------+---------------|
;; | draft.org              | C-s-; | Scratchpad    |
;; |------------------------+-------+---------------|
;; | init.el                | C-s-e | M-E           |
;; |------------------------+-------+---------------|
;; | DEVELOPMENT.org        | C-s-l |               |
;; | Milestones.org         | C-s-m |               |
;; | Oncall.org             | C-s-o |               |
;; | work.org               | C-s-w |               |
;; | work-retrospective.org | C-s-r |               |
;;

;; TODO: Refactor these keybindings into a loop over a map of a
;; file-list and a keybinding string

;; Jump to scratch file --- Command + Shift + ;
(global-set-key (kbd "C-s-;")
                (lambda () (interactive) (find-file "~/Projects/Drafts/draft.org")))

;; Jump to the Emacs configuration file, init.el --- Command + Control + e
(global-set-key (kbd "C-s-e")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "M-E")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Jump to work documents
(global-set-key (kbd "C-s-l")
                (lambda () (interactive) (find-file "~/Projects/Knowledge/WorkNotes/DEVELOPMENT.org")))
(global-set-key (kbd "C-s-m")
                (lambda () (interactive) (find-file "~/Projects/Knowledge/WorkNotes/Milestones.org")))
(global-set-key (kbd "C-s-o")
                (lambda () (interactive) (find-file "~/Projects/Knowledge/WorkNotes/Oncall.org")))


;; Jump to the Inbox --- Command + Control + i
(global-set-key (kbd "C-s-i")
                (lambda () (interactive) (find-file inbox-tasks-file)))
;; (global-set-key (kbd "<C-s-268632073>")
;;                 (lambda () (interactive) (find-file inbox-tasks-file)))

;; Jump to the Journal --- Command + Control + j
(global-set-key (kbd "C-s-w")
                (lambda () (interactive) (find-file work-tasks-file)))
(global-set-key (kbd "C-s-r")
                (lambda () (interactive) (find-file work-retrospective-file)))
(global-set-key (kbd "C-s-j")
                (lambda () (interactive) (find-file work-journal-file)))
;; (global-set-key (kbd "<C-s-268632074>")
;;                 (lambda () (interactive) (find-file work-journal-file)))

;; Miscellaneous
(global-set-key (kbd "C-s-g")
                (lambda () (interactive) (find-file "~/workspace/source/golinks/src/main/web/src/App/index.tsx")))
