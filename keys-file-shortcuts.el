;;
;; ~/.emacs.d/keys-file-shortcuts.el
;;
;; Keybindings to open certain preconfigured files quickly.
;;

;; Jump to scratch file --- Shift + Option + Enter
(global-set-key (kbd "M-RET")
                (lambda () (interactive) (find-file "/tmp/draft.org")))

;; Jump to Milestones --- Command + Control + k
(global-set-key (kbd "C-s-k")
                (lambda () (interactive) (find-file "~/WorkDocs/Documents/Milestones.org")))
(global-set-key (kbd "M-K")
                (lambda () (interactive) (find-file "~/WorkDocs/Documents/DEVELOPMENT.org")))

;; Jump to the Emacs configuration file, init.el --- Command + Control + e
(global-set-key (kbd "M-E")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Jump to the Inbox --- Command + Control + i
(global-set-key (kbd "C-s-i")
                (lambda () (interactive) (find-file inbox-tasks-file)))


;; Jump to the Journal --- Command + Control + j
(global-set-key (kbd "C-s-j")
                (lambda () (interactive) (find-file work-journal-file)))
