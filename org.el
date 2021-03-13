;; Org mode
(use-package org
  ;; :ensure t
  :init
  (setq org-startup-indented t               ; Turn on org-indent-mode
        org-startup-folded nil               ; Start expanded
        org-cycle-separator-lines 0          ; Add newline between collapsed headers
        org-ellipsis "   ↩"                  ; Collapsed heading suffix
        org-startup-with-inline-images t     ; Show images inline
        org-export-with-section-numbers nil  ; TODO: Not working
        htmlize-output-type 'css
        org-html-htmlize-output-type 'css
        org-use-speed-commands t             ; Navigate and control org headings quickly.
        org-use-fast-todo-selection t        ; Mark agenda task as complete quickly. https://orgmode.org/manual/TODO-basics.html
        )

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS" "WAITING-FOR" "WAITING_FOR_CUSTOMER" "CODE-REVIEW" "DEPLOYING" "WAITING_FOR_SCHEDULE" "BLOCKED" "|" "DONE(x!)" "HANDED OFF" "DELEGATED" "CANCELED(c@)")))

  ;; Org mode keyboard shortcuts
  :bind (:map org-mode-map
              ("s-." . org-open-at-point)
              ("s-," . org-mark-ring-goto)
              ("s-u" . org-up-element)
              ("s-1" . org-table-sort-lines)
              ("s-A" . org-archive-subtree)
              ("C-S-SPC" . org-toggle-checkbox)
              )

  ;; Global keyboard shortcuts
  :bind (("M-S-SPC" . org-capture)
         ;; ("C-S-SPC" . org-agenda)
         )

  :config
  (custom-set-variables '(org-hide-emphasis-markers t)) ; Hide bold, italic markers
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (elixir . t)
     (R . t)
     (ruby . t)
     (shell . t)
     ))

  (add-hook 'org-mode-hook
            '(lambda ()
               ;; (auto-fill-mode)                     ; Hard wrap automatically
               (whitespace-mode 0)                  ; Do not show trailing whitespace
               (setq org-src-fontify-natively t
                     org-export-with-section-numbers nil)))  ; Syntax-highlight code snippets

  ;; Diminish org-indent-mode
  ;; see http://emacs.stackexchange.com/questions/22531/diminish-org-indent-mode
  (eval-after-load 'org-indent '(diminish 'org-indent-mode)))

;; Unicode Org-mode bullets for improved typography
(use-package org-bullets
  :ensure t
  :diminish org-bullets-mode
  :init
  (setq org-bullets-bullet-list
        '("◉" "○" "✸" "○" "☯" "⚫" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ;; Render org agenda priorities as bullets instead of [#A], [#B]
;; (use-package org-fancy-priorities
;;   :ensure t
;;   :hook
;;   (org-mode . org-fancy-priorities-mode)
;;   :config
;;   (setq org-fancy-priorities-list '("⬆" "➽" "⬇")
;;         org-priority-faces '((?A :foreground "red")
;;                              (?B :foreground "orange")
;;                              (?C . "blue"))))

(use-package org-journal
  :ensure t)

(use-package ox-twbs
  :ensure t)

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      ;; (org-roam-directory "~/Projects/Knowledge/notes")
      (org-roam-directory "~/Projects/Code/algo-and-data-structures")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n c" . org-roam-db-build-cache)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))
              (("C-c n t" . org-roam-tag-add))
              ))

(with-eval-after-load 'org

  ;; Markup

  ;;
  ;; Command + b
  ;; - Make text bold if there is a selection
  ;; - Otherwise fall back to bookmarks
  ;;
  (define-key org-mode-map (kbd "s-b")
    (lambda ()
      (interactive)
      (if (use-region-p)
          (org-emphasize ?\*)
        (helm-bookmarks))))
  (define-key org-mode-map (kbd "s-i") (lambda () (interactive) (org-emphasize ?\/)))

  ;; Navigation
  (define-key org-mode-map (kbd "M-p") 'org-previous-visible-heading)
  (define-key org-mode-map (kbd "M-n") 'org-next-visible-heading)

  ;; Macros
  (define-key org-mode-map (kbd "s-B") 'embolden-line)
  (define-key org-mode-map (kbd "s-l") 'list-itemify)
  (define-key org-mode-map (kbd "s-I") 'italicize-line)
  (define-key org-mode-map (kbd "s-H") 'list-item-with-heading)
  (define-key org-mode-map (kbd "s-U") 'codify-line)

  )

(with-eval-after-load 'org-agenda

  ;; Use x to mark tasks as done in Org agenda. See
  ;; https://sachachua.com/blog/2013/01/emacs-org-task-related-keyboard-shortcuts-agenda/
  (defun sacha/org-agenda-done (&optional arg)
    "Mark current TODO as done. This changes the line at point,
     all other lines in the agenda referring to the same tree
     node, and the headline of the tree node in the Org-mode
     file."
    (interactive "P")
    (org-agenda-todo "DONE"))
  ;; Override the key definition for org-exit
  (define-key org-agenda-mode-map "x" 'sacha/org-agenda-done)

  (defun sacha/org-agenda-mark-done-and-add-followup ()
    "Mark the current TODO as done and add another task after it.
     Creates it at the same level as the previous task, so it's
     better to use this with to-do items than with projects or
     headings."
    (interactive)
    (org-agenda-todo "DONE")
    (org-agenda-switch-to)
    (org-capture 0 "i"))
  ;; Override the key definition
  (define-key org-agenda-mode-map "X" 'sacha/org-agenda-mark-done-and-add-followup)


  )
