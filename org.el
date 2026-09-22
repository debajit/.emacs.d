;;; -*- lexical-binding: t -*-
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
        org-export-with-footnotes nil        ; See https://emacs.stackexchange.com/questions/68986/ignore-footnotes-when-exporting-org-texts-to-html
        org-imenu-depth 5
        )

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS" "WAITING-FOR" "WAITING_FOR_CUSTOMER" "CODE-REVIEW" "DEPLOYING" "WAITING_FOR_SCHEDULE" "BLOCKED" "|" "DONE(x!)" "HANDED OFF" "DELEGATED" "CANCELED(c@)")))

  ;; Set up link abbreviations. See
  ;; https://orgmode.org/manual/Link-Abbreviations.html
  (setq org-link-abbrev-alist
      '(("bugzilla"        . "https://10.1.2.9/bugzilla/show_bug.cgi?id=")
        ("Nu Html Checker" . "https://validator.w3.org/nu/?doc=%h")
        ("duckduckgo"      . "https://duckduckgo.com/?q=%s")
        ("omap"            . "https://nominatim.openstreetmap.org/search?q=%s&polygon=1")
        ("ads"             . "https://ui.adsabs.harvard.edu/search/q=%20author%3A\"%s\"")))

  ;; Org mode keyboard shortcuts
  :bind (:map org-mode-map
              ("s-." . org-open-at-point)
              ("s-," . org-mark-ring-goto)
              ;; ("s-u" . org-up-element)
              ("s-1" . org-table-sort-lines)
              ("s-A" . org-archive-subtree)
              ("M-N" . org-next-link)
              ("M-P" . org-previous-link)
              ("C-S-SPC" . org-toggle-checkbox)
              ("C-c SPC" . org-table-blank-field) ; See https://emacs.stackexchange.com/a/22
              )

  ;; Global keyboard shortcuts
  :bind (("M-S-SPC" . org-capture)
         ;; ("C-S-SPC" . org-agenda)
         )

  :config
  (custom-set-variables '(org-hide-emphasis-markers t)) ; Hide bold, italic markers

  ;; Open PDF links in the external PDF viewer.
  ;; See https://emacs.stackexchange.com/a/28047
  (pcase system-type
    (gnu/linux
     (add-to-list 'org-file-apps '("pdf" . "evince %s"))
     (add-to-list 'org-file-apps '("epub" . "foliate %s")))
    (darwin
     (add-to-list 'org-file-apps '("pdf" . "open %s"))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     ;; (elixir . t)
     (java . t)
     (js . t)
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
  :ensure t
  :defer t)

(use-package ox-twbs
  :ensure t
  :defer t
  )

;; Org-Roam basic configuration
(setq org-directory (concat (getenv "HOME") "/Archive/Knowledge/Notes/"))

;; (use-package org-roam
;;       :ensure t
;;       :hook
;;       ;; (after-init . org-roam-mode)
;;       (org-roam-db-autosync-mode)
;;       ;; (setq org-roam-directory (file-truename "~/org-roam"))
;;       :custom
;;       (org-roam-directory "~/Projects/Knowledge/notes")
;;       :bind (:map org-roam-mode-map
;;               (("C-c n l" . org-roam)
;;                ("C-c n c" . org-roam-db-build-cache)
;;                ("C-c n f" . org-roam-find-file)
;;                ("s-T" . org-roam-find-file)
;;                ("C-c n g" . org-roam-graph))
;;               :map org-mode-map
;;               (("C-c n i" . org-roam-insert))
;;               (("C-c n I" . org-roam-insert-immediate))
;;               (("C-c n t" . org-roam-tag-add))
;;               ))

;; Org-roam startup and completion-cache policy
;;
;; Org-roam deliberately remains deferred below.  The key bindings created by
;; `use-package' are autoloads, so merely starting Emacs does not load Org-roam,
;; open its SQLite database, or scan the files in `org-roam-directory'.  The
;; first Org-roam command in an Emacs session pays those one-time costs instead.
;;
;; There is a second, otherwise repeated cost: `org-roam-node-find' calls the
;; internal function `org-roam-node-read--completions', which queries the
;; database and formats every node for display each time the command runs.  For
;; a large collection this work is noticeable even when the database itself is
;; already synchronized.  Cache the ordinary, unfiltered completion list after
;; its first construction so later node searches in the same Emacs session can
;; reuse it.
;;
;; Database-changing operations invalidate the cache below.  Therefore saving,
;; renaming, deleting, or synchronizing notes makes the *next* node search
;; rebuild the list once; subsequent searches are fast again.  Calls that ask
;; for a custom filter or sort order bypass the cache because their result is
;; not the same as the ordinary `org-roam-node-find' result.
;;
;; Maintenance note: the double hyphen in
;; `org-roam-node-read--completions' marks it as an Org-roam internal function.
;; After a major Org-roam upgrade, this advice is the first place to inspect if
;; node completion stops working.  `M-x my/org-roam-clear-node-completion-cache'
;; can also be used after changing completion-related settings by hand.
(defvar my/org-roam-node-completion-cache nil
  "Cached candidates for an ordinary `org-roam-node-find' prompt.")

(defvar my/org-roam-node-completion-cache-valid-p nil
  "Non-nil when `my/org-roam-node-completion-cache' may be reused.")

(defun my/org-roam-clear-node-completion-cache (&rest _ignored)
  "Invalidate the in-memory Org-roam node completion cache.

The unused arguments allow this function to be installed as `:after' advice
on database functions with different signatures.  The candidate list is
rebuilt lazily by the next ordinary node lookup, not while the database is
being updated."
  (interactive)
  (setq my/org-roam-node-completion-cache nil
        my/org-roam-node-completion-cache-valid-p nil)
  (when (called-interactively-p 'interactive)
    (message "Org-roam node completion cache cleared")))

(defun my/org-roam-node-read--completions-cached
    (original-function &optional filter-fn sort-fn)
  "Cache the result of an ordinary Org-roam node completion request.

ORIGINAL-FUNCTION is `org-roam-node-read--completions'.  FILTER-FN and SORT-FN
are passed through unchanged.  A request using either function bypasses the
shared cache because it may produce a different set or order of candidates."
  (if (or filter-fn sort-fn)
      (funcall original-function filter-fn sort-fn)
    (unless my/org-roam-node-completion-cache-valid-p
      (setq my/org-roam-node-completion-cache
            (funcall original-function)
            my/org-roam-node-completion-cache-valid-p t))
    my/org-roam-node-completion-cache))

(use-package org-roam
  ;; See https://org-roam.discourse.group/t/use-of-property-drawers-after-headlines/1687/11

  :ensure t
  :defer t
  :init
  (setq org-id-link-to-org-use-id nil
        org-roam-directory (file-truename org-directory)

        ;; Enabling `org-roam-db-autosync-mode' performs a database sync.  That
        ;; sync constructs many temporary Org syntax trees and can otherwise
        ;; pause repeatedly for garbage collection.  Org-roam dynamically uses
        ;; this higher threshold only while syncing; normal Emacs garbage
        ;; collection behavior is unchanged afterward.  This trades temporary
        ;; memory usage for a faster first Org-roam command.
        ;;
        ;; Official performance documentation:
        ;; https://www.orgroam.com/manual#Performance-Optimization
        org-roam-db-gc-threshold most-positive-fixnum)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ;; ("s-T" . org-roam-node-find)
         ("s-e" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n s" . org-roam-db-sync)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode 1)

  ;; Advice installation is guarded so evaluating this configuration again
  ;; does not stack duplicate copies of the same advice.
  (unless (advice-member-p #'my/org-roam-node-read--completions-cached
                           'org-roam-node-read--completions)
    (advice-add 'org-roam-node-read--completions :around
                #'my/org-roam-node-read--completions-cached))

  ;; These are the paths through which Org-roam changes the nodes stored in its
  ;; database.  Invalidate after the operation completes so the next lookup sees
  ;; the new database contents.  `org-roam-db-sync' is included explicitly for
  ;; the no-files-changed case, where neither update nor clear would run.
  (dolist (database-function '(org-roam-db-update-file
                               org-roam-db-clear-file
                               org-roam-db-sync))
    (unless (advice-member-p #'my/org-roam-clear-node-completion-cache
                             database-function)
      (advice-add database-function :after
                  #'my/org-roam-clear-node-completion-cache)))

  ;; (setq org-roam-capture-templates '(("d" "default" plain "%?"
  ;;                                     :target (file+head "${slug}.org.gpg"
  ;;                                                        "#+title: ${title}\n")
  ;;                                     :unnarrowed t)))
  )

;; (use-package org-roam
;;   :after org
;;   :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
;;   :custom
;;   (org-roam-directory (file-truename org-directory))
;;   :config
;;   (org-roam-setup)
;;   (org-roam-db-autosync-mode)
;;   :bind (:map org-roam-mode-map
;;               (("C-c n l" . org-roam)
;;                ("C-c n c" . org-roam-db-build-cache)
;;                ("C-c n f" . org-roam-find-file)
;;                ("s-T" . org-roam-find-file)
;;                ("C-c n g" . org-roam-graph))
;;               :map org-mode-map
;;               (("C-c n i" . org-roam-insert)
;;                ("C-c n I" . org-roam-insert-immediate)
;;                ("C-c n t" . org-roam-tag-add))
;;               ))

;; (use-package anki-editor
;;   :ensure t)

;; (use-package org-drill
;;   :defer t
;;   :config (progn
;;             (add-to-list 'org-modules 'org-drill)))

(with-eval-after-load 'org

  ;; Fontify emphasis (*bold*, /italic/ etc.) that spans hard-wrapped lines.
  ;; The fifth component is the maximum number of newlines allowed inside one
  ;; emphasis span; the default of 1 leaves most wrapped markup unfontified.
  ;; `org-set-emph-re' both sets the variable and recomputes `org-emph-re'.
  (org-set-emph-re 'org-emphasis-regexp-components
                   (append (butlast org-emphasis-regexp-components) '(20)))

  ;; man: links.  See https://orgmode.org/manual/Adding-Hyperlink-Types.html
  (require 'ol-man)
  (setq org-man-command 'woman)         ; Open man pages with woman

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
  ;; (define-key org-mode-map (kbd "s-i") (lambda () (interactive) (org-emphasize ?\/)))

  ;; Navigation
  (define-key org-mode-map (kbd "M-p") 'org-previous-visible-heading)
  (define-key org-mode-map (kbd "M-n") 'org-next-visible-heading)

  ;; Macros
  (define-key org-mode-map (kbd "s-B") 'embolden-line)
  (define-key org-mode-map (kbd "s-l") 'list-itemify)
  (define-key org-mode-map (kbd "s-I") 'italicize-line)
  (define-key org-mode-map (kbd "s-H") 'list-item-with-heading)
  (define-key org-mode-map (kbd "s-U") 'codify-line))

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
  (define-key org-agenda-mode-map "X" 'sacha/org-agenda-mark-done-and-add-followup))
