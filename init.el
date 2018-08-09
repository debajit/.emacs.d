(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in the current user’s Emacs configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "typography.el")
(load-user-file "autocomplete.el")
(load-user-file "whitespace.el")
(load-user-file "buffers.el")
(load-user-file "window-management.el")
(load-user-file "navigation.el")
(load-user-file "selection.el")
(load-user-file "keys-file-shortcuts.el")
(load-user-file "bookmarks-web.el")
(load-file "~/WorkDocs/Application Settings/Emacs/bookmarks-work.el")
(load-user-file "emacs-for-macosx.el")

;; Save customizations in a separate file (custom.el)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror) ; Prevent errors if custom.el does not exist

(defvaralias 'lazy-highlight-face 'isearch-lazy-highlight)

;;----------------------------------------------------------------------
;; General Emacs Settings
;;----------------------------------------------------------------------

;; Regular scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Toolbar off
(tool-bar-mode 0)

;; Status bar
(line-number-mode t)                    ; Show line number
(column-number-mode t)                  ; Show column number

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight matching parentheses
(setq show-paren-delay 0)               ; Highlight instantly, no delay
(show-paren-mode 1)

;; Smart tab behavior - indent or complete
;; Commented out in favor of pabbrev mode.
;; (setq tab-always-indent 'complete)

;; Desktop mode (Always save and restore the open buffers)
(desktop-save-mode 1)

;; Highlight the current line
(global-hl-line-mode +1)

;; Line numbers
(global-set-key (kbd "<f8>") 'display-line-numbers-mode)

;; Show images by default
(setq auto-image-file-mode t)


;;----------------------------------------------------------------------
;; View-related keys
;;----------------------------------------------------------------------

;; Zoom in to text (scale text up): s-=
(global-set-key (kbd "s-=") 'text-scale-increase)

;; Zoom out: s--
(global-set-key (kbd "s--") 'text-scale-decrease)

;; Reset zoom level: s-0
(global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-increase 0)))

;; Toggle truncate lines: s-p
(global-set-key (kbd "s-p") 'visual-line-mode)


;;----------------------------------------------------------------------
;; Text editing keys
;;----------------------------------------------------------------------

;; Enable downcase-region
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "<f5>") 'apply-macro-to-region-lines)

;; Copy-paste
(global-set-key (kbd "s-V") 'helm-show-kill-ring)

;; Toggle read-only mode: s-j
;; (global-set-key (kbd "s-j") 'view-mode)

;; Delete line: s-k   (default: C-k)
(global-set-key (kbd "s-k") 'kill-whole-line)

;; Wrap with parentheses: M-(
(global-set-key (kbd "M-(") (lambda () (interactive) (sp-wrap-with-pair "(")))

;; Replace selection with a single keystroke
(delete-selection-mode t)

;; Text editing --- Text alignment

;; Align all regexp in selection. Allows lining up all the '=' signs
;; in a group of variable assignments, or lining up all the '->' in
;; Elixir.
(global-set-key (kbd "s-A") 'align-regexp)

;; Sort lines
(global-set-key (kbd "s-O") 'sort-lines)


;;----------------------------------------------------------------------
;; Vertical text editing
;;----------------------------------------------------------------------

(put 'set-goal-column 'disabled nil)


;;----------------------------------------------------------------------
;; Navigation
;;----------------------------------------------------------------------

;; Go to line
(global-set-key (kbd "s-l") 'goto-line)

;; Find tag
(global-set-key (kbd "C-M-.") 'find-tag)

;; Registers - Lightweight bookmarks by character

;; Save cursor position to register
;; (global-set-key (kbd "s-,") 'point-to-register)

;; Jump to saved resigter
;; (global-set-key (kbd "s-.") 'jump-to-register)


;;----------------------------------------------------------------------
;; Mouse shortcuts
;;----------------------------------------------------------------------

;; Open link at mouse pointer: s-click
(global-set-key [s-mouse-1] 'browse-url-at-mouse)


;;----------------------------------------------------------------------
;; Version control / Source control
;;----------------------------------------------------------------------

(global-set-key [f12] 'vc-annotate)
(global-set-key (kbd "s-H") 'vc-region-history)
(global-set-key (kbd "s-C") 'magit-diff-buffer-file)
(global-set-key (kbd "s-S") 'magit-stage-file)
;; (global-set-key (kbd "s-C") 'magit-diff-visit-file)


;;----------------------------------------------------------------------
;; Focus
;;----------------------------------------------------------------------

;; (global-set-key (kbd "s-u") 'narrow-to-defun)


(global-set-key (kbd "s-3") 'highlight-symbol-at-point)
(global-set-key (kbd "s-4") 'unhighlight-regexp)

;;----------------------------------------------------------------------
;; Date shortcuts
;;----------------------------------------------------------------------

;; Yesterday’s date
(global-set-key (kbd "<f9>")
                (lambda ()
                  "Insert yesterday’s date in the format: Sunday, January 1, 2018"
                  (interactive)
                  (insert (format-time-string "%A, %B %d, %Y"
                                              (time-subtract (current-time) (* 24 60 60))))))

;; Today’s date
(global-set-key (kbd "<f10>")
                (lambda ()
                  "Insert today’s date in the format: Sunday, January 1, 2018"
                  (interactive)
                  (insert (format-time-string "%A, %B %d, %Y"))))


;;--------------------------------------------------------------------
;; Getting Things Done.
;;
;; See also
;; - http://members.optusnet.com.au/~charles57/GTD/datetree.html
;;--------------------------------------------------------------------

;;
;; The Tasks directory is ~/Documents/Tasks.
;; The Todo directory is ~/Documents/Tasks/Todo.
;;

(setq tasks-directory "~/Documents/Tasks"
      todo-directory (concat (file-name-as-directory tasks-directory) "Todo"))

(let ((default-directory tasks-directory))
  (setq my-diary-file (expand-file-name "diary")))

(let ((default-directory todo-directory))
  (setq inbox-tasks-file (expand-file-name "inbox.org")
        home-tasks-file (expand-file-name "home.org")
        work-tasks-file (expand-file-name "work.org")
        finances-tasks-file (expand-file-name "finances.org")
        projects-tasks-file (expand-file-name "projects.org")
        home-journal-file (expand-file-name "home-log.org")
        work-journal-file (expand-file-name "work-log.org")))

;;
;; Adapted from
;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;;
;; See also https://emacs.stackexchange.com/a/7487/12922 for how to
;; evaluate variables in a list in ELisp
;;
(setq org-agenda-files `(,inbox-tasks-file
                         ,home-tasks-file
                         ,work-tasks-file
                         ,finances-tasks-file
                         ,projects-tasks-file
                         ,home-journal-file
                         ,work-journal-file))

(setq diary-file my-diary-file)

(setq org-refile-targets `((,projects-tasks-file :maxlevel . 3)
                           (,home-tasks-file :level . 1)
                           (,work-tasks-file :maxlevel . 2)
                           (,finances-tasks-file :maxlevel . 2)))

(setq org-capture-templates '(

                              ;; Capture task to Inbox.
                              ;;
                              ;; You can refile directly from this
                              ;; screen using C-c C-w and assign the
                              ;; task to a project.
                              ;;
                              ("i" "Todo [inbox]" entry
                               (file+headline inbox-tasks-file "Tasks")
                               "* TODO %i%?")

                              ;; ("T" "Tickler" entry
                              ;;  (file+headline "/Users/debajita/Documents/Tasks/Todo/tickler.org" "Tickler")
                              ;;  "* %i%? \n %U")

                              ;; Add Journal entry.
                              ;;
                              ;; Taken from
                              ;; http://www.howardism.org/Technical/Emacs/journaling-org.html
                              ;;
                              ("j" "Journal Entry — Work"
                               entry (file+datetree work-journal-file)
                               "* %?")

                              ))

(setq org-archive-location (concat "~/Documents/Tasks/Archive/archive-" (format-time-string "%Y%m" (current-time)) ".org_archive::"))

;; ;;----------------------------------------------------------------------
;; ;; MobileOrg setup
;; ;;----------------------------------------------------------------------

;; ;; Local Org files
;; (setq org-directory "~/Documents/Tasks/Todo")

;; ;; Inbox for items added from the phone
;; (setq org-mobile-inbox-for-pull "~/Documents/Tasks/Todo/from-mobile.org")

;; ;; WebDAV directory mountpoint
;; (setq org-mobile-directory "/Volumes/DriveHQ Documents/Tasks/Todo")

;; ;; Files to sync
;; ;; (setq org-mobile-files `,org-agenda-files)


(defun org-agenda-show-custom-perspective (&optional arg)
  (interactive "P")
  (org-agenda arg "x"))

(global-set-key (kbd "<C-s-return>") 'org-agenda-show-custom-perspective)


;;----------------------------------------------------------------------
;; Dired
;;----------------------------------------------------------------------

;; a => Open in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;;
;; Webjump settings.
;; Quickly jump to websites from Emacs.
;;

;; (Command + Shift + Enter) - Webjump (Show list of websites that one can quickly jump to)
(global-set-key (kbd "s-g") 'webjump)



;;----------------------------------------------------------------------
;; External Packages
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; Setup use-package
;; Adapted from https://github.com/CachesToCaches/getting_started_with_use_package/blob/master/init-use-package.el
;;----------------------------------------------------------------------

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(require 'diminish)                ;; Since we use :diminish
(require 'bind-key)                ;; Since we use :bind and its variants

(add-to-list 'load-path "~/.emacs.d/custom-packages/")


;;----------------------------------------------------------------------
;; End setup for use-package
;;----------------------------------------------------------------------

;; Pick up executables from system PATH
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Ace window --- switch windows
(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window))

;; Aggressive indent
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'elixir-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haml-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'ruby-mode)
  )

;; Alchemist mode for Elixir
(use-package alchemist
  :ensure t
  :bind (:map alchemist-mode-map
              ("C-T" . alchemist-mix-test-stale)
              ("s-T" . alchemist-project-toggle-file-and-tests-other-window)))

;; Beacon --- Show little "animation" around cursor when switching
;; windows etc. to quickly indicate where the cursor is.
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

;; Comments
(use-package comment-dwim-2
  :ensure t
  :bind ("s-;" . comment-dwim-2))

;; ;; Company
;; (use-package company
;;   :ensure t
;;   :init
;;   (setq company-minimum-prefix-length 1
;;         company-selection-wrap-around t
;;         company-require-match nil
;;         company-dabbrev-downcase nil
;;         company-dabbrev-ignore-case nil
;;         company-idle-delay 0)
;;   :config
;;   (setq company-transformers '(company-sort-by-occurrence))
;;   (defun check-expansion ()
;;     (save-excursion
;;       (if (looking-at "\\_>") t
;;         (backward-char 1)
;;         (if (looking-at "\\.") t
;;           (backward-char 1)
;;           (if (looking-at "->") t nil)))))
;;   (defun do-yas-expand ()
;;     (let ((yas/fallback-behavior 'return-nil))
;;       (yas/expand)))
;;   (defun tab-indent-or-complete ()
;;     (interactive)
;;     (if (minibufferp)
;;         (minibuffer-complete)
;;       (if (or (not yas/minor-mode)
;;               (null (do-yas-expand)))
;;           (if (check-expansion)
;;               (company-complete-common-or-cycle)
;;             (indent-for-tab-command)))))
;;   ;; (global-set-key "\t" 'company-complete)
;;   ;; (global-set-key "\t" 'company-manual-begin)
;;   ;; (global-set-key "\t" 'company-select-next)
;;   (global-set-key "\t" 'tab-indent-or-complete)
;;   (global-company-mode))

;; Crux
(use-package crux
  :ensure t
  :bind (("s-d" . crux-duplicate-current-line-or-region)
         ("s-D" . crux-duplicate-and-comment-current-line-or-region)
         ("M-o" . crux-smart-open-line)
         ("s-o" . crux-smart-open-line-above)
         ;; ("s-j" . crux-top-join-line)
         ("C-j" . crux-top-join-line)
         ("s-<backspace>" . crux-kill-line-backwards)
         ("C-<backspace>" . crux-kill-line-backwards)))

;; Wrap text easily with delimiters (quotes etc.)
(use-package corral
  :ensure t
  :bind ("M-\"" . corral-double-quotes-forward))

(use-package css-mode
  :diminish aggressive-indent-mode
  :diminish helm-mode
  :init
  (setq css-indent-offset 2)
  :config
  (defun my-css-mode-hook ()
    (set-fill-column 72))
  (add-hook 'css-mode-hook 'my-css-mode-hook))

;; (use-package c++-mode
;;   :config
;;   (add-hook 'c++-mode-hook
;;             '(lambda ()
;;                (set-fill-column 72)
;;                (c-subword-mode +1)
;;                )))

;; Easy kill -- Better copy and paste
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;; ;; CTags
;; (use-package etags-select
;;   :ensure t
;;   :bind (("M-." . etags-select-find-tag-at-point)
;;          ;; ("s-." . etags-select-find-tag)
;;          ("M-," . pop-tag-mark)))       ; Jump back from tag found

;; Dash-at-point (Lookup in Dash.app)
(use-package dash-at-point
  :ensure t
  :bind ("s-u" . dash-at-point))

;; Deft -- A note-taking system like Notational Velocity. The following
;; configuration is largely adapted from
;; http://pragmaticemacs.com/emacs/make-quick-notes-with-deft/
(use-package deft
  :ensure t
  :diminish deft-mode
  :bind ("M-N" . deft)
  :init
  (setq deft-directory "/Users/debajita/Documents/org")
  (setq deft-extensions '("org" "txt" "md" "markdown"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0))

(use-package dumb-jump
  :ensure t
  :bind (
         ("s-." . dumb-jump-go)
         ("s-," . dumb-jump-back)
         )
  :init
  (dumb-jump-mode)
  )

;; Elixir mode
(use-package elixir-mode
  :ensure t)

;; Elixir snippets
(use-package elixir-yasnippets
  :ensure t)

(use-package elm-mode
  :ensure t)

;; Emacs Lisp mode
(use-package emacs-lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("s-r" . eval-buffer)
              ("s-R" . eval-region))
  )

(use-package emmet-mode
  :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("M-h" . er/mark-paragraph)))

(use-package fastnav
  :ensure t
  :bind* (("M-z" . fastnav-zap-up-to-char-forward)
          ("M-Z" . fastnav-zap-up-to-char-backward)))

;; Fill column indicator (Print margin — Enable for all files)
(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook 'fci-mode))

;; Spellcheck with flyspell
(use-package flyspell
  :diminish flyspell-mode
  :bind ("<s-return>" . flyspell-auto-correct-previous-word)
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;; FIXME:
;; ;; Flyspell with Credo
;; (use-package 'flycheck-credo
;;   :config
;;   (eval-after-load 'flycheck
;;     '(flycheck-credo-setup))
;;   (add-hook 'elixir-mode-hook 'flycheck-mode))

;; ;; Customize fringes
;; (use-package fringe-mode
;;   :config
;;   (fringe-mode 32 32))                  ; Make fringes wider for more comfortable reading

(use-package gitattributes-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package google-this
  :ensure t
  :bind ("s-G" . google-this-noconfirm))

;; HAML mode
(use-package haml-mode
  :ensure t)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("s-SPC" . helm-mini)        ; List buffers, like C-x b
         ("s-i" . helm-semantic-or-imenu)                ; Jump to method
         ("s-b" . helm-bookmarks))
  :init
  (setq helm-truncate-lines t)
  :config
  (helm-mode 1)
  (eval-after-load 'helm-mode '(diminish 'helm-mode)))

(use-package helm-git-grep
  :ensure t
  :bind ("s-F" . helm-git-grep-at-point))

(use-package helm-ls-git
  :ensure t
  :bind ("s-t" . helm-ls-git-ls))

(use-package htmlize
  :ensure t)

(use-package inf-ruby
  :ensure t)

;; Iedit --- easily rename variables + rectangle mode
(use-package iedit
  :ensure t
  :bind* ("C-;" . iedit-mode))

(use-package ivy
  :diminish ivy-mode
  :config
  (progn
    ;; Disable ido
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))))

(use-package helm-projectile
  :ensure t)

;; Highlight TODO, FIXME etc
(use-package hl-todo
  :ensure t
  :init
  (setq hl-todo-activate-in-modes
        '(prog-mode
          haml-mode
          javascript-mode
          ruby-mode
          typesript-mode
          yaml-mode))
  :config
  (global-hl-todo-mode))

;; FIXME: javascript-mode
;; ;; TODO: Add typesript mode
;; (use-package javascript-mode
;;   :init
;;   (setq js-indent-level 2)
;;   :config
;;   (defun my-javascript-mode-hook ()
;;     (set-fill-column 72))
;;   (add-hook 'javascript-mode-hook 'my-javascript-mode-hook))

(use-package jump-char
  :ensure t
  :bind (("M-j" . jump-char-forward)
         ("M-J" . jump-char-backward)))

;; Magit
(use-package magit
  :ensure t
  :bind (("<S-s-return>" . magit-status)
         ("s-L" . magit-log-buffer-file)))

;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :init
  (setq markdown-asymmetric-header t)
  (setq-default markdown-hide-markup t)
  :config
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (visual-line-mode)
               )))

;; Markdown mode
;; Use Octodown as Markdown parser
;; TODO: Organize this section
(defun markdown-mode-keyboard-shortcuts ()
  "Custom keys for Markdown mode."

  ;; Preview in browser: s-r, M-r
  (local-set-key (kbd "s-r") 'markdown-preview)
  (local-set-key (kbd "M-r") 'markdown-preview)

  ;; Super + B  =>  Bold
  (local-set-key (kbd "s-b") 'markdown-insert-bold)

  ;; Super + 1  =>  Insert heading 1
  (local-set-key (kbd "s-1") 'markdown-insert-header-atx-1)

  ;; Super + 2  =>  Insert heading 2
  (local-set-key (kbd "s-2") 'markdown-insert-header-atx-2)

  ;; Super + 3  =>  Insert heading 3
  (local-set-key (kbd "s-3") (lambda () (interactive) (markdown-insert-header-setext-dwim 3)))

  ;; Super + 4  =>  Insert heading 4
  (local-set-key (kbd "s-4") (lambda () (interactive) (markdown-insert-header-setext-dwim 4)))

  ;; Super + 5  =>  Insert heading 5
  (local-set-key (kbd "s-5") (lambda () (interactive) (markdown-insert-header-setext-dwim 5)))

  ;; Super + 6  =>  Insert heading 6
  (local-set-key (kbd "s-6") (lambda () (interactive) (markdown-insert-header-setext-dwim 6)))

  )
(add-hook 'markdown-mode-hook 'markdown-mode-keyboard-shortcuts)


;;
;; Mix format.
;; Code formatting for Elixir.
;;
(require 'mix-format)


;; Move text. Move a line of text up or down. Selection not required.
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))


;; Multiple cursors. Prefer to use iedit (even with M-{ or M-}) for
;; simple variable renames
(use-package multiple-cursors
  :ensure t
  :bind ("s-E" . mc/mark-next-word-like-this))


;; Org mode
(use-package org
  :ensure t
  :init
  (setq org-startup-indented t               ; Turn on org-indent-mode
        org-startup-folded nil               ; Start expanded
        org-cycle-separator-lines 0          ; Add newline between collapsed headers
        org-ellipsis "   ↩"                  ; Collapsed heading suffix
        org-startup-with-inline-images t     ; Show images inline
        org-export-with-section-numbers nil  ; TODO: Not working
        htmlize-output-type 'css
        org-html-htmlize-output-type 'css)

  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "WAITING-FOR" "WAITING_FOR_CUSTOMER" "CODE-REVIEW" "DEPLOYING" "WAITING_FOR_SCHEDULE" "BLOCKED" "|" "DONE" "HANDED OFF" "DELEGATED" "CANCELED")))

  ;; Org mode keyboard shortcuts
  :bind (:map org-mode-map
              ("s-1" . org-table-sort-lines)
              ("s-A" . org-archive-subtree))

  ;; Global keyboard shortcuts
  :bind (("M-S-SPC" . org-capture)
         ("C-S-SPC" . org-agenda))

  :config
  (custom-set-variables '(org-hide-emphasis-markers t)) ; Hide bold, italic markers
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     ;; (elixir . t)
     (R . t)
     (ruby . t)))

  (add-hook 'org-mode-hook
            '(lambda ()
               (auto-fill-mode)                     ; Hard wrap automatically
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

;; Predictive text completion (Predictive Abbreviation mode)
(use-package pabbrev
  :ensure t
  :init
  (setq pabbrev-idle-timer-verbose nil
        pabbrev-read-only-error nil
        pabbrev-scavenge-on-large-move nil)
  :config
  (put 'yas-expand 'pabbrev-expand-after-command t)
  (global-pabbrev-mode)
  ;; Fix for pabbrev not working in org mode
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2016-02/msg00311.html
  (define-key pabbrev-mode-map [tab] 'pabbrev-expand-maybe)
  (add-hook 'text-mode-hook (lambda () (pabbrev-mode))))

(use-package plantuml-mode
  :ensure t
  :mode (("\\.puml\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode)))

;; Projectile -- Project management
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind (("s-P" . helm-projectile-switch-project)
         ("C-o" . projectile-find-file-dwim))
  :config
  (projectile-global-mode +1))

(use-package quickrun
  :ensure t
  :bind (
         ("s-r" . quickrun)
         ("s-R" . quickrun-region)
         )
)

;; Rainbow mode. Colorise colour names in certain modes. (Taken from
;; https://github.com/bodil/ohai-emacs/blob/master/modules/ohai-html.el)
(use-package rainbow-mode
  :ensure t
  :config
  (dolist (mode '(css-mode less-css-mode html-mode plantuml-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (rainbow-mode))))
  :diminish rainbow-mode)

;; RTF mode
(autoload 'rtf-mode "rtf-mode" "RTF mode" t)
(add-to-list 'auto-mode-alist
             '("\\.rtf$" . rtf-mode))

;; Ruby mode
(use-package ruby-mode
  :config
  (defun my-ruby-mode-hook ()
    (set-fill-column 72))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

;; Ruby tools
;; Autocompletes string interpolations
(use-package ruby-tools
  :ensure t
  :diminish ruby-tools-mode)

(use-package scss-mode
  :ensure t
  :mode (("\\.scss$" . scss-mode)
         ("\\.scss.erb$" . scss-mode)))

;; Smart tab. Auto-complete text + expand snippets with Tab. Outside of
;; (pabbrev + yasnippet completion) and (dabbrev + yasnippet using
;; regular Tab) (see configuration above), this is likely the third best
;; option for configuring auto-complete.
;;
;; (use-package smart-tab
;;   :ensure t
;;   :init
;;   (cons 'yas/hippie-try-expand 'hippie-expand-try-functions-list)
;;   (setq smart-tab-using-hippie-expand t)
;;   :config
;;   (global-smart-tab-mode +1))

;; Sort words in a line
(use-package sort-words
  :ensure t)

(use-package sql-mode
  :bind (:map sql-mode-map
              ("<s-return>" . sql-send-paragraph)))

;; Subword mode. This package is configured somewhat differently from
;; others. Enabling subword-mode in :config does not work with diminish.
;; The following, however appears to work. See also
;; https://github.com/fbergroth/.emacs.d/blob/master/init.el
;;
;; TODO: See if this is slowing down Emacs startup somewhat.
(use-package subword
  :init (global-subword-mode)
  :diminish subword-mode)

;; Swiper - A better helm-swoop (for incremental search)
;; http://oremacs.com/2015/03/10/no-swiping/
(use-package swiper
  :ensure t
  :bind ("s-f" . swiper))

;; TypeScript
(use-package typescript-mode
  :ensure t
  :init
  (setq typescript-indent-level 2))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo))
  :config
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package unfill
  :ensure t
  :bind ("M-Q" . unfill-paragraph))

;; VLF (Very large files) support
(use-package vlf
  :ensure t
  :config
  (custom-set-variables '(vlf-application 'dont-ask)))

;;
;; Web mode. web-mode is a special mode for HTML which copes with
;; embedded JS/CSS, JSX, various templating systems, etc.
;;
;; see http://web-mode.org/
;;
(use-package web-mode
  :ensure t
  :mode (;; We'd like to use web-mode for HTML, instead of the default html-mode.
         ("\\.html?\\'" . web-mode)
         ;; Let's add some extensions from the web-mode docs too.
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :bind (:map web-mode-map
              ("s-r" . browse-url-of-buffer)
              ("C-P" . web-mode-element-previous)
              ("C-N" . web-mode-element-next)
              ("C-R" . web-mode-element-rename)
              ("C-U" . web-mode-element-parent)
              ("C-L" . web-mode-element-end)
              ("C-D" . web-mode-element-clone)
              ("C-S-SPC" . web-mode-element-content-select)
              ("C-W" . web-mode-element-wrap))
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode."

    (setq web-mode-markup-indent-offset 2               ; Indent with two spaces
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-style-padding 2                      ; Padding on the left of inline style blocks
          web-mode-script-padding 2                     ; Padding on the left of inline script blocks
          web-mode-enable-current-element-highlight t)) ; Highlight the element under the cursor.

  (add-hook 'web-mode-hook 'my-web-mode-hook))

;; ;; Whitespace mode
;; ;; https://www.emacswiki.org/emacs/WhiteSpace
;; (use-package whitespace
;;   :diminish whitespace-mode
;;   :init
;;   ;; (dolist (hook '(prog-mode-hook text-mode-hook))
;;   (dolist (hook '(prog-mode-hook))
;;     (add-hook hook #'whitespace-mode))
;;   (add-hook 'before-save-hook #'whitespace-cleanup)
;;   :config
;;   (setq whitespace-line-column 72) ;; limit line length
;;   (setq whitespace-style '(face tabs empty trailing)))

(use-package yaml-mode
  :ensure t)

;; Yard mode
(use-package yard-mode
  :ensure t
  :diminish yard-mode
  :config
  (add-hook 'ruby-mode-hook 'yard-mode))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'fundamental-mode))))


;;----------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------

(use-package birds-of-paradise-plus-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t)

(if (display-graphic-p)
    ;; (load-theme 'apus t)
    ;; (load-theme 'gruvbox t)
    (load-theme 'two-firewatch-light t)
  (load-theme 'stygian t))
