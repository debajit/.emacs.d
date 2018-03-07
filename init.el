;; Save customizations in a separate file (custom.el)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror) ; Prevent errors if custom.el does not exist

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

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Desktop mode (Always save and restore the open buffers)
(desktop-save-mode 1)

;; Highlight the current line
(global-hl-line-mode +1)

;; Winner mode (undo and redo window layouts)
(winner-mode 1)
(global-set-key (kbd "s-y") 'winner-undo)
(global-set-key (kbd "s-Y") 'winner-redo)

;; Line number mode. Not enabled currently. To make this look good,
;; set the fringe color to the window background color.
(setq linum-format " %4d ")

;; Whitespace
(setq require-final-newline t)          ; Add a newline at end of file

;; Narrowing and widening
(put 'narrow-to-region 'disabled nil)   ; Enable narrowing (disabled by default)

;; Show images by default
(setq auto-image-file-mode t)


;;----------------------------------------------------------------------
;; Indentation
;;----------------------------------------------------------------------

;; Use spaces not tabs
(setq-default indent-tabs-mode nil)   ;; Don't use tabs to indent
(setq-default tab-width 4)            ;; Ensure tabs are aligned well

;; C++, C indentation
(setq c-default-style "linux" ; Microsoft-style --- with { on new line
      c-basic-offset 4)       ; Indent 4 spaces


;;----------------------------------------------------------------------
;; Autocomplete
;;----------------------------------------------------------------------

;; Basic autocomplete
;; Configure hippie-expand

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Autocomplete with Tab (Yas + dabbrev)
;;
;; Configure the Tab key to autocomplete / indent / expand-snippet
;; depending on the position of the cursor. See
;; http://stackoverflow.com/questions/13576156/emacs-smart-tab-with-yasnippets
;;
;; Auto complete settings / tab settings
;; http://emacsblog.org/2007/03/12/tab-completion-everywhere/ <-- in the comments

;; (global-set-key [(tab)] 'smart-tab)
;; (defun smart-tab ()
;;   "This smart tab is minibuffer compliant: it acts as usual in
;;     the minibuffer. Else, if mark is active, indents region. Else if
;;     point is at the end of a symbol, expands it. Else indents the
;;     current line."
;;   (interactive)
;;   (if (minibufferp)
;;       (unless (minibuffer-complete)
;;         (dabbrev-expand nil))
;;     (if mark-active
;;         (indent-region (region-beginning)
;;                        (region-end))
;;       (if (looking-at "\\_>")
;;           (let ((yas/fallback-behavior nil))
;;             (unless (yas/expand)
;;               (dabbrev-expand nil)))
;;         (indent-for-tab-command)))))


;; For Pabbrev mode see below, in the third-party packages section


;;----------------------------------------------------------------------
;; General keyboard shortcuts
;;----------------------------------------------------------------------

;; Save file: s-s
(global-set-key (kbd "s-s") 'save-buffer)


;; Cut: s-x   (default: C-w)
(global-set-key (kbd "s-x") 'kill-region)

;; Copy: s-c   (default: M-w)
(global-set-key (kbd "s-c") 'kill-ring-save)

;; Paste: s-v   (default: C-y)
(global-set-key (kbd "s-v") 'yank)

;; Select all: s-a   (default: C-x h)
(global-set-key (kbd "s-a") 'mark-whole-buffer)


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

;; Indent rigidly to tab stop (a la TextMate or Sublime Text)
(global-set-key (kbd "s-]") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-[") 'indent-rigidly-left-to-tab-stop)

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
(global-set-key (kbd "s-,") 'point-to-register)

;; Jump to saved resigter
(global-set-key (kbd "s-.") 'jump-to-register)


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

(global-set-key (kbd "s-u") 'narrow-to-defun)


(global-set-key (kbd "s-3") 'highlight-symbol-at-point)
(global-set-key (kbd "s-4") 'unhighlight-regexp)

;;----------------------------------------------------------------------
;; Window management keys.
;; Windows => Emacs' frames
;; Panes   => Emacs' windows
;;----------------------------------------------------------------------

;; New window: s-n   (default: C-x 5 2)
(global-set-key (kbd "s-n") (lambda () (interactive) (find-file-other-frame "/tmp/scratch")))

;; Close buffer: s-w
(global-set-key (kbd "s-w") (lambda () (interactive) (kill-buffer (current-buffer))))

;; Close pane: s-W   (default: C-x 0)
(global-set-key (kbd "s-W") (lambda () (interactive) (delete-window) (balance-windows)))

;; Close window (Emacs' frame): s-q
(global-set-key (kbd "s-q") 'delete-frame)

;; Maximize window: s-m   (default: C-x 1)
(global-set-key (kbd "s-m") 'delete-other-windows)

;; Minimize window: s-M   (default: C-z)
(global-set-key (kbd "s-M") 'suspend-frame)

;; Split horizontally: s-J   (default: C-x 3)
(global-set-key (kbd "s-J") (lambda () (interactive) (split-window-right) (windmove-right)))

;; Split horizontally: s-K   (default: C-x 2)
(global-set-key (kbd "s-K") (lambda () (interactive) (split-window-below) (windmove-down)))

;; Move to window on left
(global-set-key (kbd "s-U") 'windmove-left)

;; Move to window on right
(global-set-key (kbd "s-I") 'windmove-right)

;; Move to window below
(global-set-key (kbd "s-P") 'windmove-down)

;; Balance windows
(global-set-key (kbd "s-:") 'balance-windows)

;; Increase window size: s-]
(global-set-key (kbd "M-]") (lambda () (interactive) (enlarge-window-horizontally 20)))

;; Decrease window size: s-[
(global-set-key (kbd "M-[") (lambda () (interactive) (shrink-window-horizontally 20)))


;;--------------------------------------------------------------------
;; Getting Things Done.
;;
;; See also
;; - http://members.optusnet.com.au/~charles57/GTD/datetree.html
;;--------------------------------------------------------------------

;; Adapted from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-agenda-files '("~/Documents/gtd/inbox.org"
                         "~/Documents/gtd/projects.org"
                         "~/Documents/gtd/journal.org"
                         "~/Documents/gtd/tickler.org"))

(setq org-refile-targets '(("~/Documents/gtd/projects.org" :maxlevel . 3)
                           ("~/Documents/gtd/someday.org" :level . 1)
                           ("~/Documents/gtd/tickler.org" :maxlevel . 2)))

(setq org-capture-templates '(

                              ;; Capture task to Inbox.
                              ;;
                              ;; You can refile directly from this
                              ;; screen using C-c C-w and assign the
                              ;; task to a project.
                              ;;
                              ("t" "Todo [inbox]" entry
                               (file+headline "/Users/debajita/Documents/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")

                              ("T" "Tickler" entry
                               (file+headline "/Users/debajita/Documents/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")

                              ;; Add Journal entry.
                              ;;
                              ;; Taken from
                              ;; http://www.howardism.org/Technical/Emacs/journaling-org.html
                              ;;
                              ("j" "Journal Entry"
                               entry (file+datetree "~/Documents/gtd/journal.org")
                               "* %?")

                              ))


;;----------------------------------------------------------------------
;; Typography
;;----------------------------------------------------------------------

;;
;; Typefaces
;;
;; Currently using Consolas for code, and Ideal Sans for longform text.
;;
(when (display-graphic-p)
  (when (member "Consolas" (font-family-list))
    (set-face-font 'default "Consolas-16")
    (copy-face 'default 'fixed-pitch))
  (when (member "Ideal Sans" (font-family-list))
    (set-face-font 'variable-pitch "Ideal Sans-18")))

;;
;; Variable-width font settings.
;;
;; Largely adapted from
;; http://www.xiangji.me/2015/07/13/a-few-of-my-org-mode-customizations/
;;
(defun set-buffer-variable-pitch ()
  (interactive)
  (variable-pitch-mode t)
  (setq line-spacing 3)
  (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch))

(add-hook 'org-mode-hook 'set-buffer-variable-pitch)
;; (add-hook 'org-agenda-mode-hook 'set-buffer-variable-pitch)
(add-hook 'eww-mode-hook 'set-buffer-variable-pitch)
(add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
(add-hook 'Info-mode-hook 'set-buffer-variable-pitch)


;;
;; Webjump settings.
;; Quickly jump to websites from Emacs.
;;

;; (Command + Shift + Enter) - Webjump (Show list of websites that one can quickly jump to)
(global-set-key (kbd "s-g") 'webjump)

(eval-after-load "webjump"
  '(setq webjump-sites
         (append '(("Elixir Formatter" . "https://elixirformatter.com/")
                   ("GitHub — Debajit — .emacs.d" . "https://github.com/debajit/.emacs.d")
                   ("Urban Dictionary" . [simple-query "www.urbandictionary.com" "http://www.urbandictionary.com/define.php?term=" ""]))
                 webjump-sample-sites)))

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

(use-package c++-mode
  :config
  (add-hook 'c++-mode-hook
            '(lambda ()
               (set-fill-column 72)
               (c-subword-mode +1)
               )))

;; Easy kill -- Better copy and paste
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;; CTags
(use-package etags-select
  :ensure t
  :bind (("M-." . etags-select-find-tag-at-point)
         ;; ("s-." . etags-select-find-tag)
         ("M-," . pop-tag-mark)))       ; Jump back from tag found

;; Dash-at-point (Lookup in Dash.app)
(use-package dash-at-point
  :ensure t
  :bind ("s-e" . dash-at-point))

;; Deft -- A note-taking system like Notational Velocity. The following
;; configuration is largely adapted from
;; http://pragmaticemacs.com/emacs/make-quick-notes-with-deft/
(use-package deft
  :ensure t
  :diminish deft-mode
  :bind ("<f6>" . deft)
  :init
  (setq deft-directory "/Users/debajita/Documents/org")
  (setq deft-extensions '("org" "txt" "md" "markdown"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0))

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
  :bind ("s-r" . eval-buffer))

(use-package emmet-mode
  :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("M-h" . er/mark-paragraph)))

(use-package fastnav
  :ensure t
  :bind (("M-j" . avy-goto-char-in-line)
         ("M-J" . avy-goto-char))
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

;; Flyspell with Credo
(use-package 'flycheck-credo
  :config
  (eval-after-load 'flycheck
    '(flycheck-credo-setup))
  (add-hook 'elixir-mode-hook 'flycheck-mode))

;; Customize fringes
(use-package fringe-mode
  :config
  (fringe-mode 32 32))                  ; Make fringes wider for more comfortable reading

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
         ("s-SPC" . helm-mini)          ; List buffers, like C-x b
         ("s-i" . helm-semantic-or-imenu) ; Jump to method
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

;; TODO: Add typesript mode
(use-package javascript-mode
  :init
  (setq js-indent-level 2)
  :config
  (defun my-javascript-mode-hook ()
    (set-fill-column 72))
  (add-hook 'javascript-mode-hook 'my-javascript-mode-hook))

;; Line numbers of left of buffer
(use-package linum-mode
  :bind ("<f8>" . linum-mode))


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


;; Multiple cursors. Prefer to use iedit (even with M-{ or M-}) for
;; simple variable renames
(use-package multiple-cursors
  :ensure t
  :bind ("s-E" . mc/mark-next-word-like-this))

;; ;; NeoTree file tree browser
;; (use-package neotree
;;   :ensure t
;;   :init
;;   (setq-default neo-smart-open t             ; Open file browser in current directory.
;;                 neo-window-fixed-size nil)   ; Make file browser resizeable
;;   ;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;   :bind ("<f7>" . neotree-toggle))

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
        '((sequence "TODO" "IN PROGRESS" "WAITING_FOR_CUSTOMER" "CODE-REVIEW" "DEPLOYING" "WAITING_FOR_SCHEDULE" "BLOCKED" "|" "✔ DONE" "DELEGATED" "CANCELED")))

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
  :bind ("s-P" . projectile-switch-project)
  :config
  (projectile-global-mode +1))

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

;; Smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (defun clear-text-inside-pairs ()
    "Clears all text inside a string or inside parentheses, or
other matching pairs"
    (interactive)
    (sp-backward-up-sexp)
    (sp-mark-sexp)
    (sp-kill-region (+ (region-beginning) 1) (- (region-end) 1))
    (forward-char))
  (global-set-key (kbd "s-j") 'clear-text-inside-pairs))


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

;; Spaceline

;; (use-package spaceline :ensure t
;;   :config
;;   (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

;; (use-package spaceline-config :ensure spaceline
;;   :config
;;   (spaceline-helm-mode 1)
;;   (spaceline-emacs-theme))


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

;; Treemacs --- A file tree visualizer like NeoTree but much better.
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  :bind ("<f7>" . treemacs-toggle)
  :config
  (treemacs-follow-mode t)
  (defun treemacs-header-with-brackets (current-root)
    (format "%s" (file-name-nondirectory current-root)))
  (setq treemacs-header-function #'treemacs-header-with-brackets))

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

(use-package visual-line
  :bind ("<f5>" . visual-line-mode))

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

;; Whitespace mode
;; https://www.emacswiki.org/emacs/WhiteSpace
(use-package whitespace
  :diminish whitespace-mode
  :init
  ;; (dolist (hook '(prog-mode-hook text-mode-hook))
  (dolist (hook '(prog-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 72) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing)))

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
