;; Save customizations in a separate file (custom.el)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Package setup
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)
(package-initialize)

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

;; Highlight matching parentheses
(setq show-paren-delay 0)               ; Highlight instantly, no delay
(show-paren-mode 1)

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

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

;; Line number mode. Not enabled currently. To make this look good, set
;; the fringe color to the window background color.
(setq linum-format " %d  ")

;; Use spaces not tabs
(setq-default indent-tabs-mode nil)   ;; Don't use tabs to indent
(setq-default tab-width 4)            ;; Ensure tabs are aligned well

;; Whitespace
(setq require-final-newline t)          ; Add a newline at end of file


;;----------------------------------------------------------------------
;; General keyboard shortcuts
;;----------------------------------------------------------------------

;; Save file: s-s
(global-set-key (kbd "s-s") 'save-buffer)

;; Autocomplete: s-/   (default: M-/)
(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Undo: s-z   (default: C-/)
(global-set-key (kbd "s-z") 'undo)

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
(global-set-key (kbd "s-p") 'toggle-truncate-lines)


;;----------------------------------------------------------------------
;; Text editing
;;----------------------------------------------------------------------

;; Delete line: s-k   (default: C-k)
(global-set-key (kbd "s-k") 'kill-whole-line)

;; Replace selection with a single keystroke
(delete-selection-mode t)


;;----------------------------------------------------------------------
;; Mouse shortcuts
;;----------------------------------------------------------------------

;; Open link at mouse pointer: s-click
(global-set-key [s-mouse-1] 'browse-url-at-mouse)


;;----------------------------------------------------------------------
;; Window management keys.
;; Windows => Emacs' frames
;; Panes   => Emacs' windows
;;----------------------------------------------------------------------

;; New window: s-n   (default: C-x 5 2)
(global-set-key (kbd "s-n") (lambda () (interactive) (find-file-other-frame "/tmp/scratch")))

;; Close buffer: s-w
(global-set-key (kbd "s-w") 'kill-this-buffer)

;; Close pane: s-W   (default: C-x 0)
(global-set-key (kbd "s-W") (lambda () (interactive) (delete-window) (balance-windows)))

;; Close window (Emacs' frame): s-q
(global-set-key (kbd "s-q") 'delete-frame)

;; Maximize window: s-m   (default: C-x 1)
(global-set-key (kbd "s-m") 'delete-other-windows)

;; Split horizontally: s-J   (default: C-x 3)
(global-set-key (kbd "s-J") (lambda () (interactive) (split-window-right) (windmove-right)))

;; Split horizontally: s-K   (default: C-x 2)
(global-set-key (kbd "s-K") (lambda () (interactive) (split-window-below) (windmove-down)))

;; Move to window on left
(global-set-key (kbd "s-U") 'windmove-left)

;; Move to window on right
(global-set-key (kbd "s-I") 'windmove-right)

;; Move to window above
(global-set-key (kbd "s-O") 'windmove-up)

;; Move to window below
(global-set-key (kbd "s-P") 'windmove-down)

;; Balance windows
(global-set-key (kbd "s-:") 'balance-windows)

;; Increase window size: s-]
(global-set-key (kbd "s-]") (lambda () (interactive) (enlarge-window-horizontally 20)))

;; Decrease window size: s-[
(global-set-key (kbd "s-[") (lambda () (interactive) (shrink-window-horizontally 20)))


;;----------------------------------------------------------------------
;; Fonts
;;----------------------------------------------------------------------

(when (display-graphic-p)
  (when (member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas-16")))
    ;; (set-face-attribute 'default nil :font "M+ 1m-18")))
    ;; (set-face-attribute 'default nil :font "Monaco-16")))
    ;; (set-face-attribute 'default nil :font "Menlo-16")))
    ;; (set-face-attribute 'default nil :font "Operator Mono-18")))

;;----------------------------------------------------------------------
;; External Packages
;;----------------------------------------------------------------------

;; Setup use-package first. We will use this for everything else.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Aggressive indent
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'haml-mode))

;; Comments
(use-package comment-dwim-2
  :ensure t
  :bind ("s-;" . comment-dwim-2))

;; Company
(use-package company
  :ensure t)

;; Crux
(use-package crux
  :ensure t
  :bind (("s-d" . crux-duplicate-current-line-or-region)
         ("M-o" . crux-smart-open-line)
         ("s-o" . crux-smart-open-line-above)
         ("s-j" . crux-top-join-line)
         ("C-j" . crux-top-join-line)
         ("s-<backspace>" . crux-kill-line-backwards)
         ("C-<backspace>" . crux-kill-line-backwards)))

(use-package css-mode
  :diminish aggressive-indent-mode
  :diminish helm-mode
  :init
  (setq css-indent-offset 2)
  :config
  (defun my-css-mode-hook ()
    (set-fill-column 72)
    (subword-mode))
  (add-hook 'css-mode-hook 'my-css-mode-hook))

;; CTags
(use-package etags-select
  :ensure t
  :bind (("s-." . etags-select-find-tag-at-point)
         ("s->" . etags-select-find-tag)
         ("M-." . pop-tag-mark)))       ; Jump back from tag found

;; Dash-at-point (Lookup in Dash.app)
(use-package dash-at-point
  :ensure t
  :bind ("s-e" . dash-at-point))

;; Elixir mode
(use-package elixir-mode
    :ensure t)

;; Elixir snippets
(use-package elixir-yasnippets
    :ensure t)

;; Emacs Lisp mode
(use-package emacs-lisp-mode
  :bind ("s-r" . eval-buffer))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Fill column indicator (Print margin — Enable for all files)
(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook 'fci-mode))

;; HAML mode
(use-package haml-mode
  :ensure t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("s-SPC" . helm-mini)          ; List buffers, like C-x b
         ("s-f" . helm-occur)           ; Find, like C-s
         ("s-i" . helm-semantic-or-imenu) ; Jump to method
         ("s-b" . helm-bookmarks))
  :config
  (helm-mode 1))

(use-package helm-git-grep
  :ensure t
  :bind ("s-F" . helm-git-grep-at-point))

(use-package helm-ls-git
  :ensure t
  :bind ("s-t" . helm-ls-git-ls))       ; Open file, like TextMate

(use-package ido-mode
  :bind ("C-x C-f" . ido-find-file))

(use-package javascript-mode
  :diminish aggressive-indent-mode
  :init
  (setq js-indent-level 2)
  :config
  (defun my-javascript-mode-hook ()
    (set-fill-column 72)
    (subword-mode))
  (add-hook 'javascript-mode-hook 'my-javascript-mode-hook))

;; Markdown Mode
(use-package markdown-mode
  :ensure t)

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
  (local-set-key (kbd "s-1") 'markdown-insert-header-setext-1)

  ;; Super + 2  =>  Insert heading 2
  (local-set-key (kbd "s-2") 'markdown-insert-header-setext-2)

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

;; NeoTree file tree browser
(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle))

;; Rainbow mode
(use-package rainbow-mode
  :ensure t)

;; Ruby mode
(use-package ruby-mode
  ;; :diminish helm-mode
  ;; :diminish subword-mode
  :config
  (defun my-ruby-mode-hook ()
    (set-fill-column 72)
    (subword-mode))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

;; Smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode 1))

;; Subword mode
(use-package subword-mode
  :diminish subword-mode)

;; VLF (Very large files) support
(use-package vlf
  :ensure t
  :config
  (custom-set-variables '(vlf-application 'dont-ask)))

;; Whitespace mode
(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 72) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; Yard mode
(use-package yard-mode
  :ensure t
  :diminish yard-mode
  :config
  (add-hook 'ruby-mode-hook 'yard-mode))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;;----------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------

(use-package birds-of-paradise-plus-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t)

(if (display-graphic-p)
    ;; (load-theme 'apus t)
    (load-theme 'gruvbox t)
  (load-theme 'stygian t))
