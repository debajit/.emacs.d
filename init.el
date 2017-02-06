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
(line-number-mode t)			; Show line number
(column-number-mode t)			; Show column number

;; Highlight matching parentheses
(setq show-paren-delay 0)		; Highlight instantly, no delay
(show-paren-mode 1)

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Desktop mode (Always save and restore the open buffers)
(desktop-save-mode 1)

;; Winner mode (undo and redo window layouts)
(winner-mode 1)
(global-set-key (kbd "s-y") 'winner-undo)
(global-set-key (kbd "s-Y") 'winner-redo)

;; Line number mode. Not enabled currently. To make this look good, set
;; the fringe color to the window background color.
(setq linum-format " %d  ")


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

;; Join line
(global-set-key (kbd "s-j") 'join-line)

;; Add a newline at end of file
(setq require-final-newline t)

;; Replace selection with a single keystroke
(delete-selection-mode t)


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

;; Comments
(use-package comment-dwim-2
  :ensure t
  :bind ("s-;" . comment-dwim-2))

;; CTags
(use-package etags-select
  :ensure t
  :bind (("s-." . etags-select-find-tag-at-point)
	 ("s->" . etags-select-find-tag)
	 ("M-." . pop-tag-mark)))	; Jump back from tag found

;; Dash-at-point (Lookup in Dash.app)
(use-package dash-at-point
  :ensure t
  :bind ("s-e" . dash-at-point))

;; Elixir snippets
(use-package elixir-yasnippets
    :ensure t)

;; Emacs Lisp mode
(use-package emacs-lisp-mode
  :bind ("s-r" . eval-buffer))

;; HAML mode
(use-package haml-mode)

;; Helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("s-SPC" . helm-buffers-list)	; List buffers, like C-x b
	 ("s-f" . helm-occur)		; Find, like C-s
	 ("s-i" . helm-semantic-or-imenu)) ; Jump to method
  :config
  (helm-mode 1))
	
(use-package helm-git-grep
  :ensure t
  :bind ("s-F" . helm-git-grep-at-point))

(use-package helm-ls-git
  :bind ("s-t" . helm-ls-git-ls))	; Open file, like TextMate

;; Markdown Mode
(use-package markdown-mode)

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"])
 '(custom-safe-themes
   (quote
    ("e84539eede64a272c84f3175f3e0aa7d09507a1dd0e85ca46725865dca779e1c" "957e1b72c5a39765c152d5967258463efde7387f177fcb72b5a8615b6d4b916a" "d55df21873ba9058e1bd85e6142c52f4c3b054cc9b3728e99e1e99ea3bcb3728" "e9b6fc5677c8e7f13fc681758cd6b0765ceaecdd7b706da156c30383852f7387" "3c837ab6df2be9c7c96bf0cc4c9cc71b2389343a68b23eb830f532639a170ae0" "bffba7a258ddd175fd85389ad2f472afe5cba8bfb9f5b723ae0c34ce290a3c09" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" default)))
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#452E2E")
 '(fringe-mode nil nil (fringe))
 '(mac-command-modifier (quote super))
 '(mac-option-modifier (quote meta))
 '(mac-right-option-modifier nil)
 '(markdown-command "/Users/debajita/.rbenv/shims/octodown --raw")
 '(package-selected-packages
   (quote
    (fill-column-indicator helm-google birds-of-paradise-plus-theme easy-kill))))

;;----------------------------------------------------------------------
;; Modes
;;----------------------------------------------------------------------

;; Fill column indicator
;; Print margin — Enable for all files
(add-hook 'after-change-major-mode-hook 'fci-mode)

;; Markdown mode

;; Use Octodown as Markdown parser

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

;;----------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------

(if (display-graphic-p)
    (load-theme 'apus t)
  (load-theme 'stygian t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold :height 1.5 :family "Verlag")))))
