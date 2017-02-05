(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
	     t)
(package-initialize)


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
    ("957e1b72c5a39765c152d5967258463efde7387f177fcb72b5a8615b6d4b916a" "d55df21873ba9058e1bd85e6142c52f4c3b054cc9b3728e99e1e99ea3bcb3728" "e9b6fc5677c8e7f13fc681758cd6b0765ceaecdd7b706da156c30383852f7387" "3c837ab6df2be9c7c96bf0cc4c9cc71b2389343a68b23eb830f532639a170ae0" "bffba7a258ddd175fd85389ad2f472afe5cba8bfb9f5b723ae0c34ce290a3c09" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" default)))
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#452E2E")
 '(fringe-mode nil nil (fringe))
 '(helm-mode t)
 '(indicate-empty-lines t)
 '(line-number-mode nil)
 '(mac-command-modifier (quote super))
 '(mac-option-modifier (quote meta))
 '(mac-right-option-modifier nil)
 '(markdown-command "/Users/debajita/.rbenv/shims/octodown --raw")
 '(package-selected-packages
   (quote
    (elixir-yasnippets dash-at-point dash yard-mode helm-ls-git comment-dwim-2 markdown-mode helm-google haml-mode birds-of-paradise-plus-theme helm-git-files helm helm-git-grep yasnippet easy-kill))))


;;----------------------------------------------------------------------
;; Emacs commands
;;----------------------------------------------------------------------

(global-set-key (kbd "M-x") 'helm-M-x)


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
;; Modes
;;----------------------------------------------------------------------

;; Desktop mode
;; Always save and restore the open buffers
(desktop-save-mode 1)

;; Toolbar off
(tool-bar-mode 0)

;; Winner mode
(winner-mode 1)


;; Emacs Lisp mode

(defun emacs-lisp-mode-keyboard-shortcuts ()
  "Custom keys for Emacs Lisp mode."

  ;; Eval-buffer
  (local-set-key (kbd "s-r") 'eval-buffer)
  )
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-keyboard-shortcuts)


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

;; Ruby mode

(add-hook 'ruby-mode-hook
          (lambda ()
            (set-fill-column 72)
            (yard-mode)))


;;----------------------------------------------------------------------
;; Keyboard shortcuts
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; Documentation
;;----------------------------------------------------------------------

(global-set-key (kbd "s-e") 'dash-at-point)


;;----------------------------------------------------------------------
;; File operations
;;----------------------------------------------------------------------

;; Open file: s-t   (like TextMate)
(global-set-key (kbd "s-t") 'helm-ls-git-ls)

;; Open file: C-x C-f   (rebind the default to Helm)
(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; Save file - (Super + s)
(global-set-key (kbd "s-s") 'save-buffer)


;;----------------------------------------------------------------------
;; Searching
;;----------------------------------------------------------------------

;; Find: s-f   (default: C-s)
(global-set-key (kbd "s-f") 'helm-occur)

;; Find: s-F  
(global-set-key (kbd "s-F") 'helm-git-grep-at-point)


;;----------------------------------------------------------------------
;; Text editing
;;----------------------------------------------------------------------

;; Autocomplete: s-/   (default: M-/)
(global-set-key (kbd "s-/") 'hippie-expand)

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

;; Comment: s-;   (default: M-;)
(global-set-key (kbd "s-;") 'comment-dwim-2)

;; Delete line: s-k   (default: C-k)
(global-set-key (kbd "s-k") 'kill-whole-line)

;;----------------------------------------------------------------------
;; Window management
;; Emacs calls windows "frames"
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

;; Buffers: super-shift-space   (as in "list bUffers, default: C-x b)
(global-set-key (kbd "s-SPC") 'helm-buffers-list)

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

;; Undo window layout: s-y
(global-set-key (kbd "s-y") 'winner-undo)

;; Undo window layout: s-Y
(global-set-key (kbd "s-Y") 'winner-redo)


;;----------------------------------------------------------------------
;; View
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
