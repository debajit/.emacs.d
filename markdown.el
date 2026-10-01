;;; markdown.el --- Markdown editing and HTML export -*- lexical-binding: t; -*-

;; Install the external renderer (requires Rust and Cargo):
;;
;;   cargo install markdown2html-converter
;;
;; Cargo normally installs the executable in ~/.cargo/bin.  Confirm that it is
;; available with `markdown2html-converter --version`.

(defun my/markdown2html-document-title-p (begin end)
  "Return non-nil when Markdown between BEGIN and END supplies a title."
  (or (save-excursion
        (goto-char begin)
        (re-search-forward "^#[[:blank:]]+[^[:blank:]\n]" end t))
      (save-excursion
        (goto-char begin)
        (and (looking-at "---[[:blank:]]*$")
             (forward-line 1)
             (let ((front-matter-end
                    (save-excursion
                      (re-search-forward "^---[[:blank:]]*$" end t))))
               (and front-matter-end
                    (re-search-forward
                     "^title:[[:blank:]]*[^[:blank:]\n]"
                     front-matter-end t)))))))

(defun my/markdown2html-convert-region (begin end output-buffer)
  "Render Markdown between BEGIN and END into OUTPUT-BUFFER.

Use markdown2html-converter to produce standalone, styled HTML.  Local images
are resolved relative to the Markdown file and embedded in the result, so both
preview and exported files remain portable."
  (let* ((cargo-program
          (expand-file-name ".cargo/bin/markdown2html-converter"))
         (program
          (or (executable-find "markdown2html-converter")
              (and (file-executable-p cargo-program) cargo-program)
              (user-error "markdown2html-converter is not installed")))
         (source-directory
          (file-name-directory (or buffer-file-name
                                   (expand-file-name (buffer-name)
                                                     default-directory))))
         (title (if buffer-file-name
                    (file-name-base buffer-file-name)
                  (buffer-name)))
         (arguments
          (append
           (list "-" "--output" "-"
                 "--theme" "auto"
                 "--embed-images"
                 "--base-path" source-directory
                 "--math-mode" "katex-embedded"
                 "--mermaid-mode" "embedded"
                 "--no-cjk-fonts")
           (unless (my/markdown2html-document-title-p begin end)
             (list "--title" title))))
         (status (apply #'call-process-region
                        begin end program nil (list output-buffer nil) nil
                        arguments)))
    (unless (zerop status)
      (user-error "markdown2html-converter failed with exit code %s" status))))

(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown$" . gfm-mode)
         ("\\.md$" . gfm-mode))
  :init
  (setq markdown-asymmetric-header t
        markdown-command #'my/markdown2html-convert-region
        markdown-command-needs-filename nil
        markdown-gfm-downcase-languages t)
  (setq-default markdown-hide-markup t)
  :bind ("M-M" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'markdown-display-inline-images))

(defun markdown-mode-keyboard-shortcuts ()
  "Set personal keyboard shortcuts in Markdown buffers."
  (local-set-key (kbd "s-r") #'markdown-preview)
  (local-set-key (kbd "M-r") #'markdown-preview)
  (local-set-key (kbd "s-b") #'markdown-insert-bold))

(add-hook 'markdown-mode-hook #'markdown-mode-keyboard-shortcuts)
