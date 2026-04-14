(require 'goto-addr)

(defvar zoom-chat-font-lock-keywords
  '(("^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9:]\\{8\\}\\) From \\(.*?\\) to \\(.*\\):$"
     (1 font-lock-constant-face)
     (2 bold)
     (3 font-lock-keyword-face))
    ("^\\s-*Replying to \".*\":$" . font-lock-comment-face)
    ("^\\s-*.*:\\(?:👍\\|🙏🏼\\|🙂\\|🙏\\|✅\\|❗\\|🔥\\|🎉\\)$" . font-lock-builtin-face)
    ("https?://[^][\\\"'() \\t\\n]+" . link))
  "Font-lock keywords for Zoom meeting chat transcripts.")

(define-derived-mode zoom-chat-mode text-mode "Zoom-Chat"
  "Major mode for Zoom meeting chat transcript exports."
  (setq-local font-lock-defaults '(zoom-chat-font-lock-keywords))
  (visual-line-mode 1)
  (goto-address-mode 1)
  (view-mode 1))

(add-to-list 'auto-mode-alist '("meeting_saved_new_chat\\.txt\\'" . zoom-chat-mode))

(provide 'zoom-chat)
