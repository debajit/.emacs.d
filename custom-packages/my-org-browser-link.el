;;; my-org-browser-link.el --- Insert browser pages through Org Protocol -*- lexical-binding: t; -*-

;;; Commentary:
;; The "Insert into Org" bookmarklet sends the current page's URL and title
;; to org-protocol://insert-link.  Insert at the last cursor position the
;; user visited in Org, even if another buffer is selected when it arrives.
;;
;; Protocol registration, query encoding, and platform setup:
;; https://orgmode.org/manual/Protocols.html
;; https://orgmode.org/manual/The-store_002dlink-protocol.html
;; https://orgmode.org/worg/org-contrib/org-protocol.html
;; See `org-protocol-protocol-alist' in the installed org-protocol.el for
;; the custom handler's decoded plist argument and nil return convention.

;;; Code:

(require 'org-protocol)
(require 'my-org-browser-dnd)

(defvar my/org-browser-link-target (make-marker)
  "Last cursor position visited in an Org buffer.
Insertion advances this marker, so successive bookmarks stay in order.")
(set-marker-insertion-type my/org-browser-link-target t)

(defun my/org-browser-link-remember-position ()
  "Remember the cursor in the selected Org window."
  (unless (active-minibuffer-window)
    (let ((window (selected-window)))
      (when (and (window-live-p window) (not (window-minibuffer-p window)))
        (with-current-buffer (window-buffer window)
          (when (derived-mode-p 'org-mode)
            (set-marker my/org-browser-link-target (window-point window)
                        (current-buffer))))))))

(defun my/org-browser-link-insert (info)
  "Insert the browser page described by decoded protocol plist INFO.
Use the last Org cursor position, without selecting a window or saving
the file.  Return nil so Org Protocol does not visit the request as a file."
  (let* ((url (plist-get info :url))
         (title (my/org-browser-dnd--label (plist-get info :title)))
         (target (marker-buffer my/org-browser-link-target)))
    (unless (and (stringp url)
                 (string-match-p "\\`https?://[^[:space:]]+\\'" url)
                 (org-string-nw-p (url-host (url-generic-parse-url url))))
      (user-error "Insert into Org needs an HTTP or HTTPS page URL"))
    (unless (buffer-live-p target)
      (user-error "Place your cursor in an Org buffer before using Insert into Org"))
    (with-current-buffer target
      (unless (derived-mode-p 'org-mode)
        (user-error "The last Org buffer is no longer in Org mode"))
      (barf-if-buffer-read-only)
      (unless (<= (point-min) my/org-browser-link-target (point-max))
        (user-error "The saved cursor is outside the visible Org subtree"))
      (goto-char my/org-browser-link-target)
      (undo-boundary)
      (atomic-change-group
        ;; A missing page title falls back to the URL; this push workflow
        ;; never needs another HTTP request to discover the browser's title.
        (let ((my/org-browser-dnd-title (or title url)))
          (my/org-browser-dnd-insert url 'copy)))
      (undo-boundary)
      (message "Inserted browser link in %s" (buffer-name))))
  nil)

(setq org-protocol-protocol-alist
      (cons '("Insert browser page into Org"
              :protocol "insert-link"
              :function my/org-browser-link-insert
              :kill-client nil)
            (cl-remove-if
             (lambda (entry)
               (equal (plist-get (cdr entry) :protocol) "insert-link"))
             org-protocol-protocol-alist)))

(add-hook 'post-command-hook #'my/org-browser-link-remember-position)
(add-hook 'buffer-list-update-hook #'my/org-browser-link-remember-position)
(my/org-browser-link-remember-position)

(provide 'my-org-browser-link)
;;; my-org-browser-link.el ends here
