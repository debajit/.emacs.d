;;; my-org-browser-dnd.el --- Drop browser links into Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Org documents file/media drops; this extension handles browser HTTP(S)
;; links alongside that existing support:
;; https://orgmode.org/manual/Drag-and-Drop-_0026-yank_002dmedia.html
;;
;; Org's [[LINK][DESCRIPTION]] syntax, descriptive display, and escaping:
;; https://orgmode.org/manual/Link-Format.html
;; Use `org-link-make-string' to apply those escaping rules.
;;
;; The shared Emacs drop API defines `dnd-protocol-alist' handler arguments
;; (URL ACTION), return actions, and the `dnd-insert-text' fallback:
;; https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/dnd.el

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'dnd)
(require 'url)
(require 'mm-url)
(require 'subr-x)

(defvar my/org-browser-dnd-title nil
  "Browser-supplied title for the URL currently being dropped.")

(defun my/org-browser-dnd--label (text)
  "Return TEXT as a single-line link label, or nil if empty."
  (when (stringp text)
    (let ((label (string-trim
                  (replace-regexp-in-string "[[:space:]\u00a0]+" " " text))))
      (unless (string-empty-p label) label))))

(defun my/org-browser-dnd--page-title ()
  "Read an HTML title from the current URL response buffer."
  (let ((case-fold-search t)
        (charset 'utf-8))
    (goto-char (point-min))
    (when (re-search-forward "charset=[\"']?\\([[:alnum:]_-]+\\)" nil t)
      (let ((coding (intern (downcase (match-string 1)))))
        (when (coding-system-p coding) (setq charset coding))))
    (goto-char (point-min))
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (when (re-search-forward
             "<title\\(?:[[:space:]][^>]*\\)?>\\(\\(?:.\\|\n\\)*?\\)</title[[:space:]]*>"
             nil t)
        (let ((title (match-string-no-properties 1)))
          (my/org-browser-dnd--label
           (mm-url-decode-entities-string
            (if (multibyte-string-p title) title
              (decode-coding-string title charset)))))))))

(defun my/org-browser-dnd--fetch-title (url start end original)
  "Fetch URL's title and update the unchanged link between START and END.
ORIGINAL is the inserted link.  Edited, deleted, or undone links are left
alone.  Stop fetching after ten seconds; the initial link remains usable."
  (let (response timer finished)
    (cl-labels
        ((cleanup ()
           (setq finished t)
           (when timer (cancel-timer timer))
           (set-marker start nil)
           (set-marker end nil)
           ;; Follow redirects when aborting an unfinished request.
           (let ((kill-buffer-query-functions nil))
             (while (and (buffer-live-p response)
                         (buffer-local-value 'url-redirect-buffer response))
               (let ((next (buffer-local-value 'url-redirect-buffer response)))
                 (kill-buffer response)
                 (setq response next)))
             (when (buffer-live-p response) (kill-buffer response)))))
      (condition-case nil
          (let ((url-request-method "GET")
                (url-request-data nil)
                (url-request-noninteractive t)
                (url-request-extra-headers nil))
            (setq response
                  (url-retrieve
                   url
                   (lambda (status)
                     (unwind-protect
                         (ignore-errors
                           (unless (or finished (plist-get status :error)
                                       (and (boundp 'url-http-response-status)
                                            (numberp url-http-response-status)
                                            (>= url-http-response-status 400)))
                             (when-let* ((title (my/org-browser-dnd--page-title))
					 (target (marker-buffer start)))
                               (with-current-buffer target
				 (when (and (derived-mode-p 'org-mode)
                                            (not buffer-read-only)
                                            (<= start end)
                                            (equal original
                                                   (buffer-substring-no-properties
                                                    start end)))
                                   (save-excursion
                                     (goto-char start)
                                     (atomic-change-group
                                       (delete-region start end)
                                       (insert (org-link-make-string url title)))))))))
                       (let ((completed (current-buffer)))
                         (cleanup)
                         (when (buffer-live-p completed)
                           (kill-buffer completed)))))
                   nil t t))
            (unless finished
              (setq timer (run-at-time 10 nil #'cleanup))))
        (error (cleanup))))))

(defun my/org-browser-dnd-insert (url _action)
  "Insert dropped URL as an Org link, then fetch a missing title."
  (barf-if-buffer-read-only)
  (let* ((title (my/org-browser-dnd--label my/org-browser-dnd-title))
         (link (org-link-make-string url (or title url)))
         (start (point)))
    (insert link)
    (unless title
      (my/org-browser-dnd--fetch-title
       url (copy-marker start t) (copy-marker (point)) link)))
  'copy)

(defun my/org-browser-dnd--text (original window action text)
  "Handle plain URL drops in Org; delegate other TEXT to ORIGINAL."
  ;; macOS's `ns-drag-n-drop' can deliver text via `dnd-insert-text':
  ;; https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/term/ns-win.el
  (if (and (window-live-p window)
           (with-current-buffer (window-buffer window)
             (and (derived-mode-p 'org-mode) (not buffer-read-only)))
           (string-match-p "\\`https?://[^[:space:]]+\\'" text))
      (with-selected-window window
        (my/org-browser-dnd-insert text action))
    (funcall original window action text)))

(defun my/org-browser-dnd--moz-url (original window action data)
  "Preserve Firefox's title while ORIGINAL handles its URL drop."
  ;; Both Linux backends document text/x-moz-url as UTF-16 URL + title,
  ;; separated by CR/LF.  Their default handlers discard the title:
  ;; https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/x-dnd.el
  ;; https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/pgtk-dnd.el
  (let* ((coding (if (eq (byteorder) ?B) 'utf-16be 'utf-16le))
         (parts (split-string (decode-coding-string data coding) "[\r\n]+" t))
         (my/org-browser-dnd-title (cadr parts)))
    (funcall original window action data)))

(defun my/org-browser-dnd-setup ()
  "Enable browser URL drops in the current Org buffer."
  (setq-local dnd-protocol-alist
              (cons '("\\`https?://" . my/org-browser-dnd-insert)
                    (cl-remove-if
                     (lambda (entry)
                       (eq (cdr entry) #'my/org-browser-dnd-insert))
                     dnd-protocol-alist)))
  ;; Firefox's richer format includes both URL and link text on X11.
  (when (boundp 'x-dnd-known-types)
    (setq-local x-dnd-known-types
                (cons "text/x-moz-url"
                      (remove "text/x-moz-url" x-dnd-known-types)))))

(add-hook 'org-mode-hook #'my/org-browser-dnd-setup)
(unless (advice-member-p #'my/org-browser-dnd--text 'dnd-insert-text)
  (advice-add 'dnd-insert-text :around #'my/org-browser-dnd--text))
(dolist (backend '(x-dnd pgtk-dnd))
  (eval-after-load backend
    `(unless (advice-member-p #'my/org-browser-dnd--moz-url
                              ',(intern (format "%s-handle-moz-url" backend)))
       (advice-add ',(intern (format "%s-handle-moz-url" backend))
                   :around #'my/org-browser-dnd--moz-url))))

;; Also support loading this file after Org buffers have already opened.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'org-mode) (my/org-browser-dnd-setup))))

(provide 'my-org-browser-dnd)
;;; my-org-browser-dnd.el ends here
