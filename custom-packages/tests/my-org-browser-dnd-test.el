;;; my-org-browser-dnd-test.el --- Browser drop tests -*- lexical-binding: t; -*-

;; emacs --batch -Q -L custom-packages -l custom-packages/tests/my-org-browser-dnd-test.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'my-org-browser-dnd)
(require 'x-dnd)
(require 'pgtk-dnd)

(defmacro my/org-browser-dnd-test--request (&rest body)
  "Run BODY with a captured asynchronous URL request."
  (declare (indent 0))
  `(let (callback timeout response)
     (cl-letf (((symbol-function 'url-retrieve)
                (lambda (_url cb _args silent no-cookies)
                  (should silent)
                  (should no-cookies)
                  (setq callback cb
                        response (generate-new-buffer " *drop-test-response*"))))
               ((symbol-function 'run-at-time)
                (lambda (_seconds _repeat function &rest _args)
                  (setq timeout function) nil)))
       (unwind-protect
           (progn ,@body)
         (when timeout (funcall timeout))
         (when (buffer-live-p response) (kill-buffer response))))))

(defun my/org-browser-dnd-test--respond (buffer callback &optional status)
  "Call CALLBACK with an HTML response in BUFFER and STATUS."
  (with-current-buffer buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string
             "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<html><title>Café &amp; tea\n guide</title></html>"
             'utf-8))
    (funcall callback status)))

(ert-deftest my/org-browser-dnd-title-updates-without-moving-point ()
  (my/org-browser-dnd-test--request
    (with-temp-buffer
      (org-mode)
      (insert "Before ")
      (my/org-browser-dnd-insert "https://example.com" 'copy)
      (should (equal (buffer-string)
                     "Before [[https://example.com][https://example.com]]"))
      (insert " after")
      (my/org-browser-dnd-test--respond response callback)
      (should (equal (buffer-string)
                     "Before [[https://example.com][Café & tea guide]] after"))
      (should (= (point) (point-max)))
      (should-not (buffer-live-p response)))))

(ert-deftest my/org-browser-dnd-browser-label-avoids-fetch ()
  (with-temp-buffer
    (org-mode)
    (let ((my/org-browser-dnd-title "  Link\n text  "))
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest _) (ert-fail "Unexpected network request"))))
        (my/org-browser-dnd-insert "https://example.com/a[b]" 'copy)))
    (should (equal (buffer-string)
                   (org-link-make-string "https://example.com/a[b]" "Link text")))))

(ert-deftest my/org-browser-dnd-does-not-overwrite-edits ()
  (my/org-browser-dnd-test--request
    (with-temp-buffer
      (org-mode)
      (my/org-browser-dnd-insert "https://example.com" 'copy)
      (goto-char (point-min))
      (search-forward "][")
      (insert "My label ")
      (let ((edited (buffer-string)))
        (my/org-browser-dnd-test--respond response callback)
        (should (equal (buffer-string) edited))))))

(ert-deftest my/org-browser-dnd-does-not-restore-deleted-links ()
  (my/org-browser-dnd-test--request
    (with-temp-buffer
      (org-mode)
      (my/org-browser-dnd-insert "https://example.com" 'copy)
      (erase-buffer)
      (my/org-browser-dnd-test--respond response callback)
      (should (equal (buffer-string) "")))))

(ert-deftest my/org-browser-dnd-failure-keeps-usable-link ()
  (my/org-browser-dnd-test--request
    (with-temp-buffer
      (org-mode)
      (my/org-browser-dnd-insert "https://example.com" 'copy)
      (my/org-browser-dnd-test--respond response callback '(:error (error "Offline")))
      (should (equal (buffer-string)
                     "[[https://example.com][https://example.com]]")))))

(ert-deftest my/org-browser-dnd-timeout-keeps-link ()
  (my/org-browser-dnd-test--request
    (with-temp-buffer
      (org-mode)
      (my/org-browser-dnd-insert "https://example.com" 'copy)
      (funcall timeout)
      (should-not (buffer-live-p response))
      (should (equal (buffer-string)
                     "[[https://example.com][https://example.com]]")))))

(ert-deftest my/org-browser-dnd-native-url-dispatch-and-scope ()
  (let ((global-handlers (default-value 'dnd-protocol-alist)))
    (with-temp-buffer
      (org-mode)
      (my/org-browser-dnd-setup)
      (should (= 1 (cl-count #'my/org-browser-dnd-insert dnd-protocol-alist
                            :key #'cdr)))
      (should (assq 'org--dnd-multi-local-file-handler
                    (mapcar (lambda (entry) (cons (cdr entry) (car entry)))
                            dnd-protocol-alist)))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (let ((my/org-browser-dnd-title "Native URL"))
          (should (eq 'copy (dnd-handle-multiple-urls
                            (selected-window) '("https://example.com") 'private))))
        (should (equal (buffer-string)
                       "[[https://example.com][Native URL]]"))))
    (should (equal (default-value 'dnd-protocol-alist) global-handlers))))

(ert-deftest my/org-browser-dnd-plain-text-url-and-ordinary-text ()
  (with-temp-buffer
    (org-mode)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((my/org-browser-dnd-title "Plain URL"))
        (dnd-insert-text (selected-window) 'copy "https://example.com"))
      (dnd-insert-text (selected-window) 'copy " ordinary text https://other.com")
      (should (equal (buffer-string)
                     "[[https://example.com][Plain URL]] ordinary text https://other.com")))))

(ert-deftest my/org-browser-dnd-other-modes-and-read-only-unaffected ()
  (with-temp-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (dnd-insert-text (selected-window) 'copy "https://example.com")
      (should (equal (buffer-string) "https://example.com"))
      (org-mode)
      (let ((buffer-read-only t)
            (kill-ring nil)
            (kill-ring-yank-pointer nil))
        (dnd-insert-text (selected-window) 'copy "https://example.com")
        (should (equal (car kill-ring) "https://example.com"))))))

(ert-deftest my/org-browser-dnd-firefox-title-on-linux-backends ()
  (dolist (handler '(x-dnd-handle-moz-url pgtk-dnd-handle-moz-url))
    (with-temp-buffer
      (org-mode)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (funcall handler (selected-window) 'copy
                 (encode-coding-string "https://example.com\r\nFirefox label"
                                       (if (eq (byteorder) ?B) 'utf-16be 'utf-16le)))
        (should (equal (buffer-string)
                       "[[https://example.com][Firefox label]]"))))))

(ert-deftest my/org-browser-dnd-html-errors-and-empty-titles ()
  (dolist (html '("<html>No title</html>" "<title> \n </title>"))
    (with-temp-buffer
      (insert "HTTP/1.1 200 OK\r\n\r\n" html)
      (should-not (my/org-browser-dnd--page-title)))))

;;; my-org-browser-dnd-test.el ends here
