;;; my-org-browser-link-test.el --- Org Protocol insertion tests -*- lexical-binding: t; -*-

;; emacs --batch -Q -L custom-packages -l custom-packages/tests/my-org-browser-link-test.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'my-org-browser-link)

(defmacro my/org-browser-link-test--target (&rest body)
  "Run BODY in an Org buffer with its own saved insertion marker."
  (declare (indent 0))
  `(let ((my/org-browser-link-target (make-marker)))
     (set-marker-insertion-type my/org-browser-link-target t)
     (with-temp-buffer
       (org-mode)
       (unwind-protect
           (progn ,@body)
         (set-marker my/org-browser-link-target nil)))))

(ert-deftest my/org-browser-link-inserts-in-remembered-buffer ()
  (my/org-browser-link-test--target
    (insert "Before  after")
    (set-marker my/org-browser-link-target 8)
    (let ((target (current-buffer)))
      (with-temp-buffer
        (insert "Other buffer")
        (should-not (my/org-browser-link-insert
                     '(:url "https://example.com" :title "Page title")))
        (should (equal (buffer-string) "Other buffer")))
      (with-current-buffer target
        (should (equal (buffer-string)
                       "Before [[https://example.com][Page title]] after"))
        (should (= (point) (marker-position my/org-browser-link-target)))))))

(ert-deftest my/org-browser-link-remembers-selected-org-window ()
  (my/org-browser-link-test--target
    (insert "abcdef")
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (goto-char 3)
      (my/org-browser-link-remember-position)
      (should (= (marker-position my/org-browser-link-target) 3))
      (let ((target (marker-buffer my/org-browser-link-target)))
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (my/org-browser-link-remember-position)
          (should (eq (marker-buffer my/org-browser-link-target) target)))))))

(ert-deftest my/org-browser-link-subsequent-links-advance ()
  (my/org-browser-link-test--target
    (set-marker my/org-browser-link-target (point))
    (my/org-browser-link-insert '(:url "https://one.example" :title "One"))
    (my/org-browser-link-insert '(:url "https://two.example" :title "Two"))
    (should (equal (buffer-string)
                   "[[https://one.example][One]][[https://two.example][Two]]"))))

(ert-deftest my/org-browser-link-encoded-request-and-title-escaping ()
  (my/org-browser-link-test--target
    (set-marker my/org-browser-link-target (point))
    (should-not
     (org-protocol-check-filename-for-protocol
      "org-protocol://insert-link?url=https%3A%2F%2Fexample.com%2Fa%5Bb%5D%3Fx%3Da%2Bb%26y%3D2&title=Caf%C3%A9+%26+%5B%5Btea%5D%5D"
      nil nil))
    (should (equal (buffer-string)
                   (org-link-make-string "https://example.com/a[b]?x=a+b&y=2"
                                         "Café & [[tea]]")))))

(ert-deftest my/org-browser-link-title-fallback-never-fetches ()
  (my/org-browser-link-test--target
    (set-marker my/org-browser-link-target (point))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _) (ert-fail "Protocol insertion must not fetch"))))
      (my/org-browser-link-insert '(:url "https://example.com" :title " \n ")))
    (should (equal (buffer-string)
                   "[[https://example.com][https://example.com]]"))))

(ert-deftest my/org-browser-link-rejects-invalid-page-urls ()
  (dolist (url '(nil "" "javascript:alert(1)" "file:///tmp/file" "https://"
                "https://example.com\nInjected text"))
    (my/org-browser-link-test--target
      (set-marker my/org-browser-link-target (point))
      (should-error (my/org-browser-link-insert (list :url url :title "Title"))
                    :type 'user-error)
      (should (equal (buffer-string) "")))))

(ert-deftest my/org-browser-link-rejects-missing-or-closed-target ()
  (my/org-browser-link-test--target
    (should-error (my/org-browser-link-insert '(:url "https://example.com"))
                  :type 'user-error)
    (let ((target (generate-new-buffer " *closed-org-target*")))
      (set-marker my/org-browser-link-target 1 target)
      (kill-buffer target)
      (should-error (my/org-browser-link-insert '(:url "https://example.com"))
                    :type 'user-error))))

(ert-deftest my/org-browser-link-rejects-read-only-or-changed-mode ()
  (my/org-browser-link-test--target
    (set-marker my/org-browser-link-target (point))
    (let ((buffer-read-only t))
      (should-error (my/org-browser-link-insert '(:url "https://example.com"))
                    :type 'buffer-read-only))
    (fundamental-mode)
    (should-error (my/org-browser-link-insert '(:url "https://example.com"))
                  :type 'user-error)
    (should (equal (buffer-string) ""))))

(ert-deftest my/org-browser-link-does-not-clip-narrowed-position ()
  (my/org-browser-link-test--target
    (insert "abcdef")
    (set-marker my/org-browser-link-target 2)
    (narrow-to-region 4 7)
    (should-error (my/org-browser-link-insert '(:url "https://example.com"))
                  :type 'user-error)
    (widen)
    (should (equal (buffer-string) "abcdef"))))

(ert-deftest my/org-browser-link-insertion-can-be-undone ()
  (my/org-browser-link-test--target
    (insert "Existing text")
    (buffer-enable-undo)
    (set-marker my/org-browser-link-target (point))
    (my/org-browser-link-insert '(:url "https://example.com" :title "Title"))
    (undo-only 1)
    (should (equal (buffer-string) "Existing text"))))

(ert-deftest my/org-browser-link-registration-survives-reload ()
  (my/org-browser-link-test--target
    (load "my-org-browser-link" nil t)
    (should (= 1 (cl-count "insert-link" org-protocol-protocol-alist
                          :test #'equal
                          :key (lambda (entry)
                                 (plist-get (cdr entry) :protocol)))))))

;;; my-org-browser-link-test.el ends here
