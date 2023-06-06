(defun browse-url-with-default-browser (event)
  (interactive "e")
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (browse-url-at-mouse event)))

;; Open links using Control+left click
;; See https://stackoverflow.com/a/9035868/2288585
(global-set-key (kbd "<C-down-mouse-1>") 'browse-url-with-default-browser)
