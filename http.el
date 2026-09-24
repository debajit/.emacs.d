;;; http.el --- HTTP request support -*- lexical-binding: t; -*-

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(provide 'http)
;;; http.el ends here
