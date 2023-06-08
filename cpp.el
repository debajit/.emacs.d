;; See https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
  :ensure t
  :init
  (modern-c++-font-lock-global-mode t)
  )

;; Run C++ programs easily by pressing Super-r
(use-package quickrun
  :ensure t

  :config
  ;; Use this parameter as C++ default. See https://github.com/emacsorphanage/quickrun
  (quickrun-add-command "c++/c++20"
    '((:command . "g++")
      (:exec    . ("%c -std=c++20 %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++")

  :bind (
         ("s-r" . quickrun)
         ("M-R" . quickrun-region)
         )
  )
