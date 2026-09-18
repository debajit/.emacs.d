;;
;; ~/.emacs.d/dictionary-config.el
;;
;; Dictionary lookup and spelling configuration.
;;

;; Local dictionary server installed with `make dict' in ~/src/setup/dotfiles.
(use-package dictionary
  :ensure nil
  :commands dictionary-search
  :custom
  (dictionary-server "127.0.0.1")
  (dictionary-search-interface 'help))

;; Spellcheck with Flyspell and Hunspell.
(use-package flyspell
  :diminish flyspell-mode
  :bind ("<s-return>" . flyspell-auto-correct-previous-word)
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name (or (executable-find "hunspell") "hunspell")
        ispell-dictionary "en_US"
        ispell-local-dictionary "en_US")
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))
