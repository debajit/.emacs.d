(defhydra hydra-switch-mode (:color blue
                                    :hint nil
                                    )
  "
 ^Writing^          ^Programming^         ^Other^                 ^Formatting
-------------    ---------------    -----------------     -------------
 _o_: Org           _e_: Elixir           _c_: Conf Mode        _Q_: Electric Quotes
 _m_: Markdown      _r_: Ruby             _f_: Fundamental      _a_: Auto-fill Mode
 ^ ^                _j_: JavaScript       _y_: YAML
 ^ ^                _g_: GraphQL
 ^ ^                _l_: Emacs-Lisp
 ^ ^                _s_: Shell-Script
"
  ("Q" electric-quote-mode)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("a" auto-fill-mode)
  ("c" conf-mode)
  ("e" elixir-mode)
  ("f" fundamental-mode)
  ("g" graphql-mode)
  ("j" js2-mode)
  ("l" emacs-lisp-mode)
  ("m" gfm-mode)
  ("o" org-mode)
  ("r" ruby-mode)
  ("s" shell-script-mode)
  ("y" yaml-mode)

  ("q" nil "Quit")
  ;; ("q" quit-window "quit" :color blue)
  )

;; (Key chord) - What the key does
(global-set-key (kbd "M-M") 'hydra-switch-mode/body)
