
(use-package haskell-mode
  :mode "\.hs$"
  :load-path "~/.emacs.d/plugins/haskell-mode/haskell-site-file"
  :config
  (add-hook 'haskell-mode-hook #'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
  ;;(add-hook 'haskell-mode-hook #'turn-on-haskell-indent)
  ;;(add-hook 'haskell-mode-hook #'turn-on-haskell-simple-indent)
  )


(provide 'init-haskell)
