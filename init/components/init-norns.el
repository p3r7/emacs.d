
(require 'rx)




(use-package norns
  :load-path "~/.emacs.d/plugins/norns"
  :config (add-hook 'lua-mode-hook #'norns-lua-mode-hook))




(provide 'init-norns)
