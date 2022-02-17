
(require 'rx)




(use-package norns
  :load-path "~/.emacs.d/plugins/norns"
  :config
  (add-hook 'lua-mode-hook #'norns-mode-hook)
  (add-hook 'sclang-mode-hook #'norns-mode-hook))




(provide 'init-norns)
