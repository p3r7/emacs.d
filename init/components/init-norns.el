
(require 'rx)




;; NB: bug w/ `encode-time' under WinNT doesn't like times before Unix Epoch, breaks `osc'
(if (windows-nt-p)
    (use-package osc
      :load-path "~/.emacs.d/plugins-spe/osc-0.4")
  (use-package osc))

(use-package norns
  :after osc
  :config
  (add-hook 'lua-mode-hook #'norns-mode-maybe-activate)
  (add-hook 'sclang-mode-hook #'norns-mode-maybe-activate))




(provide 'init-norns)
