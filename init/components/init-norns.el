
(require 'rx)




;; NB: bug w/ `encode-time' in current pre-release of Emacs 28, breaks `osc'
(unless (eq emacs-major-version 28)
  (use-package norns
    :config
    (add-hook 'lua-mode-hook #'norns-mode-maybe-activate)
    (add-hook 'sclang-mode-hook #'norns-mode-maybe-activate)))




(provide 'init-norns)
