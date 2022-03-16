
(require 'rx)




;; NB: bug w/ `encode-time' in current pre-release of Emacs 28, breaks `osc'
(unless (eq emacs-major-version 28)
  (use-package norns
    ;; :load-path "~/.emacs.d/plugins/norns"
    :quelpa (norns :fetcher github :repo "p3r7/norns.el")
    :config
    (add-hook 'lua-mode-hook #'norns-mode-hook)
    (add-hook 'sclang-mode-hook #'norns-mode-hook)))




(provide 'init-norns)
