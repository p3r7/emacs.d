
(require 'rx)




;; NB: bug w/ `encode-time' under WinNT doesn't like times before Unix Epoch, breaks `osc'
(if (windows-nt-p)
    (use-package osc
      :load-path "~/.emacs.d/plugins-spe/osc-0.4")
  (use-package osc))

(use-package norns
  :demand
  :bind (
         :map norns-mode-map
         ("C-c e b" . norns-load-current-script)
         ("<XF86Open>" . norns-load-current-script)
         ("C-c e r" . norns-send-selection)
         ("M-<XF86Open>" . norns-send-selection)

         :map norns-matron-repl-mode-map
         ("C-c e b" . norns-rerun)
         ("<XF86Open>" . norns-rerun)

         :map norns-sc-repl-mode-map
         ("C-." . norns-sc-stop))
  :init
  (setq norns-lan-domain "lan")
  :config
  (add-hook 'lua-mode-hook #'norns-mode-maybe-activate)
  (add-hook 'fennel-mode-hook #'norns-mode-maybe-activate)
  (add-hook 'sclang-mode-hook #'norns-mode-maybe-activate))




(provide 'init-norns)
