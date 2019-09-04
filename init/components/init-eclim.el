
(use-package eclim)

(use-package eclimd
  :ensure nil
  :after (eclim)
  :init
  (setq
   eclim-auto-save t
   eclim-executable "/opt/eclipse/juno/eclim")
  :config
  (global-eclim-mode)
  ;; NB: needs to be defined here, overwise it gets overriden by the above statements
  (add-hook 'c-mode-common-hook
            (lambda () (local-set-key (kbd "RET") 'newline-and-indent))))

(use-package ac-emacs-eclim
  :after (eclim eclimd)
  :init
  (ac-emacs-eclim-config))

(provide 'init-eclim)
