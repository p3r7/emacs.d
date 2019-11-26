
(use-package shell
  :ensure nil
  :demand
  :after comint

  :config

  (when (>= emacs-major-version 25)
    (add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window))))


(use-package shx
  :hook (shell-mode . shx-mode)
  :after shell)



(provide 'init-shell)
