
(use-package aggressive-indent
  :config
  (--map
   (add-to-list 'aggressive-indent-excluded-modes it)
   '(dockerfile-mode dotenv-mode sass-mode sql-mode))
  (global-aggressive-indent-mode 1))




(provide 'init-aggressive-indent)
