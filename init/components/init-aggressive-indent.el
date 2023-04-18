
(use-package aggressive-indent
  :hook
  (prog-mode . (lambda ()
                 ;; NB: `aggressive-indent-excluded-modes' only eval'ed when using `global-aggressive-indent-mode'
                 (when (boundp 'aggressive-indent-excluded-modes)
                   (unless (cl-member-if #'derived-mode-p aggressive-indent-excluded-modes)
                     aggressive-indent-mode))))
  :init
  (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'sass-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'dotenv-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode))




(provide 'init-aggressive-indent)
