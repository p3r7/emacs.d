
(use-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode)
  ;; REVIEW: could be cleaner to: (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  ((python-mode sql-mode sass-mode dotenv-mode dockerfile-mode makefile-gmake-mode) . (lambda () (aggressive-indent-mode -1))))

(provide 'init-aggressive-indent)
