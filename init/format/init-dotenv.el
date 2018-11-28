
(when (prf/require-plugin 'dotenv-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(provide 'init-dotenv)
