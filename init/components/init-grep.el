
(when (executable-find "rg")
  (prf/require-plugin 'helm-rg nil 'noerror)
  (prf/require-plugin 'deadgrep nil 'noerror))

(provide 'init-grep)
