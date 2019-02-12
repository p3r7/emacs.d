
(when (executable-find "rg")
  (prf/require-plugin 'deadgrep nil 'noerror))

(provide 'init-grep)
