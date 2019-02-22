
(use-package helm-rg
  :after (helm)
  :if (executable-find "rg"))

(use-package deadgrep
  :if (executable-find "rg"))

(provide 'init-grep)
