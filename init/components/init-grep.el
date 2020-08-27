

;; RIPGREP

(use-package helm-rg
  :after (helm)
  :if (executable-find "rg")
  :defer t)

(use-package deadgrep
  :if (executable-find "rg")
  :bind (("<C-find>" . deadgrep)))




(provide 'init-grep)
