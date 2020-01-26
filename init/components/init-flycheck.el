

(use-package flycheck)

(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))

(use-package package-lint)




(provide 'init-flycheck)
