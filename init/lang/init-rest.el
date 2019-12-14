

(use-package restclient
  :init
  (unless restclient-use-org
    (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))))




(provide 'init-rest)
