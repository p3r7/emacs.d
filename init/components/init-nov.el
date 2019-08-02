
(use-package nov
  :bind (:map nov-mode-map
	      ("<" . nov-previous-document)
	      (">" . nov-next-document))
  :mode ("\\.epub\\'" . nov-mode))


(provide 'init-nov)
