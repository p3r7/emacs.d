
(use-package yasnippet
  :delight yas-minor-mode
  :bind (:map yas-minor-mode-map
	      ("C-c y" . yas-maybe-expand)
	      ("C-c Y" . yas-expand))
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

(provide 'init-yasnippet)
