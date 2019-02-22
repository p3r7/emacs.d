
(use-package ace-jump-mode
  :after (org)
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-c C-SPC" . ace-jump-mode)
	 :map org-mode-map
	 ("C-c SPC" . ace-jump-mode)
	 ("C-c C-SPC" . ace-jump-mode)))

(provide 'init-ace-jump)
