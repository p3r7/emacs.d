
(use-package avy
  :after (org)
  :bind (("C-c SPC" . avy-goto-word-1)
	 ("C-c C-SPC" . avy-goto-word-1)
	 :map org-mode-map
	 ("C-c SPC" . avy-goto-word-1)
	 ("C-c C-SPC" . avy-goto-word-1))
  :init
  (setq avy-background t)
  ;; TODO: I got used to ace-jump-mode faces:
  ;; ace-jump-face-background and ace-jump-face-foreground
  )


(provide 'init-avy)
