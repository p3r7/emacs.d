
(use-package mixed-pitch
  :after (org)
  :bind (:map org-mode-map
	      ("C-c v" . mixed-pitch-mode)
	 )
  ;; :hook
  ;; (text-mode . mixed-pitch-mode)
  )

(provide 'init-text-common)
