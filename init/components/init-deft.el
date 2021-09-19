
(use-package deft
  :bind ("<f9>" . deft)
  :init
  (setq deft-use-filename-as-title t
	deft-auto-save-interval 0 ;; disable autosave
	deft-directory prf/dir/notes
	deft-recursive t))




(provide 'init-deft)
