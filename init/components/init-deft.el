
(use-package deft
  :bind (("<f9>" . deft)
         :map deft-mode-map
         ("<return>" . prf/deft-complete))
  :init
  (setq deft-use-filename-as-title t
	deft-auto-save-interval 0 ;; disable autosave
	deft-directory prf/dir/notes
	deft-recursive t)
  :config
  (defun prf/deft-complete ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (when (button-at (point))
        (push-button)))))




(provide 'init-deft)
