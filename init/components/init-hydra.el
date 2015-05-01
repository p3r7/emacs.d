(when (and (>= emacs-major-version 24)
	   (>= emacs-minor-version 4))
  (prf/require-plugin 'hydra nil 'noerror)
  )

(provide 'init-hydra)
