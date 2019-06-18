
(use-package hydra
  :if (or (> emacs-major-version 24)
	  (or (= emacs-major-version 24)
	      (>= emacs-minor-version 4))))

(provide 'init-hydra)
