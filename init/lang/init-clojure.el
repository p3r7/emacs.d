
(use-package clojure-mode)
(use-package cider
  :bind (:map cider-repl-mode-map
	      ("C-x k" . cider-quit)
	      ("C-x C-k" . cider-quit))
  :after (clojure-mode))

(provide 'init-clojure)
