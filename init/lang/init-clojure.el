
(use-package clojure-mode)
(use-package cider
  :demand
  :bind (:map cider-repl-mode-map
	      ("C-x k" . cider-quit)
	      ("C-x C-k" . cider-quit))
  :after (clojure-mode)
  :init
  (setq org-babel-clojure-backend 'cider)
  :config
  (require 'ob-clojure))

(use-package cider-eval-sexp-fu
  :after (cider))

(provide 'init-clojure)
