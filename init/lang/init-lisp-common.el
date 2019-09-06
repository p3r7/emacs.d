
(setq prf/lisp-modes '(lisp-mode
                       inferior-lisp-mode
                       emacs-lisp-mode
                       inferior-emacs-lisp-mode
                       lisp-interaction-mode
                       ielm-mode
                       scheme-mode
                       clojure-mode
                       cider-repl-mode))

(setq prf/lisp-file-modes '(lisp-mode
			    emacs-lisp-mode
			    scheme-mode
                            clojure-mode))

(use-package lisp-mode
  :defer t
  :ensure nil
  :hook `(,prf/lisp-file-modes
          . (lambda () (add-hook 'after-save-hook 'check-parens nil t))))

;; (require 'init-paredit)
(require 'init-lispy)

(use-package redshank
  :delight
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . redshank-mode))


(use-package eval-sexp-fu)

(provide 'init-lisp-common)
