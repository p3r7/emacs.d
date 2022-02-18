

;; VARS

(defvar prf/lisp-modes '(lisp-mode
                         inferior-lisp-mode
                         emacs-lisp-mode
                         inferior-emacs-lisp-mode
                         lisp-interaction-mode
                         ielm-mode
                         scheme-mode
                         clojure-mode
                         cider-repl-mode))

(defvar prf/lisp-file-modes '(lisp-mode
                              emacs-lisp-mode
                              scheme-mode
                              clojure-mode))



;; MAIN

(use-package lisp-mode
  :defer t
  :ensure nil
  :hook `(,prf/lisp-file-modes
          . (lambda () (add-hook 'after-save-hook #'check-parens nil t))))

;; NOTE: USEFUL case EOF, in addition to check-parens: highlight parens contents
;; (show-paren-mode 1)
;; (setq show-paren-style 'expression)



;; NAVIGATION / REFACTORING HELPERS

;; (require 'init-paredit)
(require 'init-lispy)

(use-package redshank
  :delight
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . redshank-mode))



;; EVALUATION

(use-package eval-sexp-fu)



;; VISIBILITY

(use-package lisp-butt-mode)




(provide 'init-lisp-common)
