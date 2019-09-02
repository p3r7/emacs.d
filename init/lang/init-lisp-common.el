
(setq prf/lisp-modes '(ielm-mode
                       inferior-emacs-lisp-mode
                       inferior-lisp-mode
                       lisp-interaction-mode
                       lisp-mode
                       emacs-lisp-mode))

(setq prf/lisp-file-modes '(lisp-mode
			    emacs-lisp-mode
			    scheme-mode))

(use-package lisp-mode
  :defer t
  :ensure nil
  :hook `(,prf/lisp-file-modes
          . (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
  :init
  (dolist (mode prf/lisp-modes)
    (font-lock-add-keywords
     mode
     '(("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t))))))

(require 'init-paredit)

(use-package redshank
  :delight
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . redshank-mode))


(use-package eval-sexp-fu)

(provide 'init-lisp-common)
