
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
  :hook `((lisp-mode emacs-lisp-mode scheme-mode)
          . (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
  :init
  (dolist (mode prf/lisp-modes)
    (font-lock-add-keywords
     mode
     '(("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t))))))


(use-package paredit
  ;; :delight
  ;; :hook ((lisp-mode emacs-lisp-mode scheme-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("[")
              ("<M-left>"   . paredit-backward-slurp-sexp)
              ("<M-right>"   . paredit-forward-slurp-sexp)
              ("M-k"   . paredit-raise-sexp)
              ("M-I"   . paredit-splice-sexp)
              ("C-M-l" . paredit-recentre-on-sexp)
              ("C-c ( n"   . paredit-add-to-next-list)
              ("C-c ( p"   . paredit-add-to-previous-list)
              ("C-c ( j"   . paredit-join-with-next-list)
              ("C-c ( J"   . paredit-join-with-previous-list))
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :hook (paredit-mode
         . (lambda ()
	     ;; REVIEW: could use instead of a hook (define-key paredit-mode-map <tab> nil)
             (unbind-key "<C-left>" paredit-mode-map)
             (unbind-key "<C-right>" paredit-mode-map)
             (unbind-key "C-M-p" paredit-mode-map)
             (unbind-key "C-M-n" paredit-mode-map)
             (unbind-key "C-d" paredit-mode-map)
             (unbind-key "M-r" paredit-mode-map)
             (unbind-key "M-s" paredit-mode-map)))
  :config
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

;; (use-package paredit-ext
;;   :after paredit)


(use-package redshank
  :delight
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . redshank-mode))


(provide 'init-lisp-common)
