
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

(provide 'init-paredit)
