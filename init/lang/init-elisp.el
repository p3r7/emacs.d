
(eval-after-load "dash" '(dash-enable-font-lock))

;; stolen from John Wiegley
(bind-key "C-c e b" #'eval-buffer)
(bind-key "C-c e e" #'toggle-debug-on-error)
(bind-key "C-c e f" #'emacs-lisp-byte-compile)
(bind-key "C-c e r" #'eval-region)
(bind-key "C-c e s" #'scratch)

(define-prefix-command 'prf/lisp-find-map)
(bind-key "C-h e" #'prf/lisp-find-map)
(bind-key "C-h e e" #'view-echo-area-messages)
(bind-key "C-h e f" #'find-function)
(bind-key "C-h e k" #'find-function-on-key)
(bind-key "C-h e l" #'find-library)
(bind-key "C-h e v" #'find-variable)
(bind-key "C-h e V" #'apropos-value)

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
	      ("C-c e m" . macrostep-expand)))

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :after (paredit)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(use-package elisp-slime-nav
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))


;; TODO: highlight-cl from quelpa

(provide 'init-elisp)
