
(use-package lisp-mode
  :ensure nil
  :demand
  :commands emacs-lisp-mode
  ;; :interpreter ("emacs" . emacs-lisp-mode)
  :bind (;; stolen from John Wiegley
	 ("C-c e b" . eval-buffer)
	 ("C-c e e" . toggle-debug-on-error)
	 ("C-c e f" . emacs-lisp-byte-compile)
	 ("C-c e r" . eval-region)
	 ("C-c e s" . prf/scratch))
  :init
  (defun prf/scratch nil
    "create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (insert initial-scratch-message)
    (lisp-interaction-mode)))

(use-package macrostep
  :after (emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
	      ("C-c e m" . macrostep-expand)))

(use-package eval-expr
  :after (paredit)
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(use-package elisp-slime-nav
  :after (paredit)
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))


;; TODO: highlight-cl from quelpa

(provide 'init-elisp)
