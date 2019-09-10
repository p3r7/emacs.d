
(setq prf/elisp-modes '(emacs-lisp-mode
                        inferior-emacs-lisp-mode
                        lisp-interaction-mode
                        ielm-mode))


(use-package elisp-mode
  :ensure nil
  :demand
  :commands emacs-lisp-mode
  :delight (emacs-lisp-mode "Elisp")
  ;; :interpreter ("emacs" . emacs-lisp-mode)
  :bind (;; stolen from John Wiegley
	 ("C-c e b" . eval-buffer)
	 ("C-c e e" . toggle-debug-on-error)
	 ("C-c e f" . emacs-lisp-byte-compile)
	 ("C-c e r" . eval-region)
	 ("C-c e s" . prf/scratch))
  :init
  ;; ERT
  (dolist (mode prf/elisp-modes)
    (font-lock-add-keywords
     mode
     '(("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t)))))

                                        ; *scratch* buffer
  (defun prf/scratch nil
    "create a scratch buffer"
    (interactive)
    (let ((scratch-buffer (get-buffer "*scratch*")))
      (if scratch-buffer
	  (switch-to-buffer scratch-buffer)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(insert initial-scratch-message)
	(lisp-interaction-mode)))))


(use-package macrostep
  :after (elisp-mode)
  :bind (:map emacs-lisp-mode-map
	      ("C-c e m" . macrostep-expand)))


(use-package eval-expr
  :after (elisp-mode)
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    ;; (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (local-set-key (kbd "<tab>") #'completion-at-point)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))


(use-package elisp-slime-nav
  :after (elisp-mode)
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))

(use-package lisp-extra-font-lock
  :after (elisp-mode)
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))


(use-package highlight-cl
  :after (elisp-mode)
  :load-path "~/.emacs.d/plugins/highlight-cl"
  :hook (emacs-lisp-mode . highlight-cl-add-font-lock-keywords))


(require 'init-eldoc)


(provide 'init-elisp)
