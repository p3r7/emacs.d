
(use-package eval-in-repl
  :init
  (setq eir-repl-placement 'right)
  (setq eir-jump-after-eval nil)

  ;; elisp
  (require 'eval-in-repl-ielm)
  (setq eir-ielm-eval-in-current-buffer t)
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)

  ;; python
  (require 'eval-in-repl-python)
  (add-hook 'python-mode-hook
	    (lambda ()
	      (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))

(use-package org-babel-eval-in-repl
  :init
  (define-key org-mode-map (kbd "C-<return>") 'ober-eval-in-repl)
  (define-key org-mode-map (kbd "M-<return>") 'ober-eval-block-in-repl))


(provide 'init-eval-in-repl)
