
(use-package smartparens
  :config
  (require 'smartparens-config)
  (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit nil)
  (smartparens-global-mode 1)

  ;; enable in minibuffer
  ;; TODO: use `eval-expression-minibuffer-setup-hook' instead
  (defun sp-minibuffer-eval-expr-hook ()
    (when (member this-command '(eval-expression eval-expr))
      (smartparens-mode 1)))
  (add-hook 'minibuffer-setup-hook #'sp-minibuffer-eval-expr-hook))


(provide 'init-smartparens)
