
(use-package smartparens
  :config
  (require 'smartparens-config)
  (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit nil)
  (smartparens-global-mode 1)

  ;; enable in minibuffer
  (defun prf/sp-enable-hook ()
    (smartparens-mode 1))
  (add-hook 'eval-expression-minibuffer-setup-hook #'prf/sp-enable-hook)
  (with-eval-after-load "eval-expr"
    (add-hook 'prf/eval-expr-minibuffer-setup-hook #'prf/sp-enable-hook)))


(provide 'init-smartparens)
