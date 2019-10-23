
(setq prf/lispy-modes '(emacs-lisp-mode clojure-mode))

(use-package lispy
  :after (lisp-mode)
  :demand
  :hook
;;;  REVIEW: why is this not working ?!
  ;; `(prf/lispy-mode . lispy-mode)
  (((emacs-lisp-mode clojure-mode) . lispy-mode)
   (lispy-mode
    . (lambda ()
        ;; disable conflicting module: autopair
        ;; (when (fboundp 'autopair-mode)
        ;;   (autopair-mode -1))

        ;; unbind conflicting keybindings
        ;; handled by autopair
        (unbind-key "(" lispy-mode-map-lispy)
        (unbind-key ")" lispy-mode-map-lispy)
        (unbind-key "{" lispy-mode-map-lispy)
        (unbind-key "}" lispy-mode-map-lispy)
        (unbind-key "\"" lispy-mode-map-lispy)
        (unbind-key "\"" lispy-mode-map-lispy)
        (unbind-key "M-m" lispy-mode-map-lispy)

        (unbind-key "M-m" lispy-mode-map-lispy) ; lispy-mark-symbol
        (unbind-key "C-d" lispy-mode-map-lispy)
        (unbind-key "DEL" lispy-mode-map-lispy))))

  :bind (
         :map lispy-mode-map-lispy
         ("C-c DEL" . lispy-delete-backward)
         ;; disable when region active
         ("SPC" . prf/lispy-space-no-selection))

  :init
  (setq lispy-compat '(edebug macrostep cider))

  :config
  (defun prf/lispy-space-no-selection ()
    (interactive)
    (if (region-active-p)
        (insert "")
      (lispy-space)))

  ;; enable in minibuffer
  ;; TODO: use `eval-expression-minibuffer-setup-hook' instead
  (defun lispy-minibuffer-eval-expr-hook ()
    (when (member this-command '(eval-expression eval-expr))
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook #'lispy-minibuffer-eval-expr-hook)
  ;; disable when region active
  (advice-add 'delete-selection-pre-hook :around 'lispy--delsel-advice))


(provide 'init-lispy)
