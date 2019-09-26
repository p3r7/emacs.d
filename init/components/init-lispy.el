
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

        (unbind-key "C-d" lispy-mode-map-lispy)
        (unbind-key "DEL" lispy-mode-map-lispy))))
  :bind (
         ;; :map lispy-mode-map
         ;; ("C-c DEL" . lispy-delete-backward)
         :map lispy-mode-map-lispy
         ("C-c DEL" . lispy-delete-backward))
  :init
  (setq lispy-compat '(edebug macrostep cider))
  :config
  ;; enable in minibuffer
  (defun lispy-minibuffer-eval-exp-hook ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook #'lispy-minibuffer-eval-exp-hook)
  ;; disable enabling when region active
  (advice-add 'delete-selection-pre-hook :around 'lispy--delsel-advice))


(provide 'init-lispy)
