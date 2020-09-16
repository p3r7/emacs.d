

;; CONF

(setq prf/lispy-modes '(emacs-lisp-mode
                        clojure-mode clojurescript-mode cider-repl-mode))



;; MAIN

(use-package lispy
  :after lisp-mode
  :demand
  :hook
;;;  REVIEW: why is this not working ?!
  ;; `(prf/lispy-mode . lispy-mode)
  (((emacs-lisp-mode
     clojure-mode clojurescript-mode cider-repl-mode) . lispy-mode)
   (lispy-mode
    . (lambda ()
        ;; disable conflicting module: autopair
        ;; (when (fboundp 'autopair-mode)
        ;;   (autopair-mode -1))

        ;; lispy-newline-and-indent / electric-newline-and-maybe-indent
        ;; useless as using agressive-indent
        ;; I wan't to use it for yas
        (unbind-key "C-j" lispy-mode-map-base)

        ;; unbind conflicting keybindings
        ;; handled by smart-parens
        (unbind-key "(" lispy-mode-map-lispy)
        (unbind-key ")" lispy-mode-map-lispy)
        (unbind-key "{" lispy-mode-map-lispy)
        (unbind-key "}" lispy-mode-map-lispy)
        (unbind-key "[" lispy-mode-map-lispy)
        (unbind-key "]" lispy-mode-map-lispy)
        (unbind-key "\"" lispy-mode-map-lispy)
        (unbind-key "`" lispy-mode-map-lispy)

        (unbind-key "M-m" lispy-mode-map-lispy) ; lispy-mark-symbol
        (unbind-key "C-d" lispy-mode-map-lispy)
        (unbind-key "DEL" lispy-mode-map-lispy))))

  :bind (
         :map lispy-mode-map-lispy
         ("C-c DEL" . lispy-delete-backward)
         ;; disable when region active
         :map lispy-mode-map
         ([remap lispy-space] . prf/lispy-space-no-selection))

  :init
  (setq lispy-compat '(edebug macrostep cider))

  :config
  (defun prf/lispy-space-no-selection (arg)
    (interactive "p")
    (if (region-active-p)
        (progn
          (call-interactively #'delete-region)
          ;; (insert " ")
          (lispy-space arg))
      (lispy-space arg)))

  ;; enable in minibuffer
  (defun prf/lispy-enable-hook ()
    (lispy-mode 1))
  (add-hook 'eval-expression-minibuffer-setup-hook #'prf/lispy-enable-hook)
  (with-eval-after-load "eval-expr"
    (add-hook 'prf/eval-expr-minibuffer-setup-hook #'prf/lispy-enable-hook))

  ;; disable when region active
  (advice-add 'delete-selection-pre-hook :around 'lispy--delsel-advice))




(provide 'init-lispy)
