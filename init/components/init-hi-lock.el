

(use-package hi-lock
  :ensure nil
  :demand

  :bind (("C-<f3>" . highlight-symbol-at-point-or-region)
         ("s-<f3>" . unhighlight-all))


  :init

  (setq hi-lock-face-defaults
        '("hi-yellow" "hi-pink" "hi-green" "hi-blue"))

  :config

  (defun highlight-symbol-at-point-or-region ()
    "Highlight symbol at point of slected region"
    (interactive)
    (if (use-region-p)
        (let* ((hi-lock-auto-select-face t)
               (face (hi-lock-read-face-name)))
          (or (facep face) (setq face 'hi-yellow))
          (highlight-phrase (buffer-substring (region-beginning) (region-end)) face)
          (cua-set-mark))
      (highlight-symbol-at-point)))

  (defun unhighlight-all ()
    "Unhighlight all highlightened sexp in current buffer"
    (interactive)
    (while hi-lock-interactive-patterns
      (hi-lock-unface-buffer (caar hi-lock-interactive-patterns))))

  ;; NB: this was necessary to make font-lock and hi-lock play nice
  ;; together.
  ;; Especially when changing theme.
  ;; Now does more bad than good.
  ;; (defadvice hi-lock-set-pattern (around use-overlays activate)
  ;;   (let ((font-lock-fontified nil))
  ;;     ad-do-it))
  )




(provide 'init-hi-lock)
