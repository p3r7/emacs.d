

(use-package hi-lock
  :ensure nil
  :demand

  :bind (("C-<f3>" . highlight-symbol-at-point-or-region))


  :init

  (setq hi-lock-face-defaults
        '("hi-yellow" "hi-pink" "hi-green" "hi-blue"))

  :config

  (defun highlight-symbol-at-point-or-region ()
    "Highlight symbol at point of selected region"
    (interactive)
    (if (use-region-p)
        (let* ((hi-lock-auto-select-face t)
               (face (hi-lock-read-face-name)))
          (or (facep face) (setq face 'hi-yellow))
          (highlight-phrase (buffer-substring (region-beginning) (region-end)) face)
          (cua-set-mark))
      (highlight-symbol-at-point)))

  ;; NB: this was necessary to make font-lock and hi-lock play nice
  ;; together.
  ;; Especially when changing theme.
  ;; Now does more bad than good.
  ;; (defadvice hi-lock-set-pattern (around use-overlays activate)
  ;;   (let ((font-lock-fontified nil))
  ;;     ad-do-it))
  )


(use-package hi-lock-ext
  :quelpa (hi-lock-ext :url "https://raw.githubusercontent.com/sensorflo/sensorflo-emacs/master/misc/hi-lock-ext.el" :fetcher url)
  :after hi-lock
  :bind (
         ;; ("" . highlight-toggle-sexp-or-region)
         ("s-<f3>" . unhighlight-all)))




(provide 'init-hi-lock)
