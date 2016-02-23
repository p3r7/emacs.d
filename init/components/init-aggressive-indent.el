
(when (prf/require-plugin 'aggressive-indent nil 'noerror)
  ;; (global-aggressive-indent-mode 1)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  )

(provide 'init-aggressive-indent)
