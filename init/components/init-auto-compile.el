

(setq load-prefer-newer t)

(when (prf/require-plugin 'auto-compile nil 'noerror)
  (require 'auto-compile)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))


(provide 'init-auto-compile)
