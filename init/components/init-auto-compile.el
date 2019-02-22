
(setq load-prefer-newer t)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))


(provide 'init-auto-compile)
