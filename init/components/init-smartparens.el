
(use-package smartparens
  :config
  (require 'smartparens-config)
  (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit nil)
  (smartparens-global-mode 1))

(provide 'init-smartparens)
