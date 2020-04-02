

(use-package arduino-mode
  :config
  ;; NB: should not be needed, dunno why offest gets reset to 8...
  (defun prf/arduino-offset-hook ()
    (setq c-basic-offset 2))
  (add-hook 'arduino-mode-hook #'prf/arduino-offset-hook))




(provide 'init-arduino)
