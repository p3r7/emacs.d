
(when (prf/require-plugin 'magit nil 'noerror)
  (setq magit-auto-revert-mode 1)
  (setq magit-last-seen-setup-instructions "1.4.0")
  )

(provide 'init-vcs)
