(when (prf/require-plugin 'sr-speedbar nil 'noerror)
  (global-set-key [f8] 'sr-speedbar-toggle)
  )

(setq speedbar-show-unknown-files t)

(provide 'init-speedbar)
