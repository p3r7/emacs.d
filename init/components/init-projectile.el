
;; (prf/require-plugin 'grizzl nil 'noerror)


(when (prf/require-plugin 'projectile nil 'noerror)

  (projectile-global-mode)

  ;; (if (featurep 'grizzl)
      ;; (setq projectile-completion-system 'grizzl)
    ;; )

  ;; ----------------------------------------------------------------------
  ;; HELM PROJECTILE

  (when (featurep 'projectile)
    (prf/require-plugin 'helm-projectile nil 'noerror)
    )

  )


;; ------------------------------------------------------------------------

(provide 'init-projectile)
