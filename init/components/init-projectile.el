
;; (prf/require-plugin 'grizzl nil 'noerror)


(when (prf/require-plugin 'projectile nil 'noerror)

  (setq projectile-file-exists-remote-cache-expire nil)
  ;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
  ;; (add-hook 'find-file-hook
  ;; (lambda ()
  ;; (when (file-remote-p default-directory)
  ;; (setq-local projectile-mode-line "Projectile"))))
  ;; or
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

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
