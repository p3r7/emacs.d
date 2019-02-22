
;; (use-package 'grizzl)

;; ------------------------------------------------------------------------
;; PROJECTILE

(use-package projectile
  :after (s)

  :config

  ;; https://github.com/bbatsov/projectile/pull/444
  ;; (setq projectile-file-exists-remote-cache-expire nil)
  ;; (defadvice projectile-project-root (around ignore-remote first activate)
  ;;   (unless (file-remote-p default-directory) ad-do-it))

  (projectile-global-mode)

  (projectile-register-project-type
   'laravel '("composer.json" "artisan" "app")
   :compile "composer dump-autoload"
   :test "phpunit -c app "
   :test-suffix "Test")

  ;; (projectile-register-project-type
  ;;  'jupyter '("composer.json" "artisan" "app")
  ;;  :compile "composer dump-autoload"
  ;;  :test "phpunit -c app "
  ;;  :test-suffix "Test")

  ;; (if (featurep 'grizzl)
  ;;     (setq projectile-completion-system 'grizzl))

  (when (executable-find "fd")

    (setq prf/projectile/fdignore '("\".git/\"" "\"**/.gitkeep\"" "\"**/.gitignore\""))

    (defun prf/projectile/build-fdignore ()
      (s-join " "
	      (mapcar (lambda (x) (s-prepend "-E " x))
		      prf/projectile/fdignore)))

    (setq projectile-indexing-method 'alien
	  projectile-generic-command (concat "fd . -H --ignore-file .projectile -t f -0 " (prf/projectile/build-fdignore))
	  projectile-git-command (concat "fd . -H -t f -0 " (prf/projectile/build-fdignore)))))


;; ------------------------------------------------------------------------
;; HELM PROJECTILE

(use-package helm-projectile
  :after (helm projectile)
  :bind (("C-S-n" . helm-projectile-find-file))
  :config
  (cond
   ((featurep 'helm-rg)
    (global-set-key (kbd "C-S-f") 'helm-projectile-rg))
   ((executable-find "ag")
    (global-set-key (kbd "C-S-f") 'helm-projectile-ag))
   (t
    (global-set-key (kbd "C-S-f") 'helm-projectile))))



(provide 'init-projectile)
