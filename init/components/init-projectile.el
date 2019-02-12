
;; (prf/require-plugin 'grizzl nil 'noerror)

(require 's)

(when (prf/require-plugin 'projectile nil 'noerror)

  ;; https://github.com/bbatsov/projectile/pull/444
  ;; (setq projectile-file-exists-remote-cache-expire nil)
  ;; (defadvice projectile-project-root (around ignore-remote first activate)
  ;;   (unless (file-remote-p default-directory) ad-do-it))

  (projectile-global-mode)

  ;; (if (featurep 'grizzl)
  ;; (setq projectile-completion-system 'grizzl)
  ;; )


  (projectile-register-project-type 'laravel '("composer.json" "artisan" "app")
  				    :compile "composer dump-autoload"
				    :test "phpunit -c app "
				    :test-suffix "Test")

  ;; (projectile-register-project-type 'jupyter '("composer.json" "artisan" "app")
  ;; 				    :compile "composer dump-autoload"
  ;; 				    :test "phpunit -c app "
  ;; 				    :test-suffix "Test")

  ;; https://www.reddit.com/r/emacs/comments/9jvn0f/projectile_gets_a_turboalien_indexing_mode/
  (when (executable-find "fd")

    (setq prf/projectile/fdignore '("\".git/\"" "\"**/.gitkeep\"" "\"**/.gitignore\""))

    (defun prf/projectile/build-fdignore ()
      (s-join " "
	      (mapcar (lambda (x) (s-prepend "-E " x))
		      prf/projectile/fdignore)))

    (setq projectile-indexing-method 'alien
	  projectile-generic-command (concat "fd . -H --ignore-file .projectile -t f -0 " (prf/projectile/build-fdignore))
	  projectile-git-command (concat "fd . -H -t f -0 " (prf/projectile/build-fdignore))))

  ;; ----------------------------------------------------------------------
  ;; HELM PROJECTILE

  (when (featurep 'projectile)
    (prf/require-plugin 'helm-projectile nil 'noerror))

  )


;; ------------------------------------------------------------------------

(provide 'init-projectile)
