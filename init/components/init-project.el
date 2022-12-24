
;; (use-package 'grizzl)


;; PROJECT

(use-package find-file-in-project
  :bind ("C-M-s-f" . find-file-in-project))



;; PROJECTILE

(defun prf/enable-projectile-p ()
  ;; seem to slow things down under the windows build
  (not
   (string-equal system-type "windows-nt")))

(use-package projectile
  ;; :after s
  :if (prf/enable-projectile-p)
  ;; REVIEW: cleaner defintion w/ let*
  :delight '(:eval (if (string= "-" (projectile-project-name))
		       ""
		     (concat
		      " P"
		      "[" (projectile-project-name)
		      (when (not (string= "generic" (symbol-name (projectile-project-type))))
			(concat ":" (symbol-name (projectile-project-type))))
		      "]")))

  :bind
  ;; ("C-s-f" . helm-projectile-find-file)
  ("C-s-f" . projectile-find-file)

  :config

  ;; from https://github.com/MaskRay/ccls/wiki/eglot
  (defun prf/projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'prf/projectile-project-find-function))


  ;; Disable under TRAMP
  ;; https://github.com/bbatsov/projectile/pull/444
  (setq projectile-file-exists-remote-cache-expire nil)
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))


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

  ;; (when (fboundp #'fileloop-continue)

  ;;   (defun prf/fileloop-initialize-replace-no-prompt ()
  ;;     (fileloop-initialize
  ;;      files
  ;;      (lambda ()
  ;;        (let ((case-fold-search (fileloop--case-fold from case-fold)))
  ;;          (if (re-search-forward from nil t)
  ;;              ;; When we find a match, move back
  ;;              ;; to the beginning of it so perform-replace
  ;;              ;; will see it.
  ;;              (goto-char (match-beginning 0)))))
  ;;      (lambda ()
  ;;        (let ((case-fold-search (fileloop--case-fold from case-fold)))

  ;;          (perform-replace from to t t delimited nil multi-query-replace-map)))))

  ;;   (defun prf/projectile-replace-no-prompt ()
  ;;     (interactive)
  ;;     (interactive "P")
  ;;     (let* ((directory (if arg
  ;;                           (file-name-as-directory
  ;;                            (read-directory-name "Replace in directory: "))
  ;;                         (projectile-acquire-root)))
  ;;            (old-text (read-string
  ;;                       (projectile-prepend-project-name "Replace: ")
  ;;                       (projectile-symbol-or-selection-at-point)))
  ;;            (new-text (read-string
  ;;                       (projectile-prepend-project-name
  ;;                        (format "Replace %s with: " old-text))))
  ;;            (files (projectile-files-with-string old-text directory)))
  ;;       (progn (fileloop-initialize-replace old-text new-text files 'default)
  ;;              (fileloop-continue)))))

  (when (executable-find "fd")

    (setq prf/projectile/fdignore '("\".git/\"" "\"**/.gitkeep\"" "\"**/.gitignore\""))

    (defun prf/projectile/build-fdignore ()
      (s-join " "
	      (mapcar (lambda (x) (s-prepend "-E " x))
		      prf/projectile/fdignore)))

    (setq projectile-indexing-method 'alien
	  projectile-generic-command (concat "fd . -H --ignore-file .projectile -t f -0 " (prf/projectile/build-fdignore))
	  projectile-git-command (concat "fd . -H -t f -0 " (prf/projectile/build-fdignore)))))



;; HELM PROJECTILE

(use-package helm-projectile
  :after (helm projectile)
  :bind (("C-S-n" . helm-projectile-find-file))
  :config
  (cond
   ((featurep 'helm-rg)
    (global-set-key (kbd "C-S-f") #'helm-projectile-rg))
   ((executable-find "ag")
    (global-set-key (kbd "C-S-f") #'helm-projectile-ag))
   (t
    (global-set-key (kbd "C-S-f") #'helm-projectile))))




(provide 'init-project)
