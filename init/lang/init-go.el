
(require 'project)
(require 'eglot)




(use-package go-mode
  ;; :defer t
  :after eglot
  :bind (
         :map go-mode-map
         ("C-h C-f" . godef-jump))
  :hook (
         (go-mode . (lambda ()
                      (eglot-ensure)
                      (setq eglot-workspace-configuration
                            '((:gopls
                               . ((staticcheck . t)
                                  (matcher . "CaseSensitive")))))))
         (go-mode . (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2
                            standard-indent 2)

                      (when (executable-find "diff")
                        (when (executable-find "goimports")
                          (setq gofmt-command "goimports"))
                        (add-hook 'before-save-hook #'gofmt-before-save))

                      (if (not (string-match "go" compile-command))
                          (set (make-local-variable 'compile-command)
                               "go build -v && go test -v && go vet")))))

  :config
  ;; project.el integration
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module)
  )

;; NB: gocode is deprecated, using eglot w/ company instead
;; (use-package go-autocomplete
;;   :after go)






(provide 'init-go)
