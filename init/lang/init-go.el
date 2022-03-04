
(require 'project)
(require 'eglot)




(use-package go-mode
  ;; :defer t
  :after eglot
  :bind (
         :map go-mode-map
         ("C-h C-f" . godef-jump))
  :hook (
         (go-mode . prf/go/lsp-activate-hook)
         (go-mode . (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2
                            standard-indent 2)

                      (when (executable-find "goimports" t)
                        (setq gofmt-command "goimports"))

                      (when (or (not (file-remote-p default-directory)) prf/go/lsp-on-remote)
                        (and (executable-find gofmt-command t)
                             (executable-find "diff" t))
                        (add-hook 'before-save-hook #'prf/gofmt-before-save-hook))

                      (if (not (string-match "go" compile-command))
                          (set (make-local-variable 'compile-command)
                               "go build -v && go test -v && go vet")))))
  :init
  (defvar prf/go/lsp-on-remote nil
    "If t, will plug to a LSP on remote (if found) and resolve deps on save.")

  :config
  (defun prf/go/lsp-activate-hook ()
    (when (and
           (or (not (file-remote-p default-directory)) prf/go/lsp-on-remote)
           (executable-find "gopls" t))
      (eglot-ensure)
      (setq eglot-workspace-configuration
            '((:gopls
               . ((staticcheck . t)
                  (matcher . "CaseSensitive")))))))

  (defun prf/gofmt-before-save-hook ()
    (when (executable-find gofmt-command t)
      (gofmt-before-save)))

  ;; project.el integration
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module))

;; NB: gocode is deprecated, using eglot w/ company instead
;; (use-package go-autocomplete
;;   :after go)




(provide 'init-go)
