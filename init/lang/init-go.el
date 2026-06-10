
(require 'project)
(require 'eglot)




(defvar prf/go-ts t
  "When non-nil, use `go-ts-mode' instead of `go-mode'")

;; (add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
;; (add-to-list 'treesit-language-source-alist '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; (treesit-install-language-grammar 'go)

(if (and prf/go-ts
         ;; NB: `go-ts-mode' requires both `go' and `gomod'
         (treesit-language-available-p 'go)
         (treesit-language-available-p 'gomod))

    (use-package go-ts-mode
      :hook
      (go-ts-mode . lsp-deferred)
      (go-ts-mode . go-format-on-save-mode)

      :init
      ;; (dolist (lang '(go gomod)) (treesit-install-language-grammar lang))
      (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
      (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))

      :config
      (when (executable-find "goimports")
        (reformatter-define go-format
          :program "goimports"
          :args '("/dev/stdin")))
      )

  (use-package go-mode
    ;; :defer t
    :after eglot
    :bind (
           :map go-mode-map
           ("C-h C-f" . godef-jump))
    :hook (
           (go-mode . prf/go/lsp-activate-hook)
           (go-mode . (lambda ()
                        (setq indent-tabs-mode 't
                              tab-width 4
                              standard-indent 4)

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
             (not (s-ends-with? ".org" (buffer-file-name))) ; don't trigger when using babel
             (not (s-ends-with? ".md" (buffer-file-name))) ; don't trigger when using Polymode
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

    (add-hook 'project-find-functions #'project-find-go-module)))

;; NB: gocode is deprecated, using eglot w/ company instead
;; (use-package go-autocomplete
;;   :after go)




(provide 'init-go)
