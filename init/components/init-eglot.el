
(when (version<= "26.3" emacs-version)

  (use-package eglot
    :after (company)
    :commands (eglot eglot-ensure)
    :hook ((python-mode . prf/hook/eglot-python-maybe))
    :config
    (defun prf/hook/eglot-python-maybe ()
      (when (executable-find "pyls")
        (eglot-ensure)))

    (defun prf/hook/prog-mode/company-when-eglot ()
      (cond
       (eglot--managed-mode
        (company-mode 1)
        (funcall prf/fav-completion-at-point-mode -1))
       (t
        (company-mode -1)
        (funcall prf/fav-completion-at-point-mode 1))))

    (when (not (eq prf/fav-completion-at-point 'company))
      (add-hook 'eglot--managed-mode-hook #'prf/hook/prog-mode/company-when-eglot))))




(provide 'init-eglot)
