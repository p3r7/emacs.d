
(defun prf/hook/eglot-python-maybe ()
  (when (executable-find "pyls")
    (eglot-ensure)))

(use-package eglot
  :after (company)
  :commands (eglot eglot-ensure)
  :hook ((python-mode . prf/hook/eglot-python-maybe))
  :config
  (defun prf/hook/prog-mode/company-when-eglot ()
    (cond
     (eglot--managed-mode
      (company-mode 1)
      (auto-complete-mode -1))
     (t
      (company-mode -1)
      (auto-complete-mode 1))))

  (add-hook 'eglot--managed-mode-hook #'prf/hook/prog-mode/company-when-eglot))


(provide 'init-eglot)
