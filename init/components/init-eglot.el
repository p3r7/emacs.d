
(use-package eglot
  :after (company)
  :commands (eglot eglot-ensure)
  ;; :hook ((c-mode . eglot-ensure))
  :config
  (defun prf/hook/prog-mode/company-when-eglot ()
    (when (and (fboundp 'company-mode)
               eglot--managed-mode)
      (company-mode 1)
      (auto-complete-mode -1)))

  (add-hook 'prog-mode-hook #'prf/hook/prog-mode/company-when-eglot))


(provide 'init-eglot)
