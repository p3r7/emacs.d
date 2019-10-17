
(use-package company
  :demand
  :bind (
         :map company-active-map
         ;; https://github.com/company-mode/company-mode/issues/68#issuecomment-77602328
         ("TAB" . company-complete))

  :config
  (when (eq prf/fav-completion-at-point 'company)
    (add-hook 'after-init-hook 'global-company-mode)))




(provide 'init-company)
