
(use-package company
  :demand
  :bind (
         :map company-active-map
         ;; https://github.com/company-mode/company-mode/issues/68#issuecomment-77602328
         ("TAB" . company-complete)))

(provide 'init-company)
