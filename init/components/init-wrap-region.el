
(use-package wrap-region
  :hook
  ((org-mode latex-mode) . wrap-region-mode)
  :config
  (wrap-region-remove-wrapper "(" 'org-mode)
  (wrap-region-remove-wrapper "\"" 'org-mode)
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil (org-mode latex-mode)))))


(provide 'init-wrap-region)
