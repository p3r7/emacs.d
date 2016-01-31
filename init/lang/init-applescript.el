
(when (prf/require-plugin 'applescript-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode)))

(provide 'init-applescript)
