;; [[http://jblevins.org/projects/markdown-mode/]]
(when (prf/require-plugin 'markdown-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
)

(provide 'init-markdown)
