;; [[http://jblevins.org/projects/markdown-mode/]]
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))


(provide 'init-markdown)
