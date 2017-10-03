;; [[http://jblevins.org/projects/markdown-mode/]]
(when (prf/require-plugin 'markdown-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -t html5 --mathjax --highlight-style pygments --standalone"))
  )

(provide 'init-markdown)
