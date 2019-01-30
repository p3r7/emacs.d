;; [[http://jblevins.org/projects/markdown-mode/]]
(when (prf/require-plugin 'markdown-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -t html5 --mathjax --highlight-style pygments --standalone"))

  ;; closer look to org-mode
  (face-spec-set
   'markdown-table-face
   '((t :inherit org-table))
   'face-defface-spec)
  (face-spec-set
   'markdown-header-face-1
   '((t :inherit outline-1))
   'face-defface-spec)
  (face-spec-set
   'markdown-header-face-2
   '((t :inherit outline-2))
   'face-defface-spec)
  (face-spec-set
   'markdown-header-face-3
   '((t :inherit outline-3))
   'face-defface-spec)
  (face-spec-set
   'markdown-header-face-4
   '((t :inherit outline-4))
   'face-defface-spec)
  (face-spec-set
   'markdown-header-face-5
   '((t :inherit outline-5))
   'face-defface-spec)

  ;; https://nicolas.petton.fr/ressources/presentation-stripped6.html
  (font-lock-add-keywords
   'markdown-mode
   '(("\\(<mark>\\)\\([^<]+?\\)\\(</mark>\\)" (2 highlight)))))

(provide 'init-markdown)
