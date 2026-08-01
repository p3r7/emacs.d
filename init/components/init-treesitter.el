
;; NB: dirty fix for bug#79687
;; from https://mwolson.org/blog/emacs/2026-04-20-fixing-typescript-ts-mode-in-emacs-30-2/
(load "~/.emacs.d/plugins-spe/treesit-predicate-rewrite/treesit-predicate-rewrite.el" nil nil nil t)

;; (setq treesit-language-source-alist
;;       '((tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
;;                     "v0.20.3"
;;                     "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
;;                     "v0.20.3"
;;                     "typescript/src")))



(provide 'init-treesitter)
