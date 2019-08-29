
(use-package lusty-explorer
  :load-path "~/.emacs.d/plugins-spe/lusty-explorer-prf"
  :init
  (setq lusty--completion-ignored-regexps '("^\\*tramp/.*\\*$"
					    "^\\*Help\\*$"
					    "^\\*Messages\\*$"
					    "^\\*Completions\\*$"
					    "^\\*Bookmark List\\*$"
					    "^\\*Ibuffer\\*$"
					    "^\\*Compile-Log\\*$"
					    "^\\*Ediff Registry\\*$"
					    "^\\*Occur\\*$"
					    "^\\*vc\\*$"
					    "^\\*helm M-x\\*$"
					    "^\\*magit:.*$")
	lusty--shell-open-here-fun #'prf/tramp/shell_gow)
  :bind (("C-x C-f" . lusty-file-explorer)
	 ("C-x f" . lusty-file-explorer)
	 ("C-x b" . lusty-buffer-explorer)
	 ("C-x C-b" . lusty-buffer-explorer)))

(provide 'init-lusty)
