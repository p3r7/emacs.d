
(setq lusty--completion-ignored-regexps '(
					  "^\\*tramp/pscp.*\\*$"
					  "^\\*Help\\*$"
					  "^\\*Messages\\*$"
					  "^\\*Completions\\*$"
					  "^\\*Completions\\*$"
					  "^\\*Bookmark List\\*$"
					  "^\\*Ibuffer\\*$"
					  "^\\*Compile-Log\\*$"
					  "^\\*Ediff Registry\\*$"
					  "^\\*Occur\\*$"
					  "^\\*vc\\*$"
					  ))

;; patched version to allow filtering of buffers
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins-spe/lusty-explorer-prf"))
(when (require 'lusty-explorer nil 'noerror)
  (global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
  (global-set-key (kbd "C-x f") 'lusty-file-explorer)
  (global-set-key (kbd "C-x b")   'lusty-buffer-explorer)
  (global-set-key (kbd "C-x C-b")   'lusty-buffer-explorer)
  )

(provide 'init-lusty)
