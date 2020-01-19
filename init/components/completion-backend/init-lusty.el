
(use-package lusty-explorer
  :load-path "~/.emacs.d/plugins-spe/lusty-explorer-prf"
  ;; :bind (("C-x C-f" . lusty-file-explorer)
  ;;        ("C-x f" . lusty-file-explorer)
  ;;        ("C-x b" . lusty-buffer-explorer)
  ;;        ("C-x C-b" . lusty-buffer-explorer))
  :init
  (setq lusty--completion-ignored-regexps '("^\\*tramp/.*\\*$"
					    "^\\*Help\\*$"
					    "^\\*helpful .*$"
					    ;; "^\\*scratch\\*$"
					    ;; "^\\*Messages\\*$"
					    "^\\*Completions\\*$"
					    "^\\*Bookmark List\\*$"
					    "^\\*Ibuffer\\*$"
					    "^\\*Compile-Log\\*$"
					    "^\\*Ediff Registry\\*$"
					    "^\\*Occur\\*$"
					    "^\\*vc\\*$"
					    "^\\*helm .*$"
					    "^\\*quelpa.*$"
                                            ;; older-style magit buffers
					    "^\\*magit:.*$"
                                            ;; newr style magit buffers
					    "^magit:.*$"
					    "^magit-diff:.*$"
					    "^magit-process:.*$")
	lusty--shell-open-here-fun #'prf/shell
	lusty--M-x-fun #'helm-M-x)
  :config
  (lusty-register-custom-explorer-action "launch-shell" #'prf/shell "C-x s")
  (lusty-register-custom-explorer-action "shell-command" #'shell-command "M-!")
  (lusty-register-custom-explorer-action "async-shell-command" #'async-shell-command "M-&")
  (lusty-register-custom-explorer-action "eval-expression" #'eval-expression "M-:")
  (lusty-register-custom-explorer-action "M-x" #'helm-M-x "M-x")
  (lusty-register-custom-explorer-action "magit-status" #'prf/magit-status-maybe "C-x g"))

(provide 'init-lusty)
