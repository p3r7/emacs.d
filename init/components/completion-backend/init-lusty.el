
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
                                            "^\\*straight-process\\*$"
                                            "^\\*kubel-process\\*$"
					    "^\\*kubel - .*$"
					    "^\\* docker .*$"
                                            "^\\*EGLOT .*\\*$"
                                            "^\\*Multi Buffer.*\\*$"
                                            ;; "^\\*ediff-diff\\*$"
                                            ;; "^\\*ediff-errors\\*$"

                                            ;; clojure
					    "^\\*nrepl-server .*$"

                                            ;; older-style magit buffers
					    "^\\*magit:.*$"
                                            ;; newer style magit buffers
					    "^magit:.*$"
					    "^magit-diff:.*$"
					    "^magit-process:.*$"
					    )
	lusty--shell-open-here-fun #'friendly-shell
	lusty--M-x-fun #'helm-M-x)
  :config

  (with-eval-after-load 'ibuffer
    (defun ibuffer-lusty-find-file (file &optional _wildcards)
      "Reimplementation of `ibuffer-find-file' calling `lusty-file-explorer'."
      (interactive
       (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                  (if (buffer-live-p buf)
                                      (with-current-buffer buf
                                        default-directory)
                                    default-directory))))
         (lusty-file-explorer))))
    (define-key ibuffer-mode-map [remap ibuffer-find-file] #'ibuffer-lusty-find-file))

  (lusty-register-custom-explorer-action "launch-shell" #'friendly-shell "C-x s")
  (lusty-register-custom-explorer-action "shell-command" #'shell-command "M-!")
  (lusty-register-custom-explorer-action "async-shell-command" #'async-shell-command "M-&")
  (lusty-register-custom-explorer-action "eval-expression" #'eval-expression "M-:")
  (lusty-register-custom-explorer-action "M-x" #'helm-M-x "M-x")
  (lusty-register-custom-explorer-action "magit-status" #'prf/magit-status-maybe "C-x g"))



(provide 'init-lusty)
