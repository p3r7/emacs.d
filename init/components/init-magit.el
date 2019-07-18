
;; (magit-debug-git-executable)

(use-package magit
  :defer 2
  ;; :custom
  ;; (magit-git-executable "C:/cygwin64/usr/libexec/git-core/git.exe")
  ;; (magit-git-executable "c:/cygwin64/bin/git.exe")
  ;; (magit-git-executable "C:/Program Files/Git/cmd/git.EXE")
  :bind (:map dired-mode-map
	      ("C-x g" . magit-status))
  :init
  (setq magit-auto-revert-mode 1
	magit-last-seen-setup-instructions "1.4.0")
  (add-to-list 'display-buffer-alist '("^magit:\\(.*\\)$" display-buffer-same-window)))

(provide 'init-magit)
