
;; (magit-debug-git-executable)

(use-package magit
  :defer 5
  ;; :custom
  ;; (magit-git-executable "C:/cygwin64/usr/libexec/git-core/git.exe")
  ;; (magit-git-executable "c:/cygwin64/bin/git.exe")
  ;; (magit-git-executable "C:/Program Files/Git/cmd/git.EXE")

  :bind (
         :map dired-mode-map
         ("C-x g" . prf/magit-status-maybe)
         :map with-editor-mode-map
         ("C-x k" . with-editor-cancel)
         ("C-x C-k" . with-editor-cancel))

  :init
  (setq magit-auto-revert-mode 1
	magit-last-seen-setup-instructions "1.4.0")
  (add-to-list 'display-buffer-alist '("^magit:\\(.*\\)$" display-buffer-same-window))

  :config
  (defun prf/magit-status-maybe (&optional directory)
    (interactive)
    (unless directory
      (setq directory default-directory))
    (let ((toplevel (magit-toplevel directory)))
      (if (not toplevel)
          (message "Not a git repo")
        (magit-status)))))




(provide 'init-magit)
