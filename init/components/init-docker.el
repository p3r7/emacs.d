

(use-package docker
  :bind ("C-c d" . docker)

  :config
  ;; NB: witing for https://github.com/Silex/docker.el/pull/125
  (defun docker-utils-shell-command-to-string (command)
    "Execute shell command COMMAND and return its output as a string.
Wrap the function `shell-command-to-string', ensuring variable `shell-file-name' behaves properly."
    (let* ((shell-file-name (if (and (eq system-type 'windows-nt)
                                     (not (file-remote-p default-directory)))
                                "cmdproxy.exe"
                              "/bin/sh")))
      (shell-command-to-string command))))


(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))




(provide 'init-docker)
