
(with-eval-after-load 'tramp
  ;; NB: `featurep' doesn't seem to work in that use case
  (if (locate-library (format "%s" 'tramp-container))
      (require 'tramp-container)
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
          (shell-command-to-string command)))

      ;; (with-eval-after-load 'tramp
      ;;   (let ((docker-tramp-method "crictl")
      ;;         (docker-tramp-docker-executable "crictl"))
      ;;     (docker-tramp-add-method)
      ;;     ;; NB: this doesn't work for remote paths
      ;;     ;; indeed, `docker-tramp--running-containers' relies on `process-lines' which isn't TRAMP-aware
      ;;     ;; (tramp-set-completion-function docker-tramp-method docker-tramp-completion-function-alist)
      ;;     ))
      )))


(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))




(provide 'init-docker)
