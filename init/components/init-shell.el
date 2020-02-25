
(use-package shell
  :ensure nil
  :demand
  :after comint

  :config
  (when (>= emacs-major-version 25)
    (add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window))))


(use-package with-shell-interpreter
  :config
  (setq tramp-default-user "root"))


(use-package prf-shell-command
  :quelpa (prf-shell-command :fetcher github :repo "p3r7/prf-shell")
  :after with-shell-interpreter)


(use-package prf-shell
  :quelpa (prf-shell :fetcher github :repo "p3r7/prf-shell")
  :after with-shell-interpreter
  :config
  (when (not (fboundp '_sh))
    (defalias '_sh 'prf-shell)))


(use-package shx
  :hook (shell-mode . shx-mode)
  :after (shell with-shell-interpreter)

  :config

  (when (string-equal system-type "windows-nt")
    (defun prf/shx-insert-plot (filename plot-command line-style)
      "Prepare a plot of the data in FILENAME.
Use a gnuplot specific PLOT-COMMAND (for example 'plot') and
LINE-STYLE (for example 'w lp'); insert the plot in the buffer."
      (let* ((img-name (make-temp-file "tmp" nil ".png"))
             (status (call-process
                      shx-path-to-gnuplot nil t nil "-e"
                      (concat
                       "set term png transparent truecolor;"
                       "set border lw 3 lc rgb \""
                       (color-lighten-name (face-attribute 'default :foreground) 5)
                       "\"; set out \"" img-name "\";"
                       ;; PATCHED HERE
                       plot-command " " (shell-quote-argument filename) " "
                       line-style))))
        (when (zerop status) (shx-insert-image img-name))))
    (defalias #'shx-insert-plot #'prf/shx-insert-plot))

  ;; NB: as `with-shell-interpreter' does not local-set `explicit-shell-file-name', we force it
  (defadvice shx--validate-shell-file-name (around shx--validate-shell-file-name-default-remote-interpreter activate)
    "Set `explicit-shell-file-name' to `with-shell-interpreter-default-remote' if exists"
    (let ((remote-id (file-remote-p default-directory))
          (explicit-shell-file-name explicit-shell-file-name))
      (when (and remote-id
                 (file-exists-p (concat remote-id with-shell-interpreter-default-remote)))
        (setq explicit-shell-file-name with-shell-interpreter-default-remote))
      ad-do-it)))


(use-package sh-term
  :load-path "~/.emacs.d/plugins/sh-term"
  ;; :hook (shell-mode . shell-term-mode)
  :after (shell prf-tramp)

  :config
  (add-to-list 'shell-visual-commands "htop"))


(use-package readline-complete
  :disabled
  :ensure nil
  :demand
  :after (auto-complete company)
  :config

  (cond
   ((member prf/fav-completion-at-point '(ac auto-complete))
    (add-to-list 'ac-modes 'shell-mode)
    (add-hook 'shell-mode-hook
              (lambda ()
                (when (string= explicit-shell-file-name "/bin/bash")
                  ;; TODO: support other bash paths
                  (ac-rlc-setup-sources)))))
   ((eq prf/fav-completion-at-point 'company)
    (push 'company-readline company-backends)
    (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1))))))




(provide 'init-shell)
