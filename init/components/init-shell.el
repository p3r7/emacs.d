

;; SHELL

(use-package shell
  :ensure nil
  :demand
  :after comint

  :config
  (when (>= emacs-major-version 25)
    (add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window))))


(use-package friendly-shell
  :config
  (when (not (fboundp '_sh))
    (defalias '_sh 'friendly-shell)))

(use-package friendly-shell-command)

(use-package friendly-remote-shell
  :config
  (when (not (fboundp '_rsh))
    (defalias '_rsh 'friendly-remote-shell)))

(use-package shx
  ;; :disabled
  :hook (shell-mode . shx-mode)
  :after shell

  :config
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
  (add-to-list 'sh-term-visual-commands "htop"))


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


(use-package native-complete
  :disabled
  :config
  (with-eval-after-load 'shell
    (native-complete-setup-bash)))

(use-package company-native-complete
  :after (auto-complete company)
  :config
  (push 'company-native-complete company-backends)
  (add-hook 'shell-mode-hook (lambda () (company-mode 1))))



;; TERM

(when module-file-suffix
  (use-package vterm
    :init
    (setq vterm-always-compile-module t)))




(provide 'init-shell)
