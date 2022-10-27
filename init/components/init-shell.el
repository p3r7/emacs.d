
(require 'rx)
(require 'org)


;; SHELL - MAIN

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
    (defalias '_sh 'friendly-shell))

  (org-link-set-parameters "sh"
                           :follow (lambda (path)
                                     (friendly-shell :path path)))
  (add-to-list 'prf/org-link-alt-type-alist (cons 'file 'sh)))

(use-package friendly-shell-command)

(use-package friendly-remote-shell
  :config
  (when (not (fboundp '_rsh))
    (defalias '_rsh 'friendly-remote-shell)))

(use-package shx
  :hook (shell-mode . shx-mode)
  :after shell

  :config

  (defun shx-cmd-stopall (args)
    "(SAFE) Stop ALL shx timers.
\nExamples:\n
  :stopall"
    (let ((shx-timer-list (shx--get-timer-list)))
      (--map (progn
               (cancel-timer it)
               (shx-insert "Stopped " 'font-lock-string-face
                           (shx--format-timer-string it)
                           "\n"))
             shx-timer-list)

      (shx-insert-timer-list)))

  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat "\\(\\<" it "\\>\\) .*\\'") 1 'font-lock-constant-face))
   '("bash" "sh"
     "wc"
     "df" "du" "dd"
     "top" "htop" "atop" "ctop" "ps_mem"
     "xargs" "echo" "cat" "find" "fd" "grep" "rg" "sed" "awk" "bc"
     "nano" "view" "vi"
     "ssh" "ssh-copy-id" "scp" "rsync"
     "nc" "netstat" "iptables" "tshark" "tcpdump"
     "python" "go" "php" "perl"
     "lein" "sass"
     "vagrant" "docker" "ansible" "ansible-galaxy" "kubectl" "k" "helm"))

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



;; SHELL - COMPLETION AT POINT

;; NB: all those kinda crap on me

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
  :disabled
  :after (auto-complete company)
  :config
  (push 'company-native-complete company-backends)
  (add-hook 'shell-mode-hook (lambda () (company-mode 1))))



;; TERM

(when (and module-file-suffix
           (executable-find "cmake"))
  (use-package vterm
    :init
    (setq vterm-always-compile-module t))

  (use-package vterm-toggle))




(provide 'init-shell)
