
;; NB: env built using MINGW, like MSYS


;; VARS

(defvar git-bash-cmd-root "C:/Program Files/Git/cmd") ; contains only git
(defvar git-bash-bin-root "C:/Program Files/Git/bin") ; contains only git + bash

(setq prf/local-shell-bin/git-bash (concat git-bash-bin-root "/bash.exe"))



;; SHELLS

(when (executable-find prf/local-shell-bin/git-bash)
  (with-eval-after-load 'friendly-shell
    (defun prf/shell/git-bash (&optional path)
      (interactive)
      (friendly-shell :path path :interpreter prf/local-shell-bin/git-bash))))




(provide 'init-git-bash-integration)
