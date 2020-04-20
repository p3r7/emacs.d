


;; VARS

(defvar mingw-bin-root "C:/MinGW/bin")
(defvar mingw-msys-bin-root "C:/MinGW/msys/1.0/bin")

(setq prf/local-shell-bin/msys-bash (concat mingw-msys-bin-root "/bash.exe"))



;; SHELLS

(when (executable-find prf/local-shell-bin/msys-bash)
  (with-eval-after-load 'friendly-shell
    (defun prf/shell/msys-bash (&optional path)
      (interactive)
      (friendly-shell :path path :interpreter prf/local-shell-bin/msys-bash
                      :interpreter-args '("-c" "export MSYSTEM=MSYS;stty echo; bash")
                      :buffer-name "bash(msys)"))

    (defun prf/shell/mingw32-bash (&optional path)
      (interactive)
      (friendly-shell :path path :interpreter prf/local-shell-bin/msys-bash
                      :interpreter-args '("-c" "export MSYSTEM=MINGW32;stty echo; bash")
                      :buffer-name "bash(mingw32)"))

    (defun prf/shell/mingw64-bash (&optional path)
      (interactive)
      (friendly-shell :path path :interpreter prf/local-shell-bin/msys-bash
                      :interpreter-args '("-c" "export MSYSTEM=MINGW64;stty echo; bash")
                      :buffer-name "bash(mingw64)"))))




(provide 'init-msys-integration)
