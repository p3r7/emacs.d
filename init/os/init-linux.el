

;; TRAMP

(setq tramp-default-method "ssh")



;; CLIPBOARD

;; NB: might not be necessary if using autocutsel
(setq
 x-select-enable-clipboard t
 interprogram-paste-function 'x-cut-buffer-or-selection-value
 save-interprogram-paste-before-kill t)



;; DAEMON / SERVER

(when (daemonp)
  (defun prf/emacs-daemon-reload ()
    (interactive)
    (let ((default-directory "~"))
      (save-some-buffers)
      (apply
       #'start-file-process
       "kill-emacs"
       "kill-emacs"
       (s-split " " "systemctl --user restart emacs"))))
  (global-set-key (kbd "C-x M-c") #'prf/emacs-daemon-reload))



;; FULLSCREEN (OLDER EMACS)

;; NOTE: does not work for some WM (e.g. xmonad)
;; BUG: tries to execute it on remote srv if tramp cnnx
(when (and (not (fboundp #'toggle-frame-fullscreen))
           (executable-find "wmctrl"))
  (defun prf/toggle-fullscreen-wmctrl ()
    "toggle full-screen mode"
    (interactive)
    (progn
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")))
  (global-set-key (kbd "<f11>") #'prf/toggle-fullscreen-wmctrl)
  (defalias 'toggle-frame-fullscreen #'prf/toggle-fullscreen-wmctrl))




(provide 'init-linux)
