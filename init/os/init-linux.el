
;; ------------------------------------------------------------------------
;; CLIPBOARD

;; NB: might not be necessary if using autocutsel
(setq
 x-select-enable-clipboard t
 interprogram-paste-function 'x-cut-buffer-or-selection-value
 save-interprogram-paste-before-kill t)


;; ------------------------------------------------------------------------
;; FULLSCREEN (OLDER EMACS)

;; NOTE: does not work for some WM (e.g. xmonad)
;; BUG: tries to execute it on remote srv if tramp cnnx
(when (and (not (fboundp 'toggle-frame-fullscreen))  (executable-find "wmctrl"))
  (defun prf/toggle-fullscreen-wmctrl ()
    "toggle full-screen mode"
    (interactive)
    (progn
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")))
  (global-set-key (kbd "<f11>")  'prf/toggle-fullscreen-wmctrl)
  (defalias '_sh 'toggle-frame-fullscreen))


;; ------------------------------------------------------------------------

(provide 'init-linux)
