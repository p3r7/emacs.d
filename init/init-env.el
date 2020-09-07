


(require 's)



;; OS DETECTION

(defun windows-nt-p ()
  (string-equal system-type "windows-nt"))
(defun darwin-p ()
  (string-equal system-type "darwin"))
(defun gnu/linux-p ()
  (string-equal system-type "gnu/linux"))



;; ENV VARS

;; NB: When using emacs systemd user service, .profile doesn't get loaded.

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (add-to-list 'exec-path-from-shell-variables "TERM")
  (add-to-list 'exec-path-from-shell-variables "REAL_TERM")
  (unless (windows-nt-p)
    (exec-path-from-shell-initialize)))


(defun override-env-vars (env-vars)
  "Override env vars with new ENV-VARS.
This is usefull for when Emacs in launched as a systemd service, in which case the user environment is not retrieved.
To be called when launching emacsclient:

    $ emacsclient -c -e \"(override-env-vars \\\"$(env | egrep '(TERM|REAL_TERM)' | paste -s -d: -)\\\")\"

NB: some env vars have emacs var bindings (e.g. PATH -> `exec-path').
These are not handles here.
"
  (let ((env-vars (s-split ":" env-vars)))
    (mapc
     (lambda (env-var)
       (let* ((k_v (s-split-up-to "=" env-var 1))
              (k (car k_v))
              (v (cadr k_v)))
         (setenv k v)))
     env-vars)))



;; OS-related customs

(cond
 ((gnu/linux-p)
  (require 'init-linux))
 ((windows-nt-p)
  (require 'init-w32)
  ))



;; HW DETECTION

(defun prf/current-frame-terminal-type ()
  "Return the type of the current display terminal for frame.
See the documentation of `framep' for possible return values."
  (terminal-live-p (frame-terminal)))

(defun dec-vt100-compatible-term-p ()
  (or (fboundp 'vt100-wide-mode)
      (dec-term-p)))

(defun dec-term-p ()
  (let ((term (getenv "TERM")))
    (member term '("vt100" "vt102" "vt125"
		   "vt200" "vt201" "vt220" "vt240"
		   "vt300" "vt320"
		   "vt400" "vt420"))))



;; HW-related customs

(defun prf/customize-helm-tty-faces (&optional current-frame)
  (let ((current-frame (if current-frame current-frame (selected-frame))))
    (set-face-attribute 'helm-selection (selected-frame)
                        :inverse-video t
                        :background nil
                        :foreground nil
                        :distant-foreground nil)))

(defun prf/customize-helm-tty-faces-maybe (&optional current-frame)
  (if (facep 'helm-selection)
      (prf/customize-helm-tty-faces current-frame)
    ;; REVIEW: test? (featurep 'helm)
    (eval-after-load 'helm
      (prf/customize-helm-tty-faces current-frame))))

(defun prf/customize-tty-faces (&optional current-frame)
  (let ((current-frame (if current-frame current-frame (selected-frame))))
    (prf/customize-helm-tty-faces-maybe current-frame)
    (set-face-attribute 'font-lock-function-name-face current-frame
                        :inverse-video nil)
    (set-face-attribute 'hl-line current-frame
                        :inherit nil)
    (set-face-attribute 'region current-frame
                        :foreground nil
                        :background nil
                        :inverse-video t)
    (set-face-attribute 'font-lock-constant-face current-frame
                        :underline nil)))

(defun prf/hw/set-dec-term-keys ()
  (interactive)
  ;; works
  (define-key input-decode-map "	" (kbd "TAB"))

  ;; NB: do C-q then the keypresses, than translate appropraitely
  ;; e.g. <C-up> gives [1;5A] that corresponds to \e[1;5A

  (define-key input-decode-map "\e[1;5P" (kbd "<C-f1>"))
  ;; (define-key input-decode-map "\e[1;5q" (kbd "<C-f2>"))
  (define-key input-decode-map "\e[15;5~" (kbd "<C-f5>"))
  (define-key input-decode-map "\e[21;5~" (kbd "<C-f10>"))

  (define-key input-decode-map "\e[1;5A" (kbd "<C-up>"))
  (define-key input-decode-map "\e[1;5B" (kbd "<C-down>"))
  (define-key input-decode-map "\e[1;5D" (kbd "<C-left>"))
  (define-key input-decode-map "\e[1;5C" (kbd "<C-right>"))

  (define-key input-decode-map "\e[1;3D" (kbd "<M-left>"))
  (define-key input-decode-map "\e[1;3C" (kbd "<M-right>")))

;; NB: TTY detection is based on value of (getenv "TERM")
;; It is pretty old-school elisp code, the entry point is
;; `tty-run-terminal-initialization'.
;; It get runs in lisp/startup.el as well as in
;; lisp/faces.el.

(defun prf/tty-setup-hook ()
  (when (dec-vt100-compatible-term-p)
    (prf/hw/set-dec-term-keys)))

(defun prf/tty-setup-frame-hook (&optional current-frame)
  (prf/customize-tty-faces current-frame))

(add-hook 'tty-setup-hook #'prf/tty-setup-hook)
;; BUG: not working, done in client-init.el instead
;; (add-hook 'after-make-frame-functions #'prf/tty-setup-frame-hook)



;; LOCAL SETUP

(setq homedir-truename (directory-file-name (file-truename "~")))

(setq prf/system-name system-name)

;; case Android
(when (and (string= system-name "localhost")
	   (gnu/linux-p)
	   (executable-find "getprop"))
  (shell-command-to-string "getprop net.hostname"))

(setq prf/init/host-feature
      (intern
       (concat "init-host-"
	       (if (windows-nt-p) (downcase prf/system-name) prf/system-name))))

(if (prf/plugin-available-locally-p prf/init/host-feature)
    (require prf/init/host-feature))

(unless (boundp 'prf-backup-dir)
  (setq prf-backup-dir "~/.emacs.d/.saves"))
(unless (boundp 'prf-auto-save-dir)
  (setq prf-auto-save-dir "~/.emacs.d/.saves"))




(provide 'init-env)
