
;; ------------------------------------------------------------------------
;; OS DETECTION

(defun windows-nt-p ()
  (string-equal system-type "windows-nt"))
(defun darwin-p ()
  (string-equal system-type "darwin"))
(defun gnu/linux-p ()
  (string-equal system-type "gnu/linux"))


;; ------------------------------------------------------------------------
;; OS-related customs

(cond
 ((gnu/linux-p)
  (progn
    (setq
     tramp-default-method "ssh"
     )
    (require 'init-linux)
    )
  )
 ((windows-nt-p)
  (progn
    (setq tramp-default-method "pscp")
    ;; tramp-default-method "ssh"
    ;; (if (executable-find "ssh")
    ;; (setq tramp-default-method "scpx")
    ;; (setq tramp-default-method "pscp") )
    (require 'init-w32)
    )
  )
 )


;; ------------------------------------------------------------------------
;; HW DETECTION

(defun dec-vt100-compatible-term-p ()
  (fboundp 'vt100-wide-mode))

(defun dec-term-p ()
  (let ((term (getenv "TERM")))
    (member term '("vt100" "vt102" "vt125"
		   "vt200" "vt201" "vt220" "vt240"
		   "vt300" "vt320"
		   "vt400" "vt420"))))


;; ------------------------------------------------------------------------
;; HW-related customs

(defun prf/hw/set-dec-term-keys ()
  (interactive)
  ;; works
  (define-key input-decode-map "	" (kbd "TAB"))

  ;; NB: do C-q then the keypresses, than translate appropraitely
  ;; e.g. <C-up> gives [1;5A] that corresponds to \e[1;5A

  (define-key input-decode-map "\e[1;5P" (kbd "<C-f1>"))
  (define-key input-decode-map "\e[15;5~" (kbd "<C-f5>"))
  (define-key input-decode-map "\e[21;5~" (kbd "<C-f10>"))

  (define-key input-decode-map "\e[1;5A" (kbd "<C-up>"))
  (define-key input-decode-map "\e[1;5B" (kbd "<C-down>"))
  (define-key input-decode-map "\e[1;5D" (kbd "<C-left>"))
  (define-key input-decode-map "\e[1;5C" (kbd "<C-right>"))

  (define-key input-decode-map "\e[1;3D" (kbd "<M-left>"))
  (define-key input-decode-map "\e[1;3C" (kbd "<M-right>")))

(when (dec-vt100-compatible-term-p)
  (prf/hw/set-dec-term-keys))


;; ------------------------------------------------------------------------
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


;; ------------------------------------------------------------------------

(provide 'init-env)
