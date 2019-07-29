
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
