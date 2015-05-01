;;; prf-tramp.el --- Wrapper around tramp -*- mode: emacs-lisp -*-

;; Copyright (C) 2015 Worney Renth <contact.perf@gmail.com>
;;
;; Version: 20130407.1256
;; X-Original-Version: 2.5
;; Created: July 27, 2010
;; Keywords: convenience, files, matching
;; Compatibility: GNU Emacs 22, 23, and 24
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; To install, copy this file somewhere in your load-path and add this line to
;; your .emacs:
;;
;;    (require 'prf/tramp)

;;; Code:

;; TODO: add file exists

(require 'tramp)
(require 'tramp-sh)

;; ------------------------------------------------------------------------

(defun prf/sys/touch (&optional filename)
  "touch specified file or current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (if filename filename (buffer-file-name)))))
  (clear-visited-file-modtime))


(defun prf/tramp/sanitize-path (path)
  "Used by remote-shell"
  ;; TODO: take also nto account https, ftp ...
  (if (string-match "^http://\\(.*\\)/$" path)
      (match-string 1 path)
    path
    )
  )

(setq tramp-default-user "root")

;; ------------------------------------------------------------------------


(defun prf/tramp/path/remote-p (path)
  (let ((match (string-match (nth 0 tramp-file-name-structure) path)))
    (if match
	't
      nil
      )
  ))

(defun prf/tramp/get-method-from-path (path)
  (if (string-match "^/\\(.*\\):" path)
      (match-string 1 path)
    nil
    )
  )

(defun prf/tramp/get-user-from-path (path)
  ;; TODO: make it work eventually w/ pipe proxy logic
  (if (string-match "^/\\(.*\\):\\(.*\\)@" path)
      (match-string 2 path)
    (if (string-match "\\(.*\\)@" path)
	(match-string 1 path)
	nil
      )
    )
  )

(defun prf/tramp/get-host-from-path (path)
  (if (string-match "@\\(.*\\):" path)
      (match-string 1 path)
    (if (string-match "^/\\(.*\\):\\(.*\\):" path)
	(match-string 2 path)
      path
      )
    )
  )

(defun prf/tramp/get-localname-from-path (path)
  (if (string-match "^/\\(.*\\):\\(.*\\):\\(.*\\)" path)
      (match-string 3 path)
    (if (string-match "^\\(.*\\):\\(.*\\)$" path)
	(match-string 2 path)
      nil
      )
    )
  )

(defun prf/tramp/generate-buffer-name-remote-shell (path)
  (setq prf/tramp/path/vec (tramp-dissect-file-name path))
  (setq prf/tramp/path/method (tramp-file-name-method prf/tramp/path/vec))
  (setq prf/tramp/path/user (tramp-file-name-method prf/tramp/path/user))
  (setq prf/tramp/path/host (tramp-file-name-method prf/tramp/path/host))
  (setq prf/tramp/path/localname (tramp-file-name-method prf/tramp/path/localname))

  (concat "*" prf/tramp/path/user "@" prf/tramp/path/host "*")
  )



;; ------------------------------------------------------------------------

(defun prf/tramp/shell (&optional path shellBin)
  "Create a shell at given path, using given shell binary"
  (interactive)
  (with-temp-buffer
    (let ((path (if path path default-directory)))
      ;; TODO: test file exists
      (setq prf/tramp/path/is-remote (prf/tramp/path/remote-p path))
      (cd path)
      )

    (let (
	  (current-prefix-arg '(4))
	  (explicit-shell-file-name (if shellBin shellBin (if prf/tramp/path/is-remote "/bin/bash" explicit-shell-file-name)))
	  (explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
	  (comint-process-echoes t)
	  (prf/tramp/buffer-name (if prf/tramp/path/is-remote (prf/tramp/generate-buffer-name-remote-shell path) "*shell*"))
	  )
      ;; TODO: name shell according to path and shellBin
      (shell (generate-new-buffer-name prf/tramp/buffer-name)))
      )
  )



(defun prf/tramp/remote-shell (&optional path shellBin)
  "Open a remote shell to a host."
  (interactive)
  (let ((path (if path path (read-string "Host: "))))

    (setq path (prf/tramp/sanitize-path path))

    (setq prf/tramp/path/method (prf/tramp/get-method-from-path path))
    (setq prf/tramp/path/user (prf/tramp/get-user-from-path path))
    (setq prf/tramp/path/host (prf/tramp/get-host-from-path path))
    (setq prf/tramp/path/localname (prf/tramp/get-localname-from-path path))

    (if (eq (length prf/tramp/path/method) 0)
	(setq prf/tramp/path/method tramp-default-method))
    (if (eq (length prf/tramp/path/user) 0)
	(setq prf/tramp/path/user tramp-default-user))
    (if (eq (length prf/tramp/path/localname) 0)
	(setq prf/tramp/path/localname "/"))

    (setq path
	  (tramp-make-tramp-file-name
	   prf/tramp/path/method
	   prf/tramp/path/user
	   prf/tramp/path/host
	   prf/tramp/path/localname))
    (prf/tramp/shell path)
    )
  )


(defun prf/tramp/remote-shell-old (&optional host)
  "Open a remote shell to a host."
  (interactive)
  ;; nice to have it callable => case we want to specify explicitely the host
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      ;; TODO: also test if word at point could be or contain a remote path

      (setq host (prf/tramp/sanitize-ip-path host))

      ;; USER
      (setq my-remote-shell-ip (split-string host "@" t))
      (if (= (length my-remote-shell-ip) 2) ;; user specified
	  (progn
	    (setq my-remote-shell-user (car my-remote-shell-ip))
	    (setq my-remote-shell-ip (car (cdr my-remote-shell-ip))) )

	(progn
	  (setq my-remote-shell-user "root")
	  (setq my-remote-shell-ip host) )
	)
      ;; (sys/touch (convert-standard-filename (concat homedir-truename "/.history_" my-remote-shell-ip)) )

      ;; PATH
      (setq my-remote-shell-ip (split-string my-remote-shell-ip ":" t))
      (if (= (length my-remote-shell-ip) 2)
	  (progn
	    (setq my-remote-shell-path (car (cdr my-remote-shell-ip)))
	    (setq my-remote-shell-ip (car my-remote-shell-ip)) )
	(progn
	  (setq my-remote-shell-ip (car my-remote-shell-ip))
	  (setq my-remote-shell-path "/") )
	)

      (cd (concat "/" tramp-default-method ":" my-remote-shell-user "@" my-remote-shell-ip ":" my-remote-shell-path))
      (let (
	    (current-prefix-arg '(4))
	    (explicit-shell-file-name "/bin/bash") ;; doesn't work case busybox, might need this line only case winNt ?
	    ;;(explicit-shell-file-name "/bin/sh")
	    ;; for autocomple:
	    (explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
	    (comint-process-echoes t)
	    )
	;;NOTE: we always generate a new shell
	(shell (generate-new-buffer-name (concat "*" my-remote-shell-user "@" my-remote-shell-ip "*")))
	;; NOTE: this won't work, as it would create the history file on remote server
	;; (setq comint-input-ring-file-name (concat homedir-truename "/.history_" my-remote-shell-ip))
	;; (comint-read-input-ring)
	)
      )))


;; ------------------------------------------------------------------------

;; TODO: move to TSL customs

(defun prf/tramp/shell/connect/vagrant@trusty32 (&optional path shellBin)
  (interactive)
  (prf/tramp/shell "/vagrant:trusty32_default:/usr/share/nginx/html/")
  )

(defun prf/tramp/shell/connect/root@trusty32 (&optional path shellBin)
  (interactive)
  (prf/tramp/shell "/vagrant:trusty32_default|sudo:trusty32_default:/usr/share/nginx/html/")
  )

(provide 'prf/tramp)

;;; prf-tramp.el ends here.
