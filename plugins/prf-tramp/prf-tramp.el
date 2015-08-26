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
;;    (require 'prf-tramp)

;;; Code:

;; TODO: add test file exists
;; TODO: use e.g. (file-remote-p default-directory)  instead of prf/tramp/path/remote-p

(require 'tramp)
(require 'tramp-sh)

;; ------------------------------------------------------------------------

;; NOTE FOR WIN NT
;; I recommand using .exe extension to allow setting different explicit-<PROGRAM>-args for remote bash and local bash.exe

(defvar prf/tramp/default-local-shell-bin shell-file-name)
(defvar prf/tramp/default-remote-shell-bin "/bin/bash")
(defvar prf/tramp/default-remote-shell-bin-args '("-c" "export EMACS=; stty echo; bash"))
(defvar prf/tramp/local-shell-bin/bash nil)
(defvar prf/tramp/local-shell-bin/cmd nil)
(defvar prf/tramp/default-shell-buffer-name "*shell*")


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



(defun prf/tramp/path/normalize (path)
  ;; could have used convert-standard-filename or executable-find (for local files) as well to give a coherent output
  ;; we instead to a simple backslash substitution
  (subst-char-in-string ?\\ ?/ path)
  )

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
      "/"
      )
    )
  )


(defun prf/tramp/get-shellBin-name (shellBin)
  (if (string-match "^\\(.*\\)/\\(.*\\)$" shellBin)
      (match-string 2 shellBin)
    shellBin
    )
  )
(defun prf/tramp/get-shellBin-name-noExt (shellBin)
  (if (string-match "^\\(.*\\)/\\(.*\\)\\.\\(.*\\)$" shellBin)
      (match-string 2 shellBin)
    (if (string-match "^\\(.*\\)/\\(.*\\)$" shellBin)
	(match-string 2 shellBin)
      (if (string-match "^\\(.*\\)\\.\\(.*\\)$" shellBin)
	  (match-string 1 shellBin)
	  shellBin
	)
      )
    )
  )

(defun prf/tramp/generate-buffer-name-local-shell (shellBin)
  (concat "*"
	  (if (eq shellBin nil)
	      prf/tramp/default-shell-buffer-name
	    (prf/tramp/get-shellBin-name shellBin)
	    )
	  "*")
  )

(defun prf/tramp/generate-buffer-name-remote-shell (path)
  (setq prf/tramp/path/vec (tramp-dissect-file-name path))
  (setq prf/tramp/path/method (tramp-file-name-method prf/tramp/path/vec))
  (setq prf/tramp/path/user (tramp-file-name-user prf/tramp/path/vec))
  (setq prf/tramp/path/host (tramp-file-name-host prf/tramp/path/vec))
  (setq prf/tramp/path/localname (tramp-file-name-localname prf/tramp/path/vec))

  (concat "*" prf/tramp/path/user "@" prf/tramp/path/host "*")
  )



;; ------------------------------------------------------------------------

(defun prf/tramp/shell (&optional path shellBin shellArgs)
  "Create a shell at given path, using given shell binary"
  (interactive)
  (with-temp-buffer
    (let ((path (if path path default-directory)))
      ;; TODO: test file exists
      (setq prf/tramp/path/is-remote (prf/tramp/path/remote-p path))
      (cd path)

      (setq shellBin (if shellBin shellBin (if prf/tramp/path/is-remote prf/tramp/default-remote-shell-bin shell-file-name)))
      (setq shellBin (prf/tramp/path/normalize shellBin))
      (setq shellBinName (prf/tramp/get-shellBin-name shellBin))
      (setq explicitShellBinArgsVarName (concat "explicit-" shellBinName "-args"))
      ;; (message (concat "shellBinPath=" shellBin))
      ;; (message (concat "shellBinName=" shellBinName))
      ;; TODO: use shellArgs potentially defined for a given shellBin
      ;; (setq shellArgs (if shellArgs shellArgs (if (symbol-value (intern explicitShellBinArgsVarName)) (symbol-value (intern explicitShellBinArgsVarName)) (if prf/tramp/path/is-remote prf/tramp/default-remote-shell-bin-args nil))))
      (setq shellArgs (if shellArgs shellArgs (if prf/tramp/path/is-remote prf/tramp/default-remote-shell-bin-args nil)))

      (let (current-prefix-arg explicit-shell-file-name shell-file-name
			       (intern explicitShellBinArgsVarName)
			       comint-process-echoes prf/tramp/buffer-name)
	    (setq current-prefix-arg '(4))
	    (setq explicit-shell-file-name shellBin)
	    (setq shell-file-name shellBin)
	    ;; NOTE: not changing SHELL env variable, as global and not buffer-only
	    (set (intern explicitShellBinArgsVarName) shellArgs)
	    (setq comint-process-echoes t)
	    (setq prf/tramp/buffer-name (if prf/tramp/path/is-remote (prf/tramp/generate-buffer-name-remote-shell path) (prf/tramp/generate-buffer-name-local-shell shellBin)))

	    ;; TODO: name shell according to path and shellBin
	    (shell (generate-new-buffer-name prf/tramp/buffer-name)))
      )
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


;; ------------------------------------------------------------------------


(defun prf/tramp/shell/bash (&optional path)
  (interactive)
  (prf/tramp/shell path prf/tramp/local-shell-bin/bash)
  )

(defun prf/tramp/shell/cmd (&optional path)
  (interactive)
  (prf/tramp/shell path prf/tramp/local-shell-bin/cmd)
  )

;; ------------------------------------------------------------------------

(provide 'prf-tramp)

;;; prf-tramp.el ends here.
