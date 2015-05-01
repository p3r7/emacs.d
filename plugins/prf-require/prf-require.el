;;; prf-require.el --- Wrapper around tramp -*- mode: emacs-lisp -*-

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

;;; Commentary:
;;  -----------
;;
;; To install, copy this file somewhere in your load-path and add this line to
;; your .emacs:
;;
;;    (require 'prf-require)

;;; Code:


(when (not (fboundp 'prf/alert))
  (defun prf/alert (content &optional noerror)
    (if (not (null noerror))
	(message content)
      (error content))
    )
  )


(defun prf/plugin-available-locally-p (plugin)
  (locate-library (format "%s" plugin))
  )

(defun prf/require-plugin (plugin &optional filename noerror)
  "Require a PLUGIN, either using package.el or from local source.
FILENAME is PLUGIN containg dir."

  ;; - already loaded
  (if (featurep plugin)
      plugin

    ;; from filename
    (if (not (null filename))
	(prf/require-plugin-from-file plugin filename noerror)

      ;; from load-path
      (if (prf/plugin-available-locally-p plugin)
	  (require plugin nil noerror)

	;; from package.el
	(prf/require-plugin-from-package plugin noerror)
	)
      )
    )
  )


(defun prf/require-plugin-from-file (plugin &optional filename noerror)
  "Require a PLUGIN, from local source.
FILENAME is PLUGIN containg dir."
  (if (file-exists-p filename)
      (progn
	(setq prf/require/og-load-path load-path)
	(add-to-list 'load-path (expand-file-name filename))
	(if (prf/plugin-available-locally-p plugin)
	    (require plugin nil noerror)
	  (progn
	    (prf/alert (format "FILENAME %s does not provide PLUGIN %s" filename plugin) noerror)
	    (setq load-path prf/require/og-load-path)
	    nil)
	  )
	)
    (progn
      (prf/alert (format "FILENAME %s not found" filename) noerror)
      nil)
    )
  )


(defun prf/require-plugin-from-package (plugin &optional noerror)
  "Require a PLUGIN, using package.el.
We assume that package name is equal to PLUGIN"
  (if (prf/install-package plugin nil nil noerror)
      (require plugin nil noerror)
    nil)
  )


;; stolen from https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el
(defun prf/install-package (package &optional min-version no-refresh noerror)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (condition-case err
      (if (package-installed-p package min-version)
	  t
	(if (or (assoc package package-archive-contents) no-refresh)
	    (package-install package)
	  (progn
	    (package-refresh-contents)
	    (require-package package min-version t))))
    (error
     (prf/alert (format "Couldn't install package `%s': %S" package err) noerror)
     nil)
    )
  )


;; ------------------------------------------------------------------------

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(prf\\/require-plugin\\|prf\\/install-package\\)\\_>[ 	']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 'font-lock-keyword-face)
    (2 font-lock-constant-face nil t))
 ))


;; ------------------------------------------------------------------------

(provide 'prf-require)

;;; prf-require.el ends here.
