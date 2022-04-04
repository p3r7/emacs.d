;;; osc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "osc" "osc.el" (0 0 0 0))
;;; Generated autoloads from osc.el

(autoload 'osc-make-client "osc" "\
Create an OSC client process which talks to HOST and PORT.

\(fn HOST PORT)" nil nil)

(autoload 'osc-send-message "osc" "\
Send an OSC message from PROCESS to the specified PATH with ARGS.

\(fn PROCESS PATH &rest ARGS)" nil nil)

(autoload 'osc-send-bundle "osc" "\
Send a bundle to PROCESS with timetag TIME and MESSAGES as payload.

\(fn PROCESS TIME &rest MESSAGES)" nil nil)

(autoload 'osc-make-server "osc" "\
Create an OSC server which listens on HOST and PORT.
DEFAULT-HANDLER is a function with arguments (path &rest args) which is called
when a new OSC message arrives.  See `osc-server-set-handler' for more
fine grained control.
A process object is returned which can be dicarded with `delete-process'.

\(fn HOST PORT DEFAULT-HANDLER)" nil nil)

(register-definition-prefixes "osc" '("osc-"))

;;;***

;;;### (autoloads nil nil ("osc-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; osc-autoloads.el ends here
