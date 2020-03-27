;;; sh-term.el --- Port of eshell's em-term to shell-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/sh-term
;; Package-Requires: ((cl-lib "0.6.1"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;  -----------
;;
;; Port of eshell's em-term to shell-mode.
;;
;; For detailed instructions, please look at the README.md at https://github.com/p3r7/sh-term/blob/master/README.md

;;; Code:


;; TODO: port eshell-interpreter-alist into shell-interpreter-alist
;; TODO: port eshell-find-interpreter
;; TODO: find equivalent of eshell-interactive-output-p
;; TODO: port eshell-flatten-list / eshell-stringify-list (maybe from dash.el)


;; REQUIRES


(require 'term)
(require 'cl-lib)

(require 'esh-util)
(require 'esh-cmd)
(require 'esh-ext)

(require 'prf-tramp)

(when (featurep 'shx)
  (require 'shx))



;; VARS

(defgroup sh-term nil
  "This module causes visual commands (e.g., `vi') to be executed by
the `term' package, which comes with Emacs.  This package handles most
of the ANSI control codes, allowing curses-based applications to run
within an Emacs window.  The variable `sh-term-visual-commands' defines
which commands are considered visual in nature."
  :tag "Running visual commands"
  :prefix "sh-term-"
  :group 'shell)

(defvar sh-term-simple-input-sender (function comint-simple-send)
  "Function to actually send to PROCESS the STRING submitted by user.
When `shx' is active, we'll change it to `shx-filter-input'.")

(defcustom sh-term-load-hook nil
  "A list of functions to call when loading `sh-term'."
  :version "24.1"			; removed sh-term-initialize
  :type 'hook
  :group 'sh-term)

(defcustom sh-term-visual-commands
  '("vi"                                ; what is going on??
    "screen" "top" "htop"               ; ok, a valid program...
    "less" "more"                       ; M-x view-file
    "lynx" "ncftp"                      ; w3.el, ange-ftp
    "pine" "tin" "trn" "elm")           ; GNUS!!
  "A list of commands that present their output in a visual fashion.

Commands listed here are run in a term buffer.

See also `sh-term-visual-subcommands' and `sh-term-visual-options'."
  :type '(repeat string)
  :group 'sh-term)

(defcustom sh-term-visual-subcommands
  nil
  "An alist of subcommands that present their output in a visual fashion.

An alist of the form

  ((COMMAND1 SUBCOMMAND1 SUBCOMMAND2...)
   (COMMAND2 SUBCOMMAND1 ...))

of commands with subcommands that present their output in a
visual fashion.  A likely entry is

  (\"git\" \"log\" \"diff\" \"show\")

because git shows logs and diffs using a pager by default.

See also `sh-term-visual-commands' and `sh-term-visual-options'."
  :type '(repeat (cons (string :tag "Command")
		       (repeat (string :tag "Subcommand"))))
  :version "24.4"
  :group 'sh-term)

(defcustom sh-term-visual-options
  nil
  "An alist of the form

  ((COMMAND1 OPTION1 OPTION2...)
   (COMMAND2 OPTION1 ...))

of commands with options that present their output in a visual
fashion.  For example, a sensible entry would be

  (\"git\" \"--help\" \"--paginate\")

because \"git <command> --help\" shows the command's
documentation with a pager and \"git --paginate <command>\"
always uses a pager for output.

See also `sh-term-visual-commands' and `sh-term-visual-subcommands'."
  :type '(repeat (cons (string :tag "Command")
		       (repeat (string :tag "Option"))))
  :version "24.4"
  :group 'sh-term)

;; If you change this from term-term-name, you need to ensure that the
;; value you choose exists in the system's terminfo database.  (Bug#12485)
(defcustom sh-term-name term-term-name
  "Name to use for the TERM variable when running visual commands.
See `term-term-name' in term.el for more information on how this is
used."
  :version "24.3"                   ; eterm -> term-term-name = eterm-color
  :type 'string
  :group 'sh-term)

(defcustom shell-escape-control-x t
  "If non-nil, allow <C-x> to be handled by Emacs key in visual buffers.
See the variables `sh-term-visual-commands',
`sh-term-visual-subcommands', and `sh-term-visual-options'.  If
this variable is set to nil, <C-x> will send that control
character to the invoked process."
  :type 'boolean
  :group 'sh-term)

(defcustom shell-destroy-buffer-when-process-dies nil
  "If non-nil, term buffers are destroyed after their processes die.
WARNING: Setting this to non-nil may result in unexpected
behavior for short-lived processes, see bug#18108."
  :version "25.1"
  :type 'boolean
  :group 'sh-term)



;; INTERNAL VARS

(defvar sh-term-parent-buffer nil "Shell buffer from which term buffer gets spawned")



;; FUNCTIONS: COMINT FILTER

(defun sh-term--get-simple-input-sender ()
  (if shx-mode
      #'shx-filter-input
    #'comint-simple-send))

(defun sh-term--parse-input (input)
  ;; NB: using eshell parsing capabilities
  ;; FIXME: does not yes handle whell >>, 2>...
  (let* ((raw-parsed (eshell-parse-command input))
         (is-multiline (and (eq (car raw-parsed) 'progn)
                            (eq (cl-caadr raw-parsed) 'eshell-commands))))
    (if is-multiline
        (--map
         (if (eq (car it) 'eshell-commands)
             (sh-term--parse-input-line (cadr (cadr  it)))
           (sh-term--parse-input-line(cadr it)))
         (cdr raw-parsed))
      (list
       (sh-term--parse-input-line (cadr raw-parsed))))))

(defun sh-term--parse-input-line (parsed-line)
  ;; NB: input as format of (cadr (eshell-parse-command input))
  (let* ((is-pipeline (eq (car parsed-line) 'eshell-execute-pipeline))
         command-list)
    (if is-pipeline
        (--map (cons (cadr it) (list (cl-cdaddr it))) (cl-cadadr parsed-line))
      (list (list (cadr parsed-line) (cl-cdaddr parsed-line))))))


;; NB: setting var `comint-input-sender' to this function is equivalent to
;; the function `eshell-term-initialize' in em-term.
(defun sh-term-filter-input (process input)
  "Before sending to PROCESS, filter the INPUT.
That means, if INPUT is a shx-command, do that command instead.
This function overrides `comint-input-sender'."
  (let* ((parsed-command-list (sh-term--parse-input input))
         (is-multiline (< 1 (length parsed-command-list)))
         (last-line (car (last parsed-command-list)))
         (is-pipeline (< 1 (length last-line)))
         (last-pipeline-command (car (last last-line)))
         (command (car last-pipeline-command))
         (args (cadr last-pipeline-command))
         (simple-input-sender (sh-term--get-simple-input-sender)))
    (if (or (not command)
            (not (sh-term-visual-command-p command args))
            ;; NB: not supporting remote terms as of now
            (tramp-tramp-file-p default-directory)
            ;; NB: not supporting multiline input, and will never be
            is-multiline
            ;; NB: not supporting piped commands as of now
            is-pipeline)
        (funcall simple-input-sender process input)
      (condition-case-unless-debug error-descriptor
          (progn
            (apply #'shell-exec-visual command args))
        ;; TODO: remove dependency to `shx-insert'
        (error (shx-insert 'error (error-message-string error-descriptor) "\n")))
      (with-current-buffer (process-buffer process)
        ;; advance the process mark to trick comint-mode
        (set-marker (process-mark process) (point)))
      ;; send a blank to fetch a new prompt
      (when (process-live-p process) (comint-send-string process "\n")))))



;; FUNCTIONS

;; NB: eshell-only feature, see `eshell-interpreter-alist'
;; (defun sh-term-initialize ()
;;   "Initialize the `term' interface code."
;;   (make-local-variable 'shell-interpreter-alist)
;;   (setq shell-interpreter-alist
;; 	(cons (cons #'sh-term-visual-command-p
;; 		    'shell-exec-visual)
;; 	      shell-interpreter-alist)))

(defun sh-term-visual-command-p (command args)
  "Returns non-nil when given a visual command.
If either COMMAND or a subcommand in ARGS (e.g. git log) is a
visual command, returns non-nil."
  (let ((command (file-name-nondirectory command)))
    (and
     ;; (eshell-interactive-output-p)
     (or (member command sh-term-visual-commands)
         (member (car args)
                 (cdr (assoc command sh-term-visual-subcommands)))
         (cl-intersection args
                          (cdr (assoc command sh-term-visual-options))
                          :test 'string=)))))

(defun shell-exec-visual (&rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.
ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (let* (shell-interpreter-alist
	 (interp (prf/tramp/executable-find (car args) (cdr args)))
	 (program (car interp))
	 (args (eshell-flatten-list
		(eshell-stringify-list (append (cdr interp)
					       (cdr args)))))
	 (term-buf
	  (generate-new-buffer
	   (concat "*" (file-name-nondirectory program) "*")))
	 (shell-buf (current-buffer)))
    (save-current-buffer
      (switch-to-buffer term-buf)
      (term-mode)
      (set (make-local-variable 'term-term-name) sh-term-name)
      (make-local-variable 'sh-term-parent-buffer)
      (setq sh-term-parent-buffer shell-buf)
      (term-exec term-buf program program nil args)
      (let ((proc (get-buffer-process term-buf)))
	(if (and proc (eq 'run (process-status proc)))
	    (set-process-sentinel proc 'sh-term-sentinel)
	  (error "Failed to invoke visual command")))
      (term-char-mode)
      (if shell-escape-control-x
	  (term-set-escape-char ?\C-x))))
  nil)

;; Process sentinels receive two arguments.
(defun sh-term-sentinel (proc msg)
  "Clean up the buffer visiting PROC.
If `shell-destroy-buffer-when-process-dies' is non-nil, destroy
the buffer."
  (term-sentinel proc msg) ;; First call the normal term sentinel.
  (when shell-destroy-buffer-when-process-dies
    (let ((proc-buf (process-buffer proc)))
      (when (and proc-buf (buffer-live-p proc-buf)
                 (not (eq 'run (process-status proc)))
                 (= (process-exit-status proc) 0))
        (if (eq (current-buffer) proc-buf)
            (let ((buf (and (boundp 'sh-term-parent-buffer)
                            sh-term-parent-buffer
                            (buffer-live-p sh-term-parent-buffer)
                            sh-term-parent-buffer)))
              (if buf
                  (switch-to-buffer buf))))
        (kill-buffer proc-buf)))))



;; MINOR MODE

(defcustom sh-term-mode-lighter " sht"
  "Lighter for the sh-term minor mode."
  :type 'string
  :group 'sh-term)


(define-minor-mode sh-term-mode
  "Toggle sh-term-mode on or off.
\nThis minor mode allows intercepting visual commands in
shell-mode to have them play in a term buffer."
  :lighter sh-term-mode-lighter
  ;; :keymap sh-term-mode-map
  (if sh-term-mode (sh-term--activate) (sh-term--deactivate)))


(defun sh-term--activate ()
  (unless (derived-mode-p 'shell-mode)
    (error "WARNING: shx is incompatible with `%s'" major-mode))

  (setq-local sh-term-parent-buffer (current-buffer))
  ;; do this one with a delay because spacemacs tries to set this variable too:
  (sh-term--asynch-funcall (lambda () (setq comint-input-sender 'sh-term-filter-input)))
  ;; (shx--advise)
  )

(defun sh-term--deactivate ()
  (if shx-mode
      (setq comint-input-sender 'shx-filter-input)
    (setq comint-input-sender 'comint-simple-send)))



;; UTILS

(defun sh-term--asynch-funcall (function &optional args delay)
  "Run FUNCTION with ARGS in the buffer after a short DELAY."
  (run-at-time (or delay 0.3) nil
               `(lambda ()
                  (with-current-buffer ,sh-term-parent-buffer ,(cons function args)))))

(defun sh-term--get-tramp-method-login-program (method)
  (cadr
   (--first
    (eq (car it) 'tramp-login-program)
    (cdr
     (--first (string= (car it) method)
              tramp-methods)))))

(defun sh-term--get-tramp-method-login-args (method)
  (cadr
   (--first
    (eq (car it) 'tramp-login-args)
    (cdr
     (--first (string= (car it) method)
              tramp-methods)))))

;; (defun sh-term--flatten-tramp-method-login-args (login-args host user port)
;;   (s-replace-all
;;    `(("%h" . ,host)
;;      ("%u" . ,user)
;;      ("%p" . ,port))
;;    (s-join " " (--map (s-join " " it) login-args))))

(defun sh-term--flatten-tramp-method-login-args (login-args host user port)
  (--map
   (s-replace-all
    `(("%h" . ,host)
      ("%u" . ,user)
      ("%p" . ,port))
    it)
   (-flatten login-args)))

(defun sh-term--shell-unquote-argument (argument)
  ;; REVIEW: join or get car ?
  ;; raise error if more than one element ?
  (s-join " "
          ;; if fails to parse as a quoted arg, return og value
          (condition-case _err
              (split-string-and-unquote argument)
            (end-of-file
             (list argument)))))

(defun sh-term--shell-unquote-args (args)
  (-map #'sh-term--shell-unquote-argument args))




(provide 'sh-term)
