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
	 (program interp)
         (_ (message (concat "program: " program)))
         (args (eshell-flatten-list
	        (eshell-stringify-list
                 (cdr args)
                 ;; (append program (cdr args))
                 )))
         (_ (message "args: %s" args))
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



;; PRIVATE HELPERS: GENERIC

(defun sh-term--asynch-funcall (function &optional args delay)
  "Run FUNCTION with ARGS in the buffer after a short DELAY."
  (run-at-time (or delay 0.3) nil
               `(lambda ()
                  (with-current-buffer ,sh-term-parent-buffer ,(cons function args)))))



;; PRIVATE HELPERS: TRAMP METHODS

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



;; PRIVATE HELPERS: SHELL

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




;; REMOTE TERM (w/ TRAMP)

;; NB: taken directly from https://github.com/cuspymd/tramp-term.el
;; but attempting to address:
;; - `tramp-default-method' other than ssh
;; - multi-hops

(defvar tramp-term-after-initialized-hook nil
  "Hook called after tramp has been initialized on the remote host.
Hooks should expect a single arg which contains the
hostname used to connect to the remote machine.")

;;;###autoload
(defun tramp-term (&optional host-arg)
  "Create an `ansi-term` running ssh session.
And automatically enable tramp integration in that terminal.
Optional argument HOST-ARG is a list or one or two elements,
the last of which is the host name."
  (interactive)
  (let* ((host (or host-arg (tramp-term--select-host)))
         (hostname (car (last host)))
         (_ (message "hostname: %s" hostname))
         (prompt-bound nil))
    (if (or (> (length host) 2)
            (eql (length hostname) 0))
        (message "Invalid host string")
      (unless (eql (catch 'tramp-term--abort (tramp-term--do-ssh-login host)) 'tramp-term--abort)
        (tramp-term--initialize hostname)
        (run-hook-with-args 'tramp-term-after-initialized-hook hostname)
        (message "tramp-term initialized")))))

;; I imagine TRAMP has utility functions that would replace most of
;; this.  Needs investigation.
(defun tramp-term--do-ssh-login (host)
  "Perform the ssh login at HOST."
  (let* ((user "")
         (hostname (car (last host))))
    (when (= (length host) 2)
      (setq user (format "%s@" (car host))))
    (tramp-term--create-term hostname tramp-default-method (format "%s%s" user hostname)))
  (save-excursion
    (let ((bound 0))
      (while (not (tramp-term--find-shell-prompt bound))
        (let ((yesno-prompt (tramp-term--find-yesno-prompt bound))
              (passwd-prompt (tramp-term--find-passwd-prompt bound))
              (service-unknown (tramp-term--find-service-unknown bound)))
          (cond (yesno-prompt
                 (tramp-term--confirm)
                 (setq bound (1+ yesno-prompt)))
                (passwd-prompt
                 (tramp-term--handle-passwd-prompt)
                 (setq bound (1+ passwd-prompt)))
                (service-unknown (throw 'tramp-term--abort 'tramp-term--abort))
                (t (sleep-for 0.1))))))))

(defun tramp-term--find-shell-prompt (bound)
  "Find shell prompt with a buffer position BOUND."
  (re-search-backward tramp-shell-prompt-pattern bound t))

(defun tramp-term--find-yesno-prompt (bound)
  "Find yesno prompt with a buffer position BOUND."
  (re-search-backward tramp-yesno-prompt-regexp bound t))

(defun tramp-term--find-passwd-prompt (bound)
  "Find password prompt with a buffer position BOUND."
  (re-search-backward tramp-password-prompt-regexp bound t))

(defun tramp-term--find-service-unknown (bound)
  "Find service unknown with a buffer position BOUND."
  (re-search-backward "Name or service not known" bound t))

(defun tramp-term--handle-passwd-prompt ()
  "Read a password from the user and sends it to the server."
  (term-send-raw-string
   (concat (read-passwd "Password: ") (kbd "RET"))))

(defun tramp-term--confirm ()
  "Prompts the user to continue, aborts if they decline."
  (if (yes-or-no-p "Continue? ")
      (term-send-raw-string (concat "yes" (kbd "RET")))
    (term-send-raw-string (concat "no" (kbd "RET")))
    (throw 'tramp-term--abort 'tramp-term--abort)))

(defun tramp-term--initialize (hostname)
  "Send bash commands to set up tramp integration for HOSTNAME."
  (term-send-raw-string (format "
function set-eterm-dir {
    echo -e \"\\033AnSiTu\" \"%s$USER\"
    echo -e \"\\033AnSiTc\" \"$PWD\"
    echo -e \"\\033AnSiTh\" \"%s\"
    history -a
}
PROMPT_COMMAND=\"${PROMPT_COMMAND:+$PROMPT_COMMAND ;} set-eterm-dir\"
clear
" (if (version= emacs-version "26.1") (concat tramp-default-method ":") "") hostname)))

(defun tramp-term--select-host ()
  "Return a host from a list of hosts."
  (let* ((crm-separator "@")
         (default-host (tramp-term-default-host)))
    (completing-read-multiple
     (tramp-term-prompt default-host)
     (tramp-term--parse-hosts "~/.ssh/config")
     nil
     nil
     nil
     nil
     default-host
     )))

(defun tramp-term-prompt (default-host)
  "Make prompt string with DEFAULT-HOST."
  (let ((default-string (when default-host
                          (format " (default %s)" default-host))))
    (concat "[user@]host" default-string ": ")))

(defun tramp-term-default-host ()
  "Return default host based on `default-directory` which is a tramp file."
  (when (tramp-tramp-file-p default-directory)
    (let* ((user (file-remote-p default-directory 'user))
           (host (file-remote-p default-directory 'host)))
      (if user (format "%s@%s" user host) host))))

(defun tramp-term--parse-hosts (ssh-config)
  "Parse any host directives from SSH-CONFIG file and return them as a list of strings."
  (mapcar 'cadr (delete nil (tramp-parse-sconfig ssh-config))))

(defun tramp-term--create-term (new-buffer-name cmd &rest switches)
  "Create an `ansi-term` running an arbitrary CMD with NEW-BUFFER-NAME.
Including extra parameters SWITCHES."
  (let ((new-buffer-name (generate-new-buffer-name (format "*%s*" new-buffer-name))))
    (with-current-buffer (make-term new-buffer-name cmd nil (car switches))
      (rename-buffer new-buffer-name)   ; Undo the extra "*"s that
                                        ; make-term insists on adding
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x))
    (switch-to-buffer new-buffer-name)))




(provide 'sh-term)
