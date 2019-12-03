
;; NB: attempt to port eshell's em-term to shell-mode

;; TODO: port eshell-interpreter-alist into shell-interpreter-alist
;; TODO: port eshell-find-interpreter
;; TODO: find equivalent of eshell-interactive-output-p
;; TODO: port eshell-flatten-list / eshell-stringify-list (maybe from dash.el)


(require 'cl-lib)
(require 'esh-util)
(require 'esh-cmd)
(require 'esh-ext)

(when (featurep 'shx)
  (require 'shx))



;; VARS

(defgroup shell-term nil
  "This module causes visual commands (e.g., `vi') to be executed by
the `term' package, which comes with Emacs.  This package handles most
of the ANSI control codes, allowing curses-based applications to run
within an Emacs window.  The variable `shell-visual-commands' defines
which commands are considered visual in nature."
  :tag "Running visual commands"
  :prefix "shell-term-"
  :group 'shell)

(defvar shell-term-simple-input-sender (function comint-simple-send)
  "Function to actually send to PROCESS the STRING submitted by user.
When `shx' is active, we'll change it to `shx-filter-input'.")

(defcustom shell-term-load-hook nil
  "A list of functions to call when loading `shell-term'."
  :version "24.1"			; removed shell-term-initialize
  :type 'hook
  :group 'shell-term)

(defcustom shell-visual-commands
  '("vi"                                ; what is going on??
    "screen" "top"                      ; ok, a valid program...
    "less" "more"                       ; M-x view-file
    "lynx" "ncftp"                      ; w3.el, ange-ftp
    "pine" "tin" "trn" "elm")           ; GNUS!!
  "A list of commands that present their output in a visual fashion.

Commands listed here are run in a term buffer.

See also `shell-visual-subcommands' and `shell-visual-options'."
  :type '(repeat string)
  :group 'shell-term)

(defcustom shell-visual-subcommands
  nil
  "An alist of subcommands that present their output in a visual fashion.

An alist of the form

  ((COMMAND1 SUBCOMMAND1 SUBCOMMAND2...)
   (COMMAND2 SUBCOMMAND1 ...))

of commands with subcommands that present their output in a
visual fashion.  A likely entry is

  (\"git\" \"log\" \"diff\" \"show\")

because git shows logs and diffs using a pager by default.

See also `shell-visual-commands' and `shell-visual-options'."
  :type '(repeat (cons (string :tag "Command")
		       (repeat (string :tag "Subcommand"))))
  :version "24.4"
  :group 'shell-term)

(defcustom shell-visual-options
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

See also `shell-visual-commands' and `shell-visual-subcommands'."
  :type '(repeat (cons (string :tag "Command")
		       (repeat (string :tag "Option"))))
  :version "24.4"
  :group 'shell-term)

;; If you change this from term-term-name, you need to ensure that the
;; value you choose exists in the system's terminfo database.  (Bug#12485)
(defcustom shell-term-name term-term-name
  "Name to use for the TERM variable when running visual commands.
See `term-term-name' in term.el for more information on how this is
used."
  :version "24.3"	       ; eterm -> term-term-name = eterm-color
  :type 'string
  :group 'shell-term)

(defcustom shell-escape-control-x t
  "If non-nil, allow <C-x> to be handled by Emacs key in visual buffers.
See the variables `shell-visual-commands',
`shell-visual-subcommands', and `shell-visual-options'.  If
this variable is set to nil, <C-x> will send that control
character to the invoked process."
  :type 'boolean
  :group 'shell-term)

(defcustom shell-destroy-buffer-when-process-dies nil
  "If non-nil, term buffers are destroyed after their processes die.
WARNING: Setting this to non-nil may result in unexpected
behavior for short-lived processes, see bug#18108."
  :version "25.1"
  :type 'boolean
  :group 'shell-term)



;; INTERNAL VARS

(defvar shell-term-parent-buffer)



;; FUNCTIONS: COMINT FILTER

(defun shell-term--get-simple-input-sender ()
  (if shx-mode
      #'shx-filter-input
    #'comint-simple-send))

(defun shell-term--parse-input (input)
  ;; NB: using eshell parsing capabilities
  ;; FIXME: does not yes handle whell >>, 2>...
  (let* ((raw-parsed (eshell-parse-command input))
         (is-multiline (and (eq (car raw-parsed) 'progn)
                            (eq (cl-caadr raw-parsed) 'eshell-commands))))
    (if is-multiline
        (--map
         (if (eq (car it) 'eshell-commands)
             (shell-term--parse-input-line (cadr (cadr  it)))
           (shell-term--parse-input-line(cadr it)))
         (cdr raw-parsed))
      (list
       (shell-term--parse-input-line (cadr raw-parsed))))))

(defun shell-term--parse-input-line (parsed-line)
  ;; NB: input as format of (cadr (eshell-parse-command input))
  (let* ((is-pipeline (eq (car parsed-line) 'eshell-execute-pipeline))
         command-list)
    (if is-pipeline
        (--map (cons (cadr it) (list (cl-cdaddr it))) (cl-cadadr parsed-line))
      (list (list (cadr parsed-line) (cl-cdaddr parsed-line))))))


;; NB: setting var `comint-input-sender' to this function is equivalent to
;; the function `eshell-term-initialize' in em-term.
(defun shell-term-filter-input (process input)
  "Before sending to PROCESS, filter the INPUT.
That means, if INPUT is a shx-command, do that command instead.
This function overrides `comint-input-sender'."
  (let* ((parsed-command-list (shell-term--parse-input input))
         (is-multiline (< 1 (length parsed-command-list)))
         (last-line (car (last parsed-command-list)))
         (is-pipeline (< 1 (length last-line)))
         (last-pipeline-command (car (last last-line)))
         (command (car last-pipeline-command))
         (args (cadr last-pipeline-command))
         (simple-input-sender (shell-term--get-simple-input-sender)))
    (if (or (not command)
            (not (shell-visual-command-p command args))
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
;; (defun shell-term-initialize ()
;;   "Initialize the `term' interface code."
;;   (make-local-variable 'shell-interpreter-alist)
;;   (setq shell-interpreter-alist
;; 	(cons (cons #'shell-visual-command-p
;; 		    'shell-exec-visual)
;; 	      shell-interpreter-alist)))

(defun shell-visual-command-p (command args)
  "Returns non-nil when given a visual command.
If either COMMAND or a subcommand in ARGS (e.g. git log) is a
visual command, returns non-nil."
  (let ((command (file-name-nondirectory command)))
    (and
     ;; (eshell-interactive-output-p)
     (or (member command shell-visual-commands)
         (member (car args)
                 (cdr (assoc command shell-visual-subcommands)))
         (cl-intersection args
                          (cdr (assoc command shell-visual-options))
                          :test 'string=)))))

(defun shell-exec-visual (&rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.
ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (let* (shell-interpreter-alist
	 (interp (eshell-find-interpreter (car args) (cdr args)))
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
      (set (make-local-variable 'term-term-name) shell-term-name)
      (make-local-variable 'shell-term-parent-buffer)
      (setq shell-term-parent-buffer shell-buf)
      (term-exec term-buf program program nil args)
      (let ((proc (get-buffer-process term-buf)))
	(if (and proc (eq 'run (process-status proc)))
	    (set-process-sentinel proc 'shell-term-sentinel)
	  (error "Failed to invoke visual command")))
      (term-char-mode)
      (if shell-escape-control-x
	  (term-set-escape-char ?\C-x))))
  nil)

;; Process sentinels receive two arguments.
(defun shell-term-sentinel (proc msg)
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
            (let ((buf (and (boundp 'shell-term-parent-buffer)
                            shell-term-parent-buffer
                            (buffer-live-p shell-term-parent-buffer)
                            shell-term-parent-buffer)))
              (if buf
                  (switch-to-buffer buf))))
        (kill-buffer proc-buf)))))



;; MINOR MODE

(defcustom shell-term-mode-lighter " sht"
  "Lighter for the shell-term minor mode."
  :type 'string
  :group 'shell-term)


(define-minor-mode shell-term-mode
  "Toggle shell-term-mode on or off.
\nThis minor mode allows intercepting visual commands in
shell-mode to have them play in a term buffer."
  :lighter shell-term-mode-lighter
  ;; :keymap shell-term-mode-map
  (if shell-term-mode (shell-term--activate) (shell-term--deactivate)))


(defun shell-term--activate ()
  (unless (derived-mode-p 'shell-mode)
    (error "WARNING: shx is incompatible with `%s'" major-mode))

  (setq-local shell-term-parent-buffer (current-buffer))
  ;; do this one with a delay because spacemacs tries to set this variable too:
  (shell-term--asynch-funcall (lambda () (setq comint-input-sender 'shell-term-filter-input)))
  ;; (shx--advise)
  )

(defun shell-term--deactivate ()
  (if shx-mode
      (setq comint-input-sender 'shx-filter-input)
    (setq comint-input-sender 'comint-simple-send)))



;; UTILS

(defun shell-term--asynch-funcall (function &optional args delay)
  "Run FUNCTION with ARGS in the buffer after a short DELAY."
  (run-at-time (or delay 0.3) nil
               `(lambda ()
                  (with-current-buffer ,shell-term-parent-buffer ,(cons function args)))))




(provide 'sh-term)
