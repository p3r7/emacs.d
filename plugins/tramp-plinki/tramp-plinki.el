
(require 'dash)

(defvar tramp-plinki-ppk nil)


;; ------------------------------------------------------------------------
;; MODIFY EXISTING TRAMP METHODS

(defun tramp-plinki--add-certificate-login-arg (tramp-login-args)
  (let ((login-args (car (cdr tramp-login-args))))
    (if (string= "" tramp-plinki-ppk)
        tramp-login-args
      (add-to-list 'login-args `("-i" ,(concat "\"" tramp-plinki-ppk "\"")))
      `(tramp-login-args ,login-args))))

;; REVIEW: seems to eval whole method-def-args, which is unwanted
(defun tramp-plinki--add-certificate-login-arg-to-method (tramp-method-def)
  (let ((method-name (car tramp-method-def))
        (method-def-args (cdr tramp-method-def)))
    (cons method-name
          (-map-when
           (lambda (e) (equal (car e) 'tramp-login-args))
           #'tramp-plinki--add-certificate-login-arg
           method-def-args))))

(defun tramp-plinki--get-enriched-tramp-methods ()
  (-map-when
   (lambda (e) (member (car e) '("pscp" "plink")))
   #'tramp-plinki--add-certificate-login-arg-to-method
   tramp-methods))

(defun tramp-plinki-enrich-existing ()
  (setq tramp-methods (tramp-plinki--get-enriched-tramp-methods)))


;; ------------------------------------------------------------------------
;; REGISTER NEW TRAMP METHODS

(defun tramp-plinki-register-new ()
  (when (not tramp-plinki-ppk)
    (error "empty value for tramp-plinki-ppk"))

  (add-to-list 'tramp-methods
	       `("plinki"
		 (tramp-login-program        "plink")
		 ;; ("%h") must be a single element, see `tramp-compute-multi-hops'.
		 (tramp-login-args           (("-l" "%u") ("-P" "%p") ("-ssh") ("-t")
					      ("-i" ,(concat "\"" tramp-plinki-ppk "\""))
					      ("%h") ("\"")
					      (,(format
						 "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
						 tramp-terminal-type
						 tramp-initial-end-of-output))
					      ("/bin/sh") ("\"")))
		 (tramp-remote-shell         "/bin/sh")
		 (tramp-remote-shell-login   ("-l"))
		 (tramp-remote-shell-args    ("-c"))))
  (add-to-list 'tramp-methods
	       `("pscpi"
		 (tramp-login-program        "plink")
		 (tramp-login-args           (("-l" "%u") ("-P" "%p") ("-ssh") ("-t")
					      ("-i" ,(concat "\"" tramp-plinki-ppk "\""))
					      ("%h") ("\"")
					      (,(format
						 "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
						 tramp-terminal-type
						 tramp-initial-end-of-output))
					      ("/bin/sh") ("\"")))
		 (tramp-remote-shell         "/bin/sh")
		 (tramp-remote-shell-login   ("-l"))
		 (tramp-remote-shell-args    ("-c"))
		 (tramp-copy-program         "pscp")
		 (tramp-copy-args            (("-l" "%u") ("-P" "%p") ("-scp") ("-p" "%k")
					      ("-q") ("-r")))
		 (tramp-copy-keep-date       t)
		 (tramp-copy-recursive       t)))

  (eval-after-load 'tramp
    '(progn
       (tramp-set-completion-function "plinki" tramp-completion-function-alist-ssh)
       (tramp-set-completion-function "pscpi" tramp-completion-function-alist-ssh))))


(provide 'tramp-plinki)
