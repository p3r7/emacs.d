
(defvar tramp-plinki-ppk nil)


;; ------------------------------------------------------------------------
;; NEW TRAMP METHODS

(defun tramp-plinki--register ()
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
