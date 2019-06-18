
(add-to-list 'tramp-methods
	     `("klink"
	       (tramp-login-program        "klink")
	       ;; ("%h") must be a single element, see `tramp-compute-multi-hops'.
	       (tramp-login-args           (("-l" "%u") ("-P" "%p") ("-ssh") ("-t")
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
	     `("klinkx"
	       (tramp-login-program        "klink")
	       (tramp-login-args           (("-load") ("%h") ("-t") ("\"")
					    (,(format
					       "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
					       tramp-terminal-type
					       tramp-initial-end-of-output))
					    ("/bin/sh") ("\"")))
	       (tramp-remote-shell         "/bin/sh")
	       (tramp-remote-shell-login   ("-l"))
	       (tramp-remote-shell-args    ("-c"))))
(add-to-list 'tramp-methods
	     `("kscp"
	       (tramp-login-program        "klink")
	       (tramp-login-args           (("-l" "%u") ("-P" "%p") ("-ssh") ("-t")
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
     (tramp-set-completion-function "klink" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function "klinkx" tramp-completion-function-alist-putty)
     (tramp-set-completion-function "kscp" tramp-completion-function-alist-ssh)))

(provide 'tramp-kitty)
