
;; ------------------------------------------------------------------------
;; NEW TRAMP METHODS

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
	       (tramp-copy-program         "kscp")
	       (tramp-copy-args            (("-l" "%u") ("-P" "%p") ("-scp") ("-p" "%k")
					    ("-q") ("-r")))
	       (tramp-copy-keep-date       t)
	       (tramp-copy-recursive       t)))

(eval-after-load 'tramp
  '(progn
     (tramp-set-completion-function "klink" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function "klinkx" tramp-completion-function-alist-putty)
     (tramp-set-completion-function "kscp" tramp-completion-function-alist-ssh)))


;; ------------------------------------------------------------------------
;; UTILS FUNCTIONS

(defun tramp-kitty-get-session-list-from-conf-dir ()
  (let ((kitty-bin-path (executable-find "kitty"))
	(kitty-dir)
	(session-list))
    (when kitty-bin-path
      (setq kitty-dir (file-name-directory kitty-bin-path))
      (setq session-list (directory-files (concat kitty-dir "/Sessions")))
      (setq session-list (delete "."  session-list))
      (setq session-list (delete ".."  session-list))
      (setq session-list (delete "Default%20Settings"  session-list))
      session-list)))


;; ------------------------------------------------------------------------
;; HELM INTEGRATION

;; TODO: allow both dired and shell
;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/30/More-adventures-in-helm-more-than-one-action/
(setq tramp-kitty-session-helm-source
      `((name . "Open KiTTY session")
        (candidates . tramp-kitty-get-session-list)
        (action . (
		   ("dired" .
		    (lambda (candidate)
		      (dired (concat "/klinkx:" candidate ":/"))))
		   ;; NB: not working
		   ("shell" .
		    (lambda (candidate)
		      (prf/tramp/remote-shell (concat "/klinkx:" candidate ":/"))))
		   ("debug" .
		    (lambda (candidate)
		      (message-box (concat "selected: %s" candidate))))))))

(eval-after-load "helm"
  '(defun helm-kitty-session-connect ()
     (interactive)
     (helm :sources '(tramp-kitty-session-helm-source))))


(provide 'tramp-kitty)
