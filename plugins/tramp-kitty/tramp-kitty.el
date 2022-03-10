
(require 'dash)
(require 'prf-string)


;;  VARS

(defvar tramp-kitty-session-map-cache nil)
(defvar tramp-kitty-session-map-cache-set-time nil)


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


;; UTILS FUNCTIONS

(defun tramp-kitty-get-kitty-session-dir ()
  (let ((kitty-bin-path (executable-find "kitty")))
    (when kitty-bin-path
      (file-name-directory kitty-bin-path))))

(defun tramp-kitty-get-session-list-from-conf-dir ()
  (let ((kitty-dir (tramp-kitty-get-kitty-session-dir))
        (session-list))
    (when kitty-dir
      (setq session-list (directory-files (concat kitty-dir "/Sessions")))
      (setq session-list (delete "."  session-list))
      (setq session-list (delete ".."  session-list))
      (setq session-list (delete "Default%20Settings"  session-list))
      session-list)))

(defun tramp-kitty-get-session-map-from-conf-dir ()
  (let ((kitty-dir (tramp-kitty-get-kitty-session-dir))
        (session-list (tramp-kitty-get-session-list-from-conf-dir)))
    (mapcar
     (lambda (session)
       `(,session . ,(tramp-kitty-parse-conf-file (concat kitty-dir "/Sessions/" session))))
     session-list)))

(defun tramp-kitty-parse-conf-file (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (let ((confLines (split-string (buffer-string) "\n" t)))
      (mapcar #'tramp-kitty-parse-conf-line confLines))))

(defun tramp-kitty-parse-conf-line (confLine)
  "Parse a line of kitty conf."
  (when (string-match (concat "^\\(.*\\)" (regexp-quote "\\") "\\(.*\\)" (regexp-quote "\\")) confLine)
    `(,(match-string 1 confLine) . ,(prf/url/decode (match-string 2 confLine)))))

(defun tramp-kitty-get-all-props-for-session (sessionMap session)
  (cdr (assoc session sessionMap)))

(defun tramp-kitty-get-session-prop (sessionPropMap prop)
  (cdr (assoc prop sessionPropMap)))

(defun tramp-kitty-get-prop-for-session (sessionMap session prop)
  (tramp-kitty-get-session-prop (tramp-kitty-get-all-props-for-session session sessionMap) prop))

(defun tramp-kitty-set-session-map-cache ()
  (setq tramp-kitty-session-map-cache (tramp-kitty-get-session-map-from-conf-dir)
        tramp-kitty-session-map-cache-set-time (current-time))
  (message "KiTTY Sessions cache set"))

(defun tramp-kitty-get-all-props-for-session-from-cache (session)
  (tramp-kitty-get-all-props-for-session tramp-kitty-session-map-cache))

(defun tramp-kitty-get-prop-for-session-from-cache (session prop)
  (tramp-kitty-get-prop-for-session tramp-kitty-session-map-cache session prop))

(defmacro tramp-kitty--make-vec-match-predicate (vec)
  (let* ((user (tramp-file-name-user vec))
         (host (tramp-file-name-host vec))
         (kitty-sess-hostname (string-join (remove nil `(,user ,host)) "@")))
    `(lambda (session)
       (equal ,kitty-sess-hostname (tramp-kitty-get-session-prop (cdr session) "HostName")))))

;; (defun tramp-kitty--make-vec-match-predicate (vec)
;;   (let* ((user (tramp-file-name-user vec))
;;          (host (tramp-file-name-host vec))
;;          (kitty-sess-hostname (string-join (remove nil `(,user ,host)) "@")))
;;     (lambda (session) (equal kitty-sess-hostname (tramp-kitty-get-session-prop (cdr session) "HostName")))))

(defun tramp-kitty-get-sessions-in-cache-matching-vec (vec)
  (noflet ((predicate (tramp-kitty--make-vec-match-predicate vec)))
    (-filter #'predicate
             tramp-kitty-session-map-cache)))


;; HELM INTEGRATION

;; TODO: allow both dired and shell
;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/30/More-adventures-in-helm-more-than-one-action/
(setq tramp-kitty-session-helm-source
      `((name . "Open KiTTY session")
        (candidates . tramp-kitty-get-session-list-from-conf-dir)
        (action . (("dired" .
		    (lambda (candidate)
		      (dired (concat "/klinkx:" candidate ":/"))))
		   ;; NB: not working
		   ("shell" .
		    (lambda (candidate)
		      (friendly-shell :path (concat "/klinkx:" candidate ":/"))))
                   ("kitty" .
		    (lambda (candidate)
		      (start-process (concat "kitty<" candidate ">") nil "kitty" "-load" candidate)))
		   ("debug" .
		    (lambda (candidate)
		      (message-box (concat "selected: %s" candidate))))))))

(eval-after-load 'helm
  '(defun helm-kitty-session-connect ()
     (interactive)
     (helm :sources '(tramp-kitty-session-helm-source))))




(provide 'tramp-kitty)
