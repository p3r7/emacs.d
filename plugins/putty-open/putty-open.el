
(defvar putty-open-putty-exec "putty")
(defvar putty-open-putty-default-ppk nil)

(defvar putty-open-putty--session-methods '("plinkx"))
(defvar putty-open-putty--ssh-methods '("ssh" "sshx" "pscp" "plink"))



;; PROGRAM OPENING

(defun putty-open-session (session)
  (start-process (concat putty-open-putty-exec "<" session ">") nil putty-open-putty-exec host))


(cl-defun putty-open-host (host &key protocol port user ppk)
  (let ((host-arg host)
        plink-args)
    (setq protocol
          (cond ((string= protocol "telnet") "-telnet")
                ((string= protocol "rlogin") "-rlogin")
                ((string= protocol "raw") "-raw")
                ((string= protocol "serial") "-serial")
                (t "-ssh")))
    (when user
      (setq host-arg (concat user "@" host-arg)))
    (when port
      (add-to-list 'plink-args "-P" t)
      (add-to-list 'plink-args port t))
    (when ppk
      (add-to-list 'plink-args "-i" t)
      (add-to-list 'plink-args ppk t))
    (unless ppk
      (when putty-open-putty-default-ppk
        (add-to-list 'plink-args "-i" t)
        (add-to-list 'plink-args putty-open-putty-default-ppk t)))
    (apply #'start-process (concat putty-open-putty-exec "<" host ">") nil putty-open-putty-exec host-arg plink-args)))



;; TRAMP INTEGRATION

(defun putty-open-tramp-vec (vec)
  (let (method user host localname)
    (setq method (tramp-file-name-method vec))
    (setq user (tramp-file-name-user vec))
    (setq host (tramp-file-name-host vec))
    (setq localname (tramp-file-name-localname vec))

    (when (member method putty-open-putty--session-methods)
      (putty-open-session host))

    (when (member method putty-open-putty--ssh-methods)
      (putty-open-host host :user user))))


(defun putty-open ()
  (interactive)
  (if (file-remote-p default-directory)
      (let ((vec (tramp-dissect-file-name default-directory)))
        (putty-open-tramp-vec vec))
    (message "Current directory is not remote")))




(provide 'putty-open)
