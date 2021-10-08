
(require 'dash)
(require 's)



;; VARIOUS

(use-package hide-lines)

(use-package syslog-mode
  :load-path "~/.emacs.d/plugins-spe/syslog-mode-prf"
  :mode (".*\.log'" ".*\.log\..*\.gz'" "/var/log.*\\'"
	 "\\catalina.out\\'"))



;; DIRED

;; http://www.emacswiki.org/emacs/Sunrise_Commander

(require 'init-dired)



;; COMINT

(require 'init-comint)



;; TRAMP

(require 'init-tramp)

(defun prf/tramp/extract-remote-file-name (trampFilePath)
  (let (vec localname)
    (setq vec (tramp-dissect-file-name trampFilePath))
    (if vec
        (tramp-file-name-localname vec)
      ;; REVIEW: returning path unchanged instead of nil if not valid remote
      ;; .. do we really want this ?!
      trampFilePath)))


(defun prf/tramp/convert-remoteFilePath-currentSrv (currentFilePath remoteFilePath)
  "Format a path using current server address prefix and remote server file location"
  ;; prefix current srv

  (let (vec-remote localname-remote)
    (setq vec-remote (tramp-dissect-file-name remoteFilePath))
    (when vec-remote
      (setq localname-remote (tramp-file-name-localname vec-remote))
      (prf/tramp/path/with-new-localname currentFilePath localname-remote))))


;; TODO: make it work w/ shell buffers
(defun prf/tramp/visit-remoteFile-currentSrv ()
  "Try to go to the same location as other visible buffer"
  (interactive)
  (if (= (length (window-list)) 2)
      (progn

	;; - get current host buffer filename
	(if (and (buffer-file-name)
		 (file-exists-p (buffer-file-name)))
	    (setq my-current-filepath buffer-file-name)
	  (setq my-current-filepath default-directory))

	;; - go to sibbling host buffer
	(other-window 1)

	;; - get sibbling host buffer filepath
	(if (and (buffer-file-name)
		 (file-exists-p (buffer-file-name)))
	    (setq my-sibbling-filepath buffer-file-name)
	  (setq my-sibbling-filepath default-directory))

	;; - go back to current buffer
	(other-window 1)

	;; - calculate filepath for current host
	(setq my-new-filepath (prf/tramp/convert-remoteFilePath-currentSrv my-current-filepath my-sibbling-filepath))

	(if my-new-filepath
	    (progn
	      ;; - validate file exists
	      (if (file-exists-p my-new-filepath)
		  ;; - go to file
		  (if (file-directory-p my-new-filepath)
		      (dired my-new-filepath)
		    (find-file my-new-filepath))
		(message "buffer not a file")))
	  (message "one of buffers not a remote file")))
    (message "invalid number of visible buffers")))
(defalias '_t/vrm 'prf/tramp/visit-remoteFile-currentSrv)



;; SHELL

(require 'init-shell)

(defun sudoify-path (path)
  "Enrich PATH with a sudo multi-hop TRAMP part."
  (let* ((path (expand-file-name path))
         (remote-prefix (file-remote-p path)))

    (unless (file-exists-p path)
      (error "File '%s' doesn't appear to exist!" path))

    (when (and remote-prefix
               (s-contains? "|sudo:" path)
               (s-starts-with? "sudo:"
                               (cadr (s-split "|" path))))
      (error "Path already contains a sudo at the last multi-hop position"))

    (if remote-prefix
        (replace-regexp-in-string (concat "^" (regexp-quote remote-prefix))
                                  (concat (s-chop-suffix ":" remote-prefix) "|sudo::")
                                  path)
      (concat "/sudo::" path))))

(defun root-shell (&optional path)
  "Open a shell at current location as root."
  (interactive)
  (let ((path (or path default-directory)))

    (unless (and path
                 (file-exists-p path))
      (if (called-interactively-p)
          (error "Cannot open a root shell - current buffer is not visiting a file/directory!")
        (error "Cannot open a root shell - PATH not provided and current buffer is not visiting a file/directory!")))

    (friendly-shell :path (sudoify-path path))))

(defun ff-as-root (&optional path)
  "Open file at PATH as root."
  (interactive)
  (let* ((filename (buffer-file-name))
         (p (when filename
              (point)))
         (path (or path filename default-directory)))

    (unless (and path
                 (file-exists-p path))
      (if (called-interactively-p)
          (error "Cannot open file as root - current buffer is not visiting a file/directory!")
        (error "Cannot open file as root - PATH not provided and current buffer is not visiting a file/directory!")))

    (find-file (sudoify-path path))
    (when p
      (goto-char p))))


;;TODO: lotta stuff don't work as expected
(add-hook 'shell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f8>") (lambda nil (interactive) (syslog-mode)))
	    (local-set-key (kbd "<f7>") (erase-buffer))
	    (local-set-key (kbd "<f6>") (lambda nill (interactive) (progn
								(move-beginning-of-line)
								(set-mark)
								(move-end-of-line)
								(json-format))))))

(add-hook 'syslog-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f8>") (lambda nil (interactive) (progn
							       (shell-mode)
							       ;; (toggle-read-only)
							       (setq current-prefix-arg '(-1)) ; C-u
							       (call-interactively 'read-only-mode))))))


;; [[http://snarfed.org/why_i_run_shells_inside_emacs]]
;; TODO: make comments using pager work (apt-get install ?)
;; http://stackoverflow.com/questions/12166295/disable-all-paging-in-git
;; (setenv "PAGER" "cat")



;; TEMP SHELL BUFFERS

(add-to-list 'display-buffer-alist '("^Shell Command Output\\(.*\\)$" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^Async Shell Command\\(.*\\)$" display-buffer-same-window))



;; ESHELL

(require 'init-eshell)



;; FILESYSTEM HELPERS

(defun prf/get-buffer-filepath-complete ()
  (if (member major-mode '(dired-mode shell-mode))
      default-directory
    (buffer-file-name)))

(defun prf/get-buffer-filepath-clean ()
  (let ((filename (prf/get-buffer-filepath-complete)))
    (when filename
      (if (file-remote-p filename)
          (prf/tramp/extract-remote-file-name filename)
        filename))))

(defvar prf/exec-cmd nil "file-local variable to override exec command")

(defun prf/exec-cmd-eval (exec-cmd filename &optional shebang)
  (let ((dirname (file-name-directory filename)))
    ;; TODO: add parsing of #! and be able to refference it
    (s-replace-all `(("$FILEPATH" . ,filename)
                     ("$DIRNAME" . ,dirname)
                     ("$EXEC" . ,shebang))
                   exec-cmd)))


(defun prf/exec-from-shebang ()
  "Parses the shebang (#!) for current buffer.
Inspired by src of `shell-script-mode'"
  (when (save-excursion
          (goto-char (point-min))
          (looking-at "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
    (match-string 2)))

(defun prf/exec-based-on-filename (filename)
  "Determine the exec based on filename.
Inspired by src of `shell-script-mode'.
Modified to return nil instead of `sh-shell-file' as defautl value."
  (cond
   ((string-match "\\.m?spec\\'" filename) "rpm")
   ((string-match "[.]sh\\>"     filename) "sh")
   ((string-match "[.]bash\\>"   filename) "bash")
   ((string-match "[.]ksh\\>"    filename) "ksh")
   ((string-match "[.]mkshrc\\>" filename) "mksh")
   ((string-match "[.]t?csh\\(rc\\)?\\>" filename) "csh")
   ((string-match "[.]zsh\\(rc\\|env\\)?\\>" filename) "zsh")
   ((equal (file-name-nondirectory filename) ".profile") "sh")

   ;; not originally in `shell-script-mode'
   ((s-suffix? ".py" filename) "python") ;; REVIEW: should ideally test if in virtual env
   ((s-suffix? ".php" filename) "php")
   ((s-suffix? ".go" filename) "go run")))

(defun prf/exec-based-on-mode ()
  (cond
   ((bound-and-true-p ansible) "ansible-playbook")   ; NB: ansible-mode is named `ansible` ...
   ))

(defun prf/exec-complex-rule (filename)
  (cond
   ((and (member major-mode '(clojurec-mode clojure-mode clojurescript-mode))
         (clojure-project-root-path)
         (s-starts-with-p "test/" (s-replace (expand-file-name (clojure-project-root-path)) "" filename)))
    (concat "lein test " (clojure-find-ns)))))

(defun prf/get-buffer-filepath-with-exec ()
  (let ((clean-filename (prf/get-buffer-filepath-complete)))
    (when clean-filename
      (when (file-remote-p clean-filename)
        (setq clean-filename (prf/tramp/extract-remote-file-name clean-filename)))

      (let* ((exec-shebang (prf/exec-from-shebang))
             (exec-based-on-filename (prf/exec-based-on-filename clean-filename))
             (exec-based-on-mode (prf/exec-based-on-mode))
             (exec (or exec-shebang exec-based-on-filename exec-based-on-mode))
             (exec-complex-rule (prf/exec-complex-rule clean-filename)))

        (cond
         ;; NB: set as file-local var
         (prf/exec-cmd
          (prf/exec-cmd-eval prf/exec-cmd clean-filename exec-shebang))

         (exec-complex-rule
          exec-complex-rule)

         (exec
          (concat exec " " clean-filename)))))))

(defun prf/get-buffer-dirname ()
  (let ((filename (prf/get-buffer-filepath-clean)))
    (when filename
      (file-name-directory filename))))

(defun prf/get-buffer-filename ()
  (let ((filename (prf/get-buffer-filepath-complete)))
    (when filename
      (file-name-nondirectory filename))))

(defun prf/copy-buffer-filepath-to-clipboard-raw ()
  "Copy the current buffer file path to the clipboard (no sanitization)."
  (interactive)
  (let ((filename (prf/get-buffer-filepath-complete)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file path '%s' to the clipboard." filename))))

(defun prf/copy-buffer-filepath-to-clipboard-clean ()
  "Copy the current buffer file path to the clipboard (sanitized)."
  (interactive)
  (let ((filename (prf/get-buffer-filepath-clean)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file path '%s' to the clipboard." filename))))

(defun prf/copy-buffer-filepath-to-clipboard-with-exec ()
  "Copy the current buffer file path to the clipboard, with exec prefix set."
  (interactive)
  (let ((filename (prf/get-buffer-filepath-with-exec)))
    (when filename
      (kill-new filename)
      (message "Copied exec command '%s' to the clipboard." filename))))

(defun prf/copy-buffer-basename-to-clipboard ()
  "Copy the current buffer base name to the clipboard."
  (interactive)
  (let ((filename (prf/get-buffer-dirname)))
    (when filename
      (kill-new filename)
      (message "Copied buffer base name '%s' to the clipboard." filename))))

(defun prf/copy-buffer-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (prf/get-buffer-filename)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defalias '_cfp #'prf/copy-buffer-filepath-to-clipboard-clean)


(defun prf/ffap-cider ()
  (interactive)
  (-when-let* ((full-path-str (ffap-string-at-point))
               (split (s-split ":" full-path-str))
               (_ (= 3 (length split)))
               ((path line col) split)
               (line (string-to-number line))
               (col (string-to-number col))
               ((path-no-ext _ext) (s-split "\\\." path))
               (ns (s-replace "_" "-" path-no-ext)))
    ;; (cider-find-ns ns)
    (cider--find-ns ns)
    (goto-line line)
    (move-to-column col t)))


;; REVIEW: could use directly find-file-at-point aka ffap
(defun prf/find-file-at-point ()
  "Find file at point if it exists."
  (interactive)
  (let ((file (ffap-guess-file-name-at-point)))
    (cond (file
           (find-file file))

          ((and (bound-and-true-p shell-mode)
                (clojure-project-root-path)) ; is in a Clojure project
           (prf/ffap-cider)))))

(defalias '_ffap #'prf/find-file-at-point)


;; ------------------------------------------------------------------------
;; HYDRAS

(with-eval-after-load "hydra"
  (defhydra hydra-srvUtils (:color blue)
    "server utils"
    ("s" friendly-shell "shell")
    ("r" friendly-remote-shell "remote shell")
    ("a" helm-ansible-inventory-host-connect "remote shell (Ansible)")
    ("o" prf/tramp/visit-remoteFile-currentSrv "visit other version file")
    ("e" ediff-toggle "toggle ediff")
    ("f" prf/find-file-at-point "find at point")
    ("#" ff-as-root "as root")
    ("S" root-shell "root shell")
    ("g" nil "cancel"))

  (defhydra hydra-copyPath (:color blue)
    "copy path"
    ("c" prf/copy-buffer-filepath-to-clipboard-clean "clean")
    ("r" prf/copy-buffer-filepath-to-clipboard-raw "raw")
    ("f" prf/copy-buffer-filename-to-clipboard "file")
    ("b" prf/copy-buffer-basename-to-clipboard "base name")
    ("e" prf/copy-buffer-filepath-to-clipboard-with-exec "exec")
    ("g" nil "cancel")))




(provide 'init-srv-utils)
