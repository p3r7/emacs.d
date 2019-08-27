
(require 's)

;; ------------------------------------------------------------------------
;; various

(use-package hide-lines)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins-spe/syslog-mode-prf"))
(use-package syslog-mode
  :load-path "~/.emacs.d/plugins-spe/syslog-mode-prf"
  :mode (".*\.log'" ".*\.log\..*\.gz'" "/var/log.*\\'"
	 "\\catalina.out\\'"))


;; ------------------------------------------------------------------------
;; DIRED

;; http://www.emacswiki.org/emacs/Sunrise_Commander

(require 'init-dired)

;; ------------------------------------------------------------------------
;; GENERAL COMINT

(setq
 ;; comint-completion-autolist t ;; ???
 comint-input-ignoredups t ;; no duplicate history
 comint-input-ring-size 5000 ;; history size
 ;; comint-completion-addsuffix t ;; might conflict w/ autocomplete
 comint-buffer-maximum-size 20000
 comint-scroll-to-bottom-on-input t)


;; ------------------------------------------------------------------------
;; TRAMP

(require 'init-tramp)

(defun prf/tramp/extract-remote-file-name (trampFilePath)
  (let (vec localname)
    (setq vec (tramp-dissect-file-name trampFilePath))
    (if vec
	(tramp-file-name-localname vec)
      ;; REVIEW: returning path unchanged instead of nil if not valid remote
      .. do we really want this ?!
      trampFilePath)))


(defun prf/tramp/convert-remoteFilePath-currentSrv (currentFilePath remoteFilePath)
  "Format a path using current server address prefix and remote server file location"
  ;; prefix current srv

  (let (my-prefix my-suffix
		  vec-current method-current user-current host-current localname-current
		  vec-remote method-remote user-remote host-remote localname-remote)
    (setq vec-current (tramp-dissect-file-name currentFilePath))
    (setq vec-remote (tramp-dissect-file-name remoteFilePath))

    (when (and vec-current vec-remote)
      (setq method-current (tramp-file-name-method vec-current))
      (setq user-current (tramp-file-name-user vec-current))
      (setq host-current (tramp-file-name-host vec-current))

      (setq localname-remote (tramp-file-name-localname vec-remote))

      (setq my-prefix (concat "/" method-current ":" (string-join (remove nil `(,user-current ,host-current)) "@") ":"))
      (setq my-suffix localname-remote)

      (concat my-prefix my-suffix))))


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


;; ------------------------------------------------------------------------
;; SHELL

(when (>= emacs-major-version 25)
  (add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window)))

(defun local-root-shell ()
  (interactive)
  (with-temp-buffer
    (cd "/sudo::")
    (let ((current-prefix-arg '(4)))
      (shell (generate-new-buffer-name "*root@localhost*")))
    (cd dd-old)))

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

;; [[#readline-complete]] -> auto-complete in shell
;; (setq explicit-shell-file-name "bash"
;;       explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
;;       comint-process-echoes t)



;; ------------------------------------------------------------------------
;; ESHELL

;; TODO: em-smart ? plan9 concepts ported to emacs shells
;; http://www.opensource.apple.com/source/emacs/emacs-51/emacs/lisp/eshell/em-smart.el

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq global-hl-line-mode nil)))


;; ------------------------------------------------------------------------
;; FILESYSTEM HELPERS


(defun prf/get-buffer-filepath-complete ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun prf/get-buffer-filepath-clean ()
  (let ((filename (prf/get-buffer-filepath-complete)))
    (when filename
      (when (file-remote-p filename)
	(prf/tramp/extract-remote-file-name filename))
	filename)))

(defun prf/get-buffer-filepath-with-exec ()
  (let ((clean-filename (prf/get-buffer-filepath-complete)))
    (when clean-filename
      (when (file-remote-p clean-filename)
	(setq clean-filename (prf/tramp/extract-remote-file-name clean-filename)))
      (cond ((bound-and-true-p ansible) (concat "ansible-playbook " clean-filename)) ;; NB: ansible-mode is named `ansible` ...
	    ((s-suffix? ".php" clean-filename) (concat "php " clean-filename))
	    ((s-suffix? ".py" clean-filename) (concat "python " clean-filename))) ;; REVIEW: should ideally test if in virtual env
      )))

(defun prf/get-buffer-dirname ()
  (let ((filename (prf/get-buffer-filepath-clean)))
    (when filename
      (file-name-directory filename))))

(defun prf/get-buffer-filename ()
  (let ((filename (prf/get-buffer-filepath-clean)))
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

; REVIEW: could use directly find-file-at-point aka ffap
(defun prf/find-file-at-point ()
  "Find file at point if it exists."
  (interactive)
  (let ((file (ffap-guess-file-name-at-point)))
    (when file
      (find-file file))))

(defalias '_ffap #'prf/find-file-at-point)


;; ------------------------------------------------------------------------
;; HYDRAS

(eval-after-load "hydra"
  '(progn

     (defhydra hydra-srvUtils (:color blue)
       "server utils"
       ("s" prf/tramp/shell "shell")
       ("r" prf/tramp/remote-shell "remote shell")
       ("o" prf/tramp/visit-remoteFile-currentSrv "visit other version file")
       ("e" ediff-toggle "toggle ediff")
       ("f" prf/find-file-at-point "find at point")
       ("#" local-root-shell "local root shell")
       ("g" nil "cancel"))

     (defhydra hydra-copyPath (:color blue)
       "copy path"
       ("c" prf/copy-buffer-filepath-to-clipboard-clean "clean")
       ("r" prf/copy-buffer-filepath-to-clipboard-raw "raw")
       ("f" prf/copy-buffer-filename-to-clipboard "file")
       ("b" prf/copy-buffer-basename-to-clipboard "base name")
       ("e" prf/copy-buffer-filepath-to-clipboard-with-exec "exec")
       ("g" nil "cancel"))
     )
  )

(provide 'init-srv-utils)
