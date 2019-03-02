
(require 's)

;; SHELL & TRAMP & DIRED

;; TODO: emacs function to get curren path and convert it to url if contains www

;; manually downloaded and configured upstream versions of tramp
;; http://www.gnu.org/software/tramp/#Installation
(if (and (= emacs-major-version 24)
	 (= emacs-minor-version 3))
    (add-to-list 'load-path "~/.emacs.d/plugins-src/tramp-2.2.11/lisp")
  )
(if (and (= emacs-major-version 24)
	 (= emacs-minor-version 5))
    (add-to-list 'load-path "~/.emacs.d/plugins-src/tramp-2.2.12/lisp")
  )
;; NB: solves copy & mv, but potentially crashes find-file

(use-package tramp-term
  :defer t)

(use-package hide-lines)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins-spe/syslog-mode-prf"))
(use-package syslog-mode
  :load-path "~/.emacs.d/plugins-spe/syslog-mode-prf"
  :mode (".*\.log'" ".*\.log\..*\.gz'" "/var/log.*\\'"
	 "\\catalina.out\\'"))

;; ------------------------------------------------------------------------
;; DIRED

(use-package dired+
  :load-path "~/.emacs.d/plugins/dired+"
  :defer t
  ;; :config
  ;; (eval-after-load "dired-aux"
  ;;     '(require 'dired-async))
  )

;; http://www.emacswiki.org/emacs/Sunrise_Commander

(when (executable-find "busybox")
  (setq dired-use-ls-dired nil))


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

;; disable vc for remote files (speed increase)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


;; (setq tramp-verbose 6)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins-spe/vagrant-tramp-20140709.814"))
;; (when
;;     (prf/require-plugin 'vagrant-tramp)
;;   (require 'vagrant-tramp)
;;   (eval-after-load 'tramp
;;     '(vagrant-tramp-enable))
;;   )

(defun prf/tramp/extract-remote-file-name (trampFilePath)
  (if (string-match (concat "/" tramp-default-method ":") trampFilePath)
      (car (cdr (cdr (split-string trampFilePath ":"))))
    trampFilePath))


(defun prf/tramp/convert-remoteFilePath-currentSrv (currentFilePath remoteFilePath)
  "Format a path using current server address prefix and remote server file location"
  ;; prefix current srv
  (if (string-match (concat "/" tramp-default-method ":") currentFilePath)
      (setq my-prefix (concat "/" tramp-default-method ":" (car (cdr (split-string currentFilePath ":"))) ":"))
    (setq my-prefix "")
    )

  ;; suffix remoteFilePath
  (setq my-suffix (prf/tramp/extract-remote-file-name remoteFilePath))

  (concat my-prefix my-suffix))


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

	;; - validate file exists
	(if (file-exists-p my-new-filepath)
	    ;; - go to file
	    (if (file-directory-p my-new-filepath)
		(dired my-new-filepath)
	      (find-file my-new-filepath))
	  (message "buffer not a file") )
	)
    (message "invalid number of visible buffers") )
  )
(defalias '_t/vrm 'prf/tramp/visit-remoteFile-currentSrv)


;; ------------------------------------------------------------------------
;; SHELL

(when (>= emacs-major-version 25)
  (add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window)))

(use-package prf-tramp
  :load-path "~/.emacs.d/plugins/prf-tramp"
  :config
  (if (not (fboundp '_sh))
      (defalias '_sh 'prf/tramp/shell))
  (defalias '_rsh 'prf/tramp/remote-shell))

(defun local-root-shell ()
  (interactive)
  (with-temp-buffer
    (cd "/sudo::")
    (let ((current-prefix-arg '(4)))
      (shell (generate-new-buffer-name "*root@localhost*")))
    (cd dd-old)))

;;TODO: lotta stuff don't work as expected
(add-hook 'shell-mode-hook
	  (lambda()
	    (local-set-key (kbd "<f8>")      '(lambda nil (interactive) (syslog-mode)))
	    (local-set-key (kbd "<f7>")      '(erase-buffer))
	    (local-set-key (kbd "<f6>")      '(lambda nill (interactive) (progn
									   (move-beginning-of-line)
									   (set-mark)
									   (move-end-of-line)
									   (json-format)
									   )))
	    ) )

(add-hook 'syslog-mode-hook
	  (lambda()
	    (local-set-key (kbd "<f8>")      '(lambda nil (interactive) (progn
									  (shell-mode)
									  ;; (toggle-read-only)
									  (setq current-prefix-arg '(-1)) ; C-u
									  (call-interactively 'read-only-mode))))
	    ))


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
	    (setq
	     global-hl-line-mode nil)
	    ))



;; ------------------------------------------------------------------------
;; TERM

;; http://www.emacswiki.org/emacs/AnsiTermHints#toc4
;; http://stackoverflow.com/questions/12802236/emacs-keyboard-shortcut-to-run-ansi-term-with-a-specific-shell
;; TODO: test more
(defun remote-term (new-buffer-name cmd &rest switches)
  ""
  (interactive)
  (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))



;; ------------------------------------------------------------------------
;; DIRED

;; [[http://www.emacswiki.org/emacs/DiredTweaks]]
(setq
 dired-dwim-target t ;; if other window -> set as default dir for copy
 ls-lisp-dirs-first t ;; display dirs 1st
 dired-listing-switches "-alh"
 diredp-hide-details-initially-flag nil
 diredp-hide-details-propagate-flag nil
 )
;; http://stackoverflow.com/questions/14602291/dired-how-to-get-really-human-readable-output-find-ls-option
;; http://stackoverflow.com/questions/4115465/emacs-dired-too-much-information

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook ;; do not create other dired buffers when navigating
	  ;; TODO: far from being perfect (closes all dired windows, not just current)
	  (lambda ()
	    (define-key dired-mode-map (kbd "<return>")
	      'dired-find-alternate-file) ; was dired-advertised-find-file
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))
					; was dired-up-directory
	    ))


;; ------------------------------------------------------------------------
;; FILESYSTEM HELPERS


(defun prf/get-buffer-filepath-complete ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun prf/get-buffer-filepath-clean ()
  (let ((filename (prf/get-buffer-filepath-complete)))
    (when filename
      (prf/tramp/extract-remote-file-name filename))))

(defun prf/get-buffer-filepath-with-exec ()
  (let ((filename (prf/get-buffer-filepath-complete))
	clean-filename)
    (when filename
      (setq clean-filename (prf/tramp/extract-remote-file-name filename))
      (cond ((bound-and-true-p ansible) (concat "ansible-playbook " clean-filename)) ;; NB: ansible-mode is named `ansible` ...
	    ((s-suffix? ".php" clean-filename) (concat "php " clean-filename))
	    ((s-suffix? ".py" clean-filename) (concat "python " clean-filename))) ;; REVIEW: should ideally test if in virtual env
      )))

(defun prf/get-buffer-basepath ()
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
  (let ((filename (prf/get-buffer-basepath)))
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

(defalias '_cfp 'prf/copy-buffer-filepath-to-clipboard-clean)


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
