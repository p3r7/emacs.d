
(use-package eshell
  :ensure nil
  :commands eshell-mode
  :bind (
         :map eshell-mode-map
         ((kbd "<up>") . nil)
         ((kbd "<down>") . nil)
         :map eshell-hist-mode-map
         ((kbd "<up>") . nil)
         ((kbd "<down>") . nil))
  :hook '(eshell-mode
	      . (lambda () (setq global-hl-line-mode nil)))
  :init
  (setq eshell-history-size 1000000
	    eshell-destroy-buffer-when-process-dies t))

;; TODO: em-smart ? plan9 concepts ported to emacs shells
;; http://www.opensource.apple.com/source/emacs/emacs-51/emacs/lisp/eshell/em-smart.el



;; TERM INTEGRATION

(use-package em-term
  :ensure nil
  :after eshell

  :config
  (add-to-list 'eshell-visual-commands "htop")

  ;; stolen from: https://gist.github.com/ralt/a36288cd748ce185b26237e6b85b27bb
  (defun prf/eshell-exec-visual/tramp-aware (&rest args)
    "Run the specified PROGRAM in a terminal emulation buffer.
 ARGS are passed to the program.  At the moment, no piping of input is
 allowed."
    (let* (eshell-interpreter-alist
	   (original-args args)
	   (interp (eshell-find-interpreter (car args) (cdr args)))
	   (in-ssh-tramp (and (tramp-tramp-file-p default-directory)
			      (equal (tramp-file-name-method
				      (tramp-dissect-file-name default-directory))
				     "ssh")))
	   (program (if in-ssh-tramp
			"ssh"
		      (car interp)))
	   (args (if in-ssh-tramp
		     (let ((dir-name (tramp-dissect-file-name default-directory)))
		       (eshell-flatten-list
			(list
			 "-t"
			 (tramp-file-name-host dir-name)
			 (format
			  "export TERM=xterm-256color; cd %s; exec %s"
			  (tramp-file-name-localname dir-name)
			  (string-join
			   (append
			    (list (tramp-file-name-localname (tramp-dissect-file-name (car interp))))
			    (cdr args))
			   " ")))))
		   (eshell-flatten-list
		    (eshell-stringify-list (append (cdr interp)
						   (cdr args))))))
	   (term-buf
	    (generate-new-buffer
	     (concat "*"
		     (if in-ssh-tramp
			 (format "%s %s" default-directory (string-join original-args " "))
		       (file-name-nondirectory program))
		     "*")))
	   (eshell-buf (current-buffer)))
      (save-current-buffer
	(switch-to-buffer term-buf)
	(term-mode)
	(set (make-local-variable 'term-term-name) eshell-term-name)
	(make-local-variable 'eshell-parent-buffer)
	(setq eshell-parent-buffer eshell-buf)
	(term-exec term-buf program program nil args)
	(let ((proc (get-buffer-process term-buf)))
	  (if (and proc (eq 'run (process-status proc)))
	      (set-process-sentinel proc 'eshell-term-sentinel)
	    (error "Failed to invoke visual command")))
	(term-char-mode)
	(if eshell-escape-control-x
	    (term-set-escape-char ?\C-x))))
    nil)

  (defalias 'eshell-exec-visual #'prf/eshell-exec-visual/tramp-aware))




(provide 'init-eshell)
