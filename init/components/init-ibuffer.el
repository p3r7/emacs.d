;; TODO: customize ibuffer-formats for larger name column

(require 'ibuffer)
(setq
 ibuffer-formats
 '((mark modified read-only " "
	 (name 25 25 :left :elide)
	 " "
	 (size 9 -1 :right)
	 " "
	 (mode 16 16 :left :elide)
	 " " filename-and-process)
   (mark " "
	 (name 16 -1)
	 " " filename))

 ibuffer-expert t
 ibuffer-show-empty-filter-groups nil
 ibuffer-saved-filter-groups
 (quote (("default"
	  ("shells"
	   ;; (or
	   (mode . shell-mode)
	   ;; (and (mode . syslog-mode)
	   ;; (name . "@") )
	   ;; )
	   )
	  ("dired"
	   (mode . dired-mode)
	   )
	  ("org" ;; all org-related buffers
	   (or
	    (name . "^\\*Deft\\*$")
	    (mode . org-mode))
	   )
	  ;; ("Mail"
	  ;;   (or
	  ;;    (mode . message-mode)
	  ;;    (mode . mail-mode)
	  ;;    ))

	  ("config"
	   (or (filename . ".emacs.d")
	       (filename . "AutoHotkey.ahk")
	       )
	   )
	  ("XSLT"
	   (filename . "\\.xsl$")
	   )
	  ;; ("MyProject1"
	  ;;   (filename . "src/myproject1/"))
	  ("provi"
	   (filename . "^provi")
	   )
	  ("dev"
	   (or
	    (mode . c-mode)
	    (mode . java-mode)
	    (mode . groovy-mode)
	    (mode . web-mode)
	    (mode . javascript-mode)
	    (mode . js-mode)
	    (mode . js3-mode)
	    (mode . css-mode)
	    (mode . php-mode)
	    (mode . shell-script-mode)
	    (mode . perl-mode)
	    (mode . python-mode)
	    (mode . emacs-lisp-mode)
	    ))
	  ("temp"
	   (or
	    (name . "^\\*tramp")
	    (name . "^\\*Apropos\\*$")
	    (name . "^\\*Messages\\*$")
	    (name . "^\\*Completions\\*$")
	    (name . "^\\*Help\\*$")
	    ))
	  ;; ("ERC"   (mode . erc-mode))
	  ))))

(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*tramp")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm")
(add-to-list 'ibuffer-never-show-predicates "^\\.projectile")
(add-to-list 'ibuffer-never-show-predicates "^\\*Backtrace\\$*")
(add-to-list 'ibuffer-never-show-predicates "^\\*About GNU Emacs\\*$")
(add-to-list 'ibuffer-never-show-predicates "^\\*Bookmark List\\*$")

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x B") 'ibuffer)

;; TODO: https://github.com/svend/ibuffer-tramp/blob/master/ibuffer-tramp.el

(provide 'init-ibuffer)
