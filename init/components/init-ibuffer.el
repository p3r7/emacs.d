;; TODO: customize ibuffer-formats for larger name column

;; gotten from https://emacs.stackexchange.com/a/2094
(defun prf/ibuffer/ansible-buffer-p ()
  (let ((fname (buffer-file-name)))
    (or (bound-and-true-p ansible) ;; NB: ansible-mode is named `ansible` ...
	(and fname
	     (string-match-p (regexp-quote "ansible") fname)
	     (s-suffix? ".j2" fname)))))

(defun prf/ibuffer/help-mode-or-derived-p ()
  (when (derived-mode-p 'help-mode)))


;; http://www.emacswiki.org/emacs/IbufferMode

(use-package ibuffer
  :after (s)

  :bind (("C-x B" . ibuffer))

  :init
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
	    ("Shells"
	     (or (mode . shell-mode)
                 (mode . term-mode)
		 (name . "^\\*\\(.*\\)@[0-9][0-9]?[0-9]?\.[0-9][0-9]?[0-9]?\.[0-9][0-9]?[0-9]?\.[1-9][0-9]?[0-9]?\\*\\(.*\\)$") )
	     )

	    ("Dired"
	     (mode . dired-mode))

	    ("Org" ;; all org-related buffers
	     (or
	      (name . "^\\*Deft\\*$")
	      (mode . org-mode)))

            ("Markdown"
	     (or
              (mode . markdown-mode)))

	    ;; ("Mail"
	    ;;   (or
	    ;;    (mode . message-mode)
	    ;;    (mode . mail-mode)
	    ;;    ))

	    ("Ansible"
	     (predicate . (prf/ibuffer/ansible-buffer-p)))

	    ("Clojure"
	     (or
	      (name . "^\\*nrepl-server")
	      (name . "^\\*cider-repl")
	      (name . "^\\*cider-error\\*$")
	      (mode . clojurec-mode)
              (mode . clojure-mode)
              (mode . clojurescript-mode)))

	    ("CRON"
	     (or (filename . "/etc/crontab")
		 (filename . "/etc/cron.d/")))

	    ("Logs"
	     (mode . syslog-mode))

	    ("Config: Emacs"
	     (filename . ".emacs.d"))

	    ("Config"
	     (or (filename . "AutoHotkey.ahk")
		 (mode . yaml-mode)
                 (mode . json-mode)
                 (mode . fvwm-mode)))

	    ("Provi"
	     (filename . "^provi"))

	    ("Dev"
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
	      (mode . sh-mode)          ; aka Shell-script mode
	      (mode . perl-mode)
	      (mode . python-mode)
	      (mode . emacs-lisp-mode)
              (mode . lua-mode)))

	    ("Mail"
	     (or
	      (mode . message-mode)
              ;; notmuch
              (mode . notmuch-hello-mode)
              (mode . notmuch-search-mode)
	      (mode . notmuch-show-mode)
              (mode . notmuch-message-mode)))

	    ("IRC"
	     (or
	      (mode . circe-server-mode)
	      (mode . circe-channel-mode)))

	    ("tmp: standard"
	     (or
	      (name . "^\\*scratch\\*$")
	      (name . "^\\*Messages\\*$")))
	    ("tmp: Magit"
	     (or
	      (name . "^magit")))
	    ("tmp: Help"
             (or
              (predicate . (prf/ibuffer/help-mode-or-derived-p))
              (mode . cider-docview-mode)
              (name . "^\\*Help\\*$")
              (name . "^\\*Apropos\\*$")
              (name . "^\\*Completions\\*$")
	      (name . "^\\*helpful ")))
            ("tmp: Search"
	     (or
	      (name . "^\\*Occur")
              (name . "^\\*deadgrep ")))
	    ("tmp"
	     (or
	      (name . "^\\*tramp")
	      (name . "^\\*Error")
	      (name . "^\\*Packages\\*$")
	      (name . "^\\*Compile-Log\\*$")
              (name . "^\\*quelpa-build")))
	    ;; ("ERC"   (mode . erc-mode))
	    ))))

  :config
  (require 'ibuf-ext)
  (add-to-list 'ibuffer-never-show-predicates "^\\*tramp")
  (add-to-list 'ibuffer-never-show-predicates "^\\*magit:")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm")
  (add-to-list 'ibuffer-never-show-predicates "^\\.projectile")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Backtrace\\$*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*About GNU Emacs\\*$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Bookmark List\\*$")

  (defun prf/ibuffer/switch-to-default-filter-group ()
    (interactive)
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-auto-mode 1)
	      (ibuffer-switch-to-saved-filter-groups "default"))))


;; TODO: https://github.com/svend/ibuffer-tramp/blob/master/ibuffer-tramp.el
(use-package ibuffer-tramp
  :after (ibuffer)
  :config

  (defun prf/ibuffer/switch-to-tramp-filter-group ()
    (interactive)
    (ibuffer-tramp-set-filter-groups-by-tramp-connection)
    (ibuffer-do-sort-by-alphabetic))

  (define-key ibuffer-mode-map (kbd "s t") 'prf/ibuffer/switch-to-tramp-filter-group)
  (define-key ibuffer-mode-map (kbd "s d") 'prf/ibuffer/switch-to-default-filter-group))



(provide 'init-ibuffer)
