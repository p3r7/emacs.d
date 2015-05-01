
(add-hook 'c-mode-common-hook
	  (lambda()
	    ;; Indent style
	    (c-set-style "linux")
	    (setq
	     ;; indentation
	     tab-width 4
	     c-basic-offset 4
	     ;; comments
	     comment-start "//"
	     comment-end   ""
	     )
	    ;; Auto-detect others' files indent style
	    ;; (require 'dtrt-indent)
	    ;; (dtrt-indent-mode t)
	    ;; Block of code collapsing
	    (local-set-key (kbd "C-c <right>") 'hs-show-block)
	    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
	    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
	    (local-set-key (kbd "C-c <down>")  'hs-show-all)
	    (hs-minor-mode t)
	    (hideshowvis-minor-mode t)
	    ;; Switch between header/source
	    (local-set-key  (kbd "C-c o") 'ff-find-other-file)
	    ;; C-d for deleting
	    (local-set-key (kbd "C-d") 'duplicate-line-or-region)
	    ;; line numbers
	    (linum-mode t)
	    ))

;; ------------------------------------------------------------------------
;; ECLIM

;; (require 'emacs-eclim))
;; (setq
;;  eclim-auto-save t
;;  eclim-executable "/opt/eclipse/juno/eclim")
;; (require 'eclim)
;; (require 'eclimd)
;; (global-eclim-mode)
;; NB: needs to be defined here, overwise it gets overriden by the above statements
;; (add-hook 'c-mode-common-hook
;; 	(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))


;; ------------------------------------------------------------------------

(provide 'init-c-common)
