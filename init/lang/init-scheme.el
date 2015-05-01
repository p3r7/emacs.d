(when (require 'quack nil 'noerror)
  ;;(setq scheme-program-name "scm")
  (setq quack-programs (quote ("scm" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
  (setq quack-default-program "scm")
  (fset 'scheme-evaluate-buffer
	"\C-xh\C-c\C-r\M->")

  (add-hook 'scheme-mode-hook ;; no need of a progn ????
	    (lambda()
	      (local-set-key  (kbd "C-<f3>") 'run-scheme)
	      (local-set-key (kbd "C-j") 'scheme-send-region)
	      (local-set-key (kbd "M-j") 'scheme-evaluate-buffer)
	      (linium-mode)
	      ))

  ;; TODO: define it for only scheme-mode ???
  (defadvice scheme-send-region (before slickcopy activate compile)
    "When called interactively with no active region, copy a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
	     (line-beginning-position 2)))))

  ;; SICP study layout

  ;; (defvar sicp-view nil "")
  (defun sicp () (interactive)
    ;; (if sicp-view
    ;;     ()
    ;;   ((setq sicp-view t)
    (delete-other-windows)
    (shell-command "wmctrl -r :ACTIVE: -badd,fullscreen")
    (split-window-horizontally)
    (windmove-right)
    (info "sicp")
    (windmove-left)
    (find-file "~/Dropbox/textfiles/scratch.scm")
    (split-window-vertically)
    (run-scheme "scm")
    (windmove-right))
  ;; ))
  )

(provide 'init-scheme)
