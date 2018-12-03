
(prf/require-plugin 's)

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain
      ;; ediff-diff-options "--text -w"
      ;; ediff-diff-options "-w" ;; ignore all white spaces
      ediff-diff-options "-Z -E" ;; ignore trailing white spaces + tab expansion
      )

;; http://trey-jackson.blogspot.fr/2010/10/emacs-tip-38-automatically-diff-binary.html

(defun ediff-toggle ()
  (interactive)
  (if (= (length (window-list)) 2)
      (let ((prf/left-buffer (buffer-file-name (window-buffer (car (window-list)))))
	    (prf/right-buffer (buffer-file-name (window-buffer (car (cdr (window-list))))))
	    (ediff-diff-options ediff-diff-options))

	;; NB: we re-enable whitespace matching for python files ...
	(when (s-ends-with? ".py" prf/left-buffer)
	  (setq ediff-diff-options ""))
	(when (s-ends-with? ".py" prf/right-buffer)
	  (setq ediff-diff-options ""))

	(ediff
	 (buffer-file-name (window-buffer (car (window-list))))
	 (buffer-file-name (window-buffer (car (cdr (window-list)))))))
    (message "invalid number of visible buffers")))

;; (prf/require-plugin 'ediff-trees)


(provide 'init-ediff)
