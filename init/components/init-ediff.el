
;; http://trey-jackson.blogspot.fr/2010/10/emacs-tip-38-automatically-diff-binary.html


;; EDIFF

(use-package ediff
  :after (s)
  :init
  (setq ediff-split-window-function 'split-window-horizontally
	ediff-window-setup-function 'ediff-setup-windows-plain
	;; ediff-diff-options "--text -w"
	;; ediff-diff-options "-w" ;; ignore all white spaces
	ediff-diff-options "-Z -E" ;; ignore trailing white spaces + tab expansion
	)
  :config
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
      (message "invalid number of visible buffers (expected 2)"))))



;; ZTREE

(use-package ztree
  :config
  (defun ztree-toggle ()
    (interactive)
    (if (= (length (window-list)) 2)
        (let (dir-A dir-B)

          (if (and (buffer-file-name)
                   (file-exists-p (buffer-file-name)))
              (setq dir-A buffer-file-name)
            (setq dir-A default-directory))

          (other-window 1)

          (if (and (buffer-file-name)
                   (file-exists-p (buffer-file-name)))
              (setq dir-B buffer-file-name)
            (setq dir-B default-directory))

          (other-window 1)

          (if (and (file-directory-p dir-A)
                   (file-directory-p dir-B))
              (ztree-diff dir-A dir-B)
            (message "not all buffers directories")))
      (message "invalid number of visible buffers (expected 2)"))))

;; NB: Old way to do this was w/ `ediff-trees', but `ztree' seems more
;; maintained.



(provide 'init-ediff)
