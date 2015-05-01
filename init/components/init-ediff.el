

(setq
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 ;; ediff-diff-options "--text -w"
 ediff-diff-options "-w"
 )

;; http://trey-jackson.blogspot.fr/2010/10/emacs-tip-38-automatically-diff-binary.html

(defun ediff-toggle ()
  (interactive)
  (if (= (length (window-list)) 2)
      (ediff
       (buffer-file-name (window-buffer (car (window-list))))
       (buffer-file-name (window-buffer (car (cdr (window-list))))) )
    (message "invalid number of visible buffers") )
  )

;; (prf/require-plugin 'ediff-trees)


(provide 'init-ediff)
