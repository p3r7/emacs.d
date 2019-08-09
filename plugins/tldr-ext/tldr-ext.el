

;; ------------------------------------------------------------------------
;; VARS

(defvar tldr-ext-directory-path nil)


;; ------------------------------------------------------------------------
;; FUNCTIONS

(defun prf/tldr-ext-render-markdown (command)
  (let ((tldr-directory-path tldr-ext-directory-path))
    (tldr-render-markdown command)))

(defun prf/tldr-ext-get-file-path-from-command-name (command)
  (let ((tldr-directory-path tldr-ext-directory-path))
    (tldr-get-file-path-from-command-name command)))

(defun prf/tldr-get-commands-list ()
  (let ((command-list (tldr-get-commands-list))
	(ext-command-list (prf/tldr-get-ext-commands-list)))
    (delq nil (delete-dups (append command-list ext-command-list)))))

(defun prf/tldr-get-ext-commands-list ()
  (let ((tldr-directory-path tldr-ext-directory-path))
    (tldr-get-commands-list)))


;; ------------------------------------------------------------------------
;; INIT

(defun tldr-ext-activate ()
  (interactive)
  (if (file-directory-p tldr-ext-directory-path)

      (defadvice tldr-render-markdown (around tldr-render-markdown-with-ext activate)
	;; (let ((md (funcall (ad-get-orig-definition 'tldr-render-markdown) (ad-get-arg 0))))
	(let ((md ad-do-it))
	  (let* ((tldr-directory-path tldr-ext-directory-path)
		 (md-ext (funcall (ad-get-orig-definition 'tldr-render-markdown) (ad-get-arg 0))))
	    (setq ad-return-value (concat md md-ext)))))

    (message "Path of var `tldr-ext-directory-path' %s not found on disk" tldr-ext-directory-path)))


(provide 'tldr-ext)
