;;; tldr-ext.el --- extension to tldr.el             -*- lexical-binding: t; -*-

;; Author: Jordan Besly
;; Keywords: tools, docs
;; Package-Requires: ((emacs "24.3") (request "0.3.0"))
;; Version: {{VERSION}}

(require 'tldr)


 ;; VARS

(defvar tldr-ext-directory-path nil)


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


;; INIT

(defun tldr-ext-activate ()
  (interactive)
  (if (not (and tldr-ext-directory-path
                (file-directory-p tldr-ext-directory-path)))
      (message "Path of var `tldr-ext-directory-path' %s not found on disk" tldr-ext-directory-path)
    (defadvice tldr-get-commands-list (around tldr-get-commands-list-with-ext activate)
      (let ((command-list ad-do-it))
        (let* ((tldr-directory-path tldr-ext-directory-path)
               (ext-command-list (funcall (ad-get-orig-definition 'tldr-get-commands-list))))
          (setq ad-return-value (delq nil (delete-dups (append command-list ext-command-list)))))))

    (defadvice tldr-render-markdown (around tldr-render-markdown-with-ext activate)
      (let ((md (when (tldr-get-file-path-from-command-name (ad-get-arg 0))
                  ad-do-it)))
        (let* ((tldr-directory-path tldr-ext-directory-path)
               (tldr-directory-file-path (tldr-get-file-path-from-command-name (ad-get-arg 0)))
               md-ext)
          (when tldr-directory-file-path
            (setq md-ext (funcall (ad-get-orig-definition 'tldr-render-markdown) (ad-get-arg 0))))
          (setq ad-return-value (concat md md-ext)))))))


(provide 'tldr-ext)
