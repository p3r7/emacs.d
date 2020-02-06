

;; NB: do undo in a let, typically before launching a shell: (replace-regexp-in-string (concat (prf/escape-winnt-path cygwin-bin) ";") "" (getenv "PATH"))



;; DEPS

(require 's)
(require 'prf-string)



;; PERMANENT MODIFICATIONS

(defun prf/enrich-exec-path (dir)
  (when (not (string-match-p
	      (prf/escape-winnt-path dir)
	      (downcase (getenv "PATH"))))
    (setenv "PATH" (concat
		    (prf/system/get-path-system-format dir) ";"
		    (getenv "PATH")))
    (setq exec-path (cons dir exec-path))))


(defun prf/remove-from-exec-path (dir)
  (setenv "PATH"
          (prf/string/replace (prf/system/get-path-system-format dir)
                              ""
                              (getenv "PATH")))
  (setq exec-path (delete (prf/system/get-path-normalized-format dir) exec-path)))



;; TEMP MODIFICATION

(defun prf/path-with-folders (folders &optional path)
  (let ((path (or path (getenv "PATH"))))
    (mapc (lambda (e) (setq path (concat (prf/system/get-path-system-format e) ";" path))) folders)
    path))

(defun prf/path-without-folders (folders &optional path)
  (let* ((path (or path (getenv "PATH")))
         (replacements (mapcar
                        (lambda (e) (cons (concat (prf/system/get-path-system-format e) ";") ""))
                        folders)))
    (s-replace-all replacements path)))


(defmacro with-exec-path (&rest args)
  (declare (indent 1) (debug t))
  `(with-exec-path-eval
    :form (lambda () ,(cons 'progn (prf-exec-path--plist-get args :form)))
    :path ,(plist-get args :path)
    :remove ,(plist-get args :remove)
    :add ,(plist-get args :add)))

(put 'with-exec-path 'lisp-indent-function 'defun)

(cl-defun with-exec-path-eval (&key form
                                    remove add
                                    path)
  (let* ((func
          (if (functionp form) form
            ;; Try to use the "current" lexical/dynamic mode for `form'.
            (eval `(lambda () ,form) lexical-binding)))
         (og-path (getenv "PATH"))
         (path (or path og-path))
         (path (prf/path-without-folders remove path))
         (path (prf/path-with-folders add path)))
    (setenv "PATH" path)
    (funcall func)
    (setenv "PATH" og-path)))



;; PRIVATE HELPERS

(defun prf/escape-winnt-path (path)
  (replace-regexp-in-string "\\\\" "\\\\\\\\" (downcase (prf/system/get-path-system-format path))))

;; TODO: move into prf-string
(defun prf/system/get-path-normalized-format (path)
  "Normalize PATH, converting \\ into /."
  (subst-char-in-string ?\\ ?/ path))

(defun prf-exec-path--plist-get (plist prop)
  "Extract value of porperty PROP from property list PLIST.
Like `plist-get' except allows value to be multiple elements."
  (unless (null plist)
    (cl-loop with passed = nil
             for e in plist
             until (and passed
                        (keywordp e)
                        (not (eq e prop)))
             if (and passed
                     (not (keywordp e)))
             collect e
             else if (and (not passed)
                          (keywordp e)
                          (eq e prop))
             do (setq passed 't))))



(provide 'prf-exec-path)
