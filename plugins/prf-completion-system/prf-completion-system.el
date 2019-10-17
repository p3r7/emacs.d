
(defvar prf/M-x-completion-system nil)
(defvar prf/find-file-completion-system nil)
(defvar prf/switch-to-buffer-completion-system nil)



(defun prf/get-find-file-fun ()
  (cond
   ((eq prf/find-file-completion-system 'helm) #'helm-find-files)
   ((member prf/find-file-completion-system '(ivy counsel)) #'counsel-find-file)
   ((eq prf/find-file-completion-system 'lusty) #'lusty-file-explorer)
   (t #'find-file)))

(defun prf/get-switch-to-buffer-fun ()
  (cond
   ((eq prf/switch-to-buffer-completion-system 'helm) #'helm-buffers-list)
   ((eq prf/switch-to-buffer-completion-system 'lusty) #'lusty-buffer-explorer)
   (t #'switch-to-buffer)))

(defun prf/get-M-x-fun ()
  (cond
   ((eq prf/M-x-completion-system 'helm) #'helm-M-x)
   ((member prf/M-x-completion-system '(ivy counsel)) #'counsel-M-x)
   (t #'execute-extended-command)))



(defun prf/find-file-fun ()
  (interactive)
  (call-interactively (prf/get-find-file-fun)))

(defun prf/switch-to-buffer-fun ()
  (interactive)
  (call-interactively (prf/get-switch-to-buffer-fun)))

(defun prf/M-x-fun ()
  (interactive)
  (call-interactively (prf/get-M-x-fun)))




(provide 'prf-completion-system)
