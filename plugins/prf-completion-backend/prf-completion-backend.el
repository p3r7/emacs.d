
(defvar prf/M-x-completion-backend nil)
(defvar prf/find-file-completion-backend nil)
(defvar prf/switch-to-buffer-completion-backend nil)
(defvar prf/occur-completion-backend nil)



(defun prf/get-find-file-fun ()
  (cond
   ((eq prf/find-file-completion-backend 'helm) #'helm-find-files)
   ((member prf/find-file-completion-backend '(ivy counsel)) #'counsel-find-file)
   ((eq prf/find-file-completion-backend 'lusty) #'lusty-file-explorer)
   (t #'find-file)))

(defun prf/get-switch-to-buffer-fun ()
  (cond
   ((eq prf/switch-to-buffer-completion-backend 'helm) #'helm-buffers-list)
   ((eq prf/switch-to-buffer-completion-backend 'lusty) #'lusty-buffer-explorer)
   (t #'switch-to-buffer)))

(defun prf/get-M-x-fun ()
  (cond
   ((eq prf/M-x-completion-backend 'helm) #'helm-M-x)
   ((member prf/M-x-completion-backend '(ivy counsel)) #'counsel-M-x)
   (t #'execute-extended-command)))

(defun prf/get-occur-fun ()
  (cond
   ((eq prf/occur-completion-backend 'helm) #'helm-occur)
   ((member prf/occur-completion-backend '(ivy counsel swiper)) #'swiper)
   (t #'occur)))



(defun prf/find-file-fun ()
  (interactive)
  (call-interactively (prf/get-find-file-fun)))

(defun prf/switch-to-buffer-fun ()
  (interactive)
  (call-interactively (prf/get-switch-to-buffer-fun)))

(defun prf/M-x-fun ()
  (interactive)
  (call-interactively (prf/get-M-x-fun)))

(defun prf/occur-fun ()
  (interactive)
  (call-interactively (prf/get-occur-fun)))




(provide 'prf-completion-backend)
