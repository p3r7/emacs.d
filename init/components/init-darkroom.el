


;; writeroom

(when (prf/require-plugin 'writeroom-mode nil 'noerror)
  )



;; darkroom

;; (if (bound-and-true-p darkroom-mode)
;;     (message "darkroom is on")
;;   (message "darkroom is off"))

(when (prf/require-plugin 'darkroom nil 'noerror)

  (defun prf/frame/toggle-fullscreen-darkroom ()
    (interactive)
    (let ((fullscreen (frame-parameter nil 'fullscreen)))
      (if (memq fullscreen '(fullscreen fullboth))
	  (progn
	    (if (bound-and-true-p darkroom-tentative-mode)
		(let ((darkroom-mode nil))
		  (darkroom-tentative-mode -1)))
	    (if (bound-and-true-p darkroom-mode)
		(let ((darkroom-tentative-mode nil))
		  (darkroom-mode -1))))
	(darkroom-mode))
      (toggle-frame-fullscreen)))


  (global-set-key (kbd "C-<f11>") 'prf/frame/toggle-fullscreen-darkroom))


(provide 'init-darkroom)
