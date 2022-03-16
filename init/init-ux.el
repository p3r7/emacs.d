


;; BELL(S) & WHISTLES

(setq ring-bell-function 'ignore)

(mouse-avoidance-mode 'animate)



;; PROMPTS: Y/N

(fset #'yes-or-no-p #'y-or-n-p)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))



;; PROMPTS: KILLING

(when (boundp 'confirm-kill-processes)
  (setq confirm-kill-processes nil))

(defun prf/kill-this-buffer ()
  "Kill the current buffer wo/ prompting.
More stable than default `kill-this-buffer'"
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'prf/kill-this-buffer)
(global-set-key "\C-x\C-k" #'prf/kill-this-buffer)



;; SUSPENDING (ony when in cli)

(global-set-key (kbd "C-z")
		(lambda () (interactive)
                  (unless window-system (suspend-frame))))

(global-set-key (kbd "\C-x\C-z")
		(lambda () (interactive)
		  (unless window-system (suspend-frame))))




(provide 'init-ux)
