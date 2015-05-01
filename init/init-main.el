
;; -------------------------------------------------------------------------
;; INTERRACTIONS

(setq ring-bell-function 'ignore)

(fset 'yes-or-no-p 'y-or-n-p)
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(global-set-key (kbd "C-z")
		(lambda()(interactive)
		  (unless window-system (suspend-frame))
		  ))

(global-set-key (kbd "\C-x\C-z")
		(lambda()(interactive)
		  (unless window-system (suspend-frame))
		  ))


(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; http://stackoverflow.com/questions/7031051/emacs-notify-when-a-file-has-been-modified-externally
;; NOTE: not working ?
(defun auto-revert-remote-file ()
  (interactive)
  (if (&& (file-remote-p (buffer-file-name (current-buffer)) (buffer-modified-p (buffer-file-name (current-buffer)))))
      (revert-buffer t t)
    )
  )

(defun prf/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defalias '_rb 'prf/revert-buffer-no-confirm)


;; Do not use gpg agent when runing in terminal -> doesn't work on winnt
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

;; automatic display of local help
;; used to display errors of eclim in mb
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)



;; -------------------------------------------------------------------------
;; BACKUPS

;; Backups
(setq
 ;;make-backup-files nil
 backup-directory-alist `(("." . ,prf-backup-dir))
 tramp-auto-save-directory prf-auto-save-dir ;; http://charles.plager.net/emacs.html
 password-cache-expiry nil
 ;;backup-by-copying t
 ;;backup-by-copying-when-linked t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)


;; -------------------------------------------------------------------------
;; EDITION

(visual-line-mode 1)


;; -------------------------------------------------------------------------
;; SELECTION

(cua-selection-mode t)
;(global-set-key (kbd "C-<f4>") 'cua-mode)
;; could have used pc-selection-mode as well

;; FIXME: doesn't work
(add-hook 'org-mode-hook
	  '(lambda()
	     (define-key org-mode-map (kbd "C-RET") 'org-insert-heading-respect-content)
	     ))

(global-subword-mode t)


;; -------------------------------------------------------------------------
;; DISABLED COMMANDS

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;; -------------------------------------------------------------------------
;; INDEPENDANT BEHAVIOURS

(global-auto-revert-mode 1)
;; not perfect, see [[http://stackoverflow.com/questions/6512086/emacs-reverts-buffer-to-weird-previous-state-with-git-rebase]]
;; doesn't work on remote servers: [[http://newsgroups.derkeiler.com/Archive/Comp/comp.emacs/2005-08/msg00104.html]]

(setq require-final-newline t)           ;; end files with a newline

(mouse-avoidance-mode 'animate)



(provide 'init-main)
