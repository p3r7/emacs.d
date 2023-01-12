

;; GENERAL

(use-package prf-smart-edit
  :quelpa (prf-smart-edit :fetcher github :repo "p3r7/prf-smart-edit")
  ;; :after (org groovy-mode)
  :after (org)
  :demand
  :bind (
	 ("M-w" . copy-line-or-region)
	 ("C-w" . cut-line-or-region)
	 ("C-d" . duplicate-line-or-region)
	 ("C-<f8>" . comment-or-uncomment-line-or-region)
	 ("<C-kp-divide>" . comment-or-uncomment-line-or-region)
	 :map org-mode-map
	 ("M-w" . copy-line-or-region-org)
	 ("C-w" . cut-line-or-region-org)
	 ;; :map groovy-mode-map
	 ;; ("C-d" . duplicate-line-or-region)
	 )
  )

;; REVIEW: what about c-electric-delete-forward on recent emacs versions

(defun prf/reindent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "C-c TAB") #'prf/reindent-buffer)


;; don't retain case when replacing strings
(setq case-replace nil)



;; UNDO

(use-package undo-hl
  :quelpa (undo-hl :fetcher github :repo "casouri/undo-hl")
  :delight
  :config
  (add-hook 'prog-mode-hook #'undo-hl-mode)
  (add-hook 'text-mode #'undo-hl-mode))

(when (version<= "28.1" emacs-version)
  (use-package vundo))

(use-package undo-tree
  ;; NB: The visual tree feature is great, but I have trouble adjusting to
  ;; the change of behaviour (no C-g to reverse direction)
  :disabled
  :demand
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo))

;; REVIEW: timer trick https://emacs.stackexchange.com/a/47349

(use-package autorevert
  :ensure nil
  :diminish
  :hook
  (after-init-hook . global-auto-revert-mode))
;; not perfect, see [[http://stackoverflow.com/questions/6512086/emacs-reverts-buffer-to-weird-previous-state-with-git-rebase]]
;; doesn't work on remote servers: [[http://newsgroups.derkeiler.com/Archive/Comp/comp.emacs/2005-08/msg00104.html]]

;; http://stackoverflow.com/questions/7031051/emacs-notify-when-a-file-has-been-modified-externally
;; NOTE: not working ?
(defun auto-revert-remote-file ()
  (interactive)
  (if (&& (file-remote-p (buffer-file-name (current-buffer)) (buffer-modified-p (buffer-file-name (current-buffer)))))
      (revert-buffer t t)))

(defun prf/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defalias '_rb #'prf/revert-buffer-no-confirm)
(global-set-key (kbd "<f5>") #'prf/revert-buffer-no-confirm)



;; MULTIPLE CURSORS

(require 'init-mc)



;; SMART PARENTHESIS

;; https://www.emacswiki.org/emacs/AutoPairs

(require 'init-smartparens)
;; (require 'init-autopair)
;; (require 'init-wrap-region)
;; NB: conflict between wrap-region and autopair-autowrap



;; HIGHLIGHT CHANGES

;; NB: for displaying all edits in red, use the standard `highlight-change-mode'

;; NOTE: used to be disabled as misbehaved randomly
(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode 1))



;; MANUAL HIGHLIGHTING

(require 'init-highlighting)



;; TIMESTAMPS: AUTOMATIC

;; Smart timestamps
(setq
 time-stamp-line-limit 10                                ; check in 10 first lines for Time-stamp:
 time-stamp-active t                                     ; do enable
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")

;; Update them when saving
(add-hook 'write-file-hooks #'time-stamp)



;; TIMESTAMPS: MANUAL

(defvar date-format-compact "%Y%m%d"
  "Format of date to insert with `prf/insert-current-date-compact' func
See help of `format-time-string' for possible replacements")

(defvar datetime-format-compact "%Y%m%d%H%M%S"
  "Format of date to insert with `prf/insert-current-datetime-compact' func
See help of `format-time-string' for possible replacements")

(defun prf/insert-current-date-compact ()
  "insert the current date into the current buffer."
  (interactive)
  (insert (format-time-string date-format-compact (current-time))))

(defun prf/insert-current-datetime-compact ()
  "insert the current date into the current buffer."
  (interactive)
  (insert (format-time-string datetime-format-compact (current-time))))




(provide 'init-edition)
