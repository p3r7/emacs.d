
;; CURSOR NAVIGATION

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

;; (require 'init-ace-jump)
(require 'init-avy)

;; better moving around code blocks
(defun prf/smart-forward-list ()
  (interactive)
  (let ((matching (ignore-errors (scan-lists (point) 1 0))))
    (if matching
        (call-interactively #'forward-list)
      (call-interactively #'up-list))))

(defun prf/smart-backward-list ()
  (interactive)
  (let ((matching (ignore-errors (scan-lists (point) -1 0))))
    (if matching
        (call-interactively #'backward-list)
      (call-interactively #'backward-up-list))))

(global-set-key [remap forward-list] #'prf/smart-forward-list)
(global-set-key [remap backward-list] #'prf/smart-backward-list)


;; UNDO

(use-package goto-last-change
  :bind ([(meta p)(u)] . goto-last-change))

(use-package undo-tree
  ;; The visual tree feature is great, but I have trouble adjusting to
  ;; the change of behaviour (no C-g to reverse direction)
  :disabled
  :demand
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo))

;; REVIEW: timer trick https://emacs.stackexchange.com/a/47349

(use-package autorevert
  :ensure nil
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

(defalias '_rb 'prf/revert-buffer-no-confirm)




(provide 'init-buffer-navigation)
