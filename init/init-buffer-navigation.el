
;; ------------------------------------------------------------------------
;; moving around

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

;; (require 'init-ace-jump)
(require 'init-avy)


;; ------------------------------------------------------------------------
;; undo history

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


;; ------------------------------------------------------------------------
;; killing

(defun prf/kill-this-buffer ()
  "Kill the current buffer.
More stable than default `kill-this-buffer'"
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'prf/kill-this-buffer)
(global-set-key "\C-x\C-k" #'prf/kill-this-buffer)


(provide 'init-buffer-navigation)
