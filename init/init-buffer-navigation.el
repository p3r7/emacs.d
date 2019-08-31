
(require 'init-ace-jump)


(use-package goto-last-change
  :bind ([(meta p)(u)] . goto-last-change))


(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))


(defun prf/kill-this-buffer ()
  "Kill the current buffer.
More stable than default `kill-this-buffer'"
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'prf/kill-this-buffer)


(provide 'init-buffer-navigation)
