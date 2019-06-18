
(require 'init-ace-jump)

(use-package goto-last-change
  :bind ([(meta p)(u)] . goto-last-change))

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))


(provide 'init-buffer-navigation)
