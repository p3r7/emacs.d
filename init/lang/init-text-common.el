
(use-package mixed-pitch)

(use-package prf-wysiwyg-text-mode
  :load-path "~/.emacs.d/plugins/prf-wysiwyg-text-mode"
  :after (mixed-pitch pickling)
  :bind (
         :map org-mode-map
	 ("C-c v" . prf-wysiwyg-text-mode)))




(provide 'init-text-common)
