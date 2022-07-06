
(use-package mixed-pitch)

(use-package wysiwyg-text-mode
  :load-path "~/.emacs.d/plugins/wysiwyg-text-mode"
  :after (mixed-pitch pickling)
  :bind (
         :map org-mode-map
	 ("C-c v" . wysiwyg-text-mode)))




(provide 'init-text-common)
