

;; MAJOR MODE

(use-package web-mode
  :mode ("\\.phtml\\'"
	 "\\.tpl\\.php\\'"
	 "\\.jsp\\'"
	 "\\.gsp\\'"
	 "\\.as[cp]x\\'"
	 "\\.erb\\'"
	 "\\.mustache\\'"
	 "\\.djhtml\\'"
	 "\\.html?\\'")
  :bind (
	 :map web-mode-map
	 ("C-M-n" . web-mode-tag-match)
	 ("<C-kp-divide>" . web-mode-comment-or-uncomment)
	 ("C-<f8>" . web-mode-comment-or-uncomment))
  :init
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  :config
  (add-hook 'web-mode-hook
	    (lambda()
	      (setq tab-width 4)
	      (if (fboundp 'display-line-numbers-mode)
		  (display-line-numbers-mode 1)
		(linum-mode 1)))))



;; HTML TEMPLATING

(use-package emmet-mode
  :hook (sgml-mode web-mode))




(provide 'init-html)
