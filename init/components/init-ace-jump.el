
(when (prf/require-plugin 'ace-jump-mode nil 'noerror)
  (progn
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
    (add-hook 'org-mode-hook
	      '(lambda()
		 (define-key org-mode-map (kbd "C-c SPC") 'ace-jump-mode)
		 (define-key org-mode-map (kbd "C-c C-SPC") 'ace-jump-mode)
		 ))
    ))

(provide 'init-ace-jump)
