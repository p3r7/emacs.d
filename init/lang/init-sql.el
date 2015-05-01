(add-hook 'sql-mode-hook
	  (lambda ()
	    (define-key sql-mode-map (kbd "RET") 'newline-and-indent)
	    (setq
	     tab-width 2
	     indent-tabs-mode nil)
	    (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
	    (set-syntax-table sql-mode-syntax-table) ))

(provide 'init-sql)
