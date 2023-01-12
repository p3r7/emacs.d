(add-hook 'sql-mode-hook
	      (lambda ()
	        (define-key sql-mode-map (kbd "RET") #'newline-and-indent)
	        (setq
	         tab-width 2
	         indent-tabs-mode nil)
	        (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
	        (set-syntax-table sql-mode-syntax-table) ))

(use-package sqlformat
  :if (or (executable-find "sqlformat")
	      (executable-find "pg_format"))
  :config
  (add-hook 'sql-mode-hook #'sqlformat-on-save-mode))




(provide 'init-sql)
