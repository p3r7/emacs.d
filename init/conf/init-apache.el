
(use-package apache-mode
  :mode ("\\.htaccess\\'"
	     "httpd\\.conf\\'"
	     "srm\\.conf\\'"
	     "access\\.conf\\'"
	     "sites-\\(available\\|enabled\\)/")
  :config
  (autoload 'apache-mode "apache-mode" nil t))


(provide 'init-apache)
