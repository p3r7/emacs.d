
(use-package groovy-mode
  :mode (
	 "\.groovy$"
	 ;; "groovy"
	 "\.template$"
	 )
  :config
  (autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t))


(provide 'init-groovy)
