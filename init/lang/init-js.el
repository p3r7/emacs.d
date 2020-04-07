

;; JS

(use-package js2-mode
  :delight (js2-mode "JS2")
  :mode "\\.js\\'"
  :config
  ;; (customize-set-variable 'js2-include-node-externs t)
  )

(add-hook 'js3-mode-hook
	  (lambda ()
	    (setq js3-auto-indent-p t
		  js3-indent-level 4
		  js3-expr-indent-offset 4
		  js3-paren-indent-offset 2
		  js3-square-indent-offset 4
		  js3-curly-indent-offset 0
		  js3-enter-indents-newline t
		  js3-indent-on-enter-key t
		  js3-lazy-commas t
		  js3-lazy-dots t
		  js3-lazy-operators t
		  )
	    (linum-mode 1)))
;; (add-to-list 'ac-modes 'js3-mode)



;; JSX (REACT)

(use-package rjsx-mode
  :defer t)




(provide 'init-js)
