;; - js
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; (add-hook 'js3-mode-hook (lambda () (tern-mode t)))
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
;; (setq tern-command (cons (executable-find "tern") '()))
;; (eval-after-load 'tern
;; '(progn
;; (require 'tern-auto-complete)
;; (tern-ac-setup)))

;; - jsx
(prf/require-plugin 'rjsx-mode nil 'noerror)


(provide 'init-js)
