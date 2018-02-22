
(when (prf/require-plugin 'ansible nil 'noerror)

  (defun prf/yml-hook/enable-ansible-minor-mode ()
    (if (string-match-p (regexp-quote "ansible") default-directory)
	(when (string-match-p (regexp-quote "ansible") default-directory)
	  (ansible 1))))

  (add-hook 'yaml-mode-hook 'prf/yml-hook/enable-ansible-minor-mode))

;; NB: Jinja2 support via ../lang/init-python.el


(provide 'init-ansible)
