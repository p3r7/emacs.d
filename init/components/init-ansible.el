
;; could have also added: https://github.com/lunaryorn/ansible-doc.el

;; alternative for jinja2 integration: http://melpa.org/#/poly-ansible

(use-package ansible
  :init
  (defun prf/yml-hook/enable-ansible-minor-mode ()
    (if (string-match-p (regexp-quote "ansible") default-directory)
	(when (string-match-p (regexp-quote "ansible") default-directory)
	  (ansible 1))))
  :config
  (add-hook 'yaml-mode-hook #'prf/yml-hook/enable-ansible-minor-mode))


;; NB: Jinja2 support via ../lang/init-python.el

(provide 'init-ansible)
