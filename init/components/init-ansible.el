
;; could have also added: https://github.com/lunaryorn/ansible-doc.el

;; alternative w/ better jinja2 integration: http://melpa.org/#/poly-ansible

;; ------------------------------------------------------------------------
;; DOC

(defun prf/ansible/open-module-doc (module &optional version)
  (let ((version (if version version "latest")))
    (browse-url (concat "https://docs.ansible.com/ansible/" version "/modules/" module "_module.html"))))

(defun prf/ansible/open-ansible-module-doc-at-point ()
  (interactive)
  (let ((module (thing-at-point 'symbol)))
    (when module
      (prf/ansible/open-module-doc module))))


;; ------------------------------------------------------------------------
;; MODE

(use-package ansible
  :bind (:map ansible-key-map
	      ("C-c h" . prf/ansible/open-ansible-module-doc-at-point)
	      ("C-c C-h" . prf/ansible/open-ansible-module-doc-at-point))
  :init
  (defun prf/yml-hook/enable-ansible-minor-mode ()
    (if (string-match-p (regexp-quote "ansible") default-directory)
	(when (string-match-p (regexp-quote "ansible") default-directory)
	  (ansible 1))))
  :config
  (add-hook 'yaml-mode-hook #'prf/yml-hook/enable-ansible-minor-mode))

;; NB: Jinja2 support via ../lang/init-python.el

(provide 'init-ansible)
