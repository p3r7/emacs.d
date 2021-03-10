
(require 's)



;; DOC

(defun prf/ansible/open-module-doc (module &optional version)
  (let ((version (if version version "latest")))
    (browse-url (concat "https://docs.ansible.com/ansible/" version "/modules/" module "_module.html"))))

(defun prf/ansible/open-ansible-module-doc-at-point ()
  (interactive)
  (let ((module (thing-at-point 'symbol)))
    (when module
      (prf/ansible/open-module-doc module))))

;; REVIEW: elternatively https://github.com/lunaryorn/ansible-doc.el
;; view in emacs buffer, auto-gathering of list of modules
;; unmaintained


;; GOTO DEF

(defun prf/ansible/go-to-role (role)
  (interactive
   (list
    (read-string (format "role (%s): " (dotted-symbol-at-point))
                 nil nil (dotted-symbol-at-point))))
  (let* ((roles-path-list (prf/ansible/extract-roles-path-list-from-conf))
         (matched-path-list (prf/ansible/role/get-path role roles-path-list))
         (matched-path (car matched-path-list)))
    (if matched-path
        (dired matched-path)
      (message "Not found role %s in %S" role roles-path-list))))


(defun prf/ansible/role/get-path (role roles-path-list)
  (--keep
   (let ((role-path (concat it "/" role)))
     (when (file-directory-p role-path) role-path)) roles-path-list))


(defun prf/ansible/extract-roles-path-list-from-conf (&optional assume-local conf-file-localname)
  "Extract list of roles_path from Ansible conf"

  (unless conf-file-localname
    (setq conf-file-localname "/etc/ansible/ansible.cfg"))

  (unless assume-local
    (setq conf-file-localname
          (prf/ansible/conf-file-path-from-dd conf-file-localname)))

  (with-temp-buffer
    (insert-file-contents conf-file-localname)
    (goto-char (point-min))
    (re-search-forward "^roles_path *= *\\(.*\\)$")
    (let* ((roles-path-str (match-string 1))
           (roles-path-list (s-split ":" roles-path-str)))
      (if (and (not assume-local)
               (file-remote-p default-directory))
          (--map (prf/tramp/path/with-new-localname default-directory it) roles-path-list)
        roles-path-list))))


(defun prf/ansible/conf-file-path-from-dd (&optional conf-file-localname)
  "Build path for Ansible conf file depending on whether current location is local or remote."

  (unless conf-file-localname
    (setq conf-file-localname "/etc/ansible/ansible.cfg"))

  (if (file-remote-p default-directory)
      (let* ((vec (tramp-dissect-file-name default-directory))
             (new-vec (prf/tramp/vec/with-new-localname vec conf-file-localname)))
        (prf/tramp/vec/undissect new-vec))
    conf-file-localname))



;; MODE

(defun prf/ansible-file-p ()
  (string-match-p (regexp-quote "ansible") default-directory))

;; REVIEW: could have also used:
;; (add-to-list 'auto-mode-alist
;;              '("/ansible/.*\\.ya?ml\\'" . poly-ansible-mode))

(use-package ansible
  :demand
  :bind (:map ansible-key-map
	      ("C-h f" . prf/ansible/open-ansible-module-doc-at-point)
	      ("C-h C-f" . prf/ansible/open-ansible-module-doc-at-point))
  :init
  (defun prf/yml-hook/enable-ansible-minor-mode ()
    (when (prf/ansible-file-p) (ansible 1)))
  :config
  (add-hook 'yaml-mode-hook #'prf/yml-hook/enable-ansible-minor-mode))

;; Jinja2 inline support via polymode
;; NB: disabled as indents aggressively tasks in roles
;; also, seems to load strangely, the :disabled keyword doesn't appear to work
(when nil
  (use-package poly-ansible
    :disabled
    :after ansible))

;; NB: Jinja2 inside templates in ../lang/init-python.el, via mmm


;; OUTPUT

(defun json/ansible/format-region-strings ()
  (replace-regexp (regexp-quote "\\n") "\n"
                  nil (region-beginning) (region-end)))

(defun json/ansible/format ()
  (interactive)
  (call-interactively #'json/format)
  (apply-cmd-on-delimited-region (lambda ()
                                   (interactive)
                                   (replace-regexp (regexp-quote "\\n") "\n"
                                                   nil (region-beginning) (region-end)))))




(provide 'init-ansible)
