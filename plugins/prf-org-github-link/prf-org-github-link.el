;;; prf-org-github-link.el - Support for short links to Github in org-mode

(require 'org)

(defvar prf-org-github-link-baseUrl "https://github.com")

(org-add-link-type "gh" 'org-github-open)

(defun org-github-open (element)
  "Visit the element on Github."
  (browse-url (concat prf-org-github-link-baseUrl "/" element)))

(provide 'prf-org-github-link)

;;; prf-org-github-link.el ends here
