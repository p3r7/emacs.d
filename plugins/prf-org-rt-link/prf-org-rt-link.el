;;; prf-org-rt-link.el - Support for links to Best Practical Request Tracker in Org

(require 'org)

(defvar prf-org-rt-link-baseUrl "http://rt.com")

(org-add-link-type "rt" 'org-rt-open)

(defun org-rt-open (element)
  "Visit the element on RT.
     PATH should be a topic that can be thrown at the man command."
  (browse-url (concat prf-org-rt-link-baseUrl "/Ticket/Display.html?id=" element)))

(provide 'prf-org-rt-link)

;;; prf-org-rt-link.el ends here
