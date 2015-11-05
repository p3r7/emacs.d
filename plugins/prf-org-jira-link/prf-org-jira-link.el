;;; prf-org-jira-link.el - Support for links to Atlassian JIRA in Org

(require 'org)

(defvar prf-org-jira-link-baseUrl "http://jira.com")

(org-add-link-type "jira" 'org-jira-open)

(defun org-jira-open (element)
  "Visit the element on JIRA.
     PATH should be a topic that can be thrown at the man command."
  (browse-url (concat prf-org-jira-link-baseUrl "/browse/" element)))

(provide 'prf-org-jira-link)

;;; prf-org-jira-link.el ends here
