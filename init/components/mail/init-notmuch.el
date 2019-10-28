

(use-package notmuch
  :preface (setq-default notmuch-command (executable-find "notmuch"))
  :if notmuch-command
  :hook
  (((notmuch-search-mode notmuch-show-mode) . emojify-mode)
   ;; ((notmuch-search-mode notmuch-show-mode) . (lambda () (message "notmuch hooks")))
   )
  :bind (
         :map notmuch-hello-mode-map
         ("u" . prf/notmuch-search-unread)
         ("i" . prf/notmuch-search-inbox)
         :map notmuch-search-mode-map
         ("r" . notmuch-search-reply-to-thread)
         ("R" . notmuch-search-reply-to-thread-sender)
         ("k" . prf/notmuch-mark-read-archive)
         ("K" . prf/notmuch-mark-read-delete)
         :map notmuch-show-mode-map
         ("C-c C-o" . notmuch-show-browse-urls))

  :init
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format '(("date" . "%12s ")
                                       ("tags" . "(%s)")
                                       ("count" . "%-7s ")
                                       ("authors" . "%-20s ")
                                       ("subject" . "%s ")))

  :config
  (defun prf/notmuch-search-unread ()
    (interactive)
    (notmuch-hello-search "tag:unread"))
  (defun prf/notmuch-search-inbox ()
    (interactive)
    (notmuch-hello-search "tag:inbox"))

  (defun prf/notmuch-mark-read-archive ()
    (interactive)
    (notmuch-search-tag '("-unread"))
    (notmuch-search-archive-thread))
  (defun prf/notmuch-mark-read-delete ()
    (interactive)
    (notmuch-search-tag '("-unread" "+delete"))
    (notmuch-search-archive-thread)))


(use-package helm-notmuch
  :after (notmuch helm))




(provide 'init-notmuch)
