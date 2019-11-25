
(use-package org
  :init

  ;; GENERAL
  (setq org-directory default-directory)

  ;; DISPLAY
  (setq org-highlight-latex-and-related '(latex)
        org-ellipsis "⤵"
        ;; org-hide-leading-stars t
        )

  ;; EDITING BEAHVIOURS
  ;; adjust level while pasting, if not wanted do C-u C-y
  (setq org-yank-adjusted-subtrees t)

  ;; LINKS
  ;; http://orgmode.org/manual/Code-evaluation-security.html
  ;; Safe as long as I only use my own org files
  ;; (require 'org-link-sshell)
  (setq org-confirm-shell-link-function nil
	org-confirm-elisp-link-function nil)
  ;; (setq org-link-abbrev-alist
  ;;       '(
  ;;         ("google"   . "http://www.google.com/search?q=")
  ;;         ("gmap"     . "http://maps.google.com/maps?q=%s")
  ;;         ))

  :config

  ;; NB: org-mode loading specifity
  ;; http://orgmode.org/worg/org-faq.html#load-org-after-setting-variables
  (require 'org-install)

  ;; TIME TRACKING
  ;; [[http://orgmode.org/manual/Clocking-work-time.html]]
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)

  ;; WYSIWYG
  (defvar prf/org/wysiwyg-active nil)
  (defvar prf/org/wysiwyg-vars-default-alist nil)
  (defvar prf/org/wysiwyg-vars-override-alist
    '((org-startup-indented . t)
      (org-bullets-bullet-list . '(" "))
      (org-pretty-entities . t)
      (org-hide-emphasis-markers . t)
      (org-agenda-block-separator . "")
      (org-fontify-whole-heading-line . t)
      (org-fontify-done-headline . t)
      (org-fontify-quote-and-verse-blocks . t)))


  ;; (defun prf/org/toggle-wysiwyg ()
  ;;   (interactive)

  ;;   (if prf/org/wysiwyg-active
  ;; 	(progn ())
  ;;     (setq prf/org/wysiwyg-vars-default-alist
  ;; 	    (--map (cons (car it) (eval (car it))) prf/org/wysiwyg-vars-override-alist))
  ;;     (setq org-startup-indented t
  ;; 	    org-bullets-bullet-list '(" ")
  ;; 	    org-pretty-entities t
  ;; 	    org-hide-emphasis-markers t
  ;; 	    org-agenda-block-separator ""
  ;; 	    org-fontify-whole-heading-line t
  ;; 	    org-fontify-done-headline t
  ;; 	    org-fontify-quote-and-verse-blocks
  ;; 	    )
  ;;     (setq prf/org/wysiwyg-active t)))
  )



;; CAPTURE

(use-package org-capture
  :ensure nil
  :no-require
  :demand
  :after org

  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c i" . org-insert-link))

  :init

  ;; (setq org-default-notes-file (concat org-directory "/captures.org")
  ;; 	org-agenda-files (list (concat org-directory "/tasks.org")
  ;; 			       (concat org-directory "/captures.org")))

  (setq org-mobile-inbox-for-pull "~/Dropbox/textfiles/mobileorg.org"
	org-mobile-directory "~/Dropbox/textfiles/MobileOrg"))



;; EXPORT

(use-package ox
  :ensure nil
  :no-require
  :demand
  :after org

  :init

  (setq
   org-export-with-section-numbers nil
   org-html-validation-link nil))

(use-package htmlize
  :after ox)

(use-package ox-slack
  :after ox)

(use-package ox-jekyll-lite
  :quelpa (ox-jekyll-lite :fetcher github :repo "peterewills/ox-jekyll-lite")
  :after ox)

(defun prf/org-export-to-doc ()
  (interactive)
  (let* ((org-export-odt-preferred-output-format "doc")
	 (org-odt-preferred-output-format "doc")
	 (soffice-cmd (executable-find "soffice"))
	 (org-export-odt-convert-processes `(("LibreOffice" ,(concat "\"" soffice-cmd "\" --headless --convert-to %f%x --outdir %d %i"))))
	 (org-odt-convert-processes `(("LibreOffice" ,(concat "\"" soffice-cmd "\" --headless --convert-to %f%x --outdir %d %i")))))
    (if (and (file-exists-p soffice-cmd)
	     (executable-find "zip"))
	(org-odt-export-to-odt)
      (message "Missing either soffice and/or zip program(s)."))))

;; TODO: https://github.com/fniessen/org-html-themes



;; BABEL

(use-package org-babel
  :ensure nil
  :no-require
  :demand
  :after org

  :config

  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-confirm-babel-evaluate nil
	org-edit-src-content-indentation 0)

  (when (featurep 'ob-sh)
    (add-to-list 'org-babel-load-languages '(sh . t)))
  (add-to-list 'org-babel-load-languages '(calc . t))
  (add-to-list 'org-babel-load-languages '(gnuplot . t))
  (add-to-list 'org-babel-load-languages '(sql . t))
  (add-to-list 'org-babel-load-languages '(sqlite . t))
  (when (executable-find "python")
    (add-to-list 'org-babel-load-languages '(python . t)))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))



;; AGENDA

(use-package org-babel
  :ensure nil
  :no-require
  :demand
  :after org

  :bind (("C-c a" . org-agenda)))



;; HABIT

(use-package org-habit
  :ensure nil
  :no-require
  :demand
  :after org)




(provide 'init-org)
