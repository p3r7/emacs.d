;;TODO: [[http://orgmode.org/worg/org-faq.html#load-org-after-setting-variables]]

(use-package org
  :init

  ;; GENERAL
  (setq
   org-directory default-directory)

  ;; DISPLAY
  (setq
   ;; org-hide-leading-stars t
   org-ellipsis "⤵"
   org-highlight-latex-and-related '(latex))

  ;; EDITING BEAHVIOURS
  (setq
   org-yank-adjusted-subtrees t ;; adjust level while pasting, if not wanted do C-u C-y
   )

  ;; CAPTURE
  ;; (setq org-default-notes-file (concat org-directory "/captures.org")
  ;; 	org-agenda-files (list (concat org-directory "/tasks.org")
  ;; 			       (concat org-directory "/captures.org")))

  ;; CAPTURE: MobileOrg
  (setq org-mobile-inbox-for-pull "~/Dropbox/textfiles/mobileorg.org"
	org-mobile-directory "~/Dropbox/textfiles/MobileOrg")

  ;; EXPORT
  (setq
   org-export-with-section-numbers nil
   org-html-validation-link nil)

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
  (require 'org-install)
  (require 'org-habit)

  ;; DISPLAY
  (org-display-inline-images t t)

  ;; BABEL
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)))
  (when (executable-find "python")
    (org-babel-do-load-languages
     'org-babel-load-languages '((python . t))))
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-confirm-babel-evaluate nil
	org-edit-src-content-indentation 0)

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


;; ------------------------------------------------------------------------
;; FORMAT

;; http://stackoverflow.com/questions/17621495/emacs-org-display-inline-images
(org-display-inline-images t t)


;; ------------------------------------------------------------------------
;; EXPORT

;; TODO: https://github.com/fniessen/org-html-themes

(use-package htmlize
  :after (org))

(use-package ox-slack
  :after (org))

(use-package ox-jekyll-lite
  :quelpa (ox-jekyll-lite :fetcher github :repo "peterewills/ox-jekyll-lite")
  :after (org))

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


;; ------------------------------------------------------------------------
;; CAPTURE

(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ci" 'org-insert-link)
(global-set-key "\C-ca" 'org-agenda)


;; ------------------------------------------------------------------------

(provide 'init-org)
