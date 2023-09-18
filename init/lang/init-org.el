
(require 'dash)
(require 's)
(require 'f)



;; MAIN

(use-package org
  :bind (
         :map org-mode-map
         ;; NB: og bound to `org-fill-paragraph'
         ("M-q" . toggle-truncate-lines)
         ;; NB: got overriden by `cua-selection-mode'
         ("C-y" . org-yank)
         ("C-c O" . prf/org-link-open-alt)
         )

  :hook ((org-mode . (lambda () (setq word-wrap t))))

  :init

  ;; GENERAL
  (setq org-directory prf/dir/notes)

  ;; DISPLAY
  (setq org-highlight-latex-and-related '(latex)
        org-ellipsis "⤵"
        ;; org-hide-leading-stars t
        org-startup-folded t
        org-startup-with-inline-images t)

  (setq org-adapt-indentation t)

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

  ;; NB: org-mode loading specifity, needed pre emacs 29
  ;; http://orgmode.org/worg/org-faq.html#load-org-after-setting-variables
  (when (featurep 'org-install)
    (require 'org-install))

  ;; enable back easy-template
  (when (>= emacs-major-version 27)
    (require 'org-tempo))

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



  
  ;; LINKS

  ;; TODO: use https://orgmode.org/manual/Link-Abbreviations.html instead for those

  (setq p3r7/org-link-abbrev-alist '(("archwiki" . "https://wiki.archlinux.org/index.php/")
                                     ("gh" . "https://github.com/")
                                     ("gl" . "https://gitlab.com/")
                                     ("hn" . "https://news.ycombinator.com/item?id=")
                                     ("lines" . "https://llllllll.co/t/")
                                     ("thing" . "https://www.thingiverse.com/thing:")))

  (--each p3r7/org-link-abbrev-alist
    (let* ((link-prefix (car it))
           (browse-fn `(lambda (e)
                         (let ((org-link-abbrev-alist p3r7/org-link-abbrev-alist))
                           (browse-url (org-link-expand-abbrev (concat ,link-prefix ":" e)))))))
      (org-link-set-parameters link-prefix :follow browse-fn)))

  (defun prf/org/link-apply-prefix (txt)
    "Rework link TXT, swapping prefix w/ shorted one if matches `p3r7/org-link-abbrev-alist'."
    (let ((prfx (--some (and (s-starts-with? (cdr it) txt) (not (string= (cdr it) txt)) it) p3r7/org-link-abbrev-alist)))
      (if prfx
          (s-replace (cdr prfx) (concat (car prfx) ":") txt)
        txt)))


  (defvar prf/org-link-alt-type-alist nil "Alist of link type to alt link type mapping, for `prf/org-link-open-alt'")

  (defvar prf/fn-v/org-element-context nil "Copy of original value of fn `org-element-context'")
  (with-eval-after-load 'org-element
    (setq prf/fn-v/org-element-context (symbol-function #'org-element-context)))

  (defun prf/org-element-alt-context ()
    "Same as `org-element-context' but replaces link type with alternative one from `prf/org-link-alt-type-alist'.

If thing under point is not a link or has no matching alt type"
    (if-let* ((context (funcall prf/fn-v/org-element-context))
              (type (car context))
              (_ (eq type 'link))
              (link-type (plist-get (cadr context) :type))
              (alt-link-type (symbol-name (alist-get (intern link-type) prf/org-link-alt-type-alist))))
        (list 'link
              (plist-put (cadr context) :type alt-link-type))
      (user-error "No alt link method available for thing at point.")))

  (defun prf/org-link-open-alt ()
    (interactive)
    (cl-letf (((symbol-function #'org-element-context) #'prf/org-element-alt-context))
      (org-open-at-point))))



;; COMMANDS

(defadvice org-yank (around prf/org-yank-prefix-link activate)
  "Advice around `org-yank' that will auto-compact current entry in `kill-ring' if it matches `p3r7/org-link-abbrev-alist'."
  (let* ((kill (or (and kill-ring (current-kill 0)) ""))
         (new-kill (prf/org/link-apply-prefix kill)))
    (unless (s-blank? new-kill)
      (kill-new new-kill t))
    ad-do-it))



;; ORG INDEX

(defun prf/org/file-path-org-p (f)
  "Return t if file path F corresponds to an org file."
  (let ((cleaned-f (s-chop-suffixes '("gpg" "bak") f)))
    (equal (f-ext cleaned-f) "org")))

(defvar prf/org/index-file-exclude-regexp "\\.gpg\\'")

(defun prf/org/file-path-indexable-p (f)
  "Return t if file path F corresponds to an indexable org file."
  (and (prf/org/file-path-org-p f)
       (f-descendant-of? f prf/dir/notes) ; REVIEW: not sure we should enforce this one here
       (not (f-descendant-of? f prf-backup-dir))
       (not (f-descendant-of? f prf-auto-save-dir))
       (not (string-match-p prf/org/index-file-exclude-regexp f))))

(defun prf/org/index/update-id-locations-no-open (files)
  "Wrapper around `org-id-update-id-locations' that closes files after they got opened for indexing."
  (let ((buffs-snapshot (buffer-list)))
    (org-id-update-id-locations files)
    (mapc #'kill-buffer
          (-difference (buffer-list) buffs-snapshot))))

(defun prf/org/index-rescan-all ()
  "Populate `org-id-locations' by rescaning recursively all files in `prf/dir/notes'."
  (interactive)
  (prf/org/index/update-id-locations-no-open
   (f-files prf/dir/notes #'prf/org/file-path-indexable-p t)))



;; BACK-LINKS (ROAM)

(unless (windows-nt-p)

  (use-package org-roam
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n a" . prf/org-roam/add-index-current)
           ("C-c n r" . prf/org-roam/rescan))

    :custom
    (org-roam-directory prf/dir/notes)

    :init
    (setq org-roam-v2-ack t)
    (setq org-roam-file-exclude-regexp prf/org/index-file-exclude-regexp)
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?" :target
             ;; (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
             (file+head "${slug}.org" "#+title: ${title}\n")
             :unnarrowed t)))

    :config
    (org-roam-setup)

    (defun prf/org-roam/add-index-current ()
      "Add index to file of currently visited buffer, if applicable."
      (interactive)

      (unless (and (buffer-file-name)
		   (file-exists-p (buffer-file-name)))
        (user-error "Current buffer is not visiting a file that exists on disk."))

      (unless (prf/org/file-path-indexable-p (buffer-file-name))
        (user-error "Current buffer is not visiting an indexable file."))

      (unless (org-id-get)
        (org-id-get-create)
        (call-interactively #'save-buffer))

      (prf/org/index/update-id-locations-no-open (list (buffer-file-name)))

      (org-roam-db-update-file))

    (defun prf/org-roam/rescan ()
      "Force rescan of whole `prf/dir/notes'."
      (interactive)
      (prf/org/index-rescan-all)
      (org-roam-db-sync)))


  (use-package org-roam-ui
    :after (simple-httpd websocket org-roam)
    :delight
    (org-roam-ui-mode " RoamUI")
    (org-roam-ui-follow-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)))



;; TABLES

(use-package valign
  :disabled
  :quelpa (valign :fetcher github :repo "casouri/valign")
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode)
  ;; (setq valign-fancy-bar t)
  )



;; IMAGES

(use-package org-download
  :init
  (setq org-download-method 'directory))



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

(use-package poly-org
  :after polymode)



;; AGENDA

(use-package org-agenda
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



;; INDEX & SEARCH

(use-package org-ql
  ;; REVIEW: in MELPA, but failed to install at time of writting
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"))

(use-package om
  :disabled
  :quelpa (om :fetcher github :repo "ndwarshuis/om.el"))

(use-package helm-org-rifle
  :after (org helm))

(require 'init-deft)




(provide 'init-org)
