;;TODO: [[http://orgmode.org/worg/org-faq.html#load-org-after-setting-variables]]
(require 'org-install)
(require 'org-habit)

;; ------------------------------------------------------------------------
;; BASIC

(setq
 org-directory default-directory
 ;; org-hide-leading-stars t
 org-yank-adjusted-subtrees t ;; adjust level while pasting, if not wanted do C-u C-y
 ;; org-default-notes-file (concat org-directory "/captures.org")
 ;; org-agenda-files (list (concat org-directory "/tasks.org")
 ;; (concat org-directory "/captures.org"))
 )


;; ------------------------------------------------------------------------
;; FORMAT

;; http://stackoverflow.com/questions/17621495/emacs-org-display-inline-images
(org-display-inline-images t t)

(setq
 org-ellipsis "⤵"
 org-highlight-latex-and-related '(latex)
 )


;; ------------------------------------------------------------------------
;; EXPORT

(prf/require-plugin 'htmlize)
;; TODO: https://github.com/fniessen/org-html-themes

(setq org-export-with-section-numbers nil)
(setq org-html-validation-link nil)

(prf/require-plugin 'ox-slack)

;; ------------------------------------------------------------------------
;; CAPTURE

(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ci" 'org-insert-link)
(global-set-key "\C-ca" 'org-agenda)


;; ------------------------------------------------------------------------
;; TIME TRACKING

;; [[http://orgmode.org/manual/Clocking-work-time.html]]
;; http://orgmode.org/manual/Clocking-work-time.html#fnd-2
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)


;; ------------------------------------------------------------------------
;; BABEL

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   ))


;; ------------------------------------------------------------------------
;; LINKS

;; (setq org-link-abbrev-alist
;;       '(
;;         ("google"   . "http://www.google.com/search?q=")
;;         ("gmap"     . "http://maps.google.com/maps?q=%s")
;;         ))

;; http://orgmode.org/manual/Code-evaluation-security.html
;; Safe as long as I only use my own org files
;; (require 'org-link-sshell)
(setq org-confirm-shell-link-function nil
      org-confirm-elisp-link-function nil)


;; ------------------------------------------------------------------------
;; MOBILE ORG

;; MobileOrg
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/textfiles/mobileorg.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/textfiles/MobileOrg")


;; ------------------------------------------------------------------------

(provide 'init-org)
