
;; [[http://www.emacswiki.org/emacs/DiredTweaks]]
;; http://stackoverflow.com/questions/14602291/dired-how-to-get-really-human-readable-output-find-ls-option
;; http://stackoverflow.com/questions/4115465/emacs-dired-too-much-information

;; TODO: hide username / group when on windows NT
;; could use https://emacs.stackexchange.com/questions/35676/customize-direds-display
;; or get inspired by dired-hide-details-mode


(use-package dired
  :ensure nil
  :demand
  :bind (:map dired-mode-map
	      ("C-c C" . prf/dired-do-copy-not-dwin)
	      ("C-c R" . prf/dired-do-rename-not-dwin)
	      ;; do not create other dired buffers when navigating
	      ;; TODO: far from being perfect (closes all dired windows, not just current)
	      ("<return>" . dired-find-alternate-file)
	      ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :init
  (setq
   dired-dwim-target t	;; if other window -> set as default dir for copy
   ls-lisp-dirs-first t ;; display dirs 1st
   dired-listing-switches "-alh"
   ;; dired-listing-switches "-alDphgG"
   diredp-hide-details-initially-flag nil
   diredp-hide-details-propagate-flag nil)

  (when (executable-find "busybox")
    (setq dired-use-ls-dired nil))

  :config
  (defun prf/dired-do-copy-not-dwin ()
    (interactive)
    (let ((dired-dwim-target nil))
      (dired-do-copy)))
  (defun prf/dired-do-rename-not-dwin ()
    (interactive)
    (let ((dired-dwim-target nil))
      (dired-do-rename)))

  (put 'dired-find-alternate-file 'disabled nil))


(use-package dired+
  :load-path "~/.emacs.d/plugins/dired+"
  :after (dired)
  :demand
  ;; :config
  ;; (eval-after-load "dired-aux"
  ;;     '(require 'dired-async))
  )

(use-package dired-rsync
  :after (dired)
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))


(use-package dired-git-info
  :defer t
  :bind (
         :map dired-mode-map
         (")" . dired-git-info-mode)))


(provide 'init-dired)
