
;; [[http://www.emacswiki.org/emacs/DiredTweaks]]
;; http://stackoverflow.com/questions/14602291/dired-how-to-get-really-human-readable-output-find-ls-option
;; http://stackoverflow.com/questions/4115465/emacs-dired-too-much-information

(setq
 dired-dwim-target t ;; if other window -> set as default dir for copy
 ls-lisp-dirs-first t ;; display dirs 1st
 dired-listing-switches "-alh"
 diredp-hide-details-initially-flag nil
 diredp-hide-details-propagate-flag nil)

(when (executable-find "busybox")
  (setq dired-use-ls-dired nil))

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook ;; do not create other dired buffers when navigating
	  ;; TODO: far from being perfect (closes all dired windows, not just current)
	  (lambda ()
	    (define-key dired-mode-map (kbd "<return>")
	      #'dired-find-alternate-file) ; was dired-advertised-find-file
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))
					; was dired-up-directory
	    ))

(use-package dired+
  :load-path "~/.emacs.d/plugins/dired+"
  :defer t
  ;; :config
  ;; (eval-after-load "dired-aux"
  ;;     '(require 'dired-async))
  )

(use-package dired-rsync
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))


(provide 'init-dired)
