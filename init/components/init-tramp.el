
;; manually downloaded and configured upstream versions of tramp
;; http://www.gnu.org/software/tramp/#Installation
(if (and (= emacs-major-version 24)
	 (= emacs-minor-version 3))
    (add-to-list 'load-path "~/.emacs.d/plugins-src/tramp-2.2.11/lisp"))
(if (and (= emacs-major-version 24)
	 (= emacs-minor-version 5))
    (add-to-list 'load-path "~/.emacs.d/plugins-src/tramp-2.2.12/lisp"))
;; NB: solves copy & mv, but potentially crashes find-file

;; (setq tramp-verbose 6)

;; disable vc for remote files (speed increase)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


;; -------------------------------------------------------------------------
;; SHELL & TERM

(use-package tramp-term
  :defer t)

;; http://www.emacswiki.org/emacs/AnsiTermHints#toc4
;; http://stackoverflow.com/questions/12802236/emacs-keyboard-shortcut-to-run-ansi-term-with-a-specific-shell
;; REVIEW: redundant w/ tramp-term ?
(defun remote-term (new-buffer-name cmd &rest switches)
  ""
  (interactive)
  (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))


;; -------------------------------------------------------------------------
;; Supplementary methods

;; Vagrant
(when (and (executable-find "vagrant")
	   (not (windows-nt-p)))
  (use-package vagrant-tramp
    :config
    (eval-after-load 'tramp
      '(vagrant-tramp-enable))))

;; KiTTY
(when (and (executable-find "kscp")
	   (executable-find "klink"))
  (use-package tramp-kitty
    :load-path "~/.emacs.d/plugins/tramp-kitty"))


  (provide 'init-tramp)
