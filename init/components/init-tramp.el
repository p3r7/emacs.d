
;; manually downloaded and configured upstream versions of tramp
;; http://www.gnu.org/software/tramp/#Installation
(if (and (= emacs-major-version 24)
	 (= emacs-minor-version 3))
    (add-to-list 'load-path "~/.emacs.d/plugins-src/tramp-2.2.11/lisp"))
(if (and (= emacs-major-version 24)
	 (= emacs-minor-version 5))
    (add-to-list 'load-path "~/.emacs.d/plugins-src/tramp-2.2.12/lisp"))
;; NB: solves copy & mv, but potentially crashes find-file



(use-package tramp
  :demand
  :config
  ;; (setq tramp-verbose 6)

  ;; disable vc for remote files (speed increase)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))


;; -------------------------------------------------------------------------
;; SHELL & TERM

(use-package tramp-term
  :after (tramp)
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
;; Supplementary Cnnx Methods

;; Docker
(use-package docker-tramp
  :after (tramp))

;; Vagrant
;; (use-package vagrant-tramp
;;   :if (and (executable-find "vagrant")
;;            (not (windows-nt-p)))
;;   :config
;;   (eval-after-load 'tramp
;;     '(vagrant-tramp-enable)))

;; KiTTY
(when (and (executable-find "kscp")
	   (executable-find "klink"))
  (use-package tramp-kitty
    :load-path "~/.emacs.d/plugins/tramp-kitty"
    :after (tramp)
    :config
    (tramp-kitty-set-session-map-cache)
    (with-eval-after-load "putty-open"
      (add-to-list 'putty-open-putty--session-methods "klinkx")
      (add-to-list 'putty-open-putty--ssh-methods "kscp")
      (add-to-list 'putty-open-putty--ssh-methods "klink"))))

;; SSH w/ key support
(use-package tramp-sshi
  :load-path "~/.emacs.d/plugins/tramp-sshi"
  :after (tramp))

;; PuTTY w/ SSH key support
(use-package tramp-plinki
  :load-path "~/.emacs.d/plugins/tramp-plinki"
  :after (tramp))

;; open in putty / kitty
(use-package putty-open
  :load-path "~/.emacs.d/plugins/putty-open"
  :after (tramp)
  :init
  (when (executable-find "kitty")
    (setq putty-open-putty-exec "kitty")))


;; -------------------------------------------------------------------------
;; HELPER UTILS

(use-package prf-tramp
  :quelpa (prf-tramp :fetcher github :repo "p3r7/prf-tramp")
  :after (tramp)
  :config
  (if (not (fboundp '_sh))
      (defalias '_sh 'prf/tramp/shell))
  (defalias '_rsh 'prf/tramp/remote-shell))

(use-package ansible-tramp
  :load-path "~/.emacs.d/plugins/ansible-tramp"
  :after (request-deferred prf-tramp)
  :config
  (when ansible-tramp-inventory-http-url
    (ansible-tramp-set-inventory-cache-http)))



(provide 'init-tramp)
