
;; -------------------------------------------------------------------------
;; HELP / DESCRIBE (EMACS)

;; automatic display of local help
;; used to display errors of eclim in mb
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h o" . helpful-at-point)
	 ("C-h F" . helpful-function)))

;; stolen from John Wiegley
(define-prefix-command 'prf/lisp-find-map)
(bind-key "C-h e" #'prf/lisp-find-map)
(bind-key "C-h e e" #'view-echo-area-messages)
(bind-key "C-h e f" #'find-function)
(bind-key "C-h e k" #'find-function-on-key)
(bind-key "C-h e l" #'find-library)
(bind-key "C-h e v" #'find-variable)
(bind-key "C-h e V" #'apropos-value)

(use-package which-key
  :delight
  :config
  (which-key-mode))


;; -------------------------------------------------------------------------
;; TEXINFO

;; TODO: http://ergoemacs.org/emacs/emacs_adding_browser_keys.html
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")


;; -------------------------------------------------------------------------
;; EPUB

(require 'init-nov)


;; -------------------------------------------------------------------------
;; MAN

(defalias 'man 'woman)
(setq woman-use-own-frame nil)

(use-package tldr
  :init
  (setq tldr-enabled-categories '("common" "linux")))

(use-package tldr-ext
  :load-path "~/.emacs.d/plugins/tldr-ext"
  :after (tldr)
  :config
  (tldr-ext-activate))



(provide 'init-doc)
