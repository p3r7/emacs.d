
;; -------------------------------------------------------------------------
;; HELP / DESCRIBE

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h o" . helpful-at-point)
	 ("C-h F" . helpful-function)))

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
