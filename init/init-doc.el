

;; HELP / DESCRIBE (EMACS)

;; automatic display of local help
;; also used to display errors of eclim in mb
(use-package help-at-pt
  :ensure nil
  :demand

  :init
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)

  :config
  (help-at-pt-set-timer))


(require 'init-eldoc)

;; NB: elisp / emacs -specific stuff in HELP section in ~/.emacs.d/init/lang/init-elisp.el



;; TEXINFO

;; TODO: http://ergoemacs.org/emacs/emacs_adding_browser_keys.html
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")



;; EPUB

(require 'init-nov)



;; MAN

(defalias 'man 'woman)
(setq woman-use-own-frame nil)

(use-package tldr
  :init
  (setq tldr-enabled-categories '("common" "linux")))

(use-package tldr-ext
  :load-path "~/.emacs.d/plugins/tldr-ext"
  :after tldr
  :config
  (when (and tldr-ext-directory-path
             (file-exists-p tldr-ext-directory-path))
    (tldr-ext-activate)))



;; WEBSITES / ARTICLES

(require 'init-pocket)




(provide 'init-doc)
