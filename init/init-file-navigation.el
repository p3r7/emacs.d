;; file navigation & buffer manipulation


;; FILE / BUFFER NAVIGATION

(require 'init-ibuffer)
(require 'init-uniquify)

(use-package shackle
  :demand)



;; CODE NAVIGATION

(require 'init-dumb-jump)

;; NB: elisp / emacs -specific stuff in ~/.emacs.d/init/lang/init-elisp.el



;; SEARCH & REPLACE

(use-package wgrep)




(provide 'init-file-navigation)
