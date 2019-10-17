;; file navigation & buffer manipulation

;; ------------------------------------------------------------------------
;; FILE / BUFFER NAVIGATION

(require 'init-ibuffer)
(require 'init-uniquify)

(use-package shackle
  :demand)


;; ------------------------------------------------------------------------
;; CODE NAVIGATION

;; jump to source
(global-set-key (kbd "C-h C-f") 'find-function)

(require 'init-dumb-jump)



(provide 'init-file-navigation)
