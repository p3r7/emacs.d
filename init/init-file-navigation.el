;; file navigation & buffer manipulation

(require 'init-lusty)
(require 'init-ibuffer)
(require 'init-uniquify)

;; jump to source
(global-set-key (kbd "C-h C-f") 'find-function)

(global-set-key "\C-x\C-k" 'kill-buffer)

(provide 'init-file-navigation)
