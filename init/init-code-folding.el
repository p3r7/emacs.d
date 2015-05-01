
;; code folding
;; - selective-display
;; - hideshow : [[http://www.emacswiki.org/emacs/HideShow]]
;;   https://github.com/shanecelis/hideshow-org  http://gnufool.blogspot.fr/2009/03/make-hideshow-behave-more-like-org-mode.html
;;   https://gist.github.com/doitian/1571162
;; (require 'hideshow-fringe)

(require 'hideshowvis)
(setq hideshowvis-ignore-same-line t) ;; breaks stuff
(hideshowvis-symbols) ;; sam code as hideshow-fringe, but kinda breaks stuff

(provide 'init-code-folding)
