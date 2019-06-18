
;; [[http://www.emacswiki.org/emacs/HideShow]]
;; https://github.com/shanecelis/hideshow-org  http://gnufool.blogspot.fr/2009/03/make-hideshow-behave-more-like-org-mode.html
;; https://gist.github.com/doitian/1571162
;; (require 'hideshow-fringe)

(use-package hideshowvis
  :init
  (setq hideshowvis-ignore-same-line t) ;; breaks stuff
  :config
  (hideshowvis-symbols) ;; same code as hideshow-fringe, but kinda breaks stuff

  (defun prf/hideshowvis-c-mode-common-hook ()
    (hs-minor-mode t)
    (hideshowvis-minor-mode t)
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all))

  (add-hook 'c-mode-common-hook 'prf/origami-prog-mode-hook))


(provide 'init-hideshowvis)
