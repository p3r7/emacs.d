
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

  (add-hook 'c-mode-common-hook #'prf/hideshowvis-c-mode-common-hook))




(provide 'init-hideshowvis)
