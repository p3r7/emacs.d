
(use-package origami
  :config
  (defun prf/origami-prog-mode-hook ()
    (origami-mode)
    (local-set-key (kbd "C-c <right>") 'origami-show-node)
    (local-set-key (kbd "C-c <left>")  'origami-close-node)
    (local-set-key (kbd "C-c <up>")    'origami-close-all-nodes)
    (local-set-key (kbd "C-c <down>")  'origami-open-all-nodes))

  (add-hook 'prog-mode-hook 'prf/origami-prog-mode-hook))

(provide 'init-origami)
