

;; mc

(use-package multiple-cursors
  :demand
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (add-hook 'cua-mode-hook
            (lambda () (when cua-mode
                    (unbind-key "C-S-c" cua--region-keymap)))))


;; mwc

(use-package mwc
  :load-path "~/.emacs.d/plugins/mwc")





(provide 'init-mc)
