

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (with-eval-after-load 'cua-mode
    (unbind-key "C-S-c" cua--region-keymap)))




(provide 'init-mc)
