

(use-package multiple-cursors
  :demand
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (with-eval-after-load 'cua-base
    (unbind-key "C-S-c" cua--region-keymap)))




(provide 'init-mc)
