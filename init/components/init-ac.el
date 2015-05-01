
(prf/require-plugin 'auto-complete)

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-auto-complete.el

(when (require 'auto-complete-config)
  (ac-config-default)

  ;; (define-key ac-mode-map (kbd "<M-S-iso-lefttab>") 'auto-complete)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (define-key ac-completing-map (kbd "TAB") 'ac-complete)
  (define-key ac-completing-map (kbd "RET") nil)

  ;; [[<#readline-complete]]
  ;; (require 'readline-complete)
  ;; (add-to-list 'ac-modes 'shell-mode)
  ;; (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
  )

(provide 'init-ac)
