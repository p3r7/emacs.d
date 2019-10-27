
(use-package auto-complete
  :demand)

(use-package auto-complete-config
  :ensure nil
  :after (auto-complete)

  :bind (
	 :map ac-mode-map
	 ("M-TAB" . auto-complete)
	 :map ac-completing-map
	 ("TAB" . ac-complete)
	 ("RET" . nil))

  :config
  (when (member prf/fav-completion-at-point '(ac auto-complete))
    (ac-config-default))

  ;; (define-key ac-mode-map (kbd "<M-S-iso-lefttab>") 'auto-complete)
  ;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  ;; (define-key ac-completing-map (kbd "TAB") 'ac-complete)
  ;; (define-key ac-completing-map (kbd "RET") nil)

  ;; [[<#readline-complete]]
  ;; (require 'readline-complete)
  ;; (add-to-list 'ac-modes 'shell-mode)
  ;; (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
  )

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-auto-complete.el




(provide 'init-ac)
