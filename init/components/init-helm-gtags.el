
;; NOTE: https://github.com/syohex/emacs-helm-gtags/issues/17


(use-package helm-gtags
  :after (helm)
  :bind (
	 :map helm-gtags-mode-map
	      ("C-c g a" . helm-gtags-tags-in-this-function)
	      ("C-j" . helm-gtags-select)
	      ("M-." . helm-gtags-dwim)
	      ("M-," . helm-gtags-pop-stack)
	      ("C-c <" . helm-gtags-previous-history)
	      ("C-c >" . helm-gtags-next-history))
  :init
  (setq helm-gtags-prefix-key "\C-cg"
	helm-gtags-ignore-case t
	helm-gtags-auto-update t
	helm-gtags-use-input-at-cursor t
	helm-gtags-pulse-at-cursor t
	helm-gtags-suggested-key-mapping t)
  :config
  ;; REVIEW: move to :hook
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode))



(provide 'init-helm-gtags)
