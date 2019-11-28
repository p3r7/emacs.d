
(use-package auto-complete
  :demand)

(use-package auto-complete-config
  :ensure nil
  :after (auto-complete)

  :bind (
	 :map ac-mode-map
         ("M-TAB" . auto-complete)
         ;; ("<M-S-iso-lefttab>" . auto-complete)
	 :map ac-completing-map
	 ("TAB" . ac-complete)
	 ("RET" . nil))

  :config
  (when (member prf/fav-completion-at-point '(ac auto-complete))
    (ac-config-default)))




(provide 'init-ac)
