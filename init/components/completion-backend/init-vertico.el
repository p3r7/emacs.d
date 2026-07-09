
(use-package vertico
  :ensure t
  ;; :init
  ;; (vertico-mode)

  :config
  (savehist-mode 1))

;; `embark' allows multi-candidate selection + different actions
;; kinda like helm
(use-package embark
  :ensure t)

;; (use-package vertico-multiform
;;   :after vertico
;;   :init
;;   (vertico-multiform-mode)
;;   :custom
;;   ;; Disable Vertico for specific commands
;;   (vertico-multiform-commands
;;    '((find-file unobtrusive)
;;      (switch-to-buffer unobtrusive))))

;; `marginalia' provides doc
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))




(provide 'init-vertico)
