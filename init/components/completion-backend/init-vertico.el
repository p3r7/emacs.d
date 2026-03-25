
(use-package vertico
  :ensure t
  ;; :init
  ;; (vertico-mode)
  )

;; (use-package vertico-multiform
;;   :after vertico
;;   :init
;;   (vertico-multiform-mode)
;;   :custom
;;   ;; Disable Vertico for specific commands
;;   (vertico-multiform-commands
;;    '((find-file unobtrusive)
;;      (switch-to-buffer unobtrusive))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))




(provide 'init-vertico)
