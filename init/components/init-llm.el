

;; github copilot

(use-package copilot
  :ensure t
  ;; :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)))


;; NB: needs manual actions
;; to install copilot cli:
;;   M-x copilot-install-server
;; and then to auth:
;;   M-x copilot-login




(provide 'init-llm)
