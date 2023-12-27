

(define-key global-map (kbd "M-(") "{")
(define-key global-map (kbd "M-)") "}")
(define-key global-map (kbd "M-5") "[")
(define-key global-map (kbd "M-°") "]")
(global-unset-key (kbd "M-l"))
(define-key global-map (kbd "M-l") "|")
(define-key global-map (kbd "M-n") "~")

;; TODO: remap `comint-next-input' in `comint-mode-map'



(provide 'init-mac)
