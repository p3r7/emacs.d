


;; shortcuts / special chars

(define-key global-map (kbd "M-(") "{")
(define-key global-map (kbd "M-)") "}")
(define-key global-map (kbd "M-5") "[")
(define-key global-map (kbd "M-°") "]")
(global-unset-key (kbd "M-l"))
(define-key global-map (kbd "M-l") "|")
(define-key global-map (kbd "M-n") "~")
(define-key global-map (kbd "M-S-!") "\\") ; NB: doesn't seem to work...

;; TODO: remap `comint-next-input' in `comint-mode-map'



;; TRAMP

(with-eval-after-load 'tramp
  (setq tramp-remote-process-environment
        (cons "LC_CTYPE=en_US.UTF-8"
              (delete "LC_CTYPE=''" tramp-remote-process-environment))))




(provide 'init-mac)
