
(use-package php-mode
  ;; :load-path "~/.emacs.d/plugins-spe/nxhtml/related"
  :mode "\\.php?\\'"
  :hook (php-mode #'php-enable-default-coding-style))



;; COMPLETION AT POINT

;; NB: disabled as causes:
;; File mode specification error: (error Lisp nesting exceeds ‘max-lisp-eval-depth’)

(use-package ac-php
  :disabled
  ;; :hook (php-mode . prf/php-mode-completion-generic-hook)
  :after (auto-complete company)

  :init
  (defun prf/php-mode-completion-generic-hook ()
    (ac-php-core-eldoc-setup)
    (define-key php-mode-map (kbd "M-]")
      #'ac-php-find-symbol-at-point)
    (define-key php-mode-map (kbd "M-[")
      #'ac-php-location-stack-back))

  (defun prf/php-mode-ac-hook ()
    (setq ac-sources '(ac-source-php)))

  (defun prf/php-mode-company-hook ()
    (set (make-local-variable 'company-backends)
         '((company-ac-php-backend company-dabbrev-code)
           company-capf company-files)))

  :config
  (add-hook 'php-mode-hook #'prf/php-mode-completion-generic-hook)
  (when (member prf/fav-completion-at-point '(ac auto-complete))
    (add-hook 'php-mode-hook #'prf/php-mode-ac-hook))
  (when (eq prf/fav-completion-at-point 'company)
    (add-hook 'php-mode-hook #'prf/php-mode-company-hook)))




(provide 'init-php)
