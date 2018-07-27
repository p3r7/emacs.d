;; We use an old ass version of php-mode bundled inside nxhtml-mode.
;; The reason is that modern php-mode versions have crappy as hell indentation.
;; Even when forcing it back w/ prf/indentation-php-mode-hook, it craps with
;; functions outside of classes.

(defun prf/indentation-php-mode-hook ()
  ;; php-mode identation is rather esotheric
  ;; it misinterprets c-common rules
  ;; it interprets indent-tabs-mode as indenting w/ spaces...
  (setq tab-width 4
	indent-tabs-mode t)
  (c-set-style "symfony2")
  )


(when (prf/require-plugin-from-file 'php-mode "~/.emacs.d/plugins-spe/nxhtml/related" 'noerror)
  ;; (when (prf/require-plugin 'php-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . php-mode))


  ;; (add-hook 'php-mode-hook 'prf/indentation-php-mode-hook)
  )

;; alternative: https://github.com/echosa/phpplus-mode


;;(eval-after-load 'php-mode
;;    (require 'php-extras))


(provide 'init-php)
