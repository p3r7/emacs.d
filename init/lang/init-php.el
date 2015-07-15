
;; indentation is crappy as hell
;; https://github.com/ejmr/php-mode/
;;(prf/require-plugin 'php-mode)

;(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins-spe/nxhtml/related/php-mode.el"))
(when (require 'php-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . php-mode))
 )

;; alternative: https://github.com/echosa/phpplus-mode


;;(eval-after-load 'php-mode
;;    (require 'php-extras))


(provide 'init-php)
