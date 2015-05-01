
;; https://github.com/ejmr/php-mode/
(prf/require-plugin 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php?\\'" . php-mode))

;; alternative: https://github.com/echosa/phpplus-mode


;;(eval-after-load 'php-mode
;;    (require 'php-extras))


(provide 'init-php)
