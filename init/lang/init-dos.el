;; http://www.emacswiki.org/emacs/DosScripts
(require 'dos)
(require 'dos-indent)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

(provide 'init-dos)
