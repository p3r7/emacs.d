
(setq auto-mode-alist (cons '("\\.expect$" . tcl-mode) auto-mode-alist))
(require 'expect)

(provide 'init-tcl)
