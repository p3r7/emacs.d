;; http://www.emacswiki.org/emacs/DosScripts

(use-package dos
  :load-path "~/.emacs.d/plugins/dos-mode"
  :mode ("\\.bat$" . dos-mode))
(use-package dos-indent
  :load-path "~/.emacs.d/plugins/dos-mode"
  :after (dos))

(provide 'init-dos)
