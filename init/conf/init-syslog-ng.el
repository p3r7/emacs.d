(use-package syslogngconf-mode
  :magic (("@version: " . syslogngconf-mode))
  :mode (("/scl/.*\\.conf\\'" . syslogngconf-mode)))

(provide 'init-syslog-ng)
