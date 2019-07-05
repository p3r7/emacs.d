(use-package syslogngconf-mode
  :delight "conf[syslog-ng] "
  :magic (("@version: " . syslogngconf-mode))
  :mode (("/scl/.*\\.conf\\'" . syslogngconf-mode)))

(provide 'init-syslog-ng)
