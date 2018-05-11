
(when (prf/require-plugin 'ein nil 'noerror)
  (require 'ein-loaddefs)
  (require 'ein-notebook)
  (require 'ein-subpackages))

(provide 'init-jupyter)
