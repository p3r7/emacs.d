
(defvar prf/highlight-implem 'hi-lock)

(cond
 ((eq prf/highlight-implem 'highlight-thing)
  (require 'init-highlight-thing))
 (t
  (require 'init-hi-lock)))




(provide 'init-highlighting)
