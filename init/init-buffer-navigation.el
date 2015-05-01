
(require 'init-ace-jump)

(when (require 'goto-last-change nil 'noerror)
  (global-set-key [(meta p)(u)] 'goto-last-change))


(provide 'init-buffer-navigation)
