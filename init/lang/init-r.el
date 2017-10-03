
(when (prf/require-plugin 'ess nil 'noerror)
  (setq ess-eval-visibly-p nil
	ess-ask-for-ess-directory nil) )


(provide 'init-r)
