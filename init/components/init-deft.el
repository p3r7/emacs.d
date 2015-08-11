(when (prf/require-plugin 'deft nil 'noerror)
  (setq
   deft-use-filename-as-title t
   deft-auto-save-interval 0 ;; disable autosave
   deft-directory default-directory
   deft-recursive t
   )

   (global-set-key [f9] 'deft)
   )

(provide 'init-deft)
