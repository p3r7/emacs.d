
;; http://stackoverflow.com/questions/4643206/how-to-configure-indentation-in-emacs-lua-mode

(when (prf/require-plugin 'lua-mode nil 'noerror)

  (defun p8-hook()
    (when (and (buffer-file-name)
	       (equal (file-name-extension (buffer-file-name)) "p8"))
      (setq lua-indent-level 1)))
  (add-hook 'lua-mode-hook 'p8-hook)
  (add-to-list 'auto-mode-alist '("\\.p8$" . lua-mode)))

(provide 'init-lua)
