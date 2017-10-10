
(when (prf/require-plugin 'anaconda-mode nil 'noerror)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode) )

(when (prf/require-plugin 'ac-anaconda  nil 'noerror)
  (add-hook 'python-mode-hook 'ac-anaconda-setup) )


  (provide 'init-python)
