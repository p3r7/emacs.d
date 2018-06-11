
(when (prf/require-plugin 'anaconda-mode nil 'noerror)
  (setenv "no_proxy" "127.0.0.1")
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode) )

;; (when (prf/require-plugin 'ac-anaconda  nil 'noerror)
;;   (add-hook 'python-mode-hook 'ac-anaconda-setup) )

(when (prf/require-plugin 'mmm-jinja2  nil 'noerror)
  ;; - conf
  (add-to-list 'auto-mode-alist '("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\.j2\\'" . conf-mode-maybe))
  (add-to-list 'auto-mode-alist '("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\.j2\\'" . conf-mode))
  ;; (mmm-add-mode-ext-class 'conf-mode "\\.j2\\'" 'jinja2)
  (mmm-add-mode-ext-class 'conf-space-mode "\\.j2\\'" 'jinja2)
  (mmm-add-mode-ext-class 'conf-unix-mode "\\.j2\\'" 'jinja2)
  (mmm-add-mode-ext-class 'conf-windows-mode "\\.j2\\'" 'jinja2)
  ;; - toml
  (add-to-list 'auto-mode-alist '("\\.toml\\.j2\\'" . toml-mode))
  (mmm-add-mode-ext-class 'toml-mode "\\.j2\\'" 'jinja2)
  ;; - yaml
  (add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\.j2\\'" . yaml-mode))
  (mmm-add-mode-ext-class 'yaml-mode "\\.j2\\'" 'jinja2)
  ;; - shell scripts
  (add-to-list 'auto-mode-alist '("\\.[ckz]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\.j2\\'" . sh-mode))
  (mmm-add-mode-ext-class 'shell-script-mode "\\.j2\\'" 'jinja2)
  )


(provide 'init-python)
