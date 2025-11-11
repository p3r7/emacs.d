
;; since we use `cua-selection-mode', pasting w/ `cua-yank' results in an auto-indent
;; we force the `C-u' prefix for python mode to disable this behavior
(with-eval-after-load 'cua-base
  (defun prf/cua-paste-no-indent ()
    (interactive)
    (let ((current-prefix-arg '(4)))    ; C-u
      (call-interactively #'cua-paste)))

  (dolist (hook '(python-mode-hook python-ts-mode-hook))
    (add-hook hook
              (lambda () (local-set-key (kbd "C-y") #'prf/cua-paste-no-indent)))))
(use-package anaconda-mode
  :hook ((python-mode-hook . prf/py/anaconda-activate-hook))
  :config
  (defun prf/py/anaconda-activate-hook ()
    (unless (file-remote-p default-directory)
      (anaconda-mode)
      (anaconda-eldoc-mode)))
  (setenv "no_proxy" "127.0.0.1"))

(use-package pyvenv
  :config
  (let ((w32-conda-envs-dir "C:/ProgramData/Anaconda2/envs"))
    (when (and (string-equal system-type "windows-nt")
               (file-directory-p w32-conda-envs-dir))
      (setenv "WORKON_HOME" w32-conda-envs-dir))))

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

;; (use-package ac-anaconda
;;   :hook (python-mode-hook . ac-anaconda-setup))

(use-package mmm-jinja2
  :config
  ;; - conf
  (add-to-list 'auto-mode-alist '("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\.j2\\'" . conf-mode-maybe))
  (add-to-list 'auto-mode-alist '("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\.j2\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.desktop\\.j2\\'" . conf-desktop-mode))
  ;; (mmm-add-mode-ext-class 'conf-mode "\\.j2\\'" 'jinja2)
  (mmm-add-mode-ext-class 'conf-space-mode "\\.j2\\'" 'jinja2)
  (mmm-add-mode-ext-class 'conf-unix-mode "\\.j2\\'" 'jinja2)
  (mmm-add-mode-ext-class 'conf-windows-mode "\\.j2\\'" 'jinja2)
  (mmm-add-mode-ext-class 'conf-desktop-mode "\\.j2\\'" 'jinja2)

  ;; - toml
  (add-to-list 'auto-mode-alist '("\\.toml\\.j2\\'" . toml-mode))
  (mmm-add-mode-ext-class 'toml-mode "\\.j2\\'" 'jinja2)

  ;; - yaml
  (add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\.j2\\'" . yaml-mode))
  (mmm-add-mode-ext-class 'yaml-mode "\\.j2\\'" 'jinja2)

  ;; - .env
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\.j2\\'" . dotenv-mode))
  (mmm-add-mode-ext-class 'dotenv-mode "\\.j2\\'" 'jinja2)

  ;; - .edn
  (add-to-list 'auto-mode-alist '("\\.edn\\.j2\\'" . clojure-mode))
  (mmm-add-mode-ext-class 'clojure-mode "\\.j2\\'" 'jinja2)

  ;; - shell scripts
  (add-to-list 'auto-mode-alist '("\\.[ckz]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\.j2\\'" . sh-mode))
  (mmm-add-mode-ext-class 'shell-script-mode "\\.j2\\'" 'jinja2))


(provide 'init-python)
