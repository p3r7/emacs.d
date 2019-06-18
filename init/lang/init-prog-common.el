
(defun prf/hook/prog-mode/main ()

  (when (fboundp 'prettify-symbols-mode)
    (unless (derived-mode-p 'python-mode)
      (prettify-symbols-mode 1)))

  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 1)
    (linum-mode 1)))

(add-hook 'prog-mode-hook #'prf/hook/prog-mode/main)

(provide 'init-prog-common)
