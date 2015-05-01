
(when (executable-find "python")
  (defun json/format ()
    (interactive)
    (save-excursion
      (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
      )
    )
  )

(provide 'init-json)
