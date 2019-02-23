
(if (fboundp 'json-pretty-print)
    (defalias 'json/format 'json-pretty-print)
  (if (executable-find "python")
      (defun json/format ()
	(interactive)
	(save-excursion
	  (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))))


(provide 'init-json)
