
(when (fboundp 'nxml-mode)
  (defun xml/format (begin end)
    "Pretty format XML markup in region.
The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
	(backward-char) (insert "\n") (setq end (1+ end)))
      (indent-region begin end))
    )
)

(add-hook 'nxml-mode-hook
	  (lambda ()
	    (setq
	     tab-width 4
	     indent-tabs-mode nil) ))


(provide 'init-xml)
