

(if (fboundp #'json-pretty-print)
    (defalias 'json/format-region #'json-pretty-print)
  (when (executable-find "python")
    (defun json/format-region (begin end)
      "Pretty-print selected region."
      (interactive "r")
      (save-excursion
        (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))))


(when (fboundp #'json/format-region)
  (defun json/format ()
    (interactive)
    (if (not (region-active-p))
        (let* (
               (matching-prev (scan-lists (point) -1 0))
               (matching-next (scan-lists (point) 1 0))
               (matching (or matching-prev matching-next)))
          (when matching
            (goto-char matching)
            (activate-mark)
            (call-interactively #'json/format-region))
          )
      (call-interactively #'json/format-region))))




(provide 'init-json)
