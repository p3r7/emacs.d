
(require 'paren)

(defun prf/parens/get-siblings ()
  "Returns a 3 element list: (DIRECTION FIRST-POS LAST-POS), where direction is relative to current pos (i.e. (point))"
  (let ((parens-couple-pos (show-paren--default)))
    (when parens-couple-pos
      (pcase-let ((`(,here-beg ,here-end ,there-beg ,there-end) parens-couple-pos))
        (if (< there-beg here-beg)
            (list -1 there-beg here-end)
          (list 1 here-beg there-end))))))




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
        (let* ((siblings (prf/parens/get-siblings))
               current matching)
          (when siblings
            (if (eq 1 (car siblings))
                (progn
                  (setq current (nth 1 siblings))
                  (setq matching (nth 2 siblings)))
              (setq current (nth 2 siblings))
              (setq matching (nth 1 siblings)))
            (set-mark current)
            (goto-char matching)
            (activate-mark)
            (call-interactively #'json/format-region)))
      (call-interactively #'json/format-region))))




(provide 'init-json)
