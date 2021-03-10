
(require 'paren)

;; REVIEW: maybe use lower-level `syntax-ppss' ?
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
      "Pretty-print selected json region."
      (interactive "r")
      (save-excursion
        (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))))

(defun apply-cmd-on-delimited-region (apply-cmd)
  "Apply command APPLY-CMD on region.
Region is either the active region or found using delimiters around points."
  (interactive)
  (save-excursion
    (let* ((siblings (prf/parens/get-siblings))
           (dir (car siblings))
           (first-pos (nth 1 siblings))
           (last-pos (nth 2 siblings))
           current matching)
      (when siblings
        (if (eq 1 dir)
            (progn
              (setq current first-pos)
              (setq matching last-pos))
          (setq current last-pos)
          (setq matching first-pos))
        (if (region-active-p)
            (unless (and (eq first-pos (region-beginning))
                         (eq last-pos (region-end)))
              (error "Region not json"))
          (set-mark matching)
          (goto-char current)
          (activate-mark))
        (call-interactively apply-cmd)
        (when (region-active-p)
          (deactivate-mark))))))

(when (fboundp #'json/format-region)
  (defun json/format ()
    "Pretty-print json near point or region"
    (interactive)
    (apply-cmd-on-delimited-region #'json/format-region)))




(provide 'init-json)
