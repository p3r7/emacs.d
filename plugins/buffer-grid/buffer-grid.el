

(defun buffer-grid-diplay (buf-list &optional delete-other-wins max-columns)
  (unless max-columns
    (setq max-columns 4))

  (let ((i 0)
        wind-A
        wind-width wind-height
        nb-cells nb-cols nb-rows)

    ;; if in minibuffer go somewhere else
    (if (save-match-data
	  (string-match "\\*Minibuf-" (buffer-name (window-buffer))))
	(select-window (next-window nil 'ignore-minibuf)))

    (when delete-other-wins
      (delete-other-windows))

    (setq nb-cells (length buf-list)
          nb-cols (min nb-cells max-columns)
          nb-rows (ceiling (/ (float nb-cells) max-columns)))

    (switch-to-buffer (car buf-list))
    (setq wind-A (selected-window))

    (setq wind-width
          (/ (window-width wind-A) nb-cols))
    (setq wind-height
          (/ (window-height wind-A) nb-rows))

    (let ((trans (lambda (e)
                   (let ((wind (selected-window)))
                     (when (and (> nb-rows 1)
                                (eq 0 (% i max-columns)))
                       (split-window-vertically wind-height))
                     (unless (eq 0 (% (+ i 1) max-columns))
                       (split-window-horizontally wind-width))
                     (if (eq (selected-window) wind)
                         (other-window 1))
                     (switch-to-buffer e))
                   (setq i (+ i 1)))))
      (mapc trans (cdr buf-list)))))




(provide 'buffer-grid)
