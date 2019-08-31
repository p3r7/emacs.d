

(setq comint-input-ignoredups t         ; no duplicate history
      comint-input-ring-size 5000       ; history size
      comint-buffer-maximum-size 20000
      comint-scroll-to-bottom-on-input t)

(defun prf/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(define-key comint-mode-map (kbd "C-c E") #'prf/comint-clear-buffer)

;;
;; https://oremacs.com/2019/03/24/shell-apt/

(advice-add
 'ansi-color-apply-on-region
 :before 'ora-ansi-color-apply-on-region)

(defun ora-ansi-color-apply-on-region (begin end)
  "Fix progress bars for e.g. apt(8).
Display progress in the mode line instead."
  (let ((end-marker (copy-marker end))
        mb)
    (save-excursion
      (goto-char (copy-marker begin))
      (while (re-search-forward "\0337" end-marker t)
        (setq mb (match-beginning 0))
        (when (re-search-forward "\0338" end-marker t)
          (ora-apt-progress-message
           (substring-no-properties
            (delete-and-extract-region mb (point))
            2 -2)))))))

(defun ora-apt-progress-message (progress)
  (message
   (replace-regexp-in-string
    "%" "%%"
    (ansi-color-apply progress))))



(provide 'init-comint)
