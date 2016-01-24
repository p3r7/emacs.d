
;; Multiple highlights
;; [[http://www.emacswiki.org/emacs/HighlightTemporarily]]
;; - [X] Hi Lock
;;   TODO: [[https://github.com/sensorflo/sensorflo-emacs/blob/master/misc/hi-lock-ext.el]]

;; TODO: replace w/ hi-lock-face-defaults
(setq hi-faces (list
		;; 'hi-black-b
		;; 'hi-black-hb
		'hi-blue
		;; 'hi-blue-b
		'hi-green
		;; 'hi-green-b
		'hi-pink
		;; 'hi-red-b
		'hi-yellow
		))
(setq hi-face-current hi-faces)
(global-set-key (kbd "C-<f3>") 'highlight-symbol-at-point-or-region)

(defun prf/hi-face-loop ()
    (setq hi-face-current (cdr hi-face-current))
  (when (null hi-face-current)
	(setq hi-face-current hi-faces)))

(defun highlight-symbol-at-point-or-region ()
    "Highlight symbol at point of slected region"
  (interactive)
  (if (use-region-p)
      (progn
	(highlight-phrase (buffer-substring (region-beginning) (region-end)) (car hi-face-current))
	(cua-set-mark))
	(highlight-symbol-at-point))
  (prf/hi-face-loop))

(defun unhighlight-all ()
  "Unhighlight all highlightened sexp in current buffer"
  (interactive)
  (while hi-lock-interactive-patterns
    (hi-lock-unface-buffer (caar hi-lock-interactive-patterns))))
(global-set-key (kbd "s-<f3>") 'unhighlight-all)


;; better comptibility w/ hl-line
(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-fontified nil))
    ad-do-it))


(provide 'init-highlighting)
