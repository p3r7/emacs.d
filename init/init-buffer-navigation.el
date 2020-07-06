

;; SELECTION

;; basically `delete-selection-mode' + goodies
;; REVIEW: read about `pc-selection-mode'
(cua-selection-mode t)



;; CURSOR NAVIGATION (in buffers)

(global-subword-mode t)

(use-package emacs
  :ensure nil
  :delight
  (subword-mode))


(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))


(use-package goto-last-change
  :bind ([(meta p)(u)] . goto-last-change))


;; (require 'init-ace-jump)
(require 'init-avy)


;; better moving around code blocks
(defun prf/smart-forward-list ()
  (interactive)
  (let ((matching (ignore-errors (scan-lists (point) 1 0))))
    (if matching
        (call-interactively #'forward-list)
      (condition-case ex
          (call-interactively #'up-list)
        ('scan-error (goto-char (point-max)))))))

(defun prf/smart-backward-list ()
  (interactive)
  (let ((matching (ignore-errors (scan-lists (point) -1 0))))
    (if matching
        (call-interactively #'backward-list)
      (condition-case ex
          (call-interactively #'backward-up-list)
        ('scan-error (goto-char (point-min)))))))

(global-set-key [remap forward-list] #'prf/smart-forward-list)
(global-set-key [remap backward-list] #'prf/smart-backward-list)




(provide 'init-buffer-navigation)
