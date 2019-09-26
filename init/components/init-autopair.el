

;; http://emacs-fu.blogspot.fr/2010/06/automatic-pairing-of-brackets-and.html

(use-package autopair
  ;; :disabled
  :delight
  :hook
  ;; NB: for those we use package wrap-region instead
  ((org-mode latex-mode) . (lambda ()
                             (message "disabled autopair-autowrap")
                             (setq autopair-autowrap nil)))
  :config
  (autopair-global-mode 1)
  (when (and (<= emacs-major-version 24)
             (<= emacs-minor-version 3))
    (setq autopair-autowrap t)
    (put 'autopair-backspace 'cua-selection 'supersede)))

(provide 'init-autopair)
