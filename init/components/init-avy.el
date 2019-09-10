
(use-package avy
  :after (org)
  :bind (("C-c SPC" . avy-goto-word-1)
	 ("C-c C-SPC" . avy-goto-word-1)
	 :map org-mode-map
	 ("C-c SPC" . avy-goto-word-1)
	 ("C-c C-SPC" . avy-goto-word-1))
  :init
  (setq avy-background t)
  :config
  ;; set defautl face to value of ace-jump-face-foreground
  (custom-set-faces
   '(avy-lead-face
     ((((class color)) (:foreground "red" :background nil :underline nil))
      (((background dark)) (:foreground "gray100" :background nil :underline nil))
      (((background light)) (:foreground "gray0" :background nil :underline nil))
      (t (:foreground "gray100" :background nil :underline nil))))))


(provide 'init-avy)
