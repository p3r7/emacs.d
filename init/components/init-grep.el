

;; RIPGREP

(use-package helm-rg
  :after (helm)
  :if (executable-find "rg")
  :defer t)

(use-package deadgrep
  :if (executable-find "rg")
  :bind (("<C-find>" . deadgrep)
         :map deadgrep-mode-map
         ("C-c e" . prf/toggle-deadgrep-edit)
         :map deadgrep-edit-mode-map
         ("C-c e" . prf/toggle-deadgrep-edit))
  :config
  (defun prf/toggle-deadgrep-edit ()
    (interactive)
    (if (eq major-mode 'deadgrep-mode)
        (deadgrep-edit-mode)
      (deadgrep-mode))))




(provide 'init-grep)
