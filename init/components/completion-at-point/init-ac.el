
(use-package auto-complete
  :demand)

(use-package auto-complete-config
  :ensure nil
  :after (auto-complete)

  :bind (
	     :map ac-mode-map
         ("M-TAB" . auto-complete)
         ;; ("<M-S-iso-lefttab>" . auto-complete)
	     :map ac-completing-map
	     ("TAB" . ac-complete)
	     ("RET" . nil))

  :config
  (when (member prf/fav-completion-at-point '(ac auto-complete))
    (ac-config-default))

  ;; see https://github.com/auto-complete/auto-complete/issues/533
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-02/msg02204.html
  (when (= emacs-major-version 30)
    (add-hook 'auto-complete-mode-hook
              (lambda ()
                (setq ac-sources (remove 'ac-source-abbrev ac-sources))))))




(provide 'init-ac)
