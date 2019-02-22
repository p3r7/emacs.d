
(use-package helm)

(use-package helm-config
  :ensure nil
  :after (helm hydra)
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-h SPC" . helm-all-mark-rings)
	 ("C-c h o" . helm-occur)
	 ;; ("C-x C-f" . helm-find-files)
	 ;; ("C-x b" . helm-mini)
	 ;; ([remap list-buffers] . helm-buffers-list)
	 ("C-c h C-c w" . helm-wikipedia-suggest)
	 ("C-c h x" . helm-register)
	 ;; ("C-x r j" . jump-to-register)
	 :map helm-map
	 ("C-c h" . helm-execute-persistent-action) ; rebind tab to do persistent action
	 ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
	 ("C-z" . helm-select-action)
	 ("C-r" . helm-previous-line)
	 ("C-s" . helm-next-line) ;; REVIEW: not working ?
	 :map help-command
	 ;; ("C-f" . helm-apropos)
	 ("C-l" . helm-locate-library)
	 ("r" . helm-info-emacs)
	 :map minibuffer-local-map
	 ("M-p" . helm-minibuffer-history))

  :init
  (setq helm-command-prefix (kbd "C-c h"))

  ;; buffer config
  (setq
   helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
   helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
   helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
   helm-candidate-number-limit 500 ; limit the number of displayed canidates
   helm-ff-file-name-history-use-recentf t
   helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   )

  :config
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (defhydra hydra-helm (:color blue)
    "visual"
    ("x" helm-M-x "M-x")
    ("b" helm-mini "buffers")
    ("f" helm-find-files "find-files")
    ("F" helm-find "find")
    ("r" helm-register "registers")
    ("p" helm-projectile "projectile")
    ("A" helm-projectile-ag "projectile-ag")
    ("g" nil "cancel"))

  ;; do not make it ubuiquous, as would conflict w/ lusty, among others
  ;; (helm-mode 1)
  )

(use-package helm-grep
  :ensure nil
  :after (helm)
  :bind (:map helm-grep-mode-map
	 ("<return>" . helm-grep-mode-jump-other-window)
	 ("p" . helm-grep-mode-jump-other-window-backward)
	 ("n" . helm-grep-mode-jump-other-window-forward)))


;; (use-package helm-swoop
;;   :after (helm))



(provide 'init-helm)
