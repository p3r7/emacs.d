(when (prf/require-plugin 'helm nil 'noerror)

  ;; ----------------------------------------------------------------------
  ;; REQUIRES

  (require 'helm-config)
  (require 'helm-grep)


  ;; ----------------------------------------------------------------------
  ;; KEYS

  (global-set-key (kbd "C-c h") 'helm-command-prefix)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (define-key helm-map (kbd "C-s") 'helm-next-line) ;; TODO: remap doesn't work
  (define-key helm-map (kbd "C-r") 'helm-previous-line)

  (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
  (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
  (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  ;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; (global-set-key (kbd "C-x b") 'helm-mini)
  ;; (define-key global-map [remap list-buffers] 'helm-buffers-list)

  (global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)

  (global-set-key (kbd "C-c h x") 'helm-register)
  ;; (global-set-key (kbd "C-x r j") 'jump-to-register)
  ;; (define-key 'help-command (kbd "C-f") 'helm-apropos)
  (define-key 'help-command (kbd "r") 'helm-info-emacs)
  ;; find-library
  (define-key 'help-command (kbd "C-l") 'helm-locate-library)

  (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)




  ;; ----------------------------------------------------------------------
  ;; HYDRA

  (eval-after-load "hydra"
    '(progn

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

       )
    )


  ;; ----------------------------------------------------------------------
  ;; HELM BUFFER

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

  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))


  ;; ----------------------------------------------------------------------
  ;; HELM SWOOP

  ;(prf/require-plugin 'helm-swoop)


  ;; ----------------------------------------------------------------------
  ;; HELM GTAGS

  ;; (when (prf/require-plugin 'helm-gtags nil 'noerror)
    ;; (require 'init-helm-gtags))


  ;; ----------------------------------------------------------------------
  ;; START

  ;; do not make it ubuiquous, as would conflict w/ lusty, among others
  ;; (helm-mode 1)


  )


(provide 'init-helm)
