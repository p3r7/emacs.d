
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  :config
  (when (eq prf/fav-completion-system 'ivy)
    (ivy-mode 1)
    (global-set-key (kbd "C-c C-r") #'ivy-resume)))

(use-package smex
  :config
  (smex-initialize))

(use-package counsel
  :after (ivy smex)
  ;; :config
  ;; (when (member prf/fav-completion-system '(ivy counsel))
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;;   (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;;   (global-set-key (kbd "C-c g") 'counsel-git)
  ;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;;   (global-set-key (kbd "C-c a") 'counsel-ag)
  ;;   (global-set-key (kbd "C-x l") 'counsel-locate)
  ;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))
  )



;; INCREMENTAL SEARCH

(use-package swiper
  :after ivy
  :config
  (when (member prf/fav-incremental-search-system '(ivy swiper))
    (global-set-key (kbd "C-s") #'swiper)))




(provide 'init-ivy)
