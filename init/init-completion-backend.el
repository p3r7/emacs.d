
;; ABOUT: Those provite minibuffer completion + advanced features.

(defvar prf/fav-completion-system nil)


;; WRAPPER

(use-package prf-completion-backend
  :load-path "~/.emacs.d/plugins/prf-completion-backend"
  :after (helm ivy lusty-explorer)

  :init
  (setq prf/M-x-completion-backend 'helm
        prf/find-file-completion-backend 'lusty
        prf/switch-to-buffer-completion-backend 'lusty)
  :bind (("C-x C-f" . prf/find-file-fun)
	 ("C-x f" . prf/find-file-fun)
	 ("C-x b" . prf/switch-to-buffer-fun)
	 ("C-x C-b" . prf/switch-to-buffer-fun)
	 ("M-x" . prf/M-x-fun)))



;; HELM

(require 'init-helm)



;; IVY

(require 'init-ivy)



;; LUSTY

(require 'init-lusty)




(provide 'init-completion-backend)
