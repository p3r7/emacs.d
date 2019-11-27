
;; ABOUT: Those provite minibuffer completion + advanced features.

(defvar prf/fav-completion-system nil)


;; WRAPPER

(use-package prf-completion-system
  :load-path "~/.emacs.d/plugins/prf-completion-system"
  :after (helm ivy lusty-explorer)

  :init
  (setq prf/M-x-completion-system 'helm
        prf/find-file-completion-system 'lusty
        prf/switch-to-buffer-completion-system 'lusty)
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
