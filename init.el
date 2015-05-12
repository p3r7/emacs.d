;;                             __         __
;;                            |  |-     -|  |
;;                            | °_°|   |O-X |
;;                            ---------------
;;                           ~|P3R7's .emacs|~
;;
;; INDEX
;; { Init }-------------------------------------------------------------[[#I]]
;;   - emacs-related config that HAS TO be done 1st
;; { Packages }---------------------------------------------------------[[#P]]
;;   - activate packages.el (aka ELPA)
;; { OS Specific Stuff }------------------------------------------------[[#O]]
;;   - windows/linux specific customizations
;; { Test Bench Start }------------------------------------------------[[#?s]]
;;   - in validation (major modes)
;; { Appearance }-------------------------------------------------------[[#A]]
;;   - alters elements/geometry of the frame
;; { Langages }---------------------------------------------------------[[#L]]
;;   - adds langages/filetype support
;; { Shells }-----------------------------------------------------------[[#S]]
;;   - tramp
;;   - dired + more file system interactions
;;   - shells
;; { Behaviour }--------------------------------------------------------[[#B]]
;;   - more general emacs-related config
;; { Edition }----------------------------------------------------------[[#E]]
;;   - improves edition
;;   - improves syntax higlighting
;; { Navigation }-------------------------------------------------------[[#N]]
;;   - window manipulation
;;   - buffer/file switching
;;   - in-file navigation
;; { Test Bench End }--------------------------------------------------[[#?e]]
;;   - in validation
;; { Custom }-----------------------------------------------------------[[#C]]
;;============================================================================

;; TODO: theming AFTER languages, to support override faces

;; TODO: C-d is currently c-electric-delete-forward, replace w/ copy-line

;; upstream version of tramp
;; http://www.gnu.org/software/tramp/#Installation

;; NOTE: case EOF, use check-parens
;; or to highlight parens contents
;; (show-paren-mode t)
;; (setq show-paren-style 'expression)

(require 'cl)
(require 'org)

;; (defvar *emacs-load-start* (current-time))
;; (setq stack-trace-on-error t) ;; DEBUG

;(setq warning-suppress-types '('(mule))


;; { Init }------------------------------------------------------------[[<#I]]


(defun edit-dot-emacs () (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-<f1>") 'edit-dot-emacs)

;; (require 'async)
;; (require 'async-file)

;; alternative to convert-standard-filename
(defun prf/system/get-path-system-format (path)
  (if (windows-nt-p)
      (subst-char-in-string ?/ ?\\ path)
    (path)
  ) )


;; { Packages }--------------------------------------------------------[[<#P]]

;; TODO: choose which of /plugins or elpa/ has biggest priority -> add to load path in correct order

;; locations for features:
;; - init/ : initialization features (manually defined)
;; - plugins/ : plugins manually installed
;; - plugins-spe : plugins manually installed, NOT added to load path
;;   usefull if I want several plugins w/ same name cohexiting
;; - themes/ : manually installed deftheme themes
;; - elpa/ : features installed through packages.el

(setq prf/load-path load-path)

(defun prf/recursive-add-to-load-path (dir)
  "Add directory and all child directories to load path"
  (let (
	(default-directory dir)
	)
    ;; (setq load-path (cons dir load-path))
    (normal-top-level-add-to-load-path (list dir))
    (normal-top-level-add-subdirs-to-load-path)
    )
  )

(prf/recursive-add-to-load-path "~/.emacs.d/plugins/")
;; TODO: custom load-path for those, w/ custom require function to access them
(prf/recursive-add-to-load-path "~/.emacs.d/init/")


(when (require 'package nil 'noerror)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  )

(require 'prf-require)
(prf/require-plugin 'esup nil 'noerror)



;; { OS Specific Stuff }-----------------------------------------------[[<#O]]


(require 'init-env)


;; { Test Bench Start }-----------------------------------------------[[<#?s]]

(require 'init-test1)



;; { Appearance }------------------------------------------------------[[<#A]]

(require 'init-gui)
(require 'init-rice)
;(require 'init-speedbar) ;; craps since emacs 24.4 due to void-function ad-advised-definition-p



;; { Behaviour }-------------------------------------------------------[[<#B]]


(require 'init-main)
(require 'init-scrolling)
(require 'init-helm)
(require 'init-hydra)



;; { Langages }--------------------------------------------------------[[<#L]]


;; - descriptive
(require 'init-org)
(require 'init-markdown)
(require 'init-html)

;; - dev
(require 'init-c-common)
(require 'init-c)
(require 'init-php)
(require 'init-js)
(require 'init-groovy)
(require 'init-scheme)
(require 'init-sql)
(require 'init-haskell)

;; - shell
(require 'init-dos)

;; - conf
(require 'init-ahk)
(require 'init-apache)
;;(require 'cisco-router-mode)

;; - format
(require 'init-json)
(require 'init-xml)
(require 'init-javaprop)
;; (require 'csv-mode)
(require 'init-guitar-tab)

(require 'init-doc)

(require 'init-srv-utils)


;; { Edition }---------------------------------------------------------[[<#E]]


(require 'init-edition)
(require 'init-ac)
(require 'init-ediff)



;; { Navigation }------------------------------------------------------[[<#N]]


(require 'init-windmove)

(require 'init-buffer-navigation)
(require 'init-file-navigation)
;; (require 'init-projectile) ; slows down tramp
;; (require 'init-vcs) ; slows down tramp

(require 'init-bookmarks)
(require 'init-deft)



;; Hydras ------------------------------------------------------------

(require 'init-macro)

(eval-after-load "hydra"
  '(progn

     (defhydra hydra-visual (:color blue)
       "visual"
       ("r" prf/reset-frame-geometry "reset-frame-geometry")
       ("g" nil "cancel"))


     ;; TODO: conditionnally append to hydra main
     (defhydra hydra-main (:color blue)
       "main"
       ("h" hydra-helm/body "helm")
       ("s" hydra-srvUtils/body "server utils")
       ("v" hydra-visual/body "visual")
       ("g" nil "cancel"))

     (global-set-key (kbd "<apps>") 'hydra-main/body)
     (global-set-key (kbd "<menu>") 'hydra-main/body)

     )
  )

;; { Custom }----------------------------------------------------------[[<#C]]


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;;----------------------------------------------------------------------------


;; (message "Benchmark: .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;; 				     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
