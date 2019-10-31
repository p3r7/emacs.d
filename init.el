;;                             __         __
;;                            |  |-     -|  |
;;                            | °_°|   |O-X |
;;                            ---------------
;;                           ~|P3R7's .emacs|~
;;


;; OPTIMIZATIONS

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
;; (setq garbage-collection-messages t)

(defvar prf/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'after-init-hook (lambda ()
			     ;; restore after startup
                             (setq gc-cons-threshold 16777216
				   gc-cons-percentage 0.1
				   file-name-handler-alist prf/file-name-handler-alist)))

;; make helm and lusty more snappy
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; (defvar *emacs-load-start* (current-time))
;; (setq stack-trace-on-error t) ;; DEBUG

;; (setq warning-suppress-types '('(mule))



;; FALLBACK

(defun edit-dot-emacs () (interactive)
       (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-<f1>") #'edit-dot-emacs)



;; PACKAGES

;; TODO: choose which of /plugins or /elpa has biggest priority -> add to load path in correct order

(setq package-check-signature nil)

(when (and (>= libgnutls-version 30603)
	   (version<= emacs-version "26.2"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; locations for features:
;; - init/ : initialization features (manually defined)
;; - plugins/ : plugins manually installed
;; - plugins-spe : plugins manually installed, NOT added to load path
;;   usefull if I want several plugins w/ same name cohexiting
;; - themes/ : manually installed deftheme themes
;; - elpa/ : features installed through packages.el

;; unused
;; was wanting to have load-path dedicated to inits, but let-bounding is not suffiscient in that case
;; we migth need a more complete implementation w/ noflet
(setq prf/load-path load-path)

;; (if (and (= emacs-major-version 24)
	 ;; (= emacs-minor-version 5))
    ;; (add-to-list 'load-path "~/.emacs.d/plugins-src/tramp-2.2.12/lisp")
  ;; )

(defun prf/recursive-add-to-load-path (dir)
  "Add directory and all child directories to load path"
  (let ((default-directory dir))
    ;; (setq load-path (cons dir load-path))
    (normal-top-level-add-to-load-path (list dir))
    (normal-top-level-add-subdirs-to-load-path)))

(prf/recursive-add-to-load-path "~/.emacs.d/plugins/")
;; TODO: custom load-path for those, w/ custom require function to access them
(prf/recursive-add-to-load-path "~/.emacs.d/init/")


(when (require 'package nil 'noerror)
  ;; still some issues w/ marmalade's certif https://github.com/nicferrier/elmarmalade/issues/55
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;; (add-to-list 'package-archives '("bagolyodu" . "https://bagolyodu.dyndns.hu/emacs-packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  )

(require 'prf-require)
(prf/require-plugin 'use-package)
(setq use-package-always-ensure t)
;; (setq use-package-verbose t)



(use-package quelpa)
(use-package quelpa-use-package
  :after (quelpa)
  :config
  (quelpa-use-package-activate-advice))

(use-package paradox)
(use-package delight)

(use-package esup)
(require 'init-auto-compile)



;; LIBS

(require 'init-libs)



;; OS / HW

(require 'init-env)



;; TEST BENCH (START)

(require 'init-test1)

;; (setq same-window-regexps '("."))

(defun prf/calc-eval-replace-selection ()
  (interactive)
  (when (use-region-p)
    (let ((text (calc-eval (buffer-substring (region-beginning) (region-end)))))
      (delete-region (region-beginning) (region-end))
      (insert text))))



;; APPEARANCE

(require 'init-gui)
(require 'init-rice)


;; GENERAL BEHAVIOUR

(require 'init-main)
(require 'init-scrolling)
(require 'init-hydra)
(require 'init-completion-at-point)
(require 'init-completion-system)


;; LANGUAGES & FORMATS

;; multilingual files
(require 'init-mmm)

;; - descriptive
(require 'init-html)
(require 'init-text-common)
(require 'init-org)
(require 'init-markdown)
(require 'init-md-org-shiatsu)

;; - dev
(require 'init-prog-common)
(require 'init-lisp-common)
(require 'init-elisp)
(require 'init-clojure)
(require 'init-c-common)
(require 'init-c)
(require 'init-go)
(require 'init-php)
(require 'init-js)
(require 'init-lua)
(require 'init-groovy)
(require 'init-scheme)
(require 'init-sql)
(require 'init-haskell)
(require 'init-applescript)
;;(require 'init-tickscript)
(require 'init-ahk)
(require 'init-r)
(require 'init-python)

;; - shell
(require 'init-dos)

;; - conf
(require 'init-apache)
(require 'init-syslog-ng)
;;(require 'cisco-router-mode)

;; - formats
(require 'init-json)
(require 'init-xml)
(require 'init-javaprop)
(require 'init-yaml)
(require 'init-ansible)
(require 'init-toml)
(require 'init-dotenv)
;; (require 'csv-mode)
(require 'init-guitar-tab)

(require 'init-compilation)



;; SHELLS

(require 'init-srv-utils)



;; SOCIAL

(require 'init-mail)
(require 'init-circe)


;; EDITION

(require 'init-edition)
(require 'init-aggressive-indent)
(require 'init-ediff)
(require 'init-origami)
(require 'init-yasnippet)

(require 'init-eglot)


;; FILE / BUFFER / POJECT NAVIGATION

(require 'init-windmove)

(require 'init-buffer-navigation)

(require 'init-file-navigation)
(require 'init-neotree)
;; (require 'init-speedbar) ;; craps since emacs 24.4 due to void-function ad-advised-definition-p

;; (require 'init-projectile)

(require 'init-grep)
(require 'init-vcs)

(require 'init-doc)

(require 'init-bookmark+)
(require 'init-register)
(require 'init-deft)


;; SHORTCUTS

(require 'init-keyboard-macro)

(require 'init-hydra)

(with-eval-after-load "hydra"
  ;; https://github.com/abo-abo/hydra/wiki/Conditional-Hydra
  (defhydra hydra-visual (:color blue)
    "visual"
    ("r" prf/reset-frame-geometry "reset-default")
    ("d" prf/double-default-frame-geometry "double")
    ("t" prf/tiny-frame-geometry "tiny")
    ("g" nil "cancel"))

  (defhydra hydra-test (:color blue)
    "test"
    ("h" hydra-helm/body "helm")
    ("g" nil "cancel"))

  ;; TODO: conditionnally append to hydra main
  (defhydra hydra-main (:color blue)
    "main"
    ("m" magit-status "magit")
    ("c" hydra-copyPath/body "copy path")
    ("s" hydra-srvUtils/body "server utils")
    ("v" hydra-visual/body "visual")
    ("?" hydra-test/body "experimental")
    ("g" nil "cancel"))

  ;; http://kitchingroup.cheme.cmu.edu/blog/2015/06/24/Conditional-hydra-menus/

  (global-set-key (kbd "<apps>") 'hydra-main/body)
  (global-set-key (kbd "<menu>") 'hydra-main/body))



;; CUSTOM

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)




;;|     .-.
;;|    /   \         .-.
;;|   /     \       /   \       .-.     .-.     ..   _
;;+--/-------\-----/-----\-----/---\---/---\---/--\-/-\-/\-'.--
;;| /         \   /       \   /     '-'     '-'    '
;;|/           '-'         '-'
;;|

;; (message "Benchmark: .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;; (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
