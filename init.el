;;                             __         __
;;                            |  |-     -|  |
;;                            | °_°|   |O-X |
;;                            ---------------
;;                           ~|P3R7's .emacs|~
;;


;; OPTIMIZATIONS

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
;; (setq garbage-collection-messages t)

(defvar prf/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; restore after startup
(add-hook 'after-init-hook
          (lambda ()
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



;; FEATURES

;; locations for features:
;; - init/ : initialization features (manually defined)
;; - plugins/ : plugins manually installed
;; - plugins-spe : plugins manually installed, NOT added to load path
;;   usefull if I want several plugins w/ same name cohexiting
;; - themes/ : manually installed deftheme themes
;; - elpa/ : features installed through packages.el



;; FEATURES - LOCAL

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



;; FEATURES - PACKAGE.EL / USE-PACKAGE

;; TODO: choose which of /plugins or /elpa has biggest priority -> add to load path in correct order

(setq prf/package/archive-cache-fp "~/.emacs.d/package-archive-contents.el")

(add-to-list 'load-path "~/.emacs.d/plugins/elpa-mirror")
(require 'elpa-mirror)
(setq elpamr-default-output-directory "~/elpa-mirror")
(setq prf/package-refetch-at-startup nil)

(setq package-check-signature nil

      ;; package-enable-at-startup nil
      ;; package--init-file-ensured t

      )

(when (and (>= libgnutls-version 30603)
	       (version<= emacs-version "26.2"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(when (require 'package nil 'noerror)

  (if prf/package-refetch-at-startup
      ;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
      (setq package-archives '(("gnu" . "https://elpa.mirrorservice.org/")))
    (setq package-archives `(("elpa-mirror" . ,(concat elpamr-default-output-directory "/")))))

  ;; still some issues w/ marmalade's certif https://github.com/nicferrier/elmarmalade/issues/55
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("bagolyodu" . "https://bagolyodu.dyndns.hu/emacs-packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (unless (or prf/package-refetch-at-startup
              (not (file-exists-p prf/package/archive-cache-fp)))
    (message "package init - reading cached package archive")
    (with-temp-buffer
      (insert-file-contents prf/package/archive-cache-fp)
      (cl-assert (eq (point) (point-min)))
      (setq package-archive-contents (read (current-buffer)))))

  (unless package-archive-contents
    (message "package init - missing package archive, featching and caching")
    (package-refresh-contents)
    (with-temp-file prf/package/archive-cache-fp
      (prin1 package-archive-contents (current-buffer))
      ;; (insert (prin1-to-string package-archive-contents))
      ))

  (package-initialize)

  (when prf/package-refetch-at-startup
    (message "package init - creating local elpa cache")
    (elpamr-create-mirror-for-installed))
  )

(setq use-package-always-ensure t
      ;; use-package-always-defer t
      ;; use-package-verbose t
      )

(require 'prf-require)
(if (= emacs-major-version 30)
    (require 'use-package)
  (prf/require-plugin 'use-package))


;; (use-package paradox)
(use-package delight)

(require 'init-auto-compile)



;; FEATURES - QUELPA

(use-package quelpa
  :init
  (setq quelpa-self-upgrade-p nil))
(use-package quelpa-use-package
  :after (quelpa)
  :config
  (quelpa-use-package-activate-advice))



;; FEATURES - STRAIGHT

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;; LIBS

(require 'init-libs)



;; FILES TO OPEN AT STARTUP

(defvar prf/startup-open-files '() "List of files to open when Emacs starts.")



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
(require 'init-ux)
(require 'init-scrolling)
(require 'init-hydra)
(require 'init-completion-at-point)
(require 'init-completion-backend)



;; LANGUAGES & FORMATS

;; LSP
(require 'init-eglot)

;; multilingual files
(require 'init-mmm)
(require 'init-polymode)

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
(require 'init-common-lisp)
(require 'init-c-common)
(require 'init-c)
(require 'init-go)
(require 'init-zig)
(require 'init-php)
(require 'init-js)
(require 'init-lua)
(require 'init-python)
(require 'init-groovy)
(require 'init-scheme)
(require 'init-sql)
(require 'init-haskell)
(require 'init-applescript)
(require 'init-tickscript)
(require 'init-ahk)
(require 'init-devilspie)
(require 'init-r)
(require 'init-supercollider)
(require 'init-gnuplot)
(require 'ld-script)

;; - shell
(require 'init-dos)

;; - conf
(require 'init-apache)
(require 'init-syslog-ng)
(require 'init-fvwm-conf)
(require 'init-systemd-unit)
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

;; - frameworks / platforms
(require 'init-arduino)
(require 'init-circuitpython)
(require 'init-norns)
(require 'init-seamstress)

(require 'init-compilation)



;; SHELLS

(require 'init-srv-utils)

(require 'init-influxdb-client)

(require 'init-vagrant)
(require 'init-kubernetes)
(require 'init-docker)

(use-package drun
  :load-path "~/.emacs.d/plugins/drun")
(use-package helm-drun
  :load-path "~/.emacs.d/plugins/drun"
  :after (helm drun))



;; SOCIAL

(require 'init-mail)
(require 'init-circe)


;; EDITION

(require 'init-edition)
(require 'init-aggressive-indent)
(require 'init-ediff)
(require 'init-origami)
(require 'init-yasnippet)

(require 'init-flycheck)
(require 'init-crypt)


;; FILE / BUFFER / POJECT NAVIGATION

(require 'init-windmove)

(require 'init-buffer-navigation)

(require 'init-file-navigation)
(require 'init-neotree)
(require 'init-treemacs)
;; (require 'init-speedbar) ;; craps since emacs 24.4 due to void-function ad-advised-definition-p

(require 'init-project)

(require 'init-grep)
(require 'init-vcs)

(require 'init-doc)

(require 'init-bookmark+)
(require 'init-register)



;; OPEN FILES

(--each prf/startup-open-files
  (when (file-exists-p it)
    (find-file-noselect it)))


 ;; SHORTCUTS

(require 'init-keyboard-macro)

(require 'init-hydra)

(with-eval-after-load 'hydra
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

  (defhydra hydra-projectSearch (:color blue)
    "project search"
    ("F" find-file-in-project "find file (fast)")
    ("f" projectile-find-file "find file")
    ("n" cider-find-ns "find class / ns")
    ("s" swiper "swiper")
    ("g" nil "cancel"))

  ;; TODO: conditionnally append to hydra main
  (defhydra hydra-main (:color blue)
    "main"
    ("m" magit-status "magit")
    ("c" hydra-copyPath/body "copy path")
    ("s" hydra-srvUtils/body "server utils")
    ("k" hydra-kube/body "kube")
    ("p" hydra-projectSearch/body "project utils")
    ("v" hydra-visual/body "visual")
    ("?" hydra-test/body "experimental")
    ("g" nil "cancel"))

  ;; http://kitchingroup.cheme.cmu.edu/blog/2015/06/24/Conditional-hydra-menus/

  (global-set-key (kbd "<apps>") #'hydra-main/body)
  (global-set-key (kbd "<menu>") #'hydra-main/body)
  (when (eq system-type 'darwin)
    (global-set-key (kbd "<f14>") #'hydra-main/body)))



;; CUSTOM

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)



;; END

(when (and (daemonp)
           (string-equal (terminal-name (frame-terminal)) "initial_terminal"))
  (notifications-notify
   :title "Emacs Daemon"
   :body "Emacs is ready"))




;;|     .-.
;;|    /   \         .-.
;;|   /     \       /   \       .-.     .-.     ..   _
;;+--/-------\-----/-----\-----/---\---/---\---/--\-/-\-/\-'.--
;;| /         \   /       \   /     '-'     '-'    '
;;|/           '-'         '-'
;;|

;; (message "Benchmark: .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;; (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
