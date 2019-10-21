
;; COMMON LISP
(require 'cl)
(use-package noflet)

;; ORG
(require 'org)

;; ASYNC
;; (require 'async)
;; (require 'async-file)

;; OBJECT MANIPULATION
(use-package f)
(use-package s)
(use-package dash
  :config
  (dash-enable-font-lock))
;; (use-package ht)
(use-package prf-string
  :quelpa (prf-string :fetcher github :repo "p3r7/prf-string"))
(use-package pickling
  :load-path "~/.emacs.d/plugins/pickling")
(use-package buffer-grid
  :load-path "~/.emacs.d/plugins/buffer-grid")

;; NB: deferred is now buggy and no more maintained
;; async is better but bases itself on a new emcs process wo/ all the plugins / vars loaded
;; aio (async-io) seems to be better
(use-package deferred)
(use-package async
  ;; :hook
  ;; (dired-mode-hook . dired-async-mode)
  :config
  (async-bytecomp-package-mode 1))
(use-package aio)

(use-package request
  :config
  (when (string= "c:/windows/system32/curl.exe" (executable-find request-curl))
    ;; NB: doesn't play nice w/ Windows 10 default cUrl
    (setq request-backend 'url-retrieve)))
(use-package request-deferred
  :after (request))

;; https://github.com/kiwanami/emacs-ctable

;; https://github.com/plexus/a.el

(provide 'init-libs)
