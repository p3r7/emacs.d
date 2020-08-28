

;; COMMON LISP

(require 'cl-lib)
(use-package noflet)



;; ORG

(require 'org)



;; ASYNC / PARALLEL PROCESSING

;; NB: deferred is now buggy and no more maintained
(use-package deferred)

;; async is better but bases itself on a new emcs process wo/ all the plugins / vars loaded
(use-package async
  ;; :hook
  ;; (dired-mode-hook . dired-async-mode)
  :config
  (async-bytecomp-package-mode 1))
;; (require 'async-file)

;; aio (async-io) works w/ a promise system
(use-package aio)



;; OBJECT MANIPULATION

(use-package dash
  :config
  (dash-enable-font-lock))

(use-package f)                         ; files

(use-package s)                         ; strings
(use-package prf-string
  :quelpa (prf-string :fetcher github :repo "p3r7/prf-string"))

(use-package ts)                        ; time

;; (use-package ht)
;; https://github.com/plexus/a.el

;; https://github.com/kiwanami/emacs-ctable

(use-package ppp)

(use-package pickling
  :load-path "~/.emacs.d/plugins/pickling")



;; HTTP CLIENT

(use-package request
  :config
  (when (string= "c:/windows/system32/curl.exe" (executable-find request-curl))
    ;; NB: doesn't play nice w/ Windows 10 default cUrl
    (setq request-backend 'url-retrieve)))
(use-package request-deferred
  :after (request))



;; BUFFERS

(use-package thingatpt+
  :quelpa (thingatpt+ :fetcher github :repo "emacsmirror/thingatpt-plus"))



;; WINDOWS

(use-package buffer-grid
  :load-path "~/.emacs.d/plugins/buffer-grid")



;; OS

(use-package prf-exec-path
  :load-path "~/.emacs.d/plugins/prf-exec-path/prf-exec-path.el")

(require 'notifications)




(provide 'init-libs)
