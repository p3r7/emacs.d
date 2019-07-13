
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
(use-package dash)
;; (use-package ht)
(use-package prf-string
  :load-path "~/.emacs.d/plugins/prf-string")
(use-package pickling
  :load-path "~/.emacs.d/plugins/pickling")

;; NB: deferred is now buggy and no more maintained
;; async is better but bases itself on a new emcs process wo/ all the plugins / vars loaded
;; aio (async-io) seems to be better
(use-package deferred)
(use-package async)
(use-package aio)

(use-package request)
(use-package request-deferred
  :after (request))

;; https://github.com/kiwanami/emacs-ctable

;; https://github.com/plexus/a.el

(provide 'init-libs)
