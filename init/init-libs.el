
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

;; https://github.com/kiwanami/emacs-ctable

;; https://github.com/plexus/a.el

(provide 'init-libs)
