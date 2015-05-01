
;; -------------------------------------------------------------------------
;; INFO

;; TODO: http://ergoemacs.org/emacs/emacs_adding_browser_keys.html
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")


;; -------------------------------------------------------------------------
;; MAN

(defalias 'man 'woman)
(setq woman-use-own-frame nil)


(provide 'init-doc)
