
;; -------------------------------------------------------------------------
;; HELP / DESCRIBE

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h o" . helpful-at-point)
	 ("C-h F" . helpful-function)))

;; -------------------------------------------------------------------------
;; INFO

;; TODO: http://ergoemacs.org/emacs/emacs_adding_browser_keys.html
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")


;; -------------------------------------------------------------------------
;; MAN

(defalias 'man 'woman)
(setq woman-use-own-frame nil)


(provide 'init-doc)
