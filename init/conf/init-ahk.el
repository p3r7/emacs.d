
;; http://www.emacswiki.org/emacs/AutoHotKey_Mode
;; (setq ahk-syntax-directory "~/.emacs.d/plugins/ahk-mode/Syntax")
;; (add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
;; (autoload 'ahk-mode "ahk-mode")

;; - [] http://ergoemacs.googlecode.com/svn/trunk/packages/xahk-mode.el
;; (when (prf/require-plugin 'xahk-mode "~/.emacs.d/plugins/xahk-mode" 't)
;; (add-to-list 'auto-mode-alist '("\\.ahk$" . xahk-mode)))

;; - [X] https://github.com/ralesi/ahk-mode
(when (prf/require-plugin 'ahk-mode nil 'noerror)
  (add-hook 'ahk-mode-hook
	    (lambda()
	      (local-set-key (kbd "C-c C-e") 'ahk-run-script))))
;; (when (prf/require-plugin 'ahk-mode noerror 'noerror)
;; (add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode)))

  (provide 'init-ahk)
