;; [AutoHotKey]
;; http://www.emacswiki.org/emacs/AutoHotKey_Mode
;; http://ergoemacs.googlecode.com/svn/trunk/packages/xahk-mode.el
;; (setq ahk-syntax-directory "~/.emacs.d/plugins/ahk-mode/Syntax")
;; (add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
;; (autoload 'ahk-mode "ahk-mode")
(when (prf/require-plugin 'xahk-mode "~/.emacs.d/plugins/xahk-mode" 't)
  (add-to-list 'auto-mode-alist '("\\.ahk$" . xahk-mode))
  )
(provide 'init-ahk)
