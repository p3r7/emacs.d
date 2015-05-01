;; [haskell]
;; (load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(autoload 'haskell-mode "~/.emacs.d/plugins/haskell-mode/haskell-site-file" "Haskell editing mode." t)
(add-to-list 'auto-mode-alist '("\.hs$" . haskell-mode))
(eval-after-load "haskell-mode"
  '(progn
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
     ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
     ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
     )
  )


(provide 'init-haskell)
