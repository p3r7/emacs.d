

;; DEFAULT

(setq
 ;;scroll-step 1   ;; smooth scroll => useless ?
 scroll-margin 0                   ;; higher value could be nice
 scroll-conservatively 100000      ;;  ???
 scroll-preserve-screen-position 1 ;; "smooth" scrolling
 auto-window-vscroll nil           ;; for slow terminals
 )

;; http://www.emacswiki.org/emacs/SmoothScrolling



;; FAST-SCROLL

(use-package fast-scroll
  :disabled
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))




(provide 'init-scrolling)
