
;; -------------------------------------------------------------------------
;; GENERAL

;; (setq x-stretch-cursor t)
(blink-cursor-mode -1)

(setq-default fill-column 75)
(global-font-lock-mode t)

(setq
 frame-title-format "%b"   ;; current buffer name in title bar
 inhibit-startup-message   t
 inhibit-startup-echo-area-message t
 )
(setq-default initial-scratch-message ";;                               Hello Master\n\n")


;; -------------------------------------------------------------------------
;; FRAME

(menu-bar-mode -1)
(global-set-key (kbd "C-<f2>") 'menu-bar-mode)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (global-set-key (kbd "M-<f2>") 'tool-bar-mode)

  (set-scroll-bar-mode 'nil)
  ;; (scroll-bar-mode -1)

  (defun prf/reset-frame-geometry ()
    (interactive)
    (set-frame-size (selected-frame) 80 40))
  (defun prf/double-default-frame-geometry ()
    (interactive)
    (set-frame-size (selected-frame) 160 40))
  (defalias '_rfg 'prf/reset-frame-geometry)
  (prf/reset-frame-geometry))


;; -------------------------------------------------------------------------
;; MODELINE

(column-number-mode t)
;; (setq display-time-24hr-format t)
;; (display-time-mode 1)
;; TODO: change format w/ battery-mode-line-format
;; (setq battery-mode-line-format "[%b%p%%]")
;; (display-battery-mode 1)


;; -------------------------------------------------------------------------
;; MINIBUFFER

(setq
 ;;enable-recursive-minibuffers nil
 ;; don't allow mb cmds in the mb
 resize-mini-windows  t             ;; mb resize itself atomatically
 max-mini-window-height .25 ;; max 2 lines
 )

;; remove outdated limitation on asynchronous redrawing on screen
(setq redisplay-dont-pause t)


;; -------------------------------------------------------------------------
;; LINUM

(if (boundp 'display-line-numbers-mode)
    (progn
      (face-spec-set
       'line-number
       '((t :inherit fringe))
       'face-defface-spec)
      (face-spec-set
       'line-number-current-line
       '((t :inherit 'line-number :foreground "Firebrick"))
       'face-defface-spec)
      (global-set-key (kbd "C-<f5>") 'display-line-numbers-mode))
  (autoload 'linum-mode "linum" "toggle line numbers on/off" t)
  (global-set-key (kbd "C-<f5>") 'linum-mode))


;; -------------------------------------------------------------------------
;; HL-LINE

;; (defface hl-line-dark '((t (:background "#333300"))) ;; current line highlight
;;    "Face to use for `hl-line-face'." :group 'hl-line)
;; (defface hl-line-light '((t (:background "#e0e0ff"))) ;; current line highlight
;;    "Face to use for `hl-line-face'." :group 'hl-line)
;; (setq hl-line-face 'hl-line-dark)
;; (setq hl-line-face 'hl-line)

;; (add-hook 'text-mode-hook (lambda () (hl-line-mode 1))) ; only one mode

(global-hl-line-mode t)              ;; on for all modes by default
(make-variable-buffer-local 'global-hl-line-mode)
;; TODO: make it frame local as well to disable case in term
;; do (add-hook 'some-mode-hook (lambda () (setq global-hl-line-mode nil)))


;; -------------------------------------------------------------------------
;; PARENS

(setq show-paren-delay 0)
(show-paren-mode t)
;; (setq show-paren-style 'expression)
(setq show-paren-style 'parenthesis)
(when (facep 'show-paren-match-face)
  (set-face-background 'show-paren-match-face "#aaaaaa")
  (set-face-attribute 'show-paren-match-face nil
		      :weight 'bold :underline nil :overline nil :slant 'normal))

(use-package rainbow-delimiters
  ;; :config
  ;; (global-rainbow-delimiters-mode)
  :hook (prog-mode-hook . rainbow-delimiters-mode))


;; -------------------------------------------------------------------------
;; FORM FEED

;; - [X] https://github.com/wasamasa/form-feed
;; - [ ] https://github.com/purcell/page-break-lines
(when (prf/require-plugin 'form-feed nil 'noerror)
  (add-hook 'prog-mode-hook 'form-feed-mode))

;; -------------------------------------------------------------------------
;; INVISIBLE CHARACTERS

(require 'whitespace)
(global-set-key (kbd "C-<f7>") 'whitespace-mode)
;; NOTE: do not use w/ web-mode, might break stuff


;; -------------------------------------------------------------------------
;; CUSTOM FACES

;; "Set up highlighting of special words for selected modes."
;; [[http://www.metasyntax.net/unix/dot-emacs.html]]
;; for colors, see [[http://raebear.net/comp/emacscolors.html]]
;; TODO: look for this [[http://www.emacswiki.org/emacs/download/fic-mode.el]]
(make-face 'font-lock-warning-face-alt)
(set-face-attribute 'font-lock-warning-face-alt nil :foreground "White" :background "Firebrick")
(let ((pattern "\\<\\(FIXME\\|TODO\\|TEST\\|NOTE\\|NOTES\\|NB\\|WARN\\|WARNING\\|BUG\\|HACK\\|ASK\\|EYESORE\\|BAD\\|FIXME\\|REVIEW\\):"))
  (mapc
   (lambda (mode)
     ;;     (font-lock-add-keywords mode `((,pattern 1 'font-lock-warning-face-alt prepend))))
     (font-lock-add-keywords mode `((,pattern 1 'font-lock-warning-face prepend))))
   '(ada-mode c-mode emacs-lisp-mode java-mode haskell-mode web-mode
              literate-haskell-mode html-mode lisp-mode php-mode python-mode ruby-mode tcl-mode
              scheme-mode sgml-mode sh-mode sml-mode markdown-mode org-mode)))

(make-face 'font-lock-ok-face)
(set-face-attribute 'font-lock-ok-face nil :bold t :weight 'bold :inherit font-lock-type-face)
(make-face 'font-lock-ok-face-alt)
(set-face-attribute 'font-lock-ok-face-alt nil :foreground "White" :background "LightGreen")
(let ((pattern "\\<\\(DONE\\|OK\\|GOOD\\|SOLVED\\|FIXED\\|IDEA\\):"))
  (mapc
   (lambda (mode)
     (font-lock-add-keywords mode `((,pattern 1 'font-lock-ok-face prepend))))
   '(ada-mode c-mode emacs-lisp-mode java-mode haskell-mode web-mode
              literate-haskell-mode html-mode lisp-mode php-mode python-mode ruby-mode tcl-mode
              scheme-mode sgml-mode sh-mode sml-mode markdown-mode org-mode)))



;; -------------------------------------------------------------------------
;; SPLASH IMAGE

;; (setq fancy-splash-image (expand-file-name "~/.emacs.d/resources/s3.xpm"))


;; -------------------------------------------------------------------------
;; EMOJIS

;; (when (prf/require-plugin 'emojify nil 't)
;; (setq emojify-emoji-styles '(unicode github))
;; (add-hook 'after-init-hook #'global-emojify-mode))


(provide 'init-gui)
