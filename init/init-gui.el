
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

(customize-set-variable 'menu-bar-mode nil)
(global-set-key (kbd "C-<f2>") 'menu-bar-mode)

(customize-set-variable 'tool-bar-mode nil)
(global-set-key (kbd "M-<f2>") 'tool-bar-mode)

(defvar prf/frame/width-default 80)
(defvar prf/frame/height-default 40)

(defun prf/reset-frame-geometry ()
  (interactive)
  (set-frame-size (selected-frame) prf/frame/width-default prf/frame/height-default))
(defalias '_rfg 'prf/reset-frame-geometry)

(defun prf/double-default-frame-geometry ()
  (interactive)
  (set-frame-size (selected-frame) (* 2 prf/frame/width-default) prf/frame/height-default))

(defun prf/tiny-frame-geometry ()
  (interactive)
  (set-frame-size (selected-frame) prf/frame/width-default (/ prf/frame/height-default 2)))

;; first frame
(prf/reset-frame-geometry)
;; subsequent frames
(setq initial-frame-alist
      `((width . ,prf/frame/width-default)
        (height . ,prf/frame/height-default)
        (vertical-scroll-bars . nil))
      default-frame-alist
      `((width . ,prf/frame/width-default)
        (height . ,prf/frame/height-default)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bar . nil)))


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

(if (fboundp 'display-line-numbers-mode)
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

(defun prf/face-at-point ()
  (let ((face (get-text-property (point) 'face)))
    (or (and (face-list-p face)
             (car face))
        (and (symbolp face)
             face))))

(defun prf/describe-face (&rest ignore)
  (interactive (list (read-face-name "Describe face"
                                     (or (prf/face-at-point) 'default)
                                     t)))
  ;; This only needs to change the `interactive` spec, so:
  nil)

(eval-after-load "hl-line"
  '(advice-add 'describe-face :before #'prf/describe-face))

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
  ;; :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;; -------------------------------------------------------------------------
;; FORM FEED

;; - [X] https://github.com/wasamasa/form-feed
;; - [ ] https://github.com/purcell/page-break-lines
(use-package form-feed
  :delight
  :config
  (add-hook 'prog-mode-hook 'form-feed-mode)
  (add-hook 'compilation-mode-hook 'form-feed-mode)
  (add-hook 'help-mode-hook 'form-feed-mode))


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

(use-package prf-highlight-keyword
  :load-path "~/.emacs.d/plugins/prf-highlight-keyword")


;; -------------------------------------------------------------------------
;; SPLASH IMAGE

;; (setq fancy-splash-image (expand-file-name "~/.emacs.d/resources/s3.xpm"))


;; -------------------------------------------------------------------------
;; EMOJIS

(use-package emojify
  :init
  (setq emojify-emoji-styles '(unicode github)))


(provide 'init-gui)
