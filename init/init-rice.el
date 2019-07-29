
;; -------------------------------------------------------------------------
;; FONT

(set-face-bold-p 'bold nil)

;; alt way to test: (when (find-font (font-spec :name "DejaVu Sans Mono") ...)

(when (and (boundp 'prf/rice/font-family)
	   (member prf/rice/font-family (font-family-list)))
  (set-face-attribute 'default nil :family prf/rice/font-family))

(when (and (boundp 'prf/rice/variable-pitch-font-family)
	   (member prf/rice/variable-pitch-font-family (font-family-list)))
  (set-face-attribute 'variable-pitch nil :family prf/rice/variable-pitch-font-family))

(when (boundp 'prf/rice/font)
  ;; (setq default-frame-alist `((font . ,prf/rice/font)))
  (set-face-attribute 'default nil :font prf/rice/font))

(when (boundp 'prf/rice/font-height)
  (set-face-attribute 'default nil :height prf/rice/font-height))
(when (boundp 'prf/rice/variable-pitch-font-height)
  (set-face-attribute 'variable-pitch nil :height prf/rice/variable-pitch-font-height))


;; -------------------------------------------------------------------------
;; THEMES

(use-package gotham-theme
  :defer t)
(use-package ample-theme
  :defer t)
(use-package plan9-theme
  ;; :defer t
  )
(use-package dracula-theme
  :defer t)
(use-package flatui-theme
  :defer t)
(use-package chocolate-theme
  :defer t)

(defvar prf/theme/list/practical
  (list
   'plan9
   'flatui
   'dracula-mod
   'comidia-mod
   'chocolate
   ;; 'gotham
   ;; 'light-blue
   ))

(defvar prf/theme/list/dark
  (list
   'comidia-mod
   'gotham
   'ample-flat
   ))

(defvar prf/theme/list/retro-dark
  (list
   'deeper-blue
   'misterioso
   'late-night
   ))

(defvar prf/theme/list/retro-light
  (list
   ;; 'flatui
   'light-blue
   ;; 'pink-bliss
   'plan9
   ;; 'soft-morning
   ;; 'soft-stone
   'tango
   ))

(use-package prf-theme
  :load-path "~/.emacs.d/plugins/prf-theme"
  :bind ([f12] . prf/theme/cycle-theme)
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (setq prf/theme/theme-list prf/theme/list/practical)
  ;; (setq prf/theme/theme-list prf/theme/list/retro-light)
  :config
  (prf/theme/initialize))


(use-package space-theming
  :load-path "~/.emacs.d/plugins/space-theming"
  :after (prf-theme)
  :init
  (setq theming-modifications
	'((chocolate
	   (region :background "#C77497" :foreground "black") ; VS primary-selection ?
	   (mode-line :background "#594A3B") ; chocolate-dark-yellow
	   (mode-line-inactive :background "#2b241d") ; darker derivative of chocolate-dark-yellow
	   (fringe :background "#2b241d") ; same as mode-line-inactive
	   (bmkp-local-directory :foreground "#45AFBD")
	   (bmkp-remote-file :foreground "#C55D67"))
	  ))
  ;; to reload: (spacemacs/update-theme)
  :config
  (theming/init-theming))


;; -------------------------------------------------------------------------
;; TRANSPARENCY

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (if alpha-or-nil alpha-or-nil 100))
	 (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

;; increase opacity (== decrease transparency) => (djcb-opacity-modify)
(global-set-key (kbd "M-°") '(lambda()(interactive)(djcb-opacity-modify)))
;; decrease opacity (== increase transparency => (djcb-opacity-modify t)
(global-set-key (kbd "M-+") '(lambda()(interactive)(djcb-opacity-modify t)))
;; return the state to normal
(global-set-key (kbd "M-=") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))



;; -------------------------------------------------------------------------
;; ZOOM

(if (>= emacs-major-version 24)
    (progn
      (define-key global-map (kbd "C-+") 'text-scale-increase)
      ;; (define-key global-map (kbd "C--") 'text-scale-decrease)
      (define-key global-map (kbd "C-°") 'text-scale-decrease)
      (define-key global-map (kbd "C-=")
	'(lambda()(interactive)(text-scale-set 0)))
      )
  (progn
    (defun zoom-emacs-pre24 (n)
      "with positive N, increase the font size, otherwise decrease it"
      (set-face-attribute 'default (selected-frame) :height
			  (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))
    (global-set-key (kbd "C-+")      '(lambda nil (interactive) (zoom-emacs-pre24 1)))
    (global-set-key [C-kp-add]       '(lambda nil (interactive) (zoom-emacs-pre24 1)))
    (global-set-key (kbd "C--")      '(lambda nil (interactive) (zoom-emacs-pre24 -1)))
    (global-set-key [C-kp-subtract]  '(lambda nil (interactive) (zoom-emacs-pre24 -1)))
    )
  )


;; -------------------------------------------------------------------------
;; VISUAL ENHANCEMENTS

(use-package rainbow-mode
  :delight
  :config
  (defun prf/rainbow-mode-prog-mode-hook ()
    (rainbow-mode 1))
  (add-hook 'prog-mode-hook 'prf/rainbow-mode-prog-mode-hook)
  (add-hook 'conf-mode-hook 'prf/rainbow-mode-prog-mode-hook))

;; Preview faces at their definition
;; - [X] https://github.com/Fanael/highlight-defined
;;   Additionally provides faces for defined symbols.
;; - [ ] https://github.com/Fuco1/fontify-face
(use-package highlight-defined
  :init
  (setq highlight-defined-face-use-itself 't))

(when (fboundp 'prettify-symbols-mode)
  (setq prettify-symbols-unprettify-at-point 'right-edge))


;; -------------------------------------------------------------------------
;; WRITEROOM / DARKROOM

(when (display-graphic-p)
  (require 'init-writeroom))


(provide 'init-rice)
