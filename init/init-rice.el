
;; -------------------------------------------------------------------------
;; FONT

(set-face-bold-p 'bold nil)

(when (boundp 'prf/font)
  (if (windows-nt-p)
      ;; (setq default-frame-alist (intern (concat"((font . " prf/font "))")))
      (setq default-frame-alist '((font . "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")))
    (setq font-default prf/font)
      )
  )

(when (boundp 'prf/font-height)
  (set-face-attribute 'default nil :height prf/font-height)
  )


;; -------------------------------------------------------------------------
;; THEMES

(prf/install-package 'gotham-theme)
(prf/install-package 'ample-theme)
(prf/install-package 'plan9-theme)

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
   					;'pink-bliss
   'plan9
   ;; 'soft-morning
   ;; 'soft-stone
   'tango
   ))

(when (require 'prf-theme nil 'noerror)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  ;; (setq prf/theme/theme-list prf/theme/list/dark)
  (setq prf/theme/theme-list prf/theme/list/retro-light)
  (prf/theme/initialize)
  (global-set-key [f12] 'prf/theme/cycle-theme)
  )


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

(if (<= emacs-major-version 24)
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
    (global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
    (global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
    (global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
    (global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))
    )
  )



(provide 'init-rice)
