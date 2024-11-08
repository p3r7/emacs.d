
(require 'ts)



;; FONT

;; (set-face-bold-p 'bold nil)

;; alt way to test: (when (find-font (font-spec :name "DejaVu Sans Mono") ...)

(defvar prf/rice/font nil)
(defvar prf/rice/x-font nil)
(defvar prf/rice/font-family nil)
(defvar prf/rice/font-height nil)
(defvar prf/rice/variable-pitch-font-family nil)
(defvar prf/rice/variable-pitch-font-height nil)

(defun prf/font/list-available ()
  "List available fonts on system.
NB: on linux, we don't call `font-family-list' but rely on command `fc-list' instead.
Indeed, on recent Emacs version, `font-family-list' returns nil when launched in daemon mode as a systemd service."
  (if (string-equal system-type "gnu/linux")
      (split-string (shell-command-to-string "fc-list"))
    (font-family-list)))

(defun prf/font/exist-p (searched-fonts)
  "Return the first available font in SEARCHED-FONTS that is present on system."
  (let ((available-fonts (prf/font/list-available)))
    (-some (lambda (f) (when (member f available-fonts) f))
           searched-fonts)))

(defun prf/font/update-frame ()
  (cond
   ;; `fontp' or "<FONT_FAMILY>-<SIZE>"
   ;; NB: this format allows setting decimal size
   ((and prf/rice/font
         (or (fontp prf/rice/font)
             (member (s-join "-" (butlast (s-split "-" prf/rice/font))) (prf/font/list-available))))
    (set-frame-font prf/rice/font nil t))

   ;; older x-format font
   (prf/rice/x-font
    ;; (setq default-frame-alist `((font . ,prf/rice/font)))
    (set-face-attribute 'default nil :font prf/rice/x-font))

   ;; split familly & height conf
   (:default
    (when (and prf/rice/font-family
	           (member prf/rice/font-family (font-family-list)))
      (set-face-attribute 'default nil :family prf/rice/font-family))
    (when prf/rice/font-height
      (set-face-attribute 'default nil :height prf/rice/font-height))))

  (when (and prf/rice/variable-pitch-font-family
             (member prf/rice/variable-pitch-font-family (font-family-list)))
    (set-face-attribute 'variable-pitch nil :family prf/rice/variable-pitch-font-family))
  (when prf/rice/variable-pitch-font-height
    (set-face-attribute 'variable-pitch nil :height prf/rice/variable-pitch-font-height)))

(prf/font/update-frame)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (prf/font/update-frame))))



;; THEMES

;; dark
(use-package chocolate-theme
  :defer t)
(use-package gotham-theme
  :defer t)
(use-package dracula-theme
  :defer t)
(use-package challenger-deep-theme
  :defer t)
(use-package zenburn-theme
  :defer t)

;; light
(use-package white-sand-theme
  :defer t)
(use-package plan9-theme
  :defer t)
(use-package ample-theme
  :defer t)
(use-package flatui-theme
  :defer t)

;; both
(use-package flucui-themes
  :defer t)
(use-package solarized-theme
  :defer t)

(defvar prf/theme/list/practical '())
(setq prf/theme/list/practical
      (list
       'chocolate
       ;; 'plan9
       'zenburn
       'white-sand
       ;; 'tango
       'flucui-light
       'dracula-mod
       'comidia-mod
       'solarized-dark
       ;; 'challenger-deep
       ;; 'gotham
       ;; 'light-blue
       'default
       ))

(defvar prf/theme/list/dark
  (list
   'comidia-mod
   'gotham
   'ample-flat
   'challenger-deep
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

;; NB: could alternatively have used DoReMi
(use-package prf-theme
  :quelpa (prf-theme :fetcher github :repo "p3r7/prf-theme")
  :after (space-theming helm)
  :demand
  :bind (([f12] . prf/theme/cycle-theme))
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (setq prf/theme/theme-list prf/theme/list/practical)
  ;; (setq prf/theme/theme-list prf/theme/list/retro-light)
  ;; (setq prf/theme/theme-list prf/theme/list/retro-dark)
  :config

  (add-hook 'prf/theme/after-update-hook
            (lambda ()
              (when (fboundp #'org-roam-ui--update-theme)
                (org-roam-ui-sync-theme))))

  ;; (prf/theme/initialize)
  (let ((now-h (ts-hour (ts-now))))
    (if (or (< now-h 9)
            (> now-h 17))
        (prf/theme/set-theme-from-list 'chocolate)
      (prf/theme/set-theme-from-list 'white-sand)))

  ;; for space-theming
  (setq space-theming--current-theme prf/theme/current-theme)
  (space-theming-update-current-theme))

(use-package helm-prf-theme
  :after (prf-theme helm)
  :ensure nil
  :bind (("C-<f12>" . helm-prf-theme-choose)))

(require 'init-doremi)

;; NB: also implemented in DoReMi, specifically `doremi-all-faces-fg' and
;;     `doremi-face-fg' from `doremi-frm'.
(use-package tame-fruit-salad
  :load-path "~/.emacs.d/plugins/tame-fruit-salad")

(use-package space-theming
  :demand
  :init
  ;; fix `custom-theme-set-faces' under emacs 27
  (setq custom--inhibit-theme-enable nil)

  (setq space-theming-headings-same-size '(flatui plan9))
  (setq space-theming-modifications
	    '((comidia-mod
           (form-feed-line :strike-through "chocolate1")
           (org-block :inherit default)
           (markdown-code-face :inherit nil)
           (bmkp-local-directory :foreground "#00ffff")
           (bmkp-remote-file :foreground "#ffc0cb")
           (fixed-pitch :family nil)    ; org block delimiters
           (diredp-other-priv :foreground "Black" :background "PaleGoldenrod")
           (bmkp-no-local :foreground "orange" :background nil))

          (solarized-dark
           (diredp-other-priv :foreground "Black" :background "PaleGoldenrod"))

          (plan9
           (form-feed-line :strike-through "#40883f")
           (org-level-1 :weight bold :height 1.0 :box nil :background nil :foreground "#4fa8a8")
           (org-level-2 :weight bold :height 1.0 :box nil :background nil :foreground "#b85c57")
           (org-level-3 :weight bold :foreground "#989848")
           (org-level-4 :weight bold :foreground "#40883f")
           (org-level-5 :weight bold :foreground "#0287c8")
           (org-level-6 :weight bold :foreground "#8888c8"))

          (white-sand
           (cursor :background "#585858")
           (region :background "#a4a4a4" :foreground "white")
           (form-feed-line :strike-through "#a9a9a9")
           (markdown-code-face :inherit nil)
           (fixed-pitch :family nil)    ; org block delimiters
           (org-headline-done :foreground nil))

          (tango
           (hl-line :inherit nil :background "#dbdbd7")
           (form-feed-line :strike-through "#b7b8b5")
           (font-lock-comment-face :foreground "#b7b8b5")
           (markdown-code-face :inherit nil))

          (flatui
           (org-block :inherit default)
           (markdown-code-face :inherit nil))

          (flucui-light
           (form-feed-line :strike-through "#95a5a6")
           (org-block :inherit default)
           (markdown-code-face :inherit nil)
           (fixed-pitch :family nil)    ; org block delimiters
           (org-headline-done :foreground nil))

          (zenburn
           (fixed-pitch :family nil)    ; org block delimiters
           (diredp-read-priv :foreground nil :background "#5F7F5F")
           (diredp-write-priv :foreground nil :background "#DC8CC3")
           (diredp-exec-priv :foreground nil :background "#CC9393")
           (diredp-other-priv :foreground "Black" :background "#E0CF9F")
           ;; (diredp-read-priv :foreground nil :background "#999932325555")
           ;; (diredp-write-priv :foreground nil :background "#25258F8F2929")
           ;; (diredp-exec-priv :foreground nil :background "#4F4F3B3B2121")
           ;; (diredp-no-priv :foreground nil :background "#2C2C2C2C2C2C")
           ;; (diredp-other-priv :foreground nil :background "PaleGoldenrod")
           (bmkp-heading :foreground "#F0DFAF") ; same as font-lock-keyword-face
           (bmkp-local-directory :foreground "#7CB8BB" :background nil)
           (bmkp-remote-file :foreground "#DF6B75")
           (form-feed-line :strike-through "#7F9F7F")
           (hl-line :background "#4a3434")
           (highlight :background "#4a3434")
           (org-headline-done :foreground nil)
           (bmkp-no-local :foreground "orange" :background nil))

          (late-night
           ;; (hl-line :inherit nil :background "#192a2a")
           (hl-line :inherit nil :background "#2a1919")
           (form-feed-line :strike-through "#555")
           (link :foreground "#006262")
           ;; (markdown-code-face :inherit nil)
           (org-block-begin-line :foreground "#594A3B")
           (org-block-end-line :foreground "#594A3B")
           (bmkp-no-local :foreground "orange" :background nil))

          (dracula-mod
           (form-feed-line :strike-through "#6272a4")
           (bmkp-local-directory :background nil :foreground "#8be9fd") ; rainbow-2
           (bmkp-remote-file :foreground "#ff79c6") ; rainbow-4
           (org-block :inherit default)
           (markdown-code-face :inherit nil)
           (fixed-pitch :family nil)    ; org block delimiters
           (diredp-other-priv :foreground "Black" :background "PaleGoldenrod")
           (org-headline-done :foreground nil)
           (bmkp-no-local :foreground "orange" :background nil))

          (chocolate
           ;; NB: file face for dired is `default'
           (dired-directory :foreground "#EAEAFE") ; chocolate-hue-2
           (org-block :inherit default)
           (org-block-begin-line :foreground "#594A3B")
           (org-block-end-line :foreground "#594A3B")
           (region :background "#C77497" :foreground "black") ; VS primary-selection ?
           (mode-line :background "#594A3B") ; chocolate-dark-yellow
           (mode-line-inactive :background "#2b241d") ; darker derivative of chocolate-dark-yellow
           (fringe :background "#2b241d") ; same as mode-line-inactive
           (show-paren-match :background "white" :foreground "black")
           (show-paren-mismatch :background "red" :foreground "white")
           (form-feed-line :strike-through "#705B5F") ;; :foreground of `font-lock-comment-delimiter-face'
           (fixed-pitch :family nil)
           (bmkp-local-directory :foreground "#45AFBD")
           (bmkp-remote-file :foreground "#C55D67")
           (diredp-other-priv :foreground "Black" :background "PaleGoldenrod")
           (org-headline-done :foreground nil)
           (bmkp-no-local :foreground "orange" :background nil))
          ))
  ;; to reload: (space-theming-update-current-theme)
  :config
  ;; current frame
  (space-theming-init-theming)

  ;; future frames
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (space-theming-update-current-theme)))))

;; allows converting emacs older (color-)themes to ~/.Xresources
(use-package color-theme-x)



;; BACKGROUND PICTURES

(use-package bgex
  :if (boundp 'bgex-exist-p)
  :quelpa (bgex :fetcher github :repo "wachikun/emacs_bgex")
  :config
  ;; (bgex-set-image-default "~/.emacs.d/resources/background/ghosts.xpm" t)
  )



;; HIGHLIGHT CURRENT WINDOW

(use-package dimmer
  :disabled
  :config
  (dimmer-mode))

;; alternatives:
;; - https://github.com/kriyative/highlight-focus
;; - https://github.com/emacsmirror/auto-dim-other-buffers
;; - https://amitp.blogspot.com/2013/05/emacs-highlight-active-buffer.html


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
(global-set-key (kbd "M-°")
                (lambda () (interactive) (djcb-opacity-modify)))
;; decrease opacity (== increase transparency => (djcb-opacity-modify t)
(global-set-key (kbd "C-M-+")
                (lambda () (interactive) (djcb-opacity-modify t)))
;; return the state to normal
(global-set-key (kbd "M-=")
                (lambda () (interactive)
                  (modify-frame-parameters nil '((alpha . 100)))))




;; ZOOM

(if (fboundp #'text-scale-set)

    (defun prf/text-scale-reset ()
      (interactive)
      (text-scale-set 0))

  (defvar prf/default-text-height (face-attribute 'default :height))

  (defun zoom-emacs-pre24 (n)
    "with positive N, increase the font size, otherwise decrease it"
    (set-face-attribute 'default (selected-frame) :height
                        (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))

  (defun prf/text-scale-reset ()
    (interactive)
    (set-face-attribute 'default (selected-frame)
                        :height prf/default-text-height))

  (defun prf/text-scale-increase () (interactive) (zoom-emacs-pre24 1))
  (defun prf/text-scale-decrease () (interactive) (zoom-emacs-pre24 -1))

  (defalias 'text-scale-increase #'prf/text-scale-increase)
  (defalias 'text-scale-decrease #'prf/text-scale-decrease))

(define-key global-map (kbd "C-+") #'text-scale-increase)
;; (define-key global-map [C-kp-add] #'text-scale-increase)
;; (define-key global-map (kbd "C--") #'text-scale-decrease)
(define-key global-map (kbd "C-°") #'text-scale-decrease)
;; (define-key global-map [C-kp-subtract] #'text-scale-decrease)
(define-key global-map (kbd "C-=") #'prf/text-scale-reset)



;; VISUAL ENHANCEMENTS

(use-package greenbar
  ;; NB: using own fork w/ reload of colors on theme change
  :load-path "~/.emacs.d/plugins/greenbar"
  :config
  (setq greenbar-background-colors 'greenbar)
  :init
  (add-hook 'comint-mode-hook #'greenbar-mode))
;; see also:: https://www.emacswiki.org/emacs/StripesMode


(use-package rainbow-mode
  ;; NB: disabled as highlights even partial symbols...
  :disabled
  :hook
  ((prog-mode conf-mode) . rainbow-mode)
  :delight)

;; Preview faces at their definition
;; - [X] https://github.com/Fanael/highlight-defined
;;   Additionally provides faces for defined symbols.
;;   moved to init/lang/init-elisp.el
;; - [ ] https://github.com/Fuco1/fontify-face

(use-package mwe-color-box
  :load-path "~/.emacs.d/plugins/mwe-color-box"
  :init
  (setq mwe:color-box-hide-parens nil))
;; REVIEW: might be better to use https://github.com/istib/rainbow-blocks or https://github.com/alphapapa/prism.el


 ;; WRITEROOM / DARKROOM

(when (display-graphic-p)
  (require 'init-writeroom))




(provide 'init-rice)
