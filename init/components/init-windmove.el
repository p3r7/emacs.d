
;; ------------------------------------------------------------------------
;; WINDMOVE

(defvar prf/key-split-window-vertically "<C-s-down>")
(defvar prf/key-split-window-horizontally "<C-s-right>")
(defvar prf/key-delete-other-windows "<C-s-left>")
(defvar prf/key-delete-window "<C-s-up>")

(require 'windmove)
(eval-after-load "windmove"
  '(cond
    ((gnu/linux-p)
     (progn
       (windmove-default-keybindings 'super)))
    ((darwin-p)
     (progn
       (windmove-default-keybindings 'super)))
    ((windows-nt-p)
     (progn
       (global-set-key (read-kbd-macro key-windmove-left)  'windmove-left)
       (global-set-key (read-kbd-macro key-windmove-righ)  'windmove-right)
       (global-set-key (read-kbd-macro key-windmove-up)    'windmove-up)
       (global-set-key (read-kbd-macro key-windmove-down)  'windmove-down)))
    )
  )

(global-set-key (read-kbd-macro prf/key-split-window-vertically)   'split-window-vertically)
(global-set-key (read-kbd-macro prf/key-split-window-horizontally) 'split-window-horizontally)
(global-set-key (read-kbd-macro prf/key-delete-other-windows)      'delete-other-windows)
(global-set-key (read-kbd-macro prf/key-delete-window)             'delete-window)


;; ------------------------------------------------------------------------
;; FRAMEMOVE

;; (when (prf/require-plugin 'framemove nil 'noerror)
;; (setq framemove-hook-into-windmove t)
;; )

;; ------------------------------------------------------------------------
;; WINDOWS ROTATION

;; TODO: reverse mode as well
;; NOTE: problem w/ sr-speedbar
(defun prf/rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))
(defun prf/rotate-windows ()
  (interactive)
  (prf/rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))
(global-set-key (kbd "<C-tab>") 'prf/rotate-windows)
(define-key org-mode-map (kbd "<C-tab>") 'prf/rotate-windows)


;; ------------------------------------------------------------------------

(provide 'init-windmove)
