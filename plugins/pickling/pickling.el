;;; pickling.el --- Save and restore state.

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; URL: https://github.com/p3r7/prf-tramp
;; Package-Requires: ((dash "2.16.0"))
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; REQUIRES

(require 'dash)



;; GENERIC

(defun pickle (type name)
  (cond
   ((pickling--permissive-eq type 'var)
    (list :var (pickle-var name)))
   ((pickling--permissive-eq type 'face)
    (list :face (pickle-face name)))
   ((pickling--permissive-eq type 'minor-mode)
    (list :minor-mode (pickle-minor-mode name)))
   (t
    (error "Unsupported type %s" type))))


(defun unpickle (pickled-stuff)
  (let ((type (car pickled-stuff))
        (stuff (cdr pickled-stuff)))
    (cond
     ((pickling--permissive-eq type 'var)
      (unpickle-var stuff))
     ((pickling--permissive-eq type 'face)
      (unpickle-face stuff))
     ((pickling--permissive-eq type 'minor-mode)
      (unpickle-minor-mode stuff))
     (t
      (error "Unsupported type %S" type)))))



;; VAR

(defun pickle-var (var-name)
  (when (listp var-name)
    (setq var-name (car var-name)))
  (when (stringp var-name)
    (setq var-name (intern var-name)))
  (when (boundp var-name)
    (cons var-name (eval var-name))))

(defun unpickle-var (pickled-var)
  (set (car pickled-var) (cdr pickled-var)))

(defun pickle-var-list (var-name-list)
  (mapcar #'pickle-var var-name-list))

(defun unpickle-var-list (pickled-var-list)
  (mapc #'unpickle-var pickled-var-list))



;; FACES

(defun pickle-face (face-name)
  (when (listp face-name)
    (setq face-name (car face-name)))
  (when (stringp face-name)
    (setq face-name (intern face-name)))
  (when (facep face-name)
    (cons face-name (face-all-attributes face-name (selected-frame)))))

(defun unpickle-face (pickled-face)
  (let ((face-name (car pickled-face))
	(face-attrs (cdr pickled-face)))
    (apply 'set-face-attribute face-name nil
	   (--mapcat (list (car it) (cdr it)) face-attrs))))

(defun pickle-face-list (face-name-list)
  (mapcar #'pickle-face face-name-list))

(defun unpickle-face-list (pickled-face-list)
  (mapc #'unpickle-face pickled-face-list))



;; MINOR MODES

(defun pickle-minor-mode (minor-mode-name)
  (when (listp minor-mode-name)
    (setq minor-mode-name (car minor-mode-name)))
  (when (stringp minor-mode-name)
    (setq minor-mode-name (intern minor-mode-name)))
  (pickle-var minor-mode-name))

(defun unpickle-minor-mode (pickled-minor-mode)
  (let ((minor-mode-name (car pickled-minor-mode))
	(minor-mode-state (cdr pickled-minor-mode)))
    (if minor-mode-state
	(funcall minor-mode-name 1)
      (funcall minor-mode-name -1))))

(defun pickle-minor-mode-list (minor-mode-name-list)
  (mapcar #'pickle-minor-mode minor-mode-name-list))

(defun unpickle-minor-mode-list (pickled-minor-mode-list)
  (mapc #'unpickle-minor-mode pickled-minor-mode-list))



;; PRIVATE HELPERS

(defun pickling--permissive-eq (val ref-symbol)
  "Like `eq' but expect REF-SYMBOL to be a symbol.
Compare VAL against its symbol, string and keyword derivatives"
  (let* ((str (symbol-name ref-symbol))
         (keyword (intern (concat ":" str))))
    (member val (list ref-symbol str keyword))))




(provide 'pickling)

;;; pickling.el ends here.
