
(require 'dash)

;; TODO: have pickles hold their types, so they we culd just call a simple `unpickle' method
;; TODO: have the possibility to have the pickles be globally set w/ a suffix of prefix, so that we would not have to bother storing them in a variable client-side

;; ------------------------------------------------------------------------
;; VARS

(defun pickle-var (var-name)
  (when (listp var-name)
    (setq var-name (car var-name)))
  (when (boundp var-name)
    (cons var-name (eval var-name))))

(defun unpickle-var (pickled-var)
  (set (car pickled-var) (cdr pickled-var)))

(defun pickle-var-list (var-name-list)
  (mapcar #'pickle-var var-name-list))

(defun unpickle-var-list (pickled-var-list)
  (mapc #'unpickle-var pickled-var-list))


;; ------------------------------------------------------------------------
;; FACES

(defun pickle-face (face-name)
  (when (listp face-name)
    (setq face-name (car face-name)))
  (when (facep check-face)
    (cons face-name (face-all-attributes face-name))))

;; FIXME: not working, dunno why
(defun unpickle-face (pickled-face)
  (let ((face-name (car pickled-face))
	(face-attrs (cdr pickled-face)))

    (apply 'set-face-attribute face-name nil
	   (--mapcat (list (car it) (cdr it)) face-attrs))

    ;; (mapc
    ;;  (lambda (x)
    ;;    (message "(set-face-attribute %S nil %S %S)" face-name (car x) (cdr x))
    ;;    (funcall 'set-face-attribute face-name nil (car x) (cdr x)))
    ;;  face-attrs)
    ;; (-each face-attrs
    ;;   (lambda (x)
    ;; 	(message "(set-face-attribute %S nil %S %S)" face-name (car x) (cdr x))
    ;; 	(funcall 'set-face-attribute face-name nil (car x) (cdr x))))

    )
  )

(defun pickle-face-list (face-name-list)
  (mapcar #'pickle-face face-name-list))

(defun unpickle-face-list (pickled-face-list)
  (mapc #'unpickle-face pickled-face-list))


;; ------------------------------------------------------------------------
;; MINOR MODES

(defun pickle-minor-mode (minor-mode-name)
  (when (listp minor-mode-name)
    (setq minor-mode-name (car minor-mode-name)))
  (pickle-var (minor-mode-name)))

(defun unpickle-minor-mode (pickled-minor-mode)
  (let ((minor-mode-name (car pickled-minor-mode))
	(minor-mode-state (cdr pickled-minor-mode)))
    (if minor-mode-state
	(funcall (intern "minor-mode-name") 1)
      (funcall (intern "minor-mode-name") -1))))

(defun pickle-minor-mode-list (minor-mode-name-list)
  (mapcar #'pickle-minor-mode minor-mode-name-list))

(defun unpickle-minor-mode-list (pickled-minor-mode-list)
  (mapc #'unpickle-minor-mode pickled-minor-mode-list))


(provide 'pickling)
