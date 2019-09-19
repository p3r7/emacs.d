
;; Stolen from user AlexSchroeder aka egoge


(defvar tame-fruit-salad--max-allowed-colours 16)



(defun tame-fruit-salad--tame-colour (colour &optional degree)
  "Return a colour string specifying a washed-out version of COLOUR."
  (let ((basec (color-values
		(face-attribute 'default :foreground)))
	(col (color-values colour))
	(list nil))
    (unless degree (setq degree 2))
    (while col
      (push (/ (/ (+ (pop col)
		     (* degree (pop basec)))
		  (1+ degree))
	       256)
	    list))
    (apply 'format "#%02x%02x%02x" (nreverse list))))

(defun tame-fruit-salad--tame-face (face &optional degree)
  "Make the foreground colour of FACE appear a bit more pale."
  (let ((colour (face-attribute face :foreground)))
    (unless (eq colour 'unspecified)
      (set-face-attribute face nil
 			  :foreground (tame-fruit-salad--tame-colour colour degree)))))

(defun tame-fruit-salad--find-faces (regexp)
  "Return a list of all faces whose names match REGEXP."
  (delq nil
 	(mapcar (lambda (face)
 		  (and (string-match regexp
 				     (symbol-name face))
 		       face))
 		(face-list))))




(defun tame-fruit-salad (&optional degree)
  (interactive)
  (mapc (lambda (elt)
 	  (tame-fruit-salad--tame-face elt degree))
 	(delq 'font-lock-warning-face
 	      (tame-fruit-salad--find-faces "^font-lock"))))

(defun tame-fruit-salad-when-too-many-colors (&optional degree)
  (interactive)
  (when (> (length (defined-colors)) tame-fruit-salad--max-allowed-colours)
    (tame-fruit-salad degree)))


(provide 'tame-fruit-salad)
