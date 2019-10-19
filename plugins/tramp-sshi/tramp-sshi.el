
(require 'dash)

(defvar tramp-sshi-cert nil)


;; ------------------------------------------------------------------------
;; MODIFY EXISTING TRAMP METHODS

(defun tramp-sshi--add-certificate-login-arg (tramp-login-args)
  (let ((login-args (car (cdr tramp-login-args))))
    (if (string= "" tramp-sshi-cert)
        tramp-login-args
      (add-to-list 'login-args `("-i" ,(concat "\"" tramp-sshi-cert "\"")))
      `(tramp-login-args ,login-args))))

;; REVIEW: seems to eval whole method-def-args, which is unwanted
(defun tramp-sshi--add-certificate-login-arg-to-method (tramp-method-def)
  (let ((method-name (car tramp-method-def))
        (method-def-args (cdr tramp-method-def)))
    (cons method-name
          (-map-when
           (lambda (e) (equal (car e) 'tramp-login-args))
           #'tramp-sshi--add-certificate-login-arg
           method-def-args))))

(defun tramp-sshi--get-enriched-tramp-methods ()
  (-map-when
   (lambda (e) (member (car e) '("ssh" "sshx")))
   #'tramp-sshi--add-certificate-login-arg-to-method
   tramp-methods))

(defun tramp-sshi-enrich-existing ()
  (setq tramp-methods (tramp-sshi--get-enriched-tramp-methods)))



(provide 'tramp-sshi)
