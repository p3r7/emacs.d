
(require 'tramp)
(require 'dash)
(require 'subr-x)

(defvar tramp-plinki-ppk nil)

(defvar tramp-plinki-methods '("pscp" "plink" "plinkx" "psftp"))


;; ------------------------------------------------------------------------
;; MODIFY EXISTING TRAMP METHODS: ARGS

(defun tramp-plinki--add-certificate-login-arg (tramp-login-args)
  (let ((login-args (car (cdr tramp-login-args))))
    (if (string= "" tramp-plinki-ppk)
        tramp-login-args
      (add-to-list 'login-args `("-i" ,(concat "\"" tramp-plinki-ppk "\"")))
      `(tramp-login-args ,login-args))))

(defun tramp-plinki--add-certificate-copy-arg (tramp-copy-args)
  (let ((copy-args (car (cdr tramp-copy-args))))
    (if (string= "" tramp-plinki-ppk)
        tramp-copy-args
      (add-to-list 'copy-args `("-i" ,(concat "\"" tramp-plinki-ppk "\"")))
      `(tramp-copy-args ,copy-args))))


;; ------------------------------------------------------------------------
;; MODIFY EXISTING TRAMP METHODS: METHOD

;; REVIEW: seems to eval whole method-def-args, which is unwanted
(defun tramp-plinki--add-certificate-login-arg-to-method (tramp-method-def)
  (let ((method-name (car tramp-method-def))
        (method-def-args (cdr tramp-method-def)))
    (cons method-name
          (-map-when
           (lambda (e) (equal (car e) 'tramp-login-args))
           #'tramp-plinki--add-certificate-login-arg
           method-def-args))))

(defun tramp-plinki--add-certificate-copy-arg-to-method (tramp-method-def)
  (let ((method-name (car tramp-method-def))
        (method-def-args (cdr tramp-method-def)))
    (cons method-name
          (-map-when
           (lambda (e) (equal (car e) 'tramp-copy-args))
           #'tramp-plinki--add-certificate-copy-arg
           method-def-args))))

(defun tramp-plinki--suffix-method-name (tramp-method-def)
  (let ((method-name (car tramp-method-def))
        (method-def-args (cdr tramp-method-def)))
    (cons (concat method-name "i")
          method-def-args)))

(defun tramp-plinki--register-tramp-completion-fun (tramp-method-def)
  (let ((method-name (car tramp-method-def)))
    (tramp-set-completion-function (concat method-name "i") tramp-completion-function-alist-ssh)
    tramp-method-def))


;; ------------------------------------------------------------------------
;; MODIFY EXISTING TRAMP METHODS: METHOD LIST

(defun tramp-plinki--get-enriched-tramp-methods ()
  (-map-when
   (lambda (e) (member (car e) tramp-plinki-methods))
   (lambda (e) (thread-first e
            (tramp-plinki--add-certificate-login-arg-to-method)
            (tramp-plinki--add-certificate-copy-arg-to-method)))
   tramp-methods))

(defun tramp-plinki-enrich-existing ()
  (setq tramp-methods (tramp-plinki--get-enriched-tramp-methods)))

(defun tramp-plinki--get-enriched-with-new-tramp-methods ()
  (-map-when
   (lambda (e) (member (car e) tramp-plinki-methods))
   (lambda (e) (thread-first e
            ;; (tramp-plinki--register-tramp-completion-fun)
            (tramp-plinki--suffix-method-name)
            (tramp-plinki--add-certificate-login-arg-to-method)
            (tramp-plinki--add-certificate-copy-arg-to-method)))
   tramp-methods))


;; ------------------------------------------------------------------------
;; REGISTER NEW TRAMP METHODS

(defun tramp-plinki-register-new ()
  (when (not tramp-plinki-ppk)
    (error "empty value for tramp-plinki-ppk"))

  (setq tramp-methods (tramp-plinki--get-enriched-with-new-tramp-methods))

  (-map-when
   (lambda (e) (member (car e) tramp-plinki-methods))
   (lambda (e)
     (let ((method-name (car tramp-method-def))
           (tramp-set-completion-function (concat method-name "i") tramp-completion-function-alist-ssh)))
     nil)
   tramp-methods))


(provide 'tramp-plinki)
