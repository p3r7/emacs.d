

(require 'ert)
(require 'pickling)



;; VARS

(ert-deftest var-pickle-test ()
  "Ensure var gets pickled correctly"
  (let* ((my/var "cucumber")
         (pickle (pickle-var 'my/var)))
    (should (equal pickle '(my/var . "cucumber")))))

(ert-deftest var-unpickle-test ()
  "Ensure var gets unpickled correctly"
  (let* ((my/var "cucumber")
         (pickle (pickle-var 'my/var))
         (my/var "potato"))
    (unpickle-var pickle)
    (should (string= my/var "cucumber"))))

(ert-deftest var-list-pickle-test ()
  "Ensure var list gets pickled correctly"
  (let* ((my/var1 "cucumber")
         (my/var2 "potato")
         (my/var-list '(my/var1 my/var2))
         (pickle (pickle-var-list my/var-list)))
    (should (equal pickle '((my/var1 . "cucumber")
                            (my/var2 . "potato"))))))

(ert-deftest var-list-unpickle-test ()
  "Ensure var list gets unpickled correctly"
  (let* ((my/var1 "cucumber")
         (my/var2 "potato")
         (my/var-list '(my/var1 my/var2))
         (pickle (pickle-var-list my/var-list))
         (my/var1 "fork")
         (my/var2 "knife"))
    (unpickle-var-list pickle)
    (should (and (string= my/var1 "cucumber")
                 (string= my/var2 "potato")))))



;; FACES

(defface pickling-test-face
  '((t :foreground "black"
       :background "aquamarine"
       :weight bold))
  "Face for testing pickling parameters.")

(defface pickling-test-face2
  '((t :foreground "black"
       :background "PaleGoldenrod"
       :inherit font-lock-comment-face))
  "Face for testing pickling parameters.")


;; NB; might wanna use `internal-lisp-face-attributes'

(ert-deftest face-pickle-test ()
  "Ensure face gets pickled correctly"
  (let* ((pickle (pickle-face 'pickling-test-face)))
    (should (equal pickle '(pickling-test-face
                            (:family . unspecified)
                            (:foundry . unspecified)
                            (:width . unspecified)
                            (:height . unspecified)
                            (:weight . bold)
                            (:slant . unspecified)
                            (:underline . unspecified)
                            (:overline . unspecified)
                            (:extend . unspecified)
                            (:strike-through . unspecified)
                            (:box . unspecified)
                            (:inverse-video . unspecified)
                            (:foreground . "black")
                            (:background . "aquamarine")
                            (:stipple . unspecified)
                            (:inherit . unspecified))))))

(ert-deftest face-unpickle-test ()
  "Ensure face gets unpickled correctly"
  (let* ((pickle (pickle-face 'pickling-test-face)))
    ;; modify face
    (set-face-underline 'pickling-test-face 't)
    (should (equal (face-attribute 'pickling-test-face :underline) 't))
    ;; restore its original definition through pickle
    (unpickle-face pickle)
    (should (equal (face-attribute 'pickling-test-face :underline) 'unspecified))))

(ert-deftest face-unpickle-prop-test ()
  "Ensure face gets unpickled correctly"
  (let* ((override-pickle '(pickling-test-face (:underline . t)))
         (nil-pickle '(pickling-test-face (:underline . nil)))
         (restore-pickle '(pickling-test-face (:underline . unspecified))))
    ;; modify face
    (unpickle-face override-pickle)
    (should (equal (face-attribute 'pickling-test-face :underline) 't))

    ;; nil
    ;; REVIEW: how to handle it?
    ;; -> really stores nil, which gets in fine interpreted as unspecified
    ;; -> for buff-local variant, only nil works to "unset"
    (unpickle-face nil-pickle)
    (should (equal (face-attribute 'pickling-test-face :underline) nil))

    ;; restore its original definition through pickle
    (unpickle-face restore-pickle)
    (should (equal (face-attribute 'pickling-test-face :underline) 'unspecified))
    ))

(ert-deftest face-unpickle-buff-local-test ()
  "Ensure face gets unpickled correctly buffer-local"
  (let* ((pickle '(pickling-test-face (:underline . t))))
    (with-temp-buffer
      (unpickle-face-buff-local pickle)
      (should (equal (alist-get 'pickling-test-face face-remapping-alist)
                     '((:underline t)
                       pickling-test-face))))))



;; MINOR MODES

(define-minor-mode pickling-test-mode
  "Test minor-mode"
  :lighter nil)


(ert-deftest minor-mode-pickle-test ()
  "Ensure minor-mode gets pickled correctly"
  (pickling-test-mode 1)
  (let ((pickle (pickle-minor-mode 'pickling-test-mode)))
    (should (equal pickle '(pickling-test-mode . t))))
  (pickling-test-mode -1)
  (let ((pickle (pickle-minor-mode 'pickling-test-mode)))
    (should (equal pickle '(pickling-test-mode . nil)))))

(ert-deftest minor-mode-unpickle-test ()
  "Ensure minor-mode gets unpickled correctly"
  (pickling-test-mode 1)
  (let ((pickle (pickle-minor-mode 'pickling-test-mode)))
    (pickling-test-mode -1)
    (unpickle-minor-mode pickle)
    (should (equal pickle '(pickling-test-mode . t))))
  (pickling-test-mode -1)
  (let ((pickle (pickle-minor-mode 'pickling-test-mode)))
    (pickling-test-mode 1)
    (unpickle-minor-mode pickle)
    (should (equal pickle '(pickling-test-mode . nil)))))
