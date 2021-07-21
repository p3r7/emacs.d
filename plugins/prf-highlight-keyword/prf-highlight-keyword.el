

;; VARS

(defvar prd-highlight-keyword-warn-keywords "\\<\\(FIXME\\|TODO\\|TEST\\|NOTE\\|NOTES\\|NB\\|WARN\\|WARNING\\|BEWARE|BUG\\|HACK\\|ASK\\|EYESORE\\|BAD\\|FIXME\\|REVIEW\\):")
(defvar prd-highlight-keyword-ok-keywords "\\<\\(DONE\\|OK\\|GOOD\\|SOLVED\\|FIXED\\|IDEA\\):")

(defvar prd-highlight-keyword-modes
  '(ada-mode c-mode emacs-lisp-mode java-mode haskell-mode web-mode
             literate-haskell-mode html-mode lisp-mode
             ;; php-mode
             python-mode ruby-mode tcl-mode
             scheme-mode sgml-mode sh-mode sml-mode markdown-mode org-mode
             yaml-mode clojure-mode clojurescript-mode clojurec-mode))



;; FACES

(make-face 'font-lock-warning-face-alt)
(set-face-attribute 'font-lock-warning-face-alt nil :foreground "White" :background "Firebrick")

(make-face 'font-lock-ok-face)
(set-face-attribute 'font-lock-ok-face nil :bold t :weight 'bold :inherit font-lock-type-face)

(make-face 'font-lock-ok-face-alt)
(set-face-attribute 'font-lock-ok-face-alt nil :foreground "White" :background "LightGreen")




(let ((pattern prd-highlight-keyword-warn-keywords))
  (mapc
   (lambda (mode)
     ;;     (font-lock-add-keywords mode `((,pattern 1 'font-lock-warning-face-alt prepend))))
     (font-lock-add-keywords mode `((,pattern 1 'font-lock-warning-face prepend))))
   prd-highlight-keyword-modes))

(let ((pattern prd-highlight-keyword-ok-keywords))
  (mapc
   (lambda (mode)
     (font-lock-add-keywords mode `((,pattern 1 'font-lock-ok-face prepend))))
   prd-highlight-keyword-modes))




(provide 'prf-highlight-keyword)
