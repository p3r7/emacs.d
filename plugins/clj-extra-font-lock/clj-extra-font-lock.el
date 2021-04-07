



;; BASIC

(defcustom clj-extra-font-lock-mode-text ""
  "String to display in the mode line when clj-extra-font-lock-mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "clj-extra-font-lock mode text"                ; To separate it from `global-...'
  :group 'clj-extra-font-lock
  :type 'string)




;; EXTRA FONT LOCKS

(defun clj-extra-font-lock--match-ns-prefixed-word-list (ns word-list limit)
  (let ((case-fold-search t))
    ;; NB: we need to copy word-list to prevent sort from altering it
    (re-search-forward
     (concat
      "\\(" ns "\\)/\\("
      (s-join "\\|"
	      (sort (copy-seq word-list) (lambda (a b) (> (length a) (length b)))))
      ;; (sort word-list 'string-lessp))
      "\\)\\_>") limit t)))

(setq clj-extra-font-lock--encore-macros '("defonce" "declare-remote" "defalias"
                                           "compile-if"
                                           "if" "if-not" "if-some" "if-let" "if-lets"
                                           "when" "when-not" "when-some" "when-let" "when-lets"
                                           "cond" "cond*" "cond-throw" "doto-cond" "case-eval"
                                           "check-some" "check-all"
                                           "have" "have?" "have-in" "have-in!"
                                           "catching" "thrown"))

(defun clj-extra-font-lock--match-encore-macros (limit)
  (clj-extra-font-lock--match-ns-prefixed-word-list "encore" clj-extra-font-lock--encore-macros limit))

(setq clj-extra-font-lock-highlights
      '((clj-extra-font-lock--match-encore-macros 2 'font-lock-keyword-face)))



;; MODE DEFINITION

(define-minor-mode clj-extra-font-lock-mode
  "Additional font locks for Clojure."
  :lighter clj-extra-font-lock-mode-text

  (font-lock-add-keywords nil clj-extra-font-lock-highlights)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))




(provide 'clj-extra-font-lock)
