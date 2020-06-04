
(use-package clojure-mode
  :hook (clojure-mode . (lambda () (eldoc-mode 1))))

(use-package cider
  :demand
  :after (clojure-mode)
  :bind (
         :map cider-mode-map
	 ("C-h f" . cider-doc)
	 ;; ("C-h F" . cider-clojuredocs-web)
	 ("C-h V" . cider-find-var)
	 :map cider-repl-mode-map
	 ("C-h f" . cider-doc)
	 ;; ("C-h F" . cider-clojuredocs-web)
	 ("C-h V" . cider-find-var)
	 ("C-c E" . cider-repl-clear-buffer)
	 ("C-x k" . cider-quit)
	 ("C-x C-k" . cider-quit))
  :init
  (setq org-babel-clojure-backend 'cider)
  :config
  (require 'ob-clojure)

  ;; stolen from https://github.com/codahale/emacs.d/blob/master/init.el
  (defun prf/cider-repl-reset ()
    (interactive)
    (save-some-buffers)
    (cider-interactive-eval
     (concat "((or (resolve 'user/reset)"
             "     (resolve 'clojure.tools.namespace.repl/refresh)))")))
  (define-key cider-mode-map (kbd "C-c C-x") 'prf/cider-repl-reset)
  (define-key clojure-mode-map (kbd "C-c C-x") 'prf/cider-repl-reset))


(use-package cider-eval-sexp-fu
  :after cider)


(defun prf/cider/send-to-repl (sexp &optional eval ns)
  "Send SEXP to Cider Repl. If EVAL is t, evaluate it.
Optionally, we can change namespace by specifying NS."
  (cider-switch-to-repl-buffer ns)
  ;; inspired by `cider-repl--grab-old-input'
  (goto-char cider-repl-input-start-mark)
  (delete-region (point) (point-max))
  (save-excursion
    (insert sexp)
    (when (equal (char-before) ?\n)
      (delete-char -1)))
  (when eval
    (cider-repl--send-input t)))



;; REFACTORING

;; NB: will be merged into CIDER in the future
(use-package clj-refactor
  :after clojure-mode
  :hook
  ((clojure-mode . clj-refactor-mode)
   (clj-refactor-mode . (lambda () (cljr-add-keybindings-with-prefix "C-c C-m")))))



;; DEPS INJECTION

;; REVIEW: only clojars or also maven?
;; otherwise would be better to use: lein search <term>
(use-package clojars)


(defun prf/clj/copy-pomegranate-dep (&optional dep)
  (interactive)
  (setq dep (or dep (read-string "Dep: ")))
  (kill-new
   (prf/clj/pomegranate-dep dep)))

(defun prf/clj/pomegranate-dep (dep)
  (concat
   (format
    "%s"
    ;; NB: this is clojure!
    `(use '[cemerick.pomegranate :only (add-dependencies)]))
   (s-replace-all
    `(("\\." . ".")
      ("mydep" . ,dep))
    (format
     "%S"
     ;; NB: this is clojure!
     `(add-dependencies :coordinates '[mydep]
                        :repositories (merge cemerick.pomegranate.aether/maven-central
                                             {"clojars" "https://clojars.org/repo"}))))))

(defun prf/cider/inject-pomegranate-dep (&optional dep ns)
  (interactive)
  (setq dep (or dep (read-string "Dep: ")))
  (prf/cider/send-to-repl (prf/clj/pomegranate-dep dep) t ns))



;; DOC

(use-package clojure-essential-ref
  :quelpa (clojure-essential-ref :fetcher github :repo "p3r7/clojure-essential-ref")
  :bind (
         :map cider-mode-map
         ("C-h F" . clojure-essential-ref)
         :map cider-repl-mode-map
         ("C-h F" . clojure-essential-ref)))




(provide 'init-clojure)
