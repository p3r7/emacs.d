
(use-package clojure-mode
  :after flycheck-clj-kondo
  :hook ((clojure-mode . (lambda () (eldoc-mode 1)))
         (clojure-mode . (lambda () (flycheck-mode 1))))
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :demand
  :after (clojure-mode)
  :bind (
         :map cider-mode-map
	 ("C-h f" . cider-doc)
	 ("C-s-n" . cider-find-ns)
	 ;; ("C-h F" . cider-clojuredocs-web)
	 ("C-h C-f" . cider-find-var)
         ("C-c e b" . cider-eval-buffer)
         ("C-c e s" . cider-scratch)
	 :map cider-repl-mode-map
	 ("C-h f" . cider-doc)
         ("C-s-n" . cider-find-ns)
	 ;; ("C-h F" . cider-clojuredocs-web)
	 ("C-h C-f" . cider-find-var)
         ("C-c e s" . cider-scratch)
	 ("C-c E" . cider-repl-clear-buffer)
	 ("C-x k" . cider-quit)
	 ("C-x C-k" . cider-quit))
  :init
  (setq org-babel-clojure-backend 'cider)
  (setq cider-repl-display-in-current-window t) ; don't pop a window randomly
  (setq cider-repl-display-help-banner nil)
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



;; EXTRA FONT LOCK

;; mainly highlight encore macro even when prefixed

(use-package clj-extra-font-lock
  :load-path "~/plugins/clj-extra-font-lock/"
  :demand
  :hook ((clojure-mode . clj-extra-font-lock-mode)))



;; DEBUGGING - PORTAL

;; NB: from https://github.com/djblue/portal#emacs-integration

(with-eval-after-load "cider"
  (setq cider-clojure-cli-global-options "-A:portal")

  (defun portal.api/open ()
    (interactive)
    (cider-nrepl-sync-request:eval
     "(require '[portal.api :as p]) (p/tap) (p/open)"))

  (defun portal.api/clear ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/clear)"))

  (defun portal.api/close ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/close)"))

  (define-key cider-mode-map (kbd "C-c p o") #'portal.api/open)
  (define-key cider-mode-map (kbd "C-c p c") #'portal.api/clear)
  (define-key cider-mode-map (kbd "C-c p k") #'portal.api/close))



;; REFACTORING

;; NB: will be merged into CIDER in the future

(use-package clj-refactor
  :after clojure-mode
  :hook
  ((clojure-mode . clj-refactor-mode)
   (clj-refactor-mode . (lambda () (cljr-add-keybindings-with-prefix "C-c C-m"))))
  :bind (
         :map cider-mode-map
	 ("<SunProps>" . hydra-cljr-help-menu/body)
         :map cider-repl-mode-map
         ("<SunProps>" . hydra-cljr-help-menu/body))
  :init
  (setq cljr-warn-on-eval nil))



;; LINT

(use-package flycheck-clj-kondo
  :after flycheck)



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

;; (use-package clojure-essential-ref-nov
;;   :bind (
;;          :map cider-mode-map
;;          ("C-h F" . clojure-essential-ref)
;;          :map cider-repl-mode-map
;;          ("C-h F" . clojure-essential-ref)))

(use-package clojure-essential-ref-nov
  :bind (
         :map cider-mode-map
         ("C-h F" . clojure-essential-ref)
         :map cider-repl-mode-map
         ("C-h F" . clojure-essential-ref))
  :init
  (setq clojure-essential-ref-nov-epub-path "/home/eigen/Calibre Library/Renzo Borgatti/Clojure_ The Essential Reference MEAP V29 (40)/Clojure_ The Essential Reference MEAP V29 - Renzo Borgatti.epub")
  (setq clojure-essential-ref-default-browse-fn #'clojure-essential-ref-nov-browse))




(provide 'init-clojure)
