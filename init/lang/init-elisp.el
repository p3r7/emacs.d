

;; VARS

(defvar prf/elisp-modes '(emacs-lisp-mode
                          inferior-emacs-lisp-mode
                          lisp-interaction-mode
                          ielm-mode))



;; MAIN

(use-package elisp-mode
  :ensure nil
  :demand
  :commands emacs-lisp-mode
  :delight (emacs-lisp-mode "Elisp")
  ;; :interpreter ("emacs" . emacs-lisp-mode)
  :bind (;; stolen from John Wiegley
	 ("C-c e b" . eval-buffer)
	 ("C-c e e" . toggle-debug-on-error)
	 ("C-c e f" . emacs-lisp-byte-compile)
	 ("C-c e r" . eval-region)
	 ("C-c e s" . prf/scratch))
  :init
  ;; ERT
  (dolist (mode prf/elisp-modes)
    (font-lock-add-keywords
     mode
     '(("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t)))))

  (defvar prf/scratch-default-directory nil)

  (defun prf/scratch nil
    "create a scratch buffer"
    (interactive)
    (let ((default-directory prf/scratch-default-directory)
          (scratch-buffer (get-buffer "*scratch*")))
      (if scratch-buffer
	  (switch-to-buffer scratch-buffer)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(insert initial-scratch-message)
	(lisp-interaction-mode)))))



;; HELP

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	     ("C-h v" . helpful-variable)
	     ("C-h k" . helpful-key)
	     ("C-h o" . helpful-at-point)
	     ("C-h F" . helpful-function)))

(use-package which-key
  :delight
  :config
  (which-key-mode))



;; CODE NAVIGATION

;; NB: generic stuff in ~/.emacs.d/init/init-file-navigation.el

;; stolen from John Wiegley
(define-prefix-command 'prf/lisp-find-map)
(bind-key "C-h e" #'prf/lisp-find-map)
(bind-key "C-h e e" #'view-echo-area-messages)
(bind-key "C-h e f" #'find-function)
(bind-key "C-h e k" #'find-function-on-key)
(bind-key "C-h e l" #'find-library)
(bind-key "C-h e v" #'find-variable)
(bind-key "C-h e V" #'apropos-value)

(use-package find-func
  :ensure nil
  :demand
  :bind ("C-h C-F" . find-function))

;; NB: elisp-def seem to handle better vars VS functions
;; elisp-def ony handled features in `require' blocks
;; whereas elisp-slime-nav handles `featurep' and `use-package'
;; also, standard way is `xref-find-definitions'

(use-package elisp-slime-nav
  :after elisp-mode
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point)
  :bind ("C-h C-f" . prf/find-elisp-thing-at-point)
  :config
  (defun prf/find-elisp-thing-at-point (symbol)
    "Go to SYMBOL definition. When called interactively, propose symbol at point.
Wrapper around `elisp-slime-nav-find-elisp-thing-at-point'."
    (interactive
     (list
      (read-string (format "symbol (%s): " (elisp-slime-nav--read-symbol-at-point))
                   nil nil (elisp-slime-nav--read-symbol-at-point))))
    (elisp-slime-nav-find-elisp-thing-at-point symbol)))

(use-package elisp-def
  :disabled
  :after elisp-mode
  :diminish
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode))



;; NAVIGATION / REFACTORING

(use-package macrostep
  :after elisp-mode
  :bind (
         :map emacs-lisp-mode-map
	     ("C-c e m" . macrostep-expand)))



;; EVALUATION / TESTS

(use-package eval-expr
  :after elisp-mode
  :bind ("M-:" . eval-expr)
  :config
  (defvar prf/eval-expr-minibuffer-setup-hook nil)

  (defun eval-expr-minibuffer-setup ()
    ;; (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (local-set-key (kbd "<tab>") #'completion-at-point)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (run-hooks 'prf/eval-expr-minibuffer-setup-hook)))

(use-package ert
  :ensure nil
  :demand
  :bind (
         :map emacs-lisp-mode-map
         ("C-x R" . ert-silently)
         :map lisp-interaction-mode-map
         ("C-x R" . ert-silently))
  :config
  (defun ert-silently ()
    (interactive)
    (ert t)))



;; FONT LOCK

;; NB: might conflict w/ lisp-extra-font-lock ?
;; primarilly wanted to highlight faces at definition
;; don't seem to work, though
(use-package highlight-defined
  :disabled
  :after elisp-mode
  :hook (emacs-lisp-mode . highlight-defined-mode)
  :init
  (setq highlight-defined-face-use-itself 't))


(use-package lisp-extra-font-lock
  :after elisp-mode
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))


(use-package cl-lib-highlight
  :quelpa (cl-lib-highlight :fetcher github :repo "skeeto/cl-lib-highlight")
  :after elisp-mode
  :config

  (defvar prf/cl-lib-highlight-valid 'prf/cl-lib-highlight-valid
    "Face name to use for valid cl-lib keywords.")

  (defvar prf/cl-lib-highlight-valid-warning 'prf/cl-lib-highlight-valid-warning
    "Face name to use for valid warning cl-lib keywords.")

  (defface prf/cl-lib-highlight-valid
    '((t :inherit font-lock-keyword-face :underline "steel blue"))
    "Face for deprecated cl functions and macros."
    :group 'cl-lib-highlight)

  (defface prf/cl-lib-highlight-valid-warning
    '((t :inherit font-lock-warning-face :underline "steel blue"))
    "Face for deprecated cl functions and macros."
    :group 'cl-lib-highlight)

  (custom-set-faces
   ;; use underline, like in highlight-cl package
   '(cl-lib-highlight-deprecated
     ((t (:inherit font-lock-keyword-face :underline "red")))))

  (defun prf/cl-lib-highlight-initialize ()
    "Add all cl-lib font lock highlighting to `emacs-lisp-mode'."
    (interactive)
    (cl-labels ((opt (syms) (regexp-opt (mapcar #'symbol-name syms) t)))
      (let ((defs (list (concat "(" (opt cl-lib-highlight-defs) "\\_>"
                                "\\s-*" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
                        '(1 prf/cl-lib-highlight-valid)
                        '(2 font-lock-function-name-face nil t)))
            (types (list (concat "(" (opt cl-lib-highlight-types) "\\_>"
                                 "\\s-*" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
                         '(1 prf/cl-lib-highlight-valid)
                         '(2 font-lock-type-face nil t)))
            (warnings (list (concat "(" (opt cl-lib-highlight-warnings) "\\_>")
                            '(1 prf/cl-lib-highlight-valid-warning)))
            (keywords (list (concat "(" (opt cl-lib-highlight-keywords) "\\_>")
                            '(1 prf/cl-lib-highlight-valid))))
        (font-lock-add-keywords 'emacs-lisp-mode
                                (list defs types warnings keywords))
        (font-lock-add-keywords 'lisp-interaction-mode
                                (list defs types warnings keywords)))))

  (prf/cl-lib-highlight-initialize)
  (cl-lib-highlight-warn-cl-initialize))


(use-package easy-escape
  :delight easy-escape-minor-mode
  :hook ((emacs-lisp-mode ielm-mode) . easy-escape-minor-mode))




(provide 'init-elisp)
