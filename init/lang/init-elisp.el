

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

  (defun prf/scratch nil
    "create a scratch buffer"
    (interactive)
    (let ((scratch-buffer (get-buffer "*scratch*")))
      (if scratch-buffer
	  (switch-to-buffer scratch-buffer)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(insert initial-scratch-message)
	(lisp-interaction-mode)))))



;; NAVIGATION / REFACTORING HELPERS

(use-package macrostep
  :after (elisp-mode)
  :bind (:map emacs-lisp-mode-map
	      ("C-c e m" . macrostep-expand)))

(use-package elisp-slime-nav
  :after (elisp-mode)
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))



;; EVALUATION

(use-package eval-expr
  :after (elisp-mode)
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    ;; (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (local-set-key (kbd "<tab>") #'completion-at-point)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))



;; FONT LOCK

(use-package lisp-extra-font-lock
  :after (elisp-mode)
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))


(use-package cl-lib-highlight
  :after (elisp-mode)
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




(require 'init-eldoc)




(provide 'init-elisp)
