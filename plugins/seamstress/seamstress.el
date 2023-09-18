


;; VARS

(defvar seamstress-file-path (executable-find "seamstress")
  "Path to the program used by `seamstress-run'")

(defvar seamstress-cli-arguments '()
  "Commandline arguments to pass to `seamstress'.")

(defvar seamstress-repl-mode-map
  (let ((kmap (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key kmap "\t" 'completion-at-point)
    kmap)
  "Basic mode map for `seamstress-run'.")

(defvar seamstress-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Prompt for `seamstress-run'.")



;; MAIN

(defvar seamstress-buffer-name "*seamstress*"
  "Name of the buffer to use for the `seamstress-run' comint instance.")

(defun seamstress-run ()
  "Run an inferior instance of `seamstress' inside Emacs."
  (interactive)
  (let* ((seamstress-program seamstress-file-path)
         (buffer (get-buffer-create seamstress-buffer-name))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    ;; if the process is dead then re-create the process and reset the
    ;; mode.
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "seamstress" buffer
               seamstress-program nil seamstress-cli-arguments)
        (seamstress-repl-mode)))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))

(defun seamstress-run-current-script ()
  (interactive)
  (let* ((script-path (buffer-file-name))
         (default-directory (file-name-directory script-path))
         (seamstress-cli-arguments `("-s" ,(file-name-sans-extension (file-name-nondirectory script-path)))))
    (seamstress-run)))



;; COMINT MODE

(defun seamstress--initialize ()
  "Helper function to initialize seamstress."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode seamstress-repl-mode comint-mode "seamstress"
  "Major mode for `run-seamstress'.

\\<seamstress-repl-mode-map>"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp seamstress-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(seamstress-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) seamstress-prompt-regexp))

(add-hook 'seamstress-repl-mode-hook 'seamstress--initialize)

(defconst seamstress-keywords
  '("screen.clear")
  "List of keywords to highlight in `seamstress-font-lock-keywords'.")

(defvar seamstress-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt seamstress-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `seamstress-repl-mode'.")




(provide 'seamstress)
