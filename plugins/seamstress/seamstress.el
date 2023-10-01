;;; seamstress.el --- Interactive development environment inspired by norns -*- lexical-binding: t; -*-



;; DEPS

(require 'dash)
(require 's)
(require 'rx)

(require 'lua-mode)



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

(defvar seamstress-buffer-name "*seamstress*"
  "Name of the buffer to use for the `seamstress-run' comint instance.")

(defvar seamstress-repl-script-path nil
  "Path to the script being launched by visited seamstress buffer.")

(defvar seamstress-lua-lib-inspect-url "https://raw.githubusercontent.com/kikito/inspect.lua/master/inspect.lua")

(defvar seamstress-repl-switch-on-cmd t
  "If non-nil, switch to seamstress REPL buffer after
sending it a command.")
(defvar seamstress-repl-switch-fn #'switch-to-buffer-other-window "Function to use when `seamstress-repl-switch-on-cmd' is non-nil.")
(defvar seamstress-repl-switch-no-focus t
  "If non-nil, don't have popping REPL window steal focus after
calling `seamstress-repl-switch-fn'.")

(defvar seamstress-mode-lighter " seamstress" "Lighter for seamstress minor mode.")



;; PATHS


(defun seamstress--script-path ()
  (cond (seamstress-repl-script-path
         seamstress-repl-script-path)

        ;; TODO: parse `seamstress-cli-arguments' for presence of -s falg

        ((s-ends-with? ".lua" (buffer-file-name))
         (buffer-file-name))

        ((f-exists-p (concat default-directory "script.lua"))
         (concat default-directory "script.lua"))

        ((f-exists-p (concat homedir-truename "/seamstress/script.lua"))
         (concat homedir-truename "/seamstress/script.lua"))

        (t
         nil)))

(defun seamstress--script-path-dir (p)
  (->> p
       (file-name-directory)
       (file-name-split)
       (--remove (string= "" it))
       (last)
       (car)))

(defun seamstress--script-path-file-sans-ext (p)
  (-> p
      (file-name-nondirectory)
      (file-name-sans-extension)))

(defun seamstress--script-path-to-shortname (p)
  (let ((d (seamstress--script-path-dir p))
        (f (seamstress--script-path-file-sans-ext p)))
    (if (string= d f)
        d
      (concat d "/" f))))

(defun seamstress--repl-buffer-name ()
  (if (derived-mode-p 'seamstress-repl-mode)
      (buffer-name)
    (if-let ((script-path (seamstress--script-path)))
        (concat "*seamstress/"
                (seamstress--script-path-to-shortname script-path) "*")
      seamstress-buffer-name)))



;; COMMANDS - INPUT

(defun seamstress-send (cmd)
  "Send CMD to seamstress and eventually pop a window to the REPL buffer."
  (interactive "s> ")
  (let* ((frame (selected-frame))
         (win (selected-window))
         (comint-buff (seamstress--repl-buffer-name))
         (visiting-windows (get-buffer-window-list comint-buff)))

    ;; send command
    ;; REVIEW: replace w/ `with-current-buffer'!
    (save-excursion
      (set-buffer comint-buff)
      (let ((proc (get-buffer-process (current-buffer))))
        (comint-simple-send proc cmd)))

    ;; focus on REPL
    (when (and seamstress-repl-switch-on-cmd
               (null visiting-windows))
      (apply seamstress-repl-switch-fn (list comint-buff))
      (goto-char (point-max))
      (when seamstress-repl-switch-no-focus
        (set-frame-selected-window frame win)))))

(defun seamstress-send-selection ()
  "Send selected buffer region to seamstress REPL."
  (interactive)
(cond
   ((use-region-p)
    (seamstress-send (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark))

   (:default (message "no selection"))))

(defun seamstress--tmp-dir ()
  (--> (temporary-file-directory)
       (if (s-ends-with? "/" it) it (concat it "/"))))

(defun seamstress--inspect-lib-path ()
  (concat (seamstress--tmp-dir) "inspect.lua"))

(defun seamstress--download-inspect-lib ()
  "Download Lua inspect lib."
  (let ((dest-file (seamstress--inspect-lib-path)))
    (unless (file-exists-p dest-file)
      (url-copy-file seamstress-lua-lib-inspect-url dest-file))))

(defun seamstress-maiden-inspect-symbol (symbol)
  "Inspect value of SYMBOL at point in seamstress REPL.
If no symbol at point, prompt.

Please note that it will only work properly for non-local lua vars."
  (interactive (list
                (if (use-region-p)
                    (let ((selection (buffer-substring (region-beginning) (region-end))))
                      (read-string (format "var (%s): " selection)
                                   nil nil selection))
                  (let ((tap (thing-at-point 'symbol)))
                    (if tap
                        (read-string (format "var (%s): " tap)
                                     nil nil tap)
                      (read-string "var: "))))))
  (seamstress--download-inspect-lib)
  (seamstress-send (s-join "; " (list (concat "local inspect = dofile '" (seamstress--inspect-lib-path) "'")
                                      (concat "print(inspect(" symbol "))")))))



;; COMMANDS - SCRIPT LOAD

(defun seamstress-reset-lvm ()
  (interactive)
  (seamstress-send "_seamstress.reset_lvm()"))

(defun seamstress-run ()
  "Run an inferior instance of `seamstress' inside Emacs."
  (interactive)
  (let* ((seamstress-program seamstress-file-path)
         (buffer (get-buffer-create (seamstress--repl-buffer-name)))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer))
         (og-seamstress-repl-script-path seamstress-repl-script-path))
    ;; if the process is dead then re-create the process and reset the
    ;; mode.
    (if proc-alive
        (seamstress-reset-lvm)
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "seamstress" buffer
               seamstress-program nil seamstress-cli-arguments)
        (when seamstress-repl-script-path
          (message (concat "SCRIPT: " seamstress-repl-script-path))
          (set (make-local-variable 'seamstress-repl-script-path) og-seamstress-repl-script-path))
        (seamstress-repl-mode)))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))

(defun seamstress-run-current-script ()
  (interactive)
  (let* ((script-path (buffer-file-name))
         (seamstress-repl-script-path script-path)
         (default-directory (file-name-directory script-path))
         (seamstress-cli-arguments `("-s" ,(file-name-sans-extension (file-name-nondirectory script-path)))))
    (seamstress-run)))



;; MAJOR MODE - REPL

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



;; MINOR MODE

(define-minor-mode seamstress-mode
  "Additional shortcuts & font lock for seamstress lua sources."
  :lighter seamstress-mode-lighter
  :keymap (let ((mmap (make-sparse-keymap)))
            (define-key mmap (kbd "C-c ! r") #'seamstress-send-selection)
            (define-key mmap (kbd "C-c ! c") #'seamstress-send)
            (define-key mmap (kbd "C-c ! R") #'seamstress-run-current-script)
            mmap)

  (when (string= "lua-mode" major-mode)
    ;; (font-lock-add-keywords nil (norns--lua-make-font-lock-keywords))
    )

  )




(provide 'seamstress)
