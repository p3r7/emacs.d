

;; IDEA is to have something like ediff buffer to enter multi-buffer editing mode


;; https://emacs.stackexchange.com/questions/44093/edit-simultaneously-many-buffers-and-save-all-of-them-after-edit
;; https://nicolas.petton.fr/blog/mutli-occur-on-projects.html

;; https://github.com/magnars/multifiles.el

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Two_002dColumn.html
;; https://emacs.stackexchange.com/questions/47200/copy-paste-text-among-split-window-buffers

;; catch all keys function mode map
;; https://stackoverflow.com/a/13215367


;; https://emacs.stackexchange.com/a/21478

;; (current-buffer)
;; (set-window-point (get-buffer-window "init.el") (point-max))
;; (with-current-buffer "init.el"
;;   (goto-char 42)
;;   (point))

;; TODO: get inspired by `mc/prompt-for-inclusion-in-whitelist', `mc/execute-this-command-for-all-cursors-1'

;; (progn
;;   (message "this-command-keys=%S" (this-command-keys))
;;   (message "this-single-command-keys=%S" (this-single-command-keys))
;;   (message "this-command-keys-vector=%S" (this-command-keys-vector))
;;   (message "this-single-command-raw-keys=%S" (this-single-command-raw-keys))
;;   (message "this-command-keys-shift-translated=%S" this-command-keys-shift-translated))



;; TEST

(defun multi-buffer/test ()
  (let ((buffs (-take 4
                      (--filter (and (not (-some
                                           (lambda (re)
                                             (s-matches? re (buffer-name it)))
                                           '("\\*Minibuf-" "\\*Helm" "\\*Lusty" "\\*Multi Buffer"))))
                                (buffer-list))
                      )))
    (prf-multi-buffer-mode--display buffs)
    )
  )



;; DEPS

(require 'dash)
(require 'buffer-grid)



;; CORE HELPERS - STRINGS

;; Plagiarized from `emerge-defvar-local' / `ediff-defvar-local'.
(defmacro multi-buffer-defvar-local (symbol value &optional doc)
  "Define SYMBOL as an advertised buffer-local variable.
Run `defvar-local', setting the value of the variable to VALUE
and its docstring to DOC.

Then set the `permanent-local' property, so that
`kill-all-local-variables' (called by major-mode setting
commands) won't destroy Ediff control variables."
  (declare (indent defun) (doc-string 3))
  `(progn
     (defvar-local ,symbol ,value ,doc)
     (put ',symbol 'permanent-local t)))



;; CORE HELPERS - STRINGS

;; Plagiarized from `ediff-whitespace'.
;; \240 is Unicode symbol for nonbreakable whitespace
(defvar-local multi-buffer-whitespace " \n\t\f\r\240"
  "Characters constituting white space.
These characters are ignored when differing regions are split into words.")



;; CORE HELPERS - BUFFERS

;; Plagiarized from `ediff-buffer-live-p'.
(defsubst multi-buffer-buffer-live-p (buf)
  (and buf (get-buffer buf) (buffer-name (get-buffer buf))))

;; Plagiarized from `ediff-with-current-buffer'.
;; Macro to switch to BUFFER, evaluate BODY, returns to original buffer.
;; Doesn't save the point and mark.
;; This is `with-current-buffer' with the added test for live buffers."
(defmacro multi-buffer-with-current-buffer (buffer &rest body)
  "Evaluate BODY in BUFFER."
  (declare (indent 1) (debug (form body)))
  `(if (multi-buffer-buffer-live-p ,buffer)
       (save-current-buffer
	 (set-buffer ,buffer)
	 ,@body)
     (or (eq this-command 'ediff-quit)
	 (error ediff-KILLED-VITAL-BUFFER))
     ))

;; Plagiarized from `ediff-unique-buffer-name'.
;; Construct a unique buffer name.
;; The first one tried is prefixsuffix, then prefix<2>suffix,
;; prefix<3>suffix, etc.
(defun multi-buffer-unique-buffer-name (prefix suffix)
  (if (null (get-buffer (concat prefix suffix)))
      (concat prefix suffix)
    (let ((n 2))
      (while (get-buffer (format "%s<%d>%s" prefix n suffix))
	(setq n (1+ n)))
      (format "%s<%d>%s" prefix n suffix))))

;; Plagiarized from `ediff-select-lowest-window'.
;; Select the lowest window on the frame.
(defun multi-buffer-select-lowest-window ()
  (let* ((lowest-window (selected-window))
	 (bottom-edge (car (cdr (cdr (cdr (window-edges))))))
	 (last-window (save-excursion
			(other-window -1) (selected-window)))
	 (window-search t))
    (while window-search
      (let* ((this-window (next-window))
	     (next-bottom-edge
	      (car (cdr (cdr (cdr (window-edges this-window)))))))
	(if (< bottom-edge next-bottom-edge)
	    (setq bottom-edge next-bottom-edge
		  lowest-window this-window))
	(select-window this-window)
	(when (eq last-window this-window)
	  (select-window lowest-window)
	  (setq window-search nil))))))



;; MAJOR MODE

(multi-buffer-defvar-local prf-multi-buffer-mode--buffer-list nil
  "List of buffers handled by current multi-buffer session")

(defun prf-multi-buffer-mode--catch-all-key-fun ()
  (interactive)
  (let (key)

    (setq key
          (cond
           ((symbolp last-input-event) last-input-event)
           (t (key-binding (vector last-input-event)))))

    (message "Caught %S" key)
    (prf-multi-buffer-edit--play-key-multi prf-multi-buffer-mode--buffer-list key)))

(define-derived-mode prf-multi-buffer-mode fundamental-mode "prf-multi-buffer"
  "major mode for editing multiple buffers at once."
  :keymap (let ((mmap (make-sparse-keymap)))

            ;; chars
            (define-key mmap [remap self-insert-command] #'prf-multi-buffer-mode--self-insert-command)

            ;; cursor
            (define-key mmap [remap left-char] (prf-multi-buffer-edit--make-command-all #'left-char))
            (define-key mmap [remap right-char] (prf-multi-buffer-edit--make-command-all #'right-char))
            (define-key mmap [remap previous-line] (prf-multi-buffer-edit--make-command-all #'previous-line))
            (define-key mmap [remap next-line] (prf-multi-buffer-edit--make-command-all #'next-line))
            (define-key mmap [remap forward-line] (prf-multi-buffer-edit--make-command-all #'forward-line))
            (define-key mmap [remap backward-sentence] (prf-multi-buffer-edit--make-command-all #'backward-sentence))
            (define-key mmap [remap forward-sentence] (prf-multi-buffer-edit--make-command-all #'forward-sentence))
            (define-key mmap [remap beginning-of-buffer] (prf-multi-buffer-edit--make-command-all #'beginning-of-buffer))
            (define-key mmap [remap end-of-buffer] (prf-multi-buffer-edit--make-command-all #'end-of-buffer))

            ;; debugging
            (define-key mmap (kbd "C-h k") #'helpful-key)
            (define-key mmap (kbd "C-h v") #'helpful-variable)
            (define-key mmap (kbd "C-h o") #'helpful-at-point)

            ;; (define-key mmap (kbd "<left>") (prf-multi-buffer-edit--make-command-all #'left-char))
            ;; (define-key mmap (kbd "<right>") (prf-multi-buffer-edit--make-command-all #'right-char))
            ;; (define-key mmap [remap back-to-indentation] (prf-multi-buffer-edit--make-command-all #'back-to-indentation))
            ;; (define-key mmap [t] #'prf-multi-buffer-mode--catch-all-key-fun)
            ))




;; DISPLAYING

;; NB: could also depend on size of the display
(defvar prf-multi-buffer--max-columns 4)

(defun prf-multi-buffer-mode--display (buf-list &optional split-window-function)
  "Launch multi-buffer windows view.
Inspired by `ediff-setup' (called by `ediff-files-internal')
+ `ediff-setup-windows-plain' -> `ediff-setup-windows-plain-compare'"

  (unless split-window-function
    (setq split-window-function #'split-window-horizontally))

  (let* ((control-buffer-name
          (multi-buffer-unique-buffer-name "*Multi Buffer" "*"))
         (control-buffer (multi-buffer-with-current-buffer (car buf-list)
                           (get-buffer-create control-buffer-name)))
         (i 0)
         wind-A
         wind-width wind-height
         multi-buffer-with-current-buffer)
    (multi-buffer-with-current-buffer control-buffer
      (prf-multi-buffer-mode)

      (make-local-variable 'window-min-height)
      (setq window-min-height 2))

    ;; if in minibuffer go somewhere else
    (if (save-match-data
	  (string-match "\\*Minibuf-" (buffer-name (window-buffer))))
	(select-window (next-window nil 'ignore-minibuf)))
    (delete-other-windows)
    (set-window-dedicated-p (selected-window) nil)
    (split-window-vertically)
    (multi-buffer-select-lowest-window)

    ;; from `ediff-setup-control-buffer'
    (if (window-dedicated-p)
        (set-buffer control-buffer) ; we are in control frame but just in case
      (switch-to-buffer control-buffer))
    (let ((window-min-height 2))
      (insert "AAAAAA")
      (setq prf-multi-buffer-mode--buffer-list buf-list)
      (shrink-window-if-larger-than-buffer)
      (set-buffer-modified-p nil)
      (set-window-dedicated-p (selected-window) t)
      (goto-char (point-min))
      (skip-chars-forward multi-buffer-whitespace))

    (other-window 1)

    (buffer-grid-diplay buf-list nil prf-multi-buffer--max-columns)

    (multi-buffer-select-lowest-window)))



;; FINDING KEY BINDING FOR BUFFER

;; TODO: see if `edmacro-format-keys' is not a better implementation
;; TODO: read about read-key vs
(defun prf-multi-buffer-edit--get-assoc-key-fun (buffer &optional key-sequence)
  "Inspired by `helpful-key'"
  (unless key-sequence
    ;; NB: read-key-sequence-vector might be better to work with
    (setq key-sequence (read-key-sequence "Press key: ")))
  (with-current-buffer buffer
    (let ((sym (key-binding key-sequence)))
      (when (commandp sym)
        sym))))

(defun prf-multi-buffer-edit--play-key (buffer &optional key-sequence)
  (unless key-sequence
    ;; NB: read-key-sequence-vector might be better to work with
    (setq key-sequence (read-key-sequence "Press key: ")))
  (with-current-buffer buffer
    (let ((cmd (prf-multi-buffer-edit--get-assoc-key-fun buffer key-sequence)))
      (when cmd
        (call-interactively cmd)))))

;; TODO: should first retrieve all commands, validate the execution, then play
;; indeed, some commands should be deduped, and prompted in some cases (e.g. `shell-command')
(defun prf-multi-buffer-edit--play-key-multi (buffer-list &optional key-sequence)
  (unless key-sequence
    ;; NB: read-key-sequence-vector might be better to work with
    (setq key-sequence (read-key-sequence "Press key: ")))
  (let ((predicate (lambda (x) (prf-multi-buffer-edit--play-key x key-sequence))))
    (mapc predicate buffer-list)))



;; INTERRACTION FUNCS

(defun prf-multi-buffer-mode--self-insert-command ()
  (interactive)
  (let ((key (this-command-keys)))
    (prf-multi-buffer-edit--insert-all prf-multi-buffer-mode--buffer-list key)))

(defun prf-multi-buffer-edit--insert (buffer text)
  (with-current-buffer buffer
    (insert text)))

(defun prf-multi-buffer-edit--insert-all (buffer-list text)
  (let ((predicate (lambda (x) (prf-multi-buffer-edit--insert x text))))
    (mapc predicate buffer-list)))

(defun prf-multi-buffer-edit--exec-command-all (cmd)
  (-map
   (lambda (buff)
     (when-let* ((_ (buffer-live-p buff))
                 (buff-wins (get-buffer-window-list buff)))
       (-map
        (lambda (win)
          (with-selected-window win
            (call-interactively cmd)))
        buff-wins)))
   prf-multi-buffer-mode--buffer-list))

(defun prf-multi-buffer-edit--make-command-all (cmd)
  `(lambda ()
     (interactive)
     (prf-multi-buffer-edit--exec-command-all #',cmd)))



;; INTERRACTION FUNCS - SPECIFICS (TO DELETE)

(defun prf-multi-buffer-edit--forward-line-all ()
  (interactive)
  (--map (prf-multi-buffer-edit--forward-line it) prf-multi-buffer-mode--buffer-list))

(defun prf-multi-buffer-edit--forward-line (buffer &optional n)
  (with-current-buffer buffer
    (forward-line n)))

(defun prf-multi-buffer-edit--next-line (buffer &optional arg try-vscroll)
  (with-current-buffer buffer
    (next-line &optional arg try-vscroll)))




(provide 'prf-multi-buffer-edit)
