;;; lusty-explorer.el --- Dynamic filesystem explorer and buffer switcher -*- lexical-binding: t; -*-
;;
;; PATCHED VERSION, search ###PRF
;; added following additional actions for `lusty-file-explorer': `lusty-launch-shell', `lusty-shell-command', `lusty-async-shell-command', `lusty-M-x'

;;
;; Copyright (C) 2008 Stephen Bach <http://items.sjbach.com/about>
;;
;; Version: 20130407.1256
;; X-Original-Version: 2.5
;; Created: July 27, 2010
;; Keywords: convenience, files, matching
;; Compatibility: GNU Emacs 22, 23, and 24
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; To install, copy this file somewhere in your load-path and add this line to
;; your .emacs:
;;
;;    (require 'lusty-explorer)
;;
;; To launch the explorer, run or bind the following commands:
;;
;;    M-x lusty-file-explorer
;;    M-x lusty-buffer-explorer
;;
;; And then use as you would `find-file' or `switch-to-buffer'. A split window
;; shows the *Lusty-Matches* buffer, which updates dynamically as you type
;; using a fuzzy matching algorithm.  One match is highlighted; you can move
;; the highlight using C-n / C-p (next, previous) and C-f / C-b (next column,
;; previous column).  Pressing TAB or RET will select the highlighted match
;; (with slightly different semantics).
;;
;; To create a new buffer with the given name, press C-x e.  To open dired at
;; the current viewed directory, press C-x d.
;;
;; Note: lusty-explorer.el benefits greatly from byte-compilation.  To byte-
;; compile this library:
;;
;;    $ emacs -Q -batch -f batch-byte-compile lusty-explorer.el
;;
;; (You can also do this from within Emacs, but it's best done in a clean
;; session.)  Then, restart Emacs or M-x load-library and choose the newly
;; generated lusty-explorer.elc file.
;;
;;; Customization:
;;  --------------
;;
;; To modify the keybindings, use something like:
;;
;;   (add-hook 'lusty-setup-hook 'my-lusty-hook)
;;   (defun my-lusty-hook ()
;;     (define-key lusty-mode-map "\C-j" 'lusty-highlight-next))
;;
;; Respects these variables:
;;   completion-ignored-extensions
;;
;; Development:    <http://github.com/sjbach/lusty-emacs>
;; Further info:   <http://www.emacswiki.org/cgi-bin/wiki/LustyExplorer>
;;                 (Probably out-of-date)
;;

;;; Contributors:
;;
;; Tassilo Horn
;; Jan Rehders
;; Hugo Schmitt
;; Volkan Yazici
;; René Kyllingstad
;; Alex Schroeder
;; Sasha Kovar
;; John Wiegley
;; Johan Walles
;; p3r7
;; Nick Alcock
;; Jonas Bernoulli
;;

;;; Code:

;; TODO:
;; - highlight opened buffers in filesystem explorer
;; - FIX: deal with permission-denied
;; - C-e/C-a -> last/first column?
;; - C-f/C-b -> next/previous column?
;; - config var: C-x d opens highlighted dir instead of current dir

;; Used for many functions and macros.
(require 'cl-lib)

;; Used only for its faces (for color-theme).
(require 'dired)
;; Backward compatibility: use noflet if present, fallback to (deprecated since 24.3) flet otherwise
(defalias 'lusty--flet 'flet)
(when (require 'noflet nil 'noerror)
  (defalias 'lusty--flet 'noflet))

(require 's)

(cl-declaim (optimize (speed 3) (safety 0)))

(defgroup lusty-explorer nil
  "Quickly open new files or switch among open buffers."
  :group 'extensions
  :group 'convenience
  :version "23")

(defcustom lusty-setup-hook nil
  "Hook run after the lusty keymap has been setup.
Additional keys can be defined in `lusty-mode-map'."
  :type 'hook
  :group 'lusty-explorer)

(defcustom lusty-idle-seconds-per-refresh 0.05
  "Seconds to wait for additional input before updating matches window.
Can be floating point; 0.05 = 50 milliseconds.  Set to 0 to disable.
Note: only affects `lusty-file-explorer'; `lusty-buffer-explorer' is
always immediate."
  :type 'number
  :group 'lusty-explorer)

(defcustom lusty-buffer-MRU-contribution 0.1
  "How much influence buffer recency-of-use should have on ordering of
buffer names in the matches window; 0.10 = %10."
  :type 'float
  :group 'lusty-explorer)

(defcustom lusty-fully-expand-matches-window-p t
  "Whether or not to expand the matches window across the whole frame."
  :type 'boolean
  :group 'lusty-explorer)

(defcustom lusty-case-fold t
  "Ignore case when matching if non-nil."
  :type 'boolean
  :group 'lusty-explorer)

(defface lusty-match-face
  '((t :inherit highlight))
  "The face used for the current match."
  :group 'lusty-explorer)

(defface lusty-directory-face
  '((t :inherit dired-directory))
  "The face used for directory completions."
  :group 'lusty-explorer)

(defface lusty-slash-face
  '((t :weight bold :foreground "red"))
  "The face used for the slash after directories."
  :group 'lusty-explorer)

(defface lusty-file-face
  nil ;; Use default face...
  "The face used for normal files."
  :group 'lusty-explorer)

(defvar lusty-buffer-name " *Lusty-Matches*")
(defvar lusty-prompt ">> ")
(defvar lusty-column-separator "    ")
(defvar lusty-no-matches-string
  (propertize "-- NO MATCHES --" 'face 'font-lock-warning-face))
(defvar lusty-truncated-string
  (propertize "-- TRUNCATED --" 'face 'font-lock-comment-face))

(defvar lusty-mode-map nil
  "Minibuffer keymap for `lusty-file-explorer' and `lusty-buffer-explorer'.")

(defvar lusty-global-map
  (let ((map (make-sparse-keymap)))
    (dolist (b '((switch-to-buffer . lusty-buffer-explorer)
                 (find-file . lusty-file-explorer)))
      (if (fboundp 'command-remapping)
          (define-key map (vector 'remap (car b)) (cdr b))
        (substitute-key-definition (car b) (cdr b) map global-map)))
    map))

(defvar lusty--active-mode nil)
(defvar lusty--wrapping-ido-p nil)
(defvar lusty--initial-window-config nil)
(defvar lusty--previous-minibuffer-contents nil)
(defvar lusty--current-idle-timer nil)
(when (not (boundp 'lusty--completion-ignored-regexps))
  (defvar lusty--completion-ignored-regexps '()) )
(defvar lusty--ignored-buffer-regex
  (mapconcat 'identity lusty--completion-ignored-regexps "\\|"))

(defvar lusty--ignored-extensions-regex
  ;; Recalculated at execution time.
  (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))

(defvar lusty--highlighted-coords (cons 0 0))  ; (x . y)

;; Set later by lusty--compute-layout-matrix
(defvar lusty--matches-matrix (make-vector 0 nil))
(defvar lusty--matrix-column-widths '())
(defvar lusty--matrix-truncated-p nil)

(defvar lusty--custom-explorer-actions (make-hash-table :test 'equal))
(defvar lusty--custom-explorer-actions-keys (make-hash-table :test 'equal))
(defvar lusty--shell-open-here-fun nil)
(defvar lusty--shell-command-fun #'shell-command)
(defvar lusty--async-shell-command-fun #'async-shell-command)
(defvar lusty--M-x-fun #'execute-extended-command)

(when lusty--wrapping-ido-p
  (require 'ido))
(defvar ido-text) ; silence compiler warning

(defsubst lusty--matrix-empty-p ()
  (zerop (length lusty--matches-matrix)))
(defsubst lusty--matrix-coord-valid-p (x y)
  (not (or (cl-minusp x)
           (cl-minusp y)
           (>= x (length lusty--matches-matrix))
           (>= y (length (aref lusty--matches-matrix 0)))
           (null (aref (aref lusty--matches-matrix x) y)))))

(defun lusty--compute-column-width (start-index end-index lengths-v lengths-h)
  ;; Dynamic programming algorithm. Split the index range into smaller and
  ;; smaller chunks in recursive calls to this function, then calculate and
  ;; memoize the widths from the bottom up. The memoized widths are likely to
  ;; be used again in subsequent calls to this function.
  ;;
  ;; Note: this gets called a lot and would best be speedy.
  (if (= start-index end-index)
      ;; This situation describes a column consisting of a single element.
      (aref lengths-v start-index)
    (let* ((range (cons start-index end-index))
           (width (gethash range lengths-h)))
      (or width
          (let* ((split-point
                  (+ start-index
                     ;; Same thing as: (/ (- end-index start-index) 2)
                     (ash (- end-index start-index) -1)))
                 (width-first-half
                  (lusty--compute-column-width
                   start-index split-point
                   lengths-v lengths-h))
                 (width-second-half
                  (lusty--compute-column-width
                   (1+ split-point) end-index
                   lengths-v lengths-h)))
            (puthash range
                     (max width-first-half width-second-half)
                     lengths-h))))))

(defun lusty--propertize-path (path)
  "Propertize the given PATH like so: <dir></> or <file>.
Uses the faces `lusty-directory-face', `lusty-slash-face', and
`lusty-file-face'."
  (let ((last (1- (length path))))
    ;; Note: shouldn't get an empty path, so for performance
    ;; I'm not going to check for that case.
    (if (eq (aref path last) ?/) ; <-- FIXME nonportable?
        (progn
          ;; Directory
          (put-text-property 0 last 'face 'lusty-directory-face path)
          (put-text-property last (1+ last) 'face 'lusty-slash-face path))
      (put-text-property 0 (1+ last) 'face 'lusty-file-face path)))
  path)


;;;###autoload
(defun lusty-file-explorer ()
  "Launch the file/directory mode of LustyExplorer."
  (interactive)
  (let ((completing-read-function #'completing-read-default)
        (lusty--active-mode :file-explorer))
    (lusty--define-mode-map)
    (let* ((lusty--ignored-extensions-regex
            (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))
	   (lusty--ignored-buffer-regex
	    (mapconcat 'identity lusty--completion-ignored-regexps "\\|"))
           (minibuffer-local-filename-completion-map lusty-mode-map)
           (file
            ;; read-file-name is silly in that if the result is equal to the
            ;; dir argument, it gets converted to the default-filename
            ;; argument.  Set it explicitly to "" so if lusty-launch-dired is
            ;; called in the directory we start at, the result is that directory
            ;; instead of the name of the current buffer.
            (lusty--run 'read-file-name default-directory ""))
	   (action :file-open))
      (when file
	(when (s-contains? "!!!lusty!!!" file)
	  (pcase-let ((`(,file-tmp ,action-tmp) (s-split "!!!lusty!!!" file)))
	    (setq file file-tmp
		  action (intern (concat ":" action-tmp)))))
        (setq file (expand-file-name file))

        (if (eq action :file-open)
            (switch-to-buffer
             (find-file-noselect
              (expand-file-name file)))
          (let ((default-directory file))
            (call-interactively (gethash action lusty--custom-explorer-actions))))

        ;; (cond
        ;;  ((eq action :file-open)
        ;;   (switch-to-buffer
        ;;    (find-file-noselect
        ;;     (expand-file-name file))))
        ;;  ((eq action :launch-shell)
        ;;   (if lusty--shell-open-here-fun
        ;;       (progn
        ;; 	(cd (expand-file-name file))
        ;; 	(call-interactively lusty--shell-open-here-fun))
        ;;     (message "No `lusty--shell-open-here-fun' defined")))
        ;;  ((eq action :shell-command)
        ;;   (if lusty--shell-command-fun
        ;;       (progn
        ;; 	(cd (expand-file-name file))
        ;; 	(call-interactively lusty--shell-command-fun))
        ;;     (message "No `lusty--shell-command-fun' defined")))
        ;;  ((eq action :async-shell-command)
        ;;   (if lusty--async-shell-command-fun
        ;;       (progn
        ;; 	(cd (expand-file-name file))
        ;; 	(call-interactively lusty--async-shell-command-fun))
        ;;     (message "No `lusty--async-shell-command-fun' defined")))
        ;;  ((eq action :M-x)
        ;;   (if lusty--M-x-fun
        ;;       (progn
        ;; 	(cd (expand-file-name file))
        ;; 	(call-interactively lusty--M-x-fun))
        ;;     (message "No `lusty--M-x-fun' defined")))
        ;;  (t (message "unsupported action")))
        ))))

;;;###autoload
(defun lusty-buffer-explorer ()
  "Launch the buffer mode of LustyExplorer."
  (interactive)
  (let ((completing-read-function #'completing-read-default)
        (lusty--active-mode :buffer-explorer))
    (lusty--define-mode-map)
    (let* ((minibuffer-local-completion-map lusty-mode-map)
           (buffer (lusty--run 'read-buffer)))
      (when buffer
        (switch-to-buffer buffer)))))

;;;###autoload
(define-minor-mode lusty-explorer-mode
  "Toggle Lusty Explorer mode.
With a prefix argument ARG, enable Lusty Explorer mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Lusty Explorer mode is a global minor mode that enables switching
between buffers and finding files using substrings, fuzzy matching,
and recency information."
  nil nil lusty-global-map :global t)

;;;###autoload
(defun lusty-highlight-next ()
  "Highlight the next match in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             (not (lusty--matrix-empty-p)))
    (cl-destructuring-bind (x . y) lusty--highlighted-coords

      ;; Unhighlight previous highlight.
      (let ((prev-highlight
             (aref (aref lusty--matches-matrix x) y)))
        (lusty--propertize-path prev-highlight))

      ;; Determine the coords of the next highlight.
      (cl-incf y)
      (unless (lusty--matrix-coord-valid-p x y)
        (cl-incf x)
        (setq y 0)
        (unless (lusty--matrix-coord-valid-p x y)
          (setq x 0)))

      ;; Refresh with new highlight.
      (setq lusty--highlighted-coords (cons x y))
      (lusty-refresh-matches-buffer :use-previous-matrix))))

;;;###autoload
(defun lusty-highlight-previous ()
  "Highlight the previous match in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             (not (lusty--matrix-empty-p)))
    (cl-destructuring-bind (x . y) lusty--highlighted-coords

      ;; Unhighlight previous highlight.
      (let ((prev-highlight
             (aref (aref lusty--matches-matrix x) y)))
        (lusty--propertize-path prev-highlight))

      ;; Determine the coords of the next highlight.
      (cl-decf y)
      (unless (lusty--matrix-coord-valid-p x y)
        (let ((n-cols (length lusty--matches-matrix))
              (n-rows (length (aref lusty--matches-matrix 0))))
          (cl-decf x)
          (setq y (1- n-rows))
          (unless (lusty--matrix-coord-valid-p x y)
            (setq x (1- n-cols))
            (while (not (lusty--matrix-coord-valid-p x y))
              (cl-decf y)))))

      ;; Refresh with new highlight.
      (setq lusty--highlighted-coords (cons x y))
      (lusty-refresh-matches-buffer :use-previous-matrix))))

;;;###autoload
(defun lusty-highlight-next-column ()
  "Highlight the next column in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             (not (lusty--matrix-empty-p)))
    (cl-destructuring-bind (x . y) lusty--highlighted-coords

      ;; Unhighlight previous highlight.
      (let ((prev-highlight
             (aref (aref lusty--matches-matrix x) y)))
        (lusty--propertize-path prev-highlight))

      ;; Determine the coords of the next highlight.
      (cl-incf x)
      (unless (lusty--matrix-coord-valid-p x y)
        (setq x 0)
        (cl-incf y)
        (unless (lusty--matrix-coord-valid-p x y)
          (setq y 0)))

      ;; Refresh with new highlight.
      (setq lusty--highlighted-coords (cons x y))
      (lusty-refresh-matches-buffer :use-previous-matrix))))

;;;###autoload
(defun lusty-highlight-previous-column ()
  "Highlight the previous column in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             (not (lusty--matrix-empty-p)))
    (cl-destructuring-bind (x . y) lusty--highlighted-coords

      ;; Unhighlight previous highlight.
      (let ((prev-highlight
             (aref (aref lusty--matches-matrix x) y)))
        (lusty--propertize-path prev-highlight))

      ;; Determine the coords of the next highlight.
      (let ((n-cols (length lusty--matches-matrix))
            (n-rows (length (aref lusty--matches-matrix 0))))
        (if (and (zerop x)
                 (zerop y))
            (progn
              (setq x (1- n-cols)
                    y (1- n-rows))
              (while (not (lusty--matrix-coord-valid-p x y))
                (cl-decf y)))
          (cl-decf x)
          (unless (lusty--matrix-coord-valid-p x y)
            (setq x (1- n-cols))
            (cl-decf y)
            (unless (lusty--matrix-coord-valid-p x y)
              (while (not (lusty--matrix-coord-valid-p x y))
                (cl-decf x))))))

      ;; Refresh with new highlight.
      (setq lusty--highlighted-coords (cons x y))
      (lusty-refresh-matches-buffer :use-previous-matrix))))

;;;###autoload
(defun lusty-open-this ()
  "Open the given file/directory/buffer, creating it if not already present."
  (interactive)
  (when lusty--active-mode
    (if (lusty--matrix-empty-p)
        ;; No matches - open a new buffer/file with the current name.
        (lusty-select-current-name)
      (cl-ecase lusty--active-mode
        (:file-explorer
         (let* ((path (minibuffer-contents-no-properties))
                (last-char (aref path (1- (length path)))))
           (if (and (file-directory-p path)
                   (not (eq last-char ?/))) ; <-- FIXME nonportable?
               ;; Current path is a directory, sans-slash.  Open in dired.
               (lusty-select-current-name)
             ;; Just activate the current match as normal.
             (lusty-select-match))))
        (:buffer-explorer (lusty-select-match))))))

;;;###autoload
(defun lusty-select-match ()
  "Activate the highlighted match in *Lusty-Matches* - recurse if dir, open if file/buffer."
  (interactive)
  (cl-destructuring-bind (x . y) lusty--highlighted-coords
    (when (and lusty--active-mode
               (not (lusty--matrix-empty-p)))
      (cl-assert (lusty--matrix-coord-valid-p x y))
      (let ((selected-match
             (aref (aref lusty--matches-matrix x) y)))
        (cl-ecase lusty--active-mode
          (:file-explorer (lusty--file-explorer-select selected-match))
          (:buffer-explorer (lusty--buffer-explorer-select selected-match)))))))

;;;###autoload
(defun lusty-yank (arg)
  "Special yank that handles nicely case when current path \"/\" and pasted path starts w/ a leading \"/\" as well.
Inspired by `lispy-yank'"
  (interactive "P")
  (setq this-command 'yank)
  (unless arg
    (setq arg 0))
  (let ((text (s-trim (current-kill arg))))
    (cond
     ((and (region-active-p)
           (bound-and-true-p delete-selection-mode))
      (delete-region (region-beginning) (region-end))
      (insert-for-yank text))
     ((and (eq (char-before) ?/)
           (eq (char-before (- (point) 1)) ?:)
           (s-starts-with? "/" text))
      (insert-for-yank (replace-regexp-in-string "^/" ""
                                                 text)))
     (t
      (push-mark (point))
      (insert-for-yank text)))))

;;;###autoload
(defun lusty-select-current-name ()
  "Open the given file/buffer or create a new buffer with the current name."
  (interactive)
  (when lusty--active-mode
    (exit-minibuffer)))

;;;###autoload
(defun lusty-launch-dired ()
  "Launch dired at the current directory."
  (interactive)
  (when (eq lusty--active-mode :file-explorer)
    (let* ((path (minibuffer-contents-no-properties))
           (dir (lusty-normalize-dir (file-name-directory path))))
      (lusty-set-minibuffer-text dir)
      (exit-minibuffer))))

;; ###PRF
;;;###autoload
(defun lusty-custom-explorer-action (action)
  "Launch shell at the current directory."
  (interactive)
  (when (eq lusty--active-mode :file-explorer)
    (let* ((path (minibuffer-contents-no-properties))
           (dir (lusty-normalize-dir (file-name-directory path))))
      (lusty-set-minibuffer-text (concat dir "!!!lusty!!!" action))
      (exit-minibuffer))))

(defun lusty-custom-explorer-action-fun-name (keyword)
  (concat "lusty-custom-explorer-action-" keyword))

(defun lusty-register-custom-explorer-action-defun (keyword)
  (when (symbolp keyword)
    (setq keyword (symbol-value keyword)))

  (let* ((fun-name (lusty-custom-explorer-action-fun-name keyword))
         (fun-symbol (intern fun-name)))
    (eval `(defun ,fun-symbol ()
             (interactive)
             (lusty-custom-explorer-action
              ,keyword)))))

(defun lusty-register-custom-explorer-action (keyword action key)
  (lusty-register-custom-explorer-action-defun keyword)

  (puthash (intern (concat ":" keyword)) action lusty--custom-explorer-actions)

  (let* ((fun-name (lusty-custom-explorer-action-fun-name keyword))
         (fun-symbol (intern fun-name)))
    (puthash key fun-symbol lusty--custom-explorer-actions-keys)))

(defun lusty-sort-by-fuzzy-score (strings abbrev)
  ;; TODO: case-sensitive when abbrev contains capital letter
  (let* ((strings+scores
          (cl-loop for str in strings
                   for score = (LM-score str abbrev)
                   unless (zerop score)
                   collect (cons str score)))
         (sorted
          (cl-sort strings+scores '> :key 'cdr)))
    (mapcar 'car sorted)))

(defun lusty-normalize-dir (dir)
  "Clean up the given directory path."
  (if (and dir (cl-plusp (length dir)))
      (setq dir (abbreviate-file-name
                 (expand-file-name
                  (substitute-in-file-name dir))))
    (setq dir "."))
  (and (file-directory-p dir)
       dir))

(defun lusty-complete-env-variable (path)
  "Look for an environment variable in PATH and try to complete it as
much as possible."
  (when (string-match "\$\\([[:word:]_]+\\)" path)
    (let* ((partial-var (match-string 1 path))
           (vars (mapcar (lambda (x)
                           (string-match "^[^=]+" x)
                           (match-string 0 x))
                         (cl-remove-if-not
                          (lambda (x)
                            (string-match (concat "^" partial-var) x))
                          process-environment)))
           (longest-completion (try-completion partial-var vars)))
      (cond ((eq t longest-completion) nil)
            ((null longest-completion) nil)
            ((> (length longest-completion) (length partial-var))
             (replace-regexp-in-string (concat "\$" partial-var)
                                       (concat "\$" longest-completion)
                                       path t t))))))

(defun lusty-filter-buffers (buffers)
  "Return BUFFERS converted to strings with hidden buffers removed."
  (cl-macrolet ((ephemeral-p (name)
                             `(eq (string-to-char ,name) ?\ ))
                (ignored-p (name)
                           `(string-match lusty--ignored-buffer-regex ,name)))
    (cl-loop for buffer in buffers
            for name = (buffer-name buffer)
            unless (or (ephemeral-p name)
                       (ignored-p name))
            collect (copy-sequence name))))

;; Written kind-of silly for performance.
(defun lusty-filter-files (file-portion files)
  "Return FILES with './' removed and hidden files if FILE-PORTION
does not begin with '.'."
  (cl-macrolet ((leading-dot-p (str)
                               `(eq (string-to-char ,str) ?.))
                (pwd-p (str)
                       `(string= ,str "./"))
                (ignored-p (name)
                           `(string-match lusty--ignored-extensions-regex ,name)))
    (let ((filtered-files '()))
      (if (leading-dot-p file-portion)
          (dolist (file files)
            (unless (or (pwd-p file)
                        (ignored-p file))
              (push file filtered-files)))
        (dolist (file files)
          (unless (or (leading-dot-p file)
                      (ignored-p file))
            (push file filtered-files))))
      (nreverse filtered-files))))

(defun lusty-set-minibuffer-text (&rest args)
  "Sets ARGS into the minibuffer after the prompt."
  (cl-assert (minibufferp))
  (delete-region (minibuffer-prompt-end) (point-max))
  (apply #'insert args))

(defun lusty--file-explorer-select (match)
  (let* ((path (minibuffer-contents-no-properties))
         (var-completed-path (lusty-complete-env-variable path)))
    (if var-completed-path
        ;; We've completed a variable name (at least partially) -- set it and
        ;; leave, since it's confusing to do two kinds of completion at once.
        (lusty-set-minibuffer-text var-completed-path)
      (let* ((dir (file-name-directory path))
             (normalized-dir (lusty-normalize-dir dir)))
        ;; Clean up the path when selecting, in case we recurse.
        (remove-text-properties 0 (length match) '(face) match)
        (lusty-set-minibuffer-text normalized-dir match)
        (if (file-directory-p (concat normalized-dir match))
            (progn
              (setq lusty--highlighted-coords (cons 0 0))
              (lusty-refresh-matches-buffer))
          (minibuffer-complete-and-exit))))))

(defun lusty--buffer-explorer-select (match)
  (lusty-set-minibuffer-text match)
  (minibuffer-complete-and-exit))

;; This may seem overkill, but it's the only way I've found to update the
;; matches list for every edit to the minibuffer.  Wrapping the keymap can't
;; account for user bindings or commands and would also fail for viper.
(defun lusty--post-command-function ()
  (cl-assert lusty--active-mode)
  (when (and (minibufferp)
             (or (null lusty--previous-minibuffer-contents)
                 (not (string= lusty--previous-minibuffer-contents
                               (minibuffer-contents-no-properties)))))

    (let ((startup-p (null lusty--initial-window-config)))

      (when startup-p
        (lusty--setup-matches-window))

      (setq lusty--previous-minibuffer-contents
            (minibuffer-contents-no-properties))
      (setq lusty--highlighted-coords
            (cons 0 0))

      ;; Refresh matches.
      (if (or startup-p
              (null lusty-idle-seconds-per-refresh)
              (zerop lusty-idle-seconds-per-refresh)
              (eq lusty--active-mode :buffer-explorer))
          ;; No idle timer on first refresh, and never for buffer explorer.
          (lusty-refresh-matches-buffer)
        (when lusty--current-idle-timer
          (cancel-timer lusty--current-idle-timer))
        (setq lusty--current-idle-timer
              (run-with-idle-timer lusty-idle-seconds-per-refresh nil
                                   #'lusty-refresh-matches-buffer))))))

(defun lusty-max-window-height ()
  "Return the expected maximum allowable height of a window on this frame"
  ;; FIXME: are there cases where this is incorrect?
  (let* ((lusty-window
          (get-buffer-window
           (get-buffer-create lusty-buffer-name)))
         (other-window
          ;; In case the *LustyMatches* window was closed
          (or lusty-window
              (if (minibufferp)
                  (next-window (selected-window) :skip-mini)
                (selected-window))))
         (test-window
          (or lusty-window other-window)))
    (cl-assert test-window)
    (- (frame-height)
       ;; Account for modeline and/or header...
       (- (window-height test-window)
          (window-body-height test-window))
       ;; And minibuffer height.
       ;; FIXME: but only if (eq (window-frame (minibuffer-window))
       ;;                        (window-frame test-window)), right?
       (window-height (minibuffer-window)))))

(defun lusty--exploitable-window-body-width ()
  (let* ((window (get-buffer-window
                  (get-buffer-create lusty-buffer-name)))
         (body-width (window-body-width window))
         (window-fringe-absent-p
          (and (equal (window-fringes) '(0 0 nil nil))
               ;; (Probabably these are redundant checks.)
               (eq (fringe-columns 'left) 0)
               (eq (fringe-columns 'right) 0)
               (eq (frame-fringe-width) 0)
               ;; There are also `left-fringe-width`, `right-fringe-width`, but
               ;; I'm not sure about them.
               )))
    ;; Emacs manual for window-body-width: "Note that the returned value
    ;; includes the column reserved for the continuation glyph." So if we're
    ;; configured such that a continuation glyph would show, we need to
    ;; subtract one column for the "true" body width.
    ;;
    ;; Elsewhere in the manual: "[When] fringes are not available, Emacs uses
    ;; the leftmost and rightmost character cells to indicate continuation and
    ;; truncation with special ASCII characters ... .";
    ;;
    ;; So if we have no fringe, we can expect to lose that one column. There
    ;; doesn't appear to be a way to reclaim it. We can possibly change the
    ;; continuation character with `set-display-table-slot`, but not elide the
    ;; character altogether.
    (if window-fringe-absent-p
        (1- body-width)
      body-width)))

;; Only needed for Emacs 23 compatibility, because the Emacs root window in an
;; already split frame is not a living window.
;; TODO: remove code required for Emacs 23 compatibility.
(defun lusty-lowest-window ()
  "Return the lowest window on the frame."
  (cl-flet ((iterate-non-dedicated-window (start-win direction)
                                          ;; Skip dedicated windows when iterating.
                                          (let ((iterating-p t)
                                                (next start-win))
                                            (while iterating-p
                                              (setq next (if (eq direction :forward)
                                                             (next-window next :skip-mini)
                                                           (previous-window next :skip-mini)))
                                              (when (or (not (window-dedicated-p next))
                                                        (eq next start-win))
                                                (setq iterating-p nil)))
                                            next)))
    (let* ((current-window (if (or (minibufferp)
                                   (window-dedicated-p (selected-window)))
                               (iterate-non-dedicated-window (selected-window)
                                                             :forward)
                             (selected-window)))
           (lowest-window current-window)
           (bottom-edge (cl-fourth (window-pixel-edges current-window)))
           (last-window (iterate-non-dedicated-window current-window :backward))
           (window-search-p t))
      (while window-search-p
        (let* ((this-window (iterate-non-dedicated-window current-window
                                                          :forward))
               (next-bottom-edge (cl-fourth (window-pixel-edges this-window))))
          (when (< bottom-edge next-bottom-edge)
            (setq bottom-edge next-bottom-edge)
            (setq lowest-window this-window))
          (setq current-window this-window)
          (when (eq last-window this-window)
            (setq window-search-p nil))))
      lowest-window)))

(defun lusty--setup-window-to-split ()
  ;; Emacs 23 compatibility
  ;; TODO: remove code required for Emacs 23 compatibility.
  (let ((root-window (frame-root-window)))
    (if (window-live-p root-window)
        root-window
      (lusty-lowest-window))))

(defun lusty--setup-matches-window ()
  (let ((lusty-buffer (get-buffer-create lusty-buffer-name)))
    (save-selected-window
      (let* ((window (lusty--setup-window-to-split))
             (lusty-window (condition-case nil (split-window window)
                               (error ; Perhaps it is too small.
                                (delete-window window)
                                (split-window (lusty--setup-window-to-split))))))
        (select-window lusty-window)
        (when lusty-fully-expand-matches-window-p
          ;; Attempt to grow the window to cover the full frame.  Sometimes
          ;; this takes more than one try, but we don't want to accidentally
          ;; loop on it infinitely in the case of some unconventional
          ;; window/frame setup.
          (cl-loop repeat 5
                   while (< (window-width) (frame-width))
                   do
                (condition-case nil
                    (enlarge-window-horizontally (- (frame-width)
                                                    (window-width)))
                  (error
                   (cl-return)))))
        (set-window-buffer lusty-window lusty-buffer))))
  ;; Window configuration may be restored intermittently.
  (setq lusty--initial-window-config (current-window-configuration)))

(defun lusty-refresh-matches-buffer (&optional use-previous-matrix-p)
  "Refresh *Lusty-Matches*."
  (cl-assert (minibufferp))
  (let* ((minibuffer-text (if lusty--wrapping-ido-p
                              ido-text
                            (minibuffer-contents-no-properties))))

    (unless use-previous-matrix-p
      ;; Refresh the matches and layout matrix
      (let ((matches
             (cl-ecase lusty--active-mode
               (:file-explorer
                (lusty-file-explorer-matches minibuffer-text))
               (:buffer-explorer
                (lusty-buffer-explorer-matches minibuffer-text)))))
        (lusty--compute-layout-matrix matches)))

    ;; Update the matches window.
    (let ((lusty-buffer (get-buffer-create lusty-buffer-name)))
      (with-current-buffer lusty-buffer
        (setq buffer-read-only t)
        (let ((buffer-read-only nil))
          (when (and (boundp 'visual-line-mode)
                     visual-line-mode)
            ;; Remove visual-line-mode if it's enabled to make wrapping --
            ;; which we don't want, and which shouldn't happen -- look a
            ;; little better. This is probably not necessary or useful
            ;; given we're setting `truncate-lines` below.
            (visual-line-mode -1))
          (unless truncate-lines
            ;; More gracefully handle any remaining bugs in the layout
            ;; algorithm. If an inserted line of completions happens
            ;; to be longer than the window's text body -- which
            ;; shouldn't happen -- don't wrap the line, just show a
            ;; truncation indicator in the fringe (or, if there's no
            ;; fringe, in the final text column).
            ;;
            ;; The below is most of what `(toggle-truncate-lines 1)` does,
            ;; but without emitting a noisy line to *Messages*.
            (setq truncate-lines t)
            (let ((window (get-buffer-window lusty-buffer)))
              (when window
                (set-window-hscroll window 0))))
          ;; Minor look-and-feel tweaks. We disable these display settings
          ;; in the completions buffer in case the user has enabled them
          ;; globally. Is this overreaching? Maybe, I'm not sure.
          (when indicate-buffer-boundaries
            (setq indicate-buffer-boundaries nil))
          (when show-trailing-whitespace
            (setq show-trailing-whitespace nil))
          ;; (Probably not necessary or useful.)
          (when indicate-empty-lines
            (setq indicate-empty-lines nil))
          ;; There is also `overflow-newline-into-fringe`, which would best
          ;; be t: "If nil, also continue lines which are exactly as wide
          ;; as the window"; but it can't be set buffer-local.
          (with-silent-modifications
            (atomic-change-group
              (erase-buffer)
              (lusty--display-matches)))
          (goto-char (point-min))
          (set-buffer-modified-p nil)))

      ;; If our matches window has somehow become the only window:
      (when (one-window-p t)
        ;; Restore original window configuration before fitting the
        ;; window so the minibuffer won't grow and look silly.
        (set-window-configuration lusty--initial-window-config))
      (fit-window-to-buffer (display-buffer lusty-buffer)))))

(defun lusty-buffer-list ()
  "Return a list of buffers ordered with those currently visible at the end."
  (let ((visible-buffers '()))
    (walk-windows
     (lambda (window)
       ;; Add visible buffers
       (let ((b (window-buffer window)))
         (unless (memq b visible-buffers)
           (push b visible-buffers))))
     nil 'visible)
    (let ((non-visible-buffers
           (cl-loop for b in (buffer-list)
                    unless (memq b visible-buffers)
                    collect b)))
      (nconc non-visible-buffers visible-buffers))))

(defun lusty-buffer-explorer-matches (match-text)
  (let ((buffers (lusty-filter-buffers (lusty-buffer-list))))
    (if (string= match-text "")
        ;; Sort by MRU.
        buffers
      ;; Sort by fuzzy score and MRU order.
      (let* ((score-table
              (cl-loop with MRU-factor-step = (/ lusty-buffer-MRU-contribution
                                                 (length buffers))
                       for b in buffers
                       for step from 0.0 by MRU-factor-step
                       for score = (lusty-LM-score b match-text)
                       for MRU-factor = (- 1.0 step)
                       unless (zerop score)
                       collect (cons b (* score MRU-factor))))
             (sorted
              (cl-sort score-table '> :key 'cdr)))
        (mapcar 'car sorted)))))

;; FIXME: return an array instead of a list?
(defun lusty-file-explorer-matches (path)
  (let* ((dir (lusty-normalize-dir (file-name-directory path)))
         (file-portion (file-name-nondirectory path))
         (files
          (and dir
               ; NOTE: directory-files is quicker but
               ;       doesn't append slash for directories.
               ;(directory-files dir nil nil t)
               (file-name-all-completions "" dir)))
         (filtered (lusty-filter-files file-portion files)))
    (if (or (string= file-portion "")
            (string= file-portion "."))
        (sort filtered 'string<)
      (lusty-sort-by-fuzzy-score filtered file-portion))))

;; Principal goal: fit as many items as possible into as few buffer/window rows
;; as possible. This leads to maximizing the number of columns (approximately).
(defun lusty--compute-layout-matrix (items)
  (let* ((max-visible-rows (1- (lusty-max-window-height)))
         (max-width
          ;; Prior to calling this function we called
          ;; `lusty--setup-matches-window`, which expanded the window for the
          ;; matches buffer horizontally as much as it could. Therefore the
          ;; current width of that window is the maximum width.
          (lusty--exploitable-window-body-width))
         ;; Upper bound of the count of displayable items.
         (upper-bound most-positive-fixnum)  ; (set below)
         (n-items (length items))
         (lengths-v (make-vector n-items 0))
         (separator-length (length lusty-column-separator)))

    (let ((length-of-longest-name 0)) ; used to determine upper-bound

      ;; Initialize lengths-v
      (cl-loop for i from 0
               for item in items
               for len = (length item)
               do
               (aset lengths-v i len)
               (setq length-of-longest-name
                     (max length-of-longest-name len)))

      ;; Calculate an upper-bound.
      (let ((width (+ length-of-longest-name
                      separator-length))
            (columns 1)
            (shortest-first (sort (append lengths-v nil) '<)))
        (cl-dolist (item-len shortest-first)
          (cl-incf width item-len)
          (when (> width max-width)
            (cl-return))
          (cl-incf columns)
          (cl-incf width separator-length))
        (setq upper-bound (* columns max-visible-rows))))

    ;; Determine optimal row count.
    (cl-multiple-value-bind (optimal-n-rows truncated-p)
        (cond ((cl-endp items)
               (cl-values 0 nil))
              ((< upper-bound n-items)
               (cl-values max-visible-rows t))
              ((<= (cl-reduce (lambda (a b) (+ a separator-length b))
                              lengths-v)
                   max-width)
               ;; All fits in a single row.
               (cl-values 1 nil))
              (t
               (lusty--compute-optimal-row-count lengths-v)))
      (let ((n-columns 0)
            (column-widths '()))

        ;; Calculate n-columns and column-widths
        (cl-loop with total-width = 0
                 for start = 0 then end
                 for end = optimal-n-rows then
                 (min (length lengths-v)
                      (+ end optimal-n-rows))
                 while (< start end)
                 for col-width = (cl-reduce 'max lengths-v
                                            :start start
                                            :end end)
                 do
                 (cl-incf total-width col-width)
                 (when (> total-width max-width)
                   (cl-return))
                 (cl-incf n-columns)
                 (push col-width column-widths)
                 (cl-incf total-width separator-length))

        (setq column-widths (nreverse column-widths))

        (when (and (zerop n-columns)
                   (cl-plusp n-items))
          ;; Turns out there's not enough window space to do anything clever,
          ;; so just stack 'em up (and truncate).
          (setq n-columns 1)
          (setq column-widths
                (list
                 (cl-reduce 'max lengths-v
                            :start 0
                            :end (min n-items max-visible-rows)))))

        (let ((matrix
               ;; Create an empty matrix using the calculated dimensions.
               (let ((col-vec (make-vector n-columns nil)))
                 (dotimes (i n-columns)
                   (aset col-vec i
                         (make-vector optimal-n-rows nil)))
                 col-vec)))

          ;; Fill the matrix with propertized match strings.
          (unless (zerop n-columns)
            (let ((x 0)
                  (y 0)
                  (col-vec (aref matrix 0)))
              (cl-dolist (item items)
                (aset col-vec y (lusty--propertize-path item))
                (cl-incf y)
                (when (>= y optimal-n-rows)
                  (cl-incf x)
                  (if (>= x n-columns)
                      (cl-return)
                    (setq col-vec (aref matrix x)))
                  (setq y 0)))))


          (setq lusty--matches-matrix matrix
                lusty--matrix-column-widths column-widths
                lusty--matrix-truncated-p truncated-p))))))

;; Returns number of rows and whether this row count will truncate the matches.
(cl-defun lusty--compute-optimal-row-count (lengths-v)
  ;;
  ;; Binary search; find the lowest number of rows at which we
  ;; can fit all the strings.
  ;;
  (let* ((separator-length (length lusty-column-separator))
         (n-items (length lengths-v))
         (max-visible-rows (1- (lusty-max-window-height)))
         (available-width (lusty--exploitable-window-body-width))
         ;; Holds memoized widths of candidate columns (ranges of items).
         (lengths-h
          ;; Hashes by cons, e.g. (0 . 2), representing the width
          ;; of the column bounded by the range of [0..2].
          (make-hash-table :test 'equal
                           ;; Not scientific; will certainly grow larger for a
                           ;; nontrivial count of items (and so probably should
                           ;; be set higher here).
                           :size n-items))
         ;; We've already failed for a single row, so start at two.
         (lower 1)
         (upper (min (1+ max-visible-rows)
                     n-items)))

    (while (/= (1+ lower) upper)
      (let* ((n-rows (/ (+ lower upper) 2)) ; Mid-point
             (col-start-index 0)
             (col-end-index (1- n-rows))
             (total-width 0))

        (cl-block :column-widths
          (while (< col-end-index n-items)
            (cl-incf total-width
                     (lusty--compute-column-width
                      col-start-index col-end-index
                      lengths-v lengths-h))

            (when (> total-width available-width)
              ;; Early exit; this row count is unworkable.
              (setq total-width most-positive-fixnum)
              (cl-return-from :column-widths))

            (cl-incf total-width separator-length)

            (cl-incf col-start-index n-rows)
            (cl-incf col-end-index n-rows)

            (when (and (>= col-end-index n-items)
                       (< col-start-index n-items))
              ;; Remainder; last iteration will not be a full column.
              (setq col-end-index (1- n-items)))))

        ;; The final column doesn't need a separator.
        (cl-decf total-width separator-length)

        (if (<= total-width available-width)
            ;; This row count fits.
            (setq upper n-rows)
          ;; This row count doesn't fit.
          (setq lower n-rows))))

    (if (> upper max-visible-rows)
        ;; No row count can accomodate all entries; have to truncate.
        ;; (-1 for the truncate indicator)
        (cl-values (1- max-visible-rows) t)
      (cl-values upper nil))))

(cl-defun lusty--display-matches ()

  (when (lusty--matrix-empty-p)
    (lusty--print-no-matches)
    (cl-return-from lusty--display-matches))

  (let* ((n-columns (length lusty--matches-matrix))
         (n-rows (length (aref lusty--matches-matrix 0))))

    ;; Highlight the selected match.
    (cl-destructuring-bind (h-x . h-y) lusty--highlighted-coords
      (setf (aref (aref lusty--matches-matrix h-x) h-y)
            (propertize (aref (aref lusty--matches-matrix h-x) h-y)
                        'face 'lusty-match-face)))

    ;; Print the match matrix.
    (dotimes (y n-rows)
      (cl-loop for column-width in lusty--matrix-column-widths
               for x from 0 upto n-columns
               do
               (let ((match (aref (aref lusty--matches-matrix x) y)))
                 (when match
                   (insert match)
                   (when (< x (1- n-columns))
                     (let* ((spacer
                             (make-string (- column-width (length match))
                                          ?\ )))
                       (insert spacer lusty-column-separator))))))
      (insert "\n")))

  (when lusty--matrix-truncated-p
    (lusty--print-truncated)))

(defun lusty--print-no-matches ()
  (insert lusty-no-matches-string)
  (let ((fill-column (window-width)))
    (center-line)))

(defun lusty--print-truncated ()
  (insert lusty-truncated-string)
  (let ((fill-column (window-width)))
    (center-line)))

(defun lusty-delete-backward (count)
  "Delete char backwards, or at beginning of buffer, go up one level."
  (interactive "P")
  (if count
      (call-interactively 'delete-backward-char)
    (if (= (char-before) ?/)
        (progn
          (backward-delete-char 1)
          (while (and (/= (char-before) ?/)
                      (not (get-text-property (1- (point)) 'read-only)))
            (backward-delete-char 1)))
      (unless (get-text-property (1- (point)) 'read-only)
        (call-interactively 'delete-backward-char)))))

(defun lusty--define-mode-map ()
  ;; Re-generated every run so that it can inherit new functions.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "RET") #'lusty-open-this)
    (define-key map "\t" #'lusty-select-match)
    (define-key map "\C-y" #'lusty-yank)
    (define-key map [remap delete-backward-char] #'lusty-delete-backward)

    (define-key map "\C-n" #'lusty-highlight-next)
    (define-key map "\C-p" #'lusty-highlight-previous)
    (define-key map "\C-s" #'lusty-highlight-next)
    (define-key map "\C-r" #'lusty-highlight-previous)
    (define-key map "\C-f" #'lusty-highlight-next-column)
    (define-key map "\C-b" #'lusty-highlight-previous-column)

    (define-key map (kbd "<left>") #'lusty-highlight-previous-column)
    (define-key map (kbd "<right>") #'lusty-highlight-next-column)
    (define-key map (kbd "<up>") #'lusty-highlight-previous)
    (define-key map (kbd "<down>") #'lusty-highlight-next)

    (define-key map "\C-xd" #'lusty-launch-dired)
    (define-key map "\C-xe" #'lusty-select-current-name)
    ;; ###PRF
    (maphash (lambda (k v) (define-key map (kbd k) v))
             lusty--custom-explorer-actions-keys)
    (setq lusty-mode-map map))
  (run-hooks 'lusty-setup-hook))


(defun lusty--run (read-fn &rest args)
  (let ((lusty--highlighted-coords (cons 0 0))
        (lusty--matches-matrix (make-vector 0 nil))
        (lusty--matrix-column-widths '())
        (lusty--matrix-truncated-p nil))
    (add-hook 'post-command-hook #'lusty--post-command-function t)
    (unwind-protect
        (save-window-excursion
          (apply read-fn lusty-prompt args))
      (remove-hook 'post-command-hook #'lusty--post-command-function)
      (setq lusty--previous-minibuffer-contents nil
            lusty--initial-window-config nil
            lusty--current-idle-timer nil))))


;;
;; Start LiquidMetal
;;
;; Port of Ryan McGeary's LiquidMetal fuzzy matching algorithm found at:
;;   http://github.com/rmm5t/liquidmetal/tree/master.
;;

(defmacro lusty--LM-score-no-match () 0.0)
(defmacro lusty--LM-score-match () 1.0)
(defmacro lusty--LM-score-trailing () 0.8)
(defmacro lusty--LM-score-trailing-but-started () 0.90)
(defmacro lusty--LM-score-buffer () 0.85)

(cl-defun lusty-LM-score (str abbrev)
  (let ((str-len (length str))
        (abbrev-len (length abbrev)))
    (cond ;((string= abbrev "")  ; Disabled; can't happen in practice
                                        ; (lusty--LM-score-trailing))
     ((> abbrev-len str-len)
      (lusty--LM-score-no-match))
     (t
      ;; Content of LM--build-score-array...
      ;; Inline for interpreted performance.
      (let* ((scores (make-vector str-len (lusty--LM-score-no-match)))
             (str-test (if lusty-case-fold (downcase str) str))
             (abbrev-test (if lusty-case-fold (downcase abbrev) abbrev))
             (last-index 0)
             (started-p nil))
        (dotimes (i abbrev-len)
          (let ((pos (cl-position (aref abbrev-test i) str-test
                                  :start last-index
                                  :end str-len)))
            (when (null pos)
              (cl-return-from lusty-LM-score (lusty--LM-score-no-match)))
            (when (zerop pos)
              (setq started-p t))
            (cond ((and (cl-plusp pos)
                        (memq (aref str (1- pos))
                              '(?. ?_ ?- ?\ )))
                   ;; New word.
                   (aset scores (1- pos) (lusty--LM-score-match))
                   (cl-fill scores (lusty--LM-score-buffer)
                            :start last-index
                            :end (1- pos)))
                  ((and (>= (aref str pos) ?A)
                        (<= (aref str pos) ?Z))
                   ;; Upper case.
                   (cl-fill scores (lusty--LM-score-buffer)
                            :start last-index
                            :end pos))
                  (t
                   (cl-fill scores (lusty--LM-score-no-match)
                            :start last-index
                            :end pos)))
            (aset scores pos (lusty--LM-score-match))
            (setq last-index (1+ pos))))

        (let ((trailing-score
               (if started-p
                   (lusty--LM-score-trailing-but-started)
                 (lusty--LM-score-trailing))))
          (cl-fill scores trailing-score :start last-index))

        (/ (cl-reduce '+ scores)
           str-len ))))))
(defalias 'LM-score 'lusty-LM-score)  ;; deprecated

;;
;; End LiquidMetal
;;

(provide 'lusty-explorer)

;;; lusty-explorer.el ends here.
