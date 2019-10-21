
(require 'buffer-grid)

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



;; MAJOR MODE

(ediff-defvar-local prf-multi-buffer-mode--buffer-list nil
  "List of buffers handled by current multi-buffer session")

(defun prf-multi-buffer-mode--catch-all-key-fun ()
  (interactive)
  (let (key)

    (setq key
          (cond
           ((symbolp last-input-event) last-input-event)
           (t (key-binding (vector last-input-event)))))

    (message "Caught %S" key)
    (prf-multi-buffer-edit--play-key-multi prf-multi-buffer-mode--buffer-list key)
    ))

(defvar prf-multi-buffer-mode-map nil "Keymap for `prf-multi-buffer-mode'")

(define-derived-mode prf-multi-buffer-mode fundamental-mode "prf-multi-buffer"
  "major mode for editing multiple buffers at once."

  (setq prf-multi-buffer-mode-map (make-sparse-keymap))
  (define-key prf-multi-buffer-mode-map [remap self-insert-command] #'prf-multi-buffer-mode--self-insert-command)
  (define-key prf-multi-buffer-mode-map (kbd "C-h k") #'helpful-key)
  (define-key prf-multi-buffer-mode-map (kbd "C-h v") #'helpful-variable)
  (define-key prf-multi-buffer-mode-map (kbd "C-h o") #'helpful-at-point)
  ;; (define-key prf-multi-buffer-mode-map [t] #'prf-multi-buffer-mode--catch-all-key-fun)
  )





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
          (ediff-unique-buffer-name "*Multi Buffer" "*"))
         (control-buffer (ediff-with-current-buffer (car buf-list)
                           (get-buffer-create control-buffer-name)))
         (i 0)
         wind-A
         wind-width wind-height
         nb-cells nb-cols nb-rows)
    (ediff-with-current-buffer control-buffer
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
    (ediff-select-lowest-window)

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
      (skip-chars-forward ediff-whitespace))

    (other-window 1)

    (buffer-grid-diplay buf-list nil prf-multi-buffer--max-columns)

    (ediff-select-lowest-window)))



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
    (prf-multi-buffer-edit--insert-all key prf-multi-buffer-mode--buffer-list)
    ))

(defun prf-multi-buffer-edit--insert (buffer text)
  (with-current-buffer buffer
    (insert text)))

(defun prf-multi-buffer-edit--insert-all (buffer-list text)
  (let ((predicate (lambda (x) (prf-multi-buffer-edit--insert x text))))
    (mapc predicate buffer-list)))

(defun prf-multi-buffer-edit--forward-line (buffer &optional n)
  (with-current-buffer buffer
    (forward-line n)))

(defun prf-multi-buffer-edit--next-line (buffer &optional arg try-vscroll)
  (with-current-buffer buffer
    (next-line &optional arg try-vscroll)))



;; IDEA is to have something like ediff buffer to enter multi-buffer editing mode


;; https://emacs.stackexchange.com/questions/44093/edit-simultaneously-many-buffers-and-save-all-of-them-after-edit
;; https://nicolas.petton.fr/blog/mutli-occur-on-projects.html

;; https://github.com/magnars/multifiles.el

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Two_002dColumn.html
;; https://emacs.stackexchange.com/questions/47200/copy-paste-text-among-split-window-buffers

;; catch all keys function mode map
;; https://stackoverflow.com/a/13215367


(provide 'prf-multi-buffer-edit)
